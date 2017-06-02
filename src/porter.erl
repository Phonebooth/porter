-module(porter).

-export([start/0, stop/0, connect/4, connect/5, close/3, on_closed/1]).

start() -> application:start(porter).
stop() -> application:stop(porter).

connect(Module, IP, Port, Opts) ->
    connect(Module, IP, Port, Opts, infinity).

% @doc Create a tcp connection using Module (gen_tcp or ssl), to a specific
% inet4 address. porter will do the following:
%   * assign a source port for the connection from the pool
%   * add {reuseaddr, true} to the Opts provided
% With this behavior in Linux, the user of this library can re-use ephemeral
% ports when connecting to different inet4 addresses.
%
% Note: We are tracking the pool of ephemeral ports because it seems that Linux
% is not capable of smartly determining source ports to different addresses on
% its own. This is likely because in a bind-before-connect scenario, the source
% port must be determined before the kernel is made aware of the address.
connect(Module, IP={_,_,_,_}, Port, Opts, Timeout) ->
    connect_attempt(Module, IP, Port, Opts, Timeout, undefined, []).

% @doc If the client side closed the socket, the source port is immediately
% returned to the pool. This is because Linux does not put the socket in the
% TIME-WAIT state
close(Module, Socket, #{ip := IP, port := Port, source_port := MySrcPort, opts := Opts}) ->
    Res = Module:close(Socket),
    porter_svr:return_source_port(IP, Port, MySrcPort, Opts),
    Res.

% @doc When the socket is closed unexpectedly, the user of this library must
% use this function to return the source port back to the aging pool. We are
% assuming that the port is in TIME-WAIT state
on_closed(#{ip := IP, port := Port, source_port := MySrcPort, opts := Opts}) ->
    porter_svr:age_source_port(IP, Port, MySrcPort, Opts).

connect_attempt(Module, IP, Port, Opts, Timeout, 0, BadPorts) ->
    [ porter_svr:age_source_port(IP, Port, X, Opts) || X <- BadPorts ],

    % give the kernel one last chance to pick the source port
    Opts2 = modify_opts(0, Opts),
    case Module:connect(IP, Port, Opts2, Timeout) of
        {ok, Socket} ->
            MySrcPort = ensure_source_port_tracked(Module, Socket,
                IP, Port, 0, Opts),
            PorterState = #{ip => IP,
                port => Port,
                source_port => MySrcPort,
                opts => Opts},
            {ok, Socket, PorterState};
        Err ->
            Err
    end;
connect_attempt(Module, IP, Port, Opts, Timeout, RemainingAttempts, BadPorts) ->
    AgeBadPorts = fun() ->
           [ porter_svr:age_source_port(IP, Port, X, Opts) || X <- BadPorts ]
        end,
    MySrcPort = case porter_svr:get_source_port(IP, Port, Opts) of
        {ok, SrcPort} ->
            SrcPort;
        _ ->
            0
    end,
    Opts2 = modify_opts(MySrcPort, Opts),
    case Module:connect(IP, Port, Opts2, Timeout) of
        PortError={error, Reason} when Reason =:= eaddrnotavail orelse
                                       Reason =:= eaddrinuse ->
            % eaddrnotavail is returned from a connect() system call when there
            % is already a socket existing for THIS {sip, sport, dip, dport}
            % in the kernel. (could be a socket in TIME-WAIT)

            % eaddrinuse is returned from a bind() system call when there is
            % already a socket existing for a DIFFERENT {sip, sport, dip, dport}
            % in the kernel that did NOT specify the SO_REUSEADDR flag.

            % if porter_svr gave a port and it turned out to be bad (eg TIME-WAIT)
            % we will retry a certain number of times. All the while, collecting
            % the bad ports, and at the end of the retry, put them back into aging
            case MySrcPort of
                0 ->
                    AgeBadPorts(),
                    PortError;
                _ ->
                    RemAttempts2 = get_remaining_connect_attempts(RemainingAttempts),
                    connect_attempt(Module, IP, Port, Opts, Timeout, RemAttempts2,
                        [MySrcPort|BadPorts])
            end;
        {error, Reason} ->
            AgeBadPorts(),
            porter_svr:age_source_port(IP, Port, MySrcPort, Opts),
            {error, Reason};
        {ok, Socket} ->
            MySrcPort2 = ensure_source_port_tracked(Module, Socket,
                IP, Port, MySrcPort, Opts),
            AgeBadPorts(),
            PorterState = #{ip => IP,
                            port => Port,
                            source_port => MySrcPort2,
                            opts => Opts},
            {ok, Socket, PorterState}
    end.

modify_opts(Port, Opts) ->
    Opts2 = lists:keystore(port, 1, Opts, {port, Port}),
    Opts3 = lists:keystore(reuseaddr, 1, Opts2, {reuseaddr, true}),
    Opts3.

get_remaining_connect_attempts(undefined) ->
    Policy = application:get_env(porter, retry_policy, []),
    Attempts = proplists:get_value(attempts, Policy, 0),
    Attempts;
get_remaining_connect_attempts(0) ->
    0;
get_remaining_connect_attempts(RemAttempts) ->
    RemAttempts - 1.

ensure_source_port_tracked(Module, Socket, IP, Port, MySrcPort, Opts) ->
    % if MySrcPort is 0, we should get and mark the real port as in use in
    % the porter_svr, and return it as part of the PorterState
    case MySrcPort of
        0 ->
            case get_auto_port(Module, Socket) of
                {ok, ActualPort} ->
                    porter_svr:remove_source_port(IP, Port, ActualPort, Opts),
                    ActualPort;
                _ ->
                    o
            end;
        _ ->
            MySrcPort
    end.

get_auto_port(gen_tcp, Socket) ->
    inet:port(Socket);
get_auto_port(ssl, Socket) ->
    case ssl:sockname(Socket) of
        {ok, {_, Port}} ->
            {ok, Port};
        Er ->
            Er
    end.
