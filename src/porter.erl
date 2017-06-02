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
    connect_attempt(Module, IP, Port, Opts, Timeout, undefined, ok, []).

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

connect_attempt(_Module, IP, Port, Opts, _Timeout, 0, LastResponse, BadPorts) ->
    [ porter_svr:age_source_port(IP, Port, X, Opts) || X <- BadPorts ],
    LastResponse;
connect_attempt(Module, IP, Port, Opts, Timeout, RemainingAttempts, _LastResponse, BadPorts) ->
    AgeBadPorts = fun() ->
           [ porter_svr:age_source_port(IP, Port, X, Opts) || X <- BadPorts ]
        end,
    MySrcPort = case porter_svr:get_source_port(IP, Port, Opts) of
        {ok, SrcPort} ->
            SrcPort;
        _ ->
            0
    end,
    Opts2 = lists:keystore(port, 1, Opts, {port, MySrcPort}),
    Opts3 = lists:keystore(reuseaddr, 1, Opts2, {reuseaddr, true}),
    case Module:connect(IP, Port, Opts3, Timeout) of
        NotAvail={error, eaddrnotavail} ->
            % if porter_svr gave a port and it turned out to be bad (eg TIME-WAIT)
            % we will retry a certain number of times. All the while, collecting
            % the bad ports, and at the end of the retry, put them back into aging
            case MySrcPort of
                0 ->
                    AgeBadPorts(),
                    NotAvail;
                _ ->
                    RemAttempts2 = case RemainingAttempts of
                        undefined ->
                            Policy = application:get_env(porter, retry_policy, []),
                            Attempts = proplists:get_value(attempts, Policy, 0),
                            Attempts;
                        _ ->
                            RemainingAttempts - 1
                    end,
                    connect_attempt(Module, IP, Port, Opts, Timeout, RemAttempts2,
                        NotAvail, [MySrcPort|BadPorts])
            end;
        {error, Reason} ->
            AgeBadPorts(),
            porter_svr:age_source_port(IP, Port, MySrcPort, Opts),
            {error, Reason};
        {ok, Socket} ->
            AgeBadPorts(),
            PorterState = #{ip => IP,
                            port => Port,
                            source_port => MySrcPort,
                            opts => Opts},
            {ok, Socket, PorterState}
    end.
