-module(porter_svr).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, get_source_port/3,
        return_source_port/4,
        age_source_port/4,
        set_port_range/1,
        flush_aging_ports/0,
        is_pooled/4,
        num_pooled/3]).

-define(AgingFlushInterval, 30000).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    PortRange = application:get_env(porter, ip_local_port_range, {1024, 65535}),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [PortRange], []).

set_port_range(PortRange) ->
    % Note: does not attempt to re-map ports, or update existing ranges
    gen_server:cast(?SERVER, {set_port_range, PortRange}).

flush_aging_ports() ->
    gen_server:call(?SERVER, {flush_aging_ports}, infinity).

get_source_port(IP, Port, Opts) ->
    % Get an ephemeral port number
    gen_server:call(?SERVER, {get_source_port, IP, Port, Opts}, infinity).

return_source_port(_IP, _Port, 0, _Opts) ->
    ok;
return_source_port(IP, Port, SourcePort, Opts) ->
    % Return an ephemeral port number back to the pool immediately
    gen_server:cast(?SERVER, {return_source_port, IP, Port, SourcePort, Opts}).

age_source_port(_IP, _Port, 0, _Opts) ->
    ok;
age_source_port(IP, Port, SourcePort, Opts) ->
    % Put an ephemeral port into aging, to be returned to the pool later
    gen_server:cast(?SERVER, {age_source_port, IP, Port, SourcePort, Opts}).

is_pooled(IP, Port, SourcePort, Opts) ->
    gen_server:call(?SERVER, {is_pooled, IP, Port, SourcePort, Opts}, infinity).

num_pooled(IP, Port, Opts) ->
    gen_server:call(?SERVER, {num_pooled, IP, Port, Opts}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([PortRange]) ->
    % The AddrMap is a mapping of IP address to the ets table for that addr
    AddrMap = ets:new(porter_addr_map, [public, named_table]),
    State = #{
           port_range => PortRange,
           addr_map => AddrMap },
    schedule_aging_flush(),
    {ok, State}.

handle_call({get_source_port, IP, Port, _Opts}, _From, State) ->
    Pool = get_addr_pool(IP, Port, State),
    case ets:first(Pool) of
        '$end_of_table' ->
            {reply, {error, exhausted}, State};
        SourcePort ->
            ets:delete(Pool, SourcePort),
            {reply, {ok, SourcePort}, State}
    end;
handle_call({is_pooled, IP, Port, SourcePort, _Opts}, _From, State) ->
    Pool = get_addr_pool(IP, Port, State),
    case ets:lookup(Pool, SourcePort) of
        [] ->
            {reply, false, State};
        _ ->
            {reply, true, State}
    end;
handle_call({num_pooled, IP, Port, _Opts}, _From, State) ->
    Pool = get_addr_pool(IP, Port, State),
    {reply, ets:info(Pool, size), State};
handle_call({flush_aging_ports}, _From, State) ->
    flush_all(State),
    {reply, ok, State}.

handle_cast({set_port_range, PortRange}, State) ->
    {noreply, State#{port_range => PortRange}};
handle_cast({return_source_port, IP, Port, SourcePort, _Opts}, State) ->
    Pool = get_addr_pool(IP, Port, State),
    ets:insert(Pool, pool_entry(SourcePort)),
    {noreply, State};
handle_cast({age_source_port, IP, Port, SourcePort, _Opts}, State) ->
    AgingPool = get_addr_aging_pool(IP, Port, State),
    ets:insert(AgingPool, pool_entry(SourcePort)),
    {noreply, State}.

handle_info(aging_flush, State) ->
    case do_aging_flush(State) of
        [] ->
            schedule_aging_flush();
        _Remaining ->
            self() ! aging_flush
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_addr_pool(IP, Port, #{port_range := {PortMin, PortMax},
                          addr_map := Map}) ->
    case ets:lookup(Map, {pool, IP, Port}) of
        [{_, Pool}] ->
            Pool;
        [] ->
            Pool = ets:new(addr_pool, [public]),
            ets:insert(Map, map_entry({pool, IP, Port}, Pool)),
            Ports = lists:seq(PortMin, PortMax),
            ets:insert(Pool, [pool_entry(X) || X <- Ports]),
            Pool
    end.

get_addr_aging_pool(IP, Port, #{addr_map := Map}) ->
    case ets:lookup(Map, {aging, IP, Port}) of
        [{_, AgingPool}] ->
            AgingPool;
        [] ->
            AgingPool = ets:new(addr_aging_pool, [public]),
            ets:insert(Map, map_entry({aging, IP, Port}, AgingPool)),
            AgingPool
    end.

map_entry({Type, IP, Port}, Pool) ->
    {{Type, IP, Port}, Pool}.

pool_entry(Port) ->
    {Port}.

schedule_aging_flush() ->
    timer:send_after(?AgingFlushInterval, self(), aging_flush).

flush_all(State) ->
    case do_aging_flush(State) of
        [] ->
            ok;
        _ ->
            flush_all(State)
    end.

do_aging_flush(State=#{addr_map := Map}) ->
    AgingPools = ets:foldl(fun(X, A) ->
                case X of
                    {{aging, _IP, _Port}, _Pool} ->
                        [X|A];
                    _ ->
                        A
                end
        end, [], Map),
    do_aging_flush(AgingPools, State).

do_aging_flush(AgingPools, State=#{addr_map := Map}) ->
    if length(AgingPools) > 0 ->
            FlushEntry = lists:nth(rand:uniform(length(AgingPools)), AgingPools),
            RemainingAgingPools = AgingPools -- [FlushEntry],
            FlushId = element(1, FlushEntry),
            FlushPool = element(2, FlushEntry),
            {IP, Port} = {element(2, FlushId), element(3, FlushId)},
            Pool = get_addr_pool(IP, Port, State),
            ets:insert(Pool, ets:tab2list(FlushPool)),
            ets:delete(Map, FlushId),
            ets:delete(FlushPool),
            true = length(RemainingAgingPools) < length(AgingPools),
            RemainingAgingPools;
        true ->
            []
    end.
