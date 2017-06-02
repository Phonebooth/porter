-module(gen_tcp_tester).

-export([init/0, uninit/0, set_mock/3, connect/4]).

init() ->
    ets:new(?MODULE, [public, named_table]).

uninit() ->
    ets:delete(?MODULE).

set_mock(connect, N, Resp) ->
    ets:insert(?MODULE, {connect, {N, Resp}}).

connect(IP, Port, Opts, Timeout) ->
    case ets:lookup(?MODULE, connect) of
        [{_, {0, _}}] ->
            gen_tcp:connect(IP, Port, Opts, Timeout);
        [{_, {N, Resp}}] ->
            set_mock(connect, N-1, Resp),
            Resp;
        [] ->
            gen_tcp:connect(IP, Port, Opts, Timeout)
    end.
