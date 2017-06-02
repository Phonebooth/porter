-module(porter_tests).
-include_lib("eunit/include/eunit.hrl").

-define(LPort, 56789).
-define(RangeFirst, 2000).
-define(RangeLast, 2009).

start() ->
    porter:start(),
    gen_tcp_tester:init(),
    porter_svr:set_port_range({?RangeFirst, ?RangeLast}),
    application:set_env(porter, retry_policy, [{attempts, 5}]),
    {ok, LSock} = gen_tcp:listen(?LPort, []),
    LSock.

stop(LSock) ->
    gen_tcp:close(LSock),
    gen_tcp_tester:uninit(),
    porter:stop().

conect_test_() ->
    {setup,
        fun start/0,
        fun stop/1,
        fun connect_test/1}.

connect_test(_Fixture) ->
    AllPooled = fun() ->
         TotalPooled = 1 + ?RangeLast - ?RangeFirst,
         TotalPooled = porter_svr:num_pooled({127,0,0,1}, ?LPort, [])
    end,

    PortAgingTest = fun() ->
        Opts = [],
        {ok, Socket, State} = porter:connect(gen_tcp, {127, 0, 0, 1}, ?LPort, Opts),
        {ok, MySrcPort} = inet:port(Socket),
        false = porter_svr:is_pooled({127,0,0,1}, ?LPort, MySrcPort, Opts),
        porter:close(gen_tcp, Socket, State),
        true = porter_svr:is_pooled({127,0,0,1}, ?LPort, MySrcPort, Opts)
    end,
    RetryTest = fun() ->
        Opts = [],
        gen_tcp_tester:set_mock(connect, 2, {error, eaddrnotavail}),
        {ok, _Socket, State} = porter:connect(gen_tcp_tester, {127,0,0,1}, ?LPort, Opts),
        porter:on_closed(State),
        porter_svr:flush_aging_ports()
    end,
    RetryFailTest = fun() ->
        Opts = [],
        gen_tcp_tester:set_mock(connect, 8, {error, eaddrnotavail}),
        {error, eaddrnotavail} = porter:connect(gen_tcp_tester, {127,0,0,1}, ?LPort, Opts),
        porter_svr:flush_aging_ports()
    end,
    [
       AllPooled
      ,PortAgingTest
      ,AllPooled
      ,RetryTest
      ,AllPooled
      ,RetryFailTest
      ,AllPooled
  ].
