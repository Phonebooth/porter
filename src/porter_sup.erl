-module(porter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one},
    DefaultSvr = #{id => porter_svr,
        start => {porter_svr, start_link, []}},
    ChildSpecs = [DefaultSvr],
    {ok, { SupFlags, ChildSpecs } }.

