-module(biten_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD_T(I, Type), {I, {I, start_link, []}, temporary, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {{one_for_one, 5, 10}, [
             {simplest_httpserver, {simplest_httpserver, start, [8888]}, permanent, 5000, worker, [simplest_httpserver]},
             ?CHILD(peer_sup, supervisor),
             ?CHILD(stat, worker),
             ?CHILD(netmanager, worker),
             ?CHILD(peerdiscovery, worker),
             ?CHILD(mempool, worker),
             ?CHILD(accepter, worker),
             ?CHILD(chain, worker),
             ?CHILD(monkey, worker),
             ?CHILD_T(bootstrap, worker)
         ]}}.

