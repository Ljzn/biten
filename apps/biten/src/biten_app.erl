-module(biten_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% API for easy startup
-export([start/0]).

%% ===================================================================
%% API
%% ===================================================================

start() ->
    io:format("NETWORK: ~s~n", [os:getenv("BITEN_NETWORK")]),
    application:start(biten).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    biten_sup:start_link().

stop(_State) ->
    ok.
