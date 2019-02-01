%% do fancy things in this system
-module(monkey).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Use module name for registered process
-define(SERVER, ?MODULE).

%%% ==========================================================================
%%% API
%%% ==========================================================================

%% @doc Start bootstrap process and register it
-spec start_link() -> pid().
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop statistics process
stop() ->
    gen_server:cast(?SERVER, stop).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
-record(state, {none}).

init([]) ->
    gen_server:cast(?SERVER, go),
    {ok, #state{}}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast(go, S) ->
    timer:sleep(timer:seconds(30)),
    stat:print(),
    gen_server:cast(?SERVER, go),
    {noreply, S};

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Msg, S) ->
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

code_change(_oldVersion, State, _Extra) ->
    {ok, State}.

%%% ===========================================
%%% Local functions
%%% ===========================================