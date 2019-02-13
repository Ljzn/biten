%% do fancy things in this system
-module(monkey).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add_node/2, remove_node/1, print_verip/0]).

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

add_node(IP, V) ->
    gen_server:cast(?SERVER, {add_node, IP, V}).

remove_node(IP) ->
    gen_server:cast(?SERVER, {remove_node, IP}).

print_verip() ->
    gen_server:cast(?SERVER, print_verip).

%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
%% version-ip talbe
-record(state, {verip}).

init([]) ->
    gen_server:cast(?SERVER, go),
    T = ets:new(),
    {ok, #state{verip = T}}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast({add_node, IP, V}, S) ->
    T = S#state.verip,
    ets:insert(T, {IP, V}),
    {noreply, S};

handle_cast({remove_node, IP}, S) ->
    T = S#state.verip,
    ets:delete(T, IP),
    {noreply, S};

handle_cast(print_verip, S) ->
    [io:format("~s, ~s~n", [V, IP]) || {IP, V} <- ets:tab2list(S#state.verip)];

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