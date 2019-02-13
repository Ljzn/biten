%% do fancy things in this system
-module(monkey).

-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, add_node/2, remove_node/1]).

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


%%% ==========================================================================
%%% gen_server callbacks.
%%% ==========================================================================

%% process state
%% version-ip talbe
-record(state, {verip, veriplist}).

init([]) ->
    gen_server:cast(?SERVER, go),
    T = ets:new(version_ip, [named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, #state{verip = T}}.

handle_call(_Request, _From, S) ->
    {reply, ok, S}.

handle_cast({add_node, IP, {_H, P}}, S) ->
    T = S#state.verip,
    Ver = extract_version(P),
    io:format("~s~n", [Ver]),
    ets:insert(T, {IP, Ver}),
    {noreply, S};

handle_cast({remove_node, IP}, S) ->
    T = S#state.verip,
    ets:delete(T, IP),
    {noreply, S};

handle_cast(go, S) ->
    % timer:sleep(timer:seconds(30)),
    stat:print(),
    % gen_server:cast(?SERVER, go),
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

extract_version(P) ->
    P1 = binary_to_list(P),
    P2 = lists:reverse(P1),
    take_version(P2, [], 0).

take_version([$/, H|T], V, 0) ->
    take_version(T, [H|V], 1);
take_version([$/|_T], V, 1) ->
    V;
take_version([_H|T], V, 0) ->
    take_version(T, V, 0);
take_version([H|T], V, 1) ->
    take_version(T, [H|V], 1).