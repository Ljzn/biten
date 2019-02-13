-module(simplest_httpserver).
-export([start/2]).

%% start a httpserver, show the iolist Data
start(Port, Data) ->
    Pid = spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
                    loop(Sock, Data) end),
    {ok, Pid}.

loop(Sock, Data) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn, Data) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock, Data).

handle(Conn, _Data) ->
    %% FIXME everytime new request arrive, send monkey to call veriplist
    Data1 = [io_lib:format("~s, ~s~n", [V, inet:ntoa(IP)]) || {IP, V} <- ets:tab2list(version_ip)],
    io:format("verip request~n", []),
    gen_tcp:send(Conn, response(Data1)),
    gen_tcp:close(Conn).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nAccess-Control-Allow-Origin: *\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).
