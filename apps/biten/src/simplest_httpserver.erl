-module(simplest_httpserver).
-export([start/1]).

%% TODO use a better http server

%% start a httpserver, show the iolist Data
start(Port) ->
    Pid = spawn(fun () -> {ok, Sock} = gen_tcp:listen(Port, [{active, false}]),
                    loop(Sock) end),
    {ok, Pid}.

loop(Sock) ->
    {ok, Conn} = gen_tcp:accept(Sock),
    Handler = spawn(fun () -> handle(Conn) end),
    gen_tcp:controlling_process(Conn, Handler),
    loop(Sock).

handle(Conn) ->
    %% FIXME everytime new request arrive, send monkey to call veriplist
    Data1 = [io_lib:format("~s, ~s~n", [V, inet:ntoa(IP)]) || {IP, V} <- ets:tab2list(version_ip)],
    gen_tcp:send(Conn, response(Data1)),
    {ok, Bin} = do_recv(Conn, []),
    ok = gen_tcp:close(Conn),
    io:format("~s~n", [Bin]).
    %% Not active close the socket
    % gen_tcp:shutdown(Conn, write).

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nAccess-Control-Allow-Origin: *\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).

do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} ->
            do_recv(Sock, [Bs, B]);
        {error, closed} ->
            {ok, list_to_binary(Bs)}
    end.