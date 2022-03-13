-module(client_chat_udp_listener).

-export([start/0, init/1, handle_continue/2, terminate/2]).

-behaviour(gen_server).

-type continue() :: {noreply, socket:socket(), {continue, nil()}}.
-type stop() :: {stop, chat_client_app:stop_reason(), nil()}.

-spec start() -> chat_client_app:gen_server_start().
start() ->
    gen_server:start_link({local, client_chat_udp_listener}, ?MODULE, {}, []).

-spec init(any()) -> {ok, socket:socket(), {continue, nil()}}.
init(_) ->
    [{udp_socket, {Socket, ServerIp, ServerPort}}] = ets:lookup(environment_variables, udp_socket),
    socket:sendto(Socket, <<1>>, #{family => inet, port => ServerPort, addr => ServerIp}),
    {ok, Socket, {continue, []}}.

-spec handle_continue(any(), socket:socket()) ->
    continue() | stop().
handle_continue(_, Socket) ->
    case socket:recvfrom(Socket) of
        {ok, {_, Data}} ->
            io:format(unicode:characters_to_list(Data, unicode)),
            io:format("\n"),
            {noreply, Socket, {continue, []}};
        {error, Reason} ->
            socket:close(Socket),
            {stop, Reason, []}
    end.

-spec terminate(any(), any()) ->
    ok.
terminate(_Reason, _State) ->
    ok.
