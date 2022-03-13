-module(server_chat_udp_listener).

-export([start/0, init/1, handle_continue/2, terminate/2]).

-behaviour(gen_server).

-type continue() :: {noreply, socket:socket(), {continue, nil()}}.
-type stop_reason() :: inet:posix() | closed | {invalid, What :: term()}.
-type stop() :: {stop, stop_reason(), nil()}.

-spec start() -> chat_server_app:gen_server_start().
start() ->
    [{server_udp_socket, Socket}] = ets:lookup(environment_variables, server_udp_socket),
    gen_server:start_link(?MODULE, Socket, []).

-spec init(socket:socket()) ->
    {ok, socket:socket(), {continue, nil()}}.
init(Socket) ->
    {ok, Socket, {continue, []}}.

-spec handle_continue(_, socket:socket()) ->
    continue() | stop().
handle_continue(_, Socket) ->
    case socket:recvfrom(Socket) of
        {ok, {Address, <<1>>}} ->
            gen_server:cast(client_list_manager, {modify_state, add_client_udp, [Address]}),
            {noreply, Socket, {continue, []}};
        {ok, {Address, Data}} ->
            send_message_to_all(Data, Address, Socket),
            {noreply, Socket, {continue, []}};
        {error, Reason} ->
            {stop, normal, [Reason]}
    end.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, {Socket, _, _, _}) ->
    socket:close(Socket),
    ok.

-spec send_message_to_all(binary(), socket:sockaddr_recv(), socket:socket()) -> ok.
send_message_to_all(Message, Address, Socket) ->
    {ok, List} = gen_server:call(client_list_manager, {get_state, get_clients_udp, [Address]}),
    lists:foreach(fun (ReceiverAddress) ->
        socket:sendto(Socket, Message, ReceiverAddress)
    end, List).
