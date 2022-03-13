-module(server_chat_tcp_listener).

-export([start/1, init/1, handle_continue/2, terminate/2]).

-behaviour(gen_server).

-type continue() :: {noreply, {socket:socket(), binary(), integer(), binary()}, {continue, nil()}}.
-type stop_reason() :: inet:posix() | closed | {invalid, What :: term()}.
-type stop() :: {stop, stop_reason(), {socket:socket(), binary(), integer(), binary()}}.

-spec start({socket:socket(), integer()}) -> chat_server_app:gen_server_start().
start({Socket, Id}) ->
    gen_server:start_link(?MODULE, {Socket, list_to_binary(integer_to_list(Id) ++ ": "), Id, <<>>}, []).

-spec init({socket:socket(), binary(), integer()}) ->
    {ok, {socket:socket(), binary(), integer(), string()}, {continue, nil()}}.  init(State) ->
    {ok, State, {continue, []}}.

-spec handle_continue(_, {socket:socket(), binary(), integer(), string()}) ->
    continue() | stop().
handle_continue(_, {Socket, MyBinaryId, Id, MessageBuffer} = State) ->
    case socket:recv(Socket) of
        {ok, Data} ->
            NewMessageBuffer = process_data(Data, MessageBuffer, Id, MyBinaryId),
            {noreply, {Socket, MyBinaryId, Id, NewMessageBuffer}, {continue, []}};
        {error, _} ->
            io:format(<<"Client disconnected\n">>),
            gen_server:cast(client_list_manager, {modify_state, delete_client_tcp, [Id]}),
            {stop, normal, State}
    end.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, {Socket, _, _, _}) ->
    socket:close(Socket),
    ok.

-spec send_message_to_all(binary(), integer()) -> ok.
send_message_to_all(Message, MyId) ->
    {ok, List} = gen_server:call(client_list_manager, {get_state, get_clients_tcp, [MyId]}),
    gen_server:cast(server_chat_tcp_message_sender, {send_message_to_all,
        [List, <<Message/binary, <<1>>/binary>>]}).

-spec process_data(binary(), binary(), integer(), binary()) -> string().
process_data(Data, Buffer, MyId, MyBinaryId) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    Messages = string:split(NewBuffer, <<1>>, all),
    case lists:reverse(Messages) of
        [LastMessage] ->
            LastMessage;
        [LastMessage | Tail] ->
            lists:foreach(fun (Message) ->
                send_message_to_all(<<MyBinaryId/binary, Message/binary>>, MyId)
            end, lists:reverse(Tail)),
            LastMessage
    end.
