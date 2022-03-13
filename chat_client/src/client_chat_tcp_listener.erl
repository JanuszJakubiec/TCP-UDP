-module(client_chat_tcp_listener).

-export([start/0, init/1, handle_continue/2, terminate/2]).

-behaviour(gen_server).

-spec start() -> chat_client_app:gen_server_start().
start() ->
    gen_server:start_link({local, client_chat_tcp_listener}, ?MODULE, {}, []).

-spec init(any()) -> {ok, {socket:socket(), string()}, {continue, nil()}}.
init(_) ->
    [{tcp_socket, Socket}] = ets:lookup(environment_variables, tcp_socket),
    {ok, {Socket, ""}, {continue, []}}.

-spec handle_continue(any(), {socket:socket(), string()}) ->
    chat_client_app:continue() | chat_client_app:stop().
handle_continue(_, {Socket, Buffer}) ->
    case socket:recv(Socket) of
        {ok, Data} ->
            NewBuffer = process_data(Data, Buffer),
            {noreply, {Socket, NewBuffer}, {continue, []}};
        {error, Reason} ->
            socket:close(Socket),
            {stop, normal, {Buffer}}
    end.

-spec terminate(any(), any()) ->
    ok.
terminate(_Reason, _State) ->
    ok.

-spec process_data(binary(), string()) -> string().
process_data(Data, Buffer) ->
    ConvertedData = unicode:characters_to_list(Data, unicode),
    NewBuffer = Buffer ++ ConvertedData,
    Messages = string:split(NewBuffer, [1], all),
    case lists:reverse(Messages) of
        [LastMessage] ->
            LastMessage;
        [LastMessage | Tail] ->
            lists:reverse(Messages),
            print_messages(lists:reverse(Tail)),
            LastMessage
    end.

-spec print_messages([string()]) -> ok.
print_messages(MessagesList) ->
    lists:foreach(fun (Message) ->
        io:format(Message),
        io:format("\n")
    end, MessagesList),
    ok.
