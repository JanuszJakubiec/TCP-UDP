-module(server_chat_tcp_message_sender).

-behaviour(gen_server).

-export([start/0, init/1, handle_cast/2, send_message_to_all/2]).

-spec start() -> chat_server_app:gen_server_start().
start() ->
    gen_server:start({local, server_chat_tcp_message_sender}, ?MODULE, [], []).

-spec init(any()) -> {ok, nil()}.
init(_) ->
    {ok, []}.

-spec handle_cast({atom(), [any()]}, any()) -> {noreply, any()}.
handle_cast({Function, Args}, State) ->
    {ok, NewState} = ?MODULE:Function(Args, State),
    {noreply, NewState}.

-spec send_message_to_all([[socket:socket()] | binary()], any()) -> {ok, any()}.
send_message_to_all([SocketList, Message], State) ->
    lists:foreach(fun (Socket) ->
        send_data(Socket, Message)
    end, SocketList),
    {ok, State}.

-spec send_data(socket:socket(), binary()) -> ok.
send_data(Socket, Message) ->
    case socket:send(Socket, Message) of
        {ok, Data} -> send_data(Socket, Data);
        _ -> ok
    end.
