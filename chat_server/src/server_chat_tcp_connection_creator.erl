-module(server_chat_tcp_connection_creator).

-export([start/0, init/1, handle_continue/2, terminate/2]).

-behaviour(gen_server).

-spec start() -> chat_server_app:gen_server_start().
start() ->
    gen_server:start_link({local, server_chat_tcp_connection_crator}, ?MODULE, {}, []).

-spec init(any()) -> {ok, {socket:socket(), integer()}, {continue, nil()}}.
init(_) ->
    [{server_tcp_socket, Socket}] = ets:lookup(environment_variables, server_tcp_socket),
    {ok, {Socket, 0}, {continue, []}}.

-spec handle_continue(any(), {socket:socket(), integer()}) ->
    {noreply, {socket:socket(), integer()}, {continue, nil()}}.
handle_continue(_, {Socket, ClientId}) ->
    {ok, NewSocket} = socket:accept(Socket),
    gen_server:cast(client_list_manager, {modify_state, add_client_tcp, [ClientId, NewSocket]}),
    server_chat_tcp_listener:start({NewSocket, ClientId}),
    io:format(<<"NEW CONNECTION\n">>),
    {noreply, {Socket, ClientId + 1}, {continue, []}}.

-spec terminate(any(), any()) -> ok.
terminate(_Reason, _State) ->
    ok.
