-module(client_list_manager).

-export([start/0, init/1, handle_cast/2, handle_call/3, add_client_tcp/2, delete_client_tcp/2,
         get_clients_tcp/2, add_client_udp/2, delete_client_udp/2, get_clients_udp/2]).

-behaviour(gen_server).

-type tcp_clients() :: #{integer() => socket:socket()}.
-type udp_clients() :: [socket:sockaddr_recv()].

-spec start() -> chat_server_app:gen_server_start().
start() ->
    gen_server:start({local, client_list_manager}, ?MODULE, [[]], []).

-spec init(any()) -> {ok, {#{}, []}}.
init(_) ->
    {ok, {#{}, []}}.

-spec handle_cast({modify_state, atom(), [any()]}, {tcp_clients(), udp_clients()}) ->
    {noreply, {tcp_clients(), udp_clients()}}.
handle_cast({modify_state, Function, Args}, State) ->
    {ok, NewState} = ?MODULE:Function(Args, State),
    {noreply, NewState}.

-spec handle_call({get_state, atom(), [any()]}, {pid(), _}, {tcp_clients(), udp_clients()}) ->
    {reply, any(), {tcp_clients(), udp_clients()}}.
handle_call({get_state, Function, Args}, _From, State) ->
    Response = ?MODULE:Function(Args, State),
    {reply, Response, State}.

-spec add_client_tcp([integer() | socket:socket()], {tcp_clients(), udp_clients()}) ->
    {ok, {tcp_clients(), udp_clients()}}.
add_client_tcp([ClientId, Socket], {State, StateUdp}) ->
    {ok, {maps:put(ClientId, Socket, State), StateUdp}}.

-spec delete_client_tcp([integer()], {tcp_clients(), udp_clients()}) ->
    {ok, {tcp_clients(), udp_clients()}}.
delete_client_tcp([ClientId], {State, StateUdp}) ->
    {ok, {maps:remove(ClientId, State), StateUdp}}.

-spec get_clients_tcp([integer()], {tcp_clients(), udp_clients()}) ->
    {ok, [socket:socket()]}.
get_clients_tcp([ClientId], {State, _}) ->
    {ok, [Socket || {Key, Socket} <- maps:to_list(State), Key =/= ClientId]}.

-spec add_client_udp([socket:sockaddr_recv()], {tcp_clients(), udp_clients()}) ->
    {ok, {tcp_clients(), udp_clients()}}.
add_client_udp([ClientAddress], {State, StateUdp}) ->
    case lists:search(fun (Element) -> compare_elements(Element, ClientAddress) end, StateUdp) of
        false -> {ok, {State, StateUdp ++ [ClientAddress]}};
        _ -> {ok, {State, StateUdp}}
    end.

-spec delete_client_udp([integer()], {tcp_clients(), udp_clients()}) ->
    {ok, {tcp_clients(), udp_clients()}}.
delete_client_udp([ClientAddress], {State, StateUdp}) ->
    {ok, {State, lists:subtract(StateUdp, [ClientAddress])}}.

-spec get_clients_udp([integer()], {tcp_clients(), udp_clients()}) ->
    {ok, [socket:socket()]}.
get_clients_udp([ClientAddress], {_, StateUdp}) ->
    {ok, [Address || Address <- StateUdp, Address =/= ClientAddress]}.

compare_elements(Element, Element) -> true;
compare_elements(_, _) -> false.
