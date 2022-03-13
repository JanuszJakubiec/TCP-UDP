%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-export_type([gen_server_start/0]).

-type gen_server_start() :: {ok, pid()} | ignore | {error, any()}.

start(_StartType, [ServerIp, ServerPort]) ->
    ets:new(environment_variables, [set, protected, named_table]),
    {ok, Socket} = socket:open(inet, stream, tcp),
    {ok, SocketUdp} = socket:open(inet, dgram, udp),
    socket:bind(Socket, #{family => inet, port => ServerPort, addr => ServerIp}),
    socket:bind(SocketUdp, #{family => inet, port => ServerPort, addr => ServerIp}),
    socket:listen(Socket),
    ets:insert(environment_variables, {server_tcp_socket, Socket}),
    ets:insert(environment_variables, {server_udp_socket, SocketUdp}),
    chat_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
