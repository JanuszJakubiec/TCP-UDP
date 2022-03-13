%%%-------------------------------------------------------------------
%% @doc chat_client public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_client_app).

-behaviour(application).

-export([start/2, stop/1]).

-export_type([continue/0, stop_reason/0, stop/0, gen_server_start/0]).

-type continue() :: {noreply, {socket:socket(), string()}, {continue, nil()}}.
-type stop_reason() :: inet:posix() | closed | {invalid, What :: term()}.
-type stop() :: {stop, stop_reason(), {string()}}.
-type gen_server_start() :: {ok, pid()} | ignore | {error, any()}.

start(_StartType, [ServerIp, ServerPort, MyIp, MyPort]) ->
    ets:new(environment_variables, [set, protected, named_table]),
    {ok, Socket} = socket:open(inet, stream, tcp),
    {ok, SocketUdp} = socket:open(inet, dgram, udp),
    case socket:bind(Socket, #{family => inet, port => MyPort, addr => MyIp}) of
        ok ->
            case socket:bind(SocketUdp, #{family => inet, port => MyPort, addr => MyIp}) of
                ok ->
                    socket:connect(Socket, #{family => inet, port => ServerPort, addr => ServerIp}),
                    ets:insert(environment_variables, {tcp_socket, Socket}),
                    ets:insert(environment_variables,
                               {udp_socket, {SocketUdp, ServerIp, ServerPort}}),
                    chat_client_sup:start_link();
                {error, Reason} ->
                    Reason
            end;
        {error, Reason} ->
            Reason
    end.

stop(_State) ->
    [{tcp_socket, Socket}] = ets:lookup(environment_variables, tcp_socket),
    [{udp_socket, {SocketUdp, _, _}}] = ets:lookup(environment_variables, udp_socket),
    socket:close(Socket),
    socket:close(SocketUdp),
    ok.

%% internal functions
