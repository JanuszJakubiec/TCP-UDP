-module(client_chat_sender).

-export([send_tcp/1, send_udp/1, send_udp_file/1]).

-spec send_tcp(string()) -> ok.
send_tcp(Message) ->
    [{tcp_socket, Socket}] = ets:lookup(environment_variables, tcp_socket),
    MessageBinary = unicode:characters_to_binary(Message ++ [1]),
    send_data_tcp(Socket, MessageBinary),
    ok.

-spec send_udp(string()) -> ok.
send_udp(Message) ->
    [{udp_socket, {Socket, ServerIp, ServerPort}}] = ets:lookup(environment_variables, udp_socket),
    MessageBinary = unicode:characters_to_binary(Message),
    socket:sendto(Socket, MessageBinary, #{family => inet, port => ServerPort, addr => ServerIp}),
    ok.

-spec send_data_tcp(socket:socket(), binary()) -> ok.
send_data_tcp(Socket, Message) ->
    case socket:send(Socket, Message) of
        {ok, Data} -> send_data_tcp(Socket, Data);
        _ -> ok
    end.

-spec send_udp_file(string()) -> ok.
send_udp_file(Filename) ->
    [{udp_socket, {Socket, ServerIp, ServerPort}}] = ets:lookup(environment_variables, udp_socket),
    case file:read_file(Filename) of
        {ok, Data} ->
            socket:sendto(Socket, Data, #{family => inet, port => ServerPort, addr => ServerIp});
        {error, Reason} ->
            io:format(Reason)
    end.
