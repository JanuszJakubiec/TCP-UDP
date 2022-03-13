%%%-------------------------------------------------------------------
%% @doc chat_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => server_chat_tcp_connection_creator,
                    restart => transient,
                    start => {server_chat_tcp_connection_creator, start, []}},
                  #{id => client_list_manager,
                    restart => transient,
                    start => {client_list_manager, start, []}},
                  #{id => server_chat_udp_listener,
                    restart => transient,
                    start => {server_chat_udp_listener, start, []}},
                  #{id => server_chat_tcp_message_sender,
                    restart => transient,
                    start => {server_chat_tcp_message_sender, start, []}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
