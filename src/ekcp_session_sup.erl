-module(ekcp_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(ConvId, UdpPid, Handler, EKCPConf) ->
    supervisor:start_child(?MODULE, [ConvId, UdpPid, Handler, EKCPConf]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 10},
    SessionSpec = #{id => ekcp_session,
                    start => {ekcp_session, start_link, []},
                    restart => temporary,
                    shutdown => 5000,
                    type => worker,
                    modules => [ekcp_session]},
    {ok, {SupFlags, [SessionSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
