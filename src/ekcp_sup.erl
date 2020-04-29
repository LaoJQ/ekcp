-module(ekcp_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 10},
    ServiceSupSpec = #{id => ekcp_service_sup,
                       start => {ekcp_service_sup, start_link, []},
                       restart => permanent,
                       shutdown => 5000,
                       type => supervisor,
                       modules => [ekcp_service]},
    SessionSupSpec = #{id => ekcp_session_sup,
                       start => {ekcp_session_sup, start_link, []},
                       restart => permanent,
                       shutdown => 5000,
                       type => supervisor,
                       modules => [ekcp_session_sup]},
    {ok, {SupFlags, [ServiceSupSpec, SessionSupSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
