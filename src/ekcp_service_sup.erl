-module(ekcp_service_sup).

-behaviour(supervisor).

-include("ekcp.hrl").

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

start_child(Port, Handler, UDPOpts, EKCPConf) ->
    supervisor:start_child(?MODULE, [Port, Handler, UDPOpts, EKCPConf]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(_) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 5,
                 period => 10},
    ServiceSpec = #{id => ekcp_service,
                    start => {ekcp_service, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [ekcp_service]},
    {ok, {SupFlags, [ServiceSpec]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
