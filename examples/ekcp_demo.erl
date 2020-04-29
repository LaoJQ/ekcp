-module(ekcp_demo).
%% a demo handler

-include("../include/ekcp.hrl").
-include("demo.hrl").

%% API
-export([
         start/1,
         start_server/2,
         start_clients/4
        ]).

%% ekcp_session callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%===================================================================
%%% API
%%%===================================================================

start([server | _]) ->
    Port = case init:get_argument(demo_port) of
               {ok, [[PortStr]]} ->
                   list_to_integer(PortStr);
               _ ->
                   ?SERVER_PORT
           end,
    ConfId = case init:get_argument(ekcp_conf) of
                 {ok, [[ConfIdStr]]} ->
                     list_to_integer(ConfIdStr);
                 _ ->
                     ?CLIENT_NUM
             end,
    start_server(Port, ConfId);
start([client | _]) ->
    IP = case init:get_argument(demo_ip) of
             {ok, [[IPStr]]} ->
                 IPStr;
             _ ->
                 ?SERVER_IP
         end,
    Port = case init:get_argument(demo_port) of
               {ok, [[PortStr]]} ->
                   list_to_integer(PortStr);
               _ ->
                   ?SERVER_PORT
           end,
    Num = case init:get_argument(demo_num) of
               {ok, [[NumStr]]} ->
                  list_to_integer(NumStr);
              _ ->
                  ?CLIENT_NUM
           end,
    ConfId = case init:get_argument(ekcp_conf) of
                 {ok, [[ConfIdStr]]} ->
                     list_to_integer(ConfIdStr);
                 _ ->
                     0
             end,
    start_clients(IP, Port, Num, ConfId).

start_server(Port, ConfId) ->
    case application:start(ekcp) of
        ok ->
            case ekcp_service:new_service(Port, ?MODULE, [], ekcp_conf(ConfId)) of
                {ok, _ServPid} ->
                    ok;
                Err ->
                    {fail, Err}
            end;
        Err ->
            {fail, Err}
    end.

start_clients(ServerIp, ServerPort, Num, ConfId) ->
    case application:start(ekcp) of
        ok ->
            [begin
                 case ekcp_service:new_service_session(ConvId, ServerIp, ServerPort, ?MODULE, [], ekcp_conf(ConfId)) of
                     {ok, SessPid} ->
                         SessPid ! set_client,
                         ok;
                     false ->
                         io:format("[EKCP DEMO] Start a client fail."),
                         false
                 end
             end || ConvId <- lists:seq(1, Num)];
        Err ->
            {fail, Err}
    end.

%%%===================================================================
%%% ekcp_session callbacks
%%%===================================================================

init(_) ->
    process_flag(trap_exit, true),
    {ok, #state{mode = kcp}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(set_client, State) ->
    demo_misc:set_client(State);
handle_info(send, State) ->
    demo_misc:send_handle(State);
handle_info({ekcp_recv, Data}, State) ->
    demo_misc:recv_handle(State, Data);
handle_info(latency_report, State) ->
    demo_misc:latency_report(State);
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ekcp_conf(0) ->
    #ekcp_conf{};
ekcp_conf(1) ->
    #ekcp_conf{send_nodelay = true};
ekcp_conf(2) ->
    #ekcp_conf{
       nodelay = 1,
       interval = 10,
       resend = 2,
       nc = 1,
       sndwnd = 128,
       rcvwnd = 128,
       mtu = 1400,
       send_nodelay = true,
       snd_queue_threshold = 256
      }.
