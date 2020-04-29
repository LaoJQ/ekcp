-module(ekcp_service).

-behaviour(gen_server).

-include("ekcp.hrl").

%% API
-export([
         start_link/4,
         new_service/2, new_service/4,
         new_session/4,
         new_service_session/4, new_service_session/6
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-define(DEFAULT_OPTS,
        [
         {mode, binary},
         {active, true}
        ]).

-record(state,
        {
         port,
         socket,
         handler,
         ekcp_conf,
         conv_map, %% ConvId => #conv{}
         sess_map %% SessPid => ConvId
        }).

-record(conv,
        {
         sess_pid,
         rmt_ip,
         rmt_port
        }).

%%%===================================================================
%%% API
%%%===================================================================

start_link(0, Handler, UDPOpts, EKCPConf) ->
    gen_server:start_link(?MODULE, [0, Handler, UDPOpts, EKCPConf], []);
start_link(Port, Handler, UDPOpts, EKCPConf) ->
    gen_server:start_link(name(Port), ?MODULE, [Port, Handler, UDPOpts, EKCPConf], []).


%% Start a new service process in Port.
%% Return: {ok, ServPid} | term().
new_service(Port, Handler) ->
    new_service(Port, Handler, [], #ekcp_conf{}).

new_service(Port, Handler, UDPOpts, EKCPConf) ->
    ekcp_service_sup:start_child(Port, Handler, UDPOpts, EKCPConf).


%% Start a session in special port service process.
%% Return: {ok, SessPid} | false.
new_session(Port, ConvId, RmtIp, RmtPort) when Port =/= 0 ->
    gen_server:call(name(Port), {new_session, ConvId, RmtIp, RmtPort}).


%% Start a client connection (new service process and new session process) in random port.
%% Return: {ok, SessPid} | false.
new_service_session(ConvId, RmtIp, RmtPort, Handler) ->
    new_service_session(ConvId, RmtIp, RmtPort, Handler, [], #ekcp_conf{}).

new_service_session(ConvId, RmtIp, RmtPort, Handler, UDPOpts, EKCPConf) ->
    case ekcp_service_sup:start_child(0, Handler, UDPOpts, EKCPConf) of
        {ok, ServicePid} ->
            case catch gen_server:call(ServicePid, {new_session, ConvId, RmtIp, RmtPort}) of
                {ok, SessPid} ->
                    {ok, SessPid};
                _ ->
                    false
            end;
        _ ->
            false
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port, Handler, UDPOpts, EKCPConf]) ->
    process_flag(trap_exit, true),
    case gen_udp:open(Port, udp_opts(UDPOpts)) of
        {ok, Socket} ->
            {ok, #state{
                    port = Port,
                    socket = Socket,
                    handler = Handler,
                    ekcp_conf = EKCPConf,
                    conv_map = maps:new(),
                    sess_map = maps:new()
                   }};
        {error, _Error} ->
            {stop, normal}
    end.

handle_call({new_session, ConvId, RmtIp, RmtPort}, _From, State) ->
    case handle_new_session(ConvId, RmtIp, RmtPort, State) of
        false ->
            {reply, false, State};
        {ok, SessPid, NewState} ->
            {reply, {ok, SessPid}, NewState}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({udp, _Socket, Ip, Port, Packet},
            #state{
               conv_map = ConvMap
              } = State)
  when size(Packet) >= ?EKCP_HEADER_SIZE ->
    case parse_packet(Packet, ConvMap) of
        false ->
            {noreply, State};
        {ok, ConvId} ->
            case handle_new_session(ConvId, Ip, Port, State) of
                false ->
                    {noreply, State};
                {ok, SessPid, NewState} ->
                    SessPid ! {ekcp_input, Packet},
                    {noreply, NewState}
            end;
        #conv{sess_pid = SessPid} ->
            SessPid ! {ekcp_input, Packet},
            {noreply, State}
    end;
handle_info({ekcp_output, Packet},
            #state{
               socket = Socket,
               conv_map = ConvMap
              } = State) ->
    case parse_packet(Packet, ConvMap) of
        #conv{rmt_ip = Ip, rmt_port = Port} ->
            gen_udp:send(Socket, Ip, Port, Packet);
        _ ->
            ignore
    end,
    {noreply, State};
handle_info({'EXIT', Pid, _Reason},
            #state{
               conv_map = ConvMap,
               sess_map = SessMap
              } = State) ->
    case maps:get(Pid, SessMap, undefined) of
        undefined ->
            {noreply, State};
        ConvId ->
            {noreply, State#state{
                        conv_map = maps:remove(ConvId, ConvMap),
                        sess_map = maps:remove(Pid, SessMap)
                       }}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    catch gen_udp:close(State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

udp_opts(Custom) ->
    lists:foldl(fun(C, DAcc) when is_tuple(C) ->
                        lists:keysort(element(1, C), 1, DAcc, C);
                   (C, DAcc) ->
                        [C | lists:delete(C, DAcc)]
                end, ?DEFAULT_OPTS, Custom).

name(Port) ->
    {via, ekcp_app, {?SERVER, Port}}.

parse_packet(Packet, ConvMap) ->
    case catch ekcp:getconv(Packet) of
        {'EXIT', _} ->
            false;
        ConvId ->
            maps:get(ConvId, ConvMap, {ok, ConvId})
    end.

handle_new_session(ConvId, RmtIp, RmtPort,
                   #state{
                      handler = Handler,
                      ekcp_conf = EKCPConf,
                      conv_map = ConvMap,
                      sess_map = SessMap
                     } = State) ->
    case maps:is_key(ConvId, ConvMap) of
        true ->
            false;
        false ->
            case ekcp_session_sup:start_child(ConvId, self(), Handler, EKCPConf) of
                {ok, SessPid} ->
                    link(SessPid),
                    NewConv = #conv{
                                 sess_pid = SessPid,
                                 rmt_ip = RmtIp,
                                 rmt_port = RmtPort
                                },
                    {ok, SessPid, State#state{
                                    conv_map = maps:put(ConvId, NewConv, ConvMap),
                                    sess_map = maps:put(SessPid, ConvId, SessMap)
                                   }};
                _ ->
                    false
            end
    end.
