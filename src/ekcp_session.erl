-module(ekcp_session).

-behaviour(gen_server).

-include("ekcp.hrl").

%% API
-export([start_link/4, new/4, send/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ConvId, UdpPid, Handler, EKCPConf) ->
    gen_server:start_link(?MODULE, [ConvId, UdpPid, Handler, EKCPConf], []).

new(Port, ConvId, RmtIp, RmtPort) ->
    gen_server:call(ekcp_service:name(Port), {new_session, ConvId, RmtIp, RmtPort}).

%% Send data from application layer to kcp layer.
-spec send(Packet) -> Ret when
      Packet :: binary(),
      Ret :: integer().

send(Packet) ->
    try
        #ekcp_conf{
           snd_queue_threshold = MaxWaitNum,
           send_nodelay = P
          } = get(ekcp_config),
        KCP = get(ekcp),
        WaitNum = ekcp:waitsnd(KCP),
        if
            WaitNum > MaxWaitNum ->
                throw(max_wait_send);
            true ->
                Ret = ekcp:send(KCP, Packet),
                P andalso ekcp:flush(KCP),
                Ret
        end
    catch
        _Error:_Reason ->
            ?EKCP_SEND_EAGAIN
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ConvId, UdpPid, Handler, EKCPConf]) ->
    process_flag(trap_exit, true),
    try
        KCP = ekcp:create(ConvId, UdpPid),
        ekcp_config(KCP, EKCPConf),
        put(ekcp_conv_id, ConvId),
        put(ekcp, KCP),
        put(ekcp_handle_module, Handler),
        put(ekcp_udp_pid, UdpPid),
        put(ekcp_start_ms, erlang:system_time(milli_seconds)),
        put(ekcp_config, EKCPConf),
        process_flag(trap_exit, true),
        {ok, State} = Handler:init([]),
        self() ! ekcp_update,
        {ok, State}
    catch
        _Error:Reason ->
            {stop, Reason}
    end.

handle_call(Request, From, State) ->
    Handler = get(ekcp_handle_module),
    case catch Handler:handle_call(Request, From, State) of
        {'EXIT', Reason} ->
            {reply, {'EXIT', Reason}, State};
        OK ->
            OK
    end.

handle_cast(Msg, State) ->
    Handler = get(ekcp_handle_module),
    case catch Handler:handle_cast(Msg, State) of
        {'EXIT', _Reason} ->
            {noreply, State};
        OK ->
            OK
    end.

handle_info(ekcp_recv, State) ->
    try
        KCP = get(ekcp),
        case ekcp:recv(KCP) of
            <<>> ->
                {noreply, State};
            Data ->
                self() ! ekcp_recv,
                handle_info({ekcp_recv, Data}, State)
        end
    catch
        _Error:_Reason ->
            {noreply, State}
    end;
handle_info({ekcp_input, Packet}, State) ->
    try
        KCP = get(ekcp),
        ekcp:input(KCP, Packet),
        self() ! ekcp_recv,
        {noreply, State}
    catch
        _Error:_Reason ->
            {noreply, State}
    end;
handle_info(ekcp_update, State) ->
    try
        KCP = get(ekcp),
        StartMS = get(ekcp_start_ms),
        NowMS = erlang:system_time(milli_seconds) - StartMS,
        ekcp:update(KCP, NowMS),
        NextMS = ekcp:check(KCP, NowMS),
        erlang:send_after(max(0, NextMS - NowMS), self(), ekcp_update),
        {noreply, State}
    catch
        _Error:_Reason ->
            {noreply, State}
    end;
handle_info(Msg, State) ->
    UdpPid = get(ekcp_udp_pid),
    case Msg of
        {'EXIT', UdpPid, Reason} ->
            {stop, Reason, State};
        _ ->
            Handler = get(ekcp_handle_module),
            case catch Handler:handle_info(Msg, State) of
                {'EXIT', _Reason} ->
                    {noreply, State};
                OK ->
                    OK
            end
    end.

terminate(Reason, State) ->
    Handler = get(ekcp_handle_module),
    KCP = get(ekcp),
    catch ekcp:release(KCP),
    Handler:terminate(Reason, State).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

ekcp_config(KCP, #ekcp_conf{
                    nodelay = Nodelay,
                    interval = Interval,
                    resend = Resend,
                    nc = NC,
                    sndwnd = SndWnd,
                    rcvwnd = RcvWnd,
                    mtu = Mtu
                   }) ->
    ekcp:nodelay(KCP, Nodelay, Interval, Resend, NC),
    ekcp:wndsize(KCP, SndWnd, RcvWnd),
    ekcp:setmtu(KCP, Mtu).

