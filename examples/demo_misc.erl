-module(demo_misc).

-include("../include/ekcp.hrl").
-include("demo.hrl").

-export([
         ms/0, send_loop_timer/0, latency_report_loop_timer/0,
         recv_handle/2, latency_report/1, send_handle/1, set_client/1
        ]).

ms() ->
    erlang:system_time(milli_seconds).

send_loop_timer() ->
    erlang:send_after(?CLIENT_SEND_RATE, self(), send).

latency_report_loop_timer() ->
    erlang:send_after(1000, self(), latency_report).


recv_handle(#state{cs = s} = State, Data) ->
    send(State, Data);
recv_handle(#state{cs = c, echo_times = EchoTimes, total_rtt = TotalRTT} = State, Data) ->
    case catch binary_to_term(Data) of
        {latency, SendMs} ->
            {noreply, State#state{
                        echo_times = EchoTimes + 1,
                        total_rtt = TotalRTT + ms() - SendMs
                       }};
        _ ->
            {noreply, State}
    end.

send(#state{mode = tcp} = State, Data) ->
    case gen_tcp:send(State#state.socket, Data) of
        {error, Reason} ->
            io:format("[tcp DEMO] send err:~p. break.~n", [Reason]),
            {stop, normal, State};
        ok ->
            {noreply, State}
    end;
send(#state{mode = kcp} = State, Data) ->
    Ret = ekcp_session:send(Data),
    if
        Ret =:= ?EKCP_SEND_EAGAIN ->
            io:format("[kcp DEMO] conn break.~n"),
            {stop, normal, State};
        Ret < 0 ->
            io:format("[kcp DEMO] send err ~p.~n", [Ret]),
            {noreply, State};
        true ->
            {noreply, State}
    end.


latency_report(#state{
                  mode = Mode,
                  echo_times = EchoTimes,
                  total_rtt = TotalRTT
                 } = State) ->
    if
        EchoTimes =< 0 ->
            ignore;
        true ->
            io:format("[~p DEMO] average rtt ~pms.~n", [Mode, TotalRTT / EchoTimes])
    end,
    latency_report_loop_timer(),
    {noreply, State#state{echo_times = 0, total_rtt = 0}}.

send_handle(State) ->
    demo_misc:send_loop_timer(),
    send(State, term_to_binary({latency, ms()})).

set_client(State) ->
    send_loop_timer(),
    latency_report_loop_timer(),
    {noreply, State#state{cs = c}}.
