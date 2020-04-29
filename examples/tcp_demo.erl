-module(tcp_demo).

-behaviour(gen_server).

-include("demo.hrl").

%% API
-export([
         start/1,
         start_server/1,
         start_clients/3,
         listener_loop/1
        ]).

%% ekcp_session callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(TCP_OPTS, [{mode, binary}, {packet, 1}, {keepalive, true}, {active, true}]).

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
    start_server(Port);
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
    start_clients(IP, Port, Num).

start_server(Port) ->
    io:format("port = ~p~n", [Port]),
    case gen_tcp:listen(Port, ?TCP_OPTS) of
        {ok, LSock} ->
            Pid = erlang:spawn(fun() -> listener_loop(LSock) end),
            gen_tcp:controlling_process(LSock, Pid),
            ok;
        Err ->
            {fail, Err}
    end.

start_clients(ServerIp, ServerPort, Num) ->
    [begin
         case gen_tcp:connect(ServerIp, ServerPort, ?TCP_OPTS) of
             {ok, Socket} ->
                 case gen_server:start(?MODULE, [Socket], []) of
                     {ok, Pid} ->
                         gen_tcp:controlling_process(Socket, Pid),
                         Pid ! set_client,
                         ok;
                     Err ->
                         io:format("[TCP_DEMO] start clent err: ~p.", [Err]),
                         false
                 end;
             Err ->
                 io:format("[TCP_DEMO] conn err: ~p.", [Err]),
                 false
         end
     end || _ <- lists:seq(1, Num)].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Socket]) ->
    process_flag(trap_exit, true),
    {ok, #state{mode = tcp, socket = Socket}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(set_client, State) ->
    demo_misc:set_client(State);
handle_info(send, State) ->
    demo_misc:send_handle(State);
handle_info({tcp, _, Data}, State) ->
    demo_misc:recv_handle(State, Data);
handle_info(latency_report, State) ->
    demo_misc:latency_report(State);
handle_info({tcp_close, _Socket}, State) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    gen_tcp:close(State#state.socket),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

listener_loop(LSock) ->
    case gen_tcp:accept(LSock) of
        {ok, Socket} ->
            case gen_server:start(?MODULE, [Socket], []) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid),
                    listener_loop(LSock);
                Err ->
                    io:format("[TCP_DEMO] accept err:~p.~n", [Err])
            end;
        Err ->
            io:format("[TCP_DEMO] listen err:~p.~n", [Err])
    end.
