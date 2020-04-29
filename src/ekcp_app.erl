-module(ekcp_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([register_name/2, unregister_name/1, whereis_name/1, send/2]).

-define(TAB, ?MODULE).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    ets:new(?TAB, [public, named_table, {read_concurrency, true}]),
    case ekcp_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% API
%%%===================================================================

register_name(Name, Pid) ->
    case ets:member(?TAB, {Name, Pid}) of
        false ->
            ets:insert(?TAB, {Name, Pid}),
            yes;
        true ->
            no
    end.

unregister_name(Name) ->
    ets:delete(?TAB, Name).

whereis_name(Name) ->
    case ets:lookup(?TAB, Name) of
        [{_, Pid}] ->
            Pid;
        _ ->
            undefined
    end.

send(Name, Msg) ->
    case ets:lookup(?TAB, Name) of
        [{_, Pid}] ->
            Pid ! Msg,
            Pid;
        _ ->
            erlang:error(badarg)
    end.
