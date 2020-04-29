-module(ekcp).

-export([load_init/0]).

-export([create/2, release/1, recv/1, send/2,
         update/2, check/2, input/2, flush/1,
         peeksize/1, setmtu/2, wndsize/3, waitsnd/1,
         nodelay/5, getconv/1]).

-on_load(init/0).

init() ->
    erlang:load_nif("./priv/ekcp", 0),
    ok = load_init().

load_init() ->
    erlang:nif_error("NIF library not loaded").

%%%===================================================================
%%% kcp interface
%%%===================================================================

-spec create(Conv, UDPpid) -> IKCPCB when
      Conv :: non_neg_integer(),
      UDPpid :: pid(),
      IKCPCB :: reference().
create(_Conv, _UDPpid) ->
    erlang:nif_error("NIF library not loaded").


-spec release(IKCPCB) -> ok when
      IKCPCB :: reference().
release(_IKCPCB) ->
    erlang:nif_error("NIF library not loaded").


-spec recv(IKCPCB) -> Binary when
      IKCPCB :: reference(),
      Binary :: binary().
recv(_IKCPCB) ->
    erlang:nif_error("NIF library not loaded").


-spec send(IKCPCB, Binary) -> Int when
      IKCPCB :: reference(),
      Binary :: binary(),
      Int :: integer().
send(_IKCPCB, _Binary) ->
    erlang:nif_error("NIF library not loaded").


-spec update(IKCPCB, Current) -> ok when
      IKCPCB :: reference(),
      Current :: integer().
update(_IKCPCB, _current) ->
    erlang:nif_error("NIF library not loaded").


-spec check(IKCPCB, Current) -> Int when
      IKCPCB :: reference(),
      Current :: integer(),
      Int :: integer().
check(_IKCPCB, _Current) ->
    erlang:nif_error("NIF library not loaded").


-spec input(IKCPCB, Binary) -> Int when
      IKCPCB :: reference(),
      Binary :: binary(),
      Int :: integer().
input(_IKCPCB, _Binary) ->
    erlang:nif_error("NIF library not loaded").


-spec flush(IKCPCB) -> ok when
      IKCPCB :: reference().
flush(_IKCPCB) ->
    erlang:nif_error("NIF library not loaded").


-spec peeksize(IKCPCB) -> Int when
      IKCPCB :: reference(),
      Int :: integer().
peeksize(_IKCPCB) ->
    erlang:nif_error("NIF library not loaded").


-spec setmtu(IKCPCB, MTU) -> Int when
      IKCPCB :: reference(),
      MTU :: integer(),
      Int :: integer().
setmtu(_IKCPCB, _MTU) ->
    erlang:nif_error("NIF library not loaded").


-spec wndsize(IKCPCB, SndWnd, RcvWnd) -> Int when
      IKCPCB :: reference(),
      SndWnd :: integer(),
      RcvWnd :: integer(),
      Int :: integer().
wndsize(_IKCPCB, _SndWnd, _RcvWnd) ->
    erlang:nif_error("NIF library not loaded").


-spec waitsnd(IKCPCB) -> Int when
      IKCPCB :: reference(),
      Int :: integer().
waitsnd(_IKCPCB) ->
    erlang:nif_error("NIF library not loaded").


-spec nodelay(IKCPCB, Nodelay, Interval, Resend, NC) -> Int when
      IKCPCB :: reference(),
      Nodelay :: integer(),
      Interval :: integer(),
      Resend :: integer(),
      NC :: integer(),
      Int :: integer().
nodelay(_IKCPCB, _Nodelay, _Interval, _Resend, _NC) ->
    erlang:nif_error("NIF library not loaded").


-spec getconv(Binary) -> Int when
      Binary :: binary(),
      Int :: integer().
getconv(_Binary) ->
    erlang:nif_error("NIF library not loaded").
