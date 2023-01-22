-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

-spec first({II, any()}) -> II.
first(Pair) ->
    {A, _@1} = Pair,
    A.

-spec second({any(), IL}) -> IL.
second(Pair) ->
    {_@1, A} = Pair,
    A.

-spec swap({IM, IN}) -> {IN, IM}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({IO, IP}, fun((IO) -> IQ)) -> {IQ, IP}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({IR, IS}, fun((IS) -> IT)) -> {IR, IT}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.
