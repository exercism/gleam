-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

-spec first({FE, any()}) -> FE.
first(Pair) ->
    {A, _@1} = Pair,
    A.

-spec second({any(), FH}) -> FH.
second(Pair) ->
    {_@1, A} = Pair,
    A.

-spec swap({FI, FJ}) -> {FJ, FI}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({FK, FL}, fun((FK) -> FM)) -> {FM, FL}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({FN, FO}, fun((FO) -> FP)) -> {FN, FP}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.
