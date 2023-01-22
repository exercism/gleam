-module(gleam@order).
-compile(no_auto_import).

-export([reverse/1, to_int/1, compare/2, max/2, min/2]).
-export_type([order/0]).

-type order() :: lt | eq | gt.

-spec reverse(order()) -> order().
reverse(Order) ->
    case Order of
        lt ->
            gt;

        eq ->
            eq;

        gt ->
            lt
    end.

-spec to_int(order()) -> integer().
to_int(Order) ->
    case Order of
        lt ->
            -1;

        eq ->
            0;

        gt ->
            1
    end.

-spec compare(order(), order()) -> order().
compare(A, B) ->
    case {A, B} of
        {X, Y} when X =:= Y ->
            eq;

        {lt, _@1} ->
            lt;

        {eq, gt} ->
            lt;

        {_@2, _@3} ->
            gt
    end.

-spec max(order(), order()) -> order().
max(A, B) ->
    case {A, B} of
        {gt, _@1} ->
            gt;

        {eq, lt} ->
            eq;

        {_@2, _@3} ->
            B
    end.

-spec min(order(), order()) -> order().
min(A, B) ->
    case {A, B} of
        {lt, _@1} ->
            lt;

        {eq, gt} ->
            eq;

        {_@2, _@3} ->
            B
    end.
