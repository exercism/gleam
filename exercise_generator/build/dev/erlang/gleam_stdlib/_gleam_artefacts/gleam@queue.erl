-module(gleam@queue).
-compile(no_auto_import).

-export([new/0, from_list/1, to_list/1, is_empty/1, length/1, push_back/2, push_front/2, pop_back/1, pop_front/1, reverse/1, is_logically_equal/3, is_equal/2]).
-export_type([queue/1]).

-opaque queue(DZB) :: {queue, list(DZB), list(DZB)}.

-spec new() -> queue(any()).
new() ->
    {queue, [], []}.

-spec from_list(list(DZI)) -> queue(DZI).
from_list(List) ->
    {queue, [], List}.

-spec to_list(queue(DZL)) -> list(DZL).
to_list(Queue) ->
    _pipe = erlang:element(3, Queue),
    gleam@list:append(_pipe, gleam@list:reverse(erlang:element(2, Queue))).

-spec is_empty(queue(any())) -> boolean().
is_empty(Queue) ->
    (erlang:element(2, Queue) =:= []) andalso (erlang:element(3, Queue) =:= []).

-spec length(queue(any())) -> integer().
length(Queue) ->
    gleam@list:length(erlang:element(2, Queue)) + gleam@list:length(
        erlang:element(3, Queue)
    ).

-spec push_back(queue(DZS), DZS) -> queue(DZS).
push_back(Queue, Item) ->
    {queue, [Item | erlang:element(2, Queue)], erlang:element(3, Queue)}.

-spec push_front(queue(DZV), DZV) -> queue(DZV).
push_front(Queue, Item) ->
    {queue, erlang:element(2, Queue), [Item | erlang:element(3, Queue)]}.

-spec pop_back(queue(DZY)) -> {ok, {DZY, queue(DZY)}} | {error, nil}.
pop_back(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, [], Out} ->
            pop_back({queue, gleam@list:reverse(Out), []});

        {queue, [First | Rest], Out@1} ->
            Queue@1 = {queue, Rest, Out@1},
            {ok, {First, Queue@1}}
    end.

-spec pop_front(queue(EAD)) -> {ok, {EAD, queue(EAD)}} | {error, nil}.
pop_front(Queue) ->
    case Queue of
        {queue, [], []} ->
            {error, nil};

        {queue, In, []} ->
            pop_front({queue, [], gleam@list:reverse(In)});

        {queue, In@1, [First | Rest]} ->
            Queue@1 = {queue, In@1, Rest},
            {ok, {First, Queue@1}}
    end.

-spec reverse(queue(EAI)) -> queue(EAI).
reverse(Queue) ->
    {queue, erlang:element(3, Queue), erlang:element(2, Queue)}.

-spec check_equal(
    list(EAL),
    list(EAL),
    list(EAL),
    list(EAL),
    fun((EAL, EAL) -> boolean())
) -> boolean().
check_equal(Xs, X_tail, Ys, Y_tail, Eq) ->
    case {Xs, X_tail, Ys, Y_tail} of
        {[], [], [], []} ->
            true;

        {[X | Xs@1], _@1, [Y | Ys@1], _@2} ->
            case Eq(X, Y) of
                false ->
                    false;

                true ->
                    check_equal(Xs@1, X_tail, Ys@1, Y_tail, Eq)
            end;

        {[], [_@3 | _@4], _@5, _@6} ->
            check_equal(gleam@list:reverse(X_tail), [], Ys, Y_tail, Eq);

        {_@7, _@8, [], [_@9 | _@10]} ->
            check_equal(Xs, X_tail, gleam@list:reverse(Y_tail), [], Eq);

        {_@11, _@12, _@13, _@14} ->
            false
    end.

-spec is_logically_equal(queue(EAQ), queue(EAQ), fun((EAQ, EAQ) -> boolean())) -> boolean().
is_logically_equal(A, B, Element_is_equal) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        Element_is_equal
    ).

-spec is_equal(queue(EAT), queue(EAT)) -> boolean().
is_equal(A, B) ->
    check_equal(
        erlang:element(3, A),
        erlang:element(2, A),
        erlang:element(3, B),
        erlang:element(2, B),
        fun(A@1, B@1) -> A@1 =:= B@1 end
    ).
