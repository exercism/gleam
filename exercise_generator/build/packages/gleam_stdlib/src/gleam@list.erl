-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map_fold/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, flatten/1, flat_map/2, fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, interleave/1, transpose/1, shuffle/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(IV) :: {continue, IV} | {stop, IV}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(JA)) -> list(JA).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(JI), JI) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | _@1] when Head =:= Elem ->
            true;

        [_@2 | Tail] ->
            contains(Tail, Elem)
    end.

-spec first(list(JK)) -> {ok, JK} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _@1] ->
            {ok, X}
    end.

-spec rest(list(JO)) -> {ok, list(JO)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_@1 | Xs] ->
            {ok, Xs}
    end.

-spec do_filter(list(JT), fun((JT) -> boolean()), list(JT)) -> list(JT).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(JX), fun((JX) -> boolean())) -> list(JX).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(KA), fun((KA) -> {ok, KC} | {error, any()}), list(KC)) -> list(KC).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _@1} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(KI), fun((KI) -> {ok, KK} | {error, any()})) -> list(KK).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(KP), fun((KP) -> KR), list(KR)) -> list(KR).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(KU), fun((KU) -> KW)) -> list(KW).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec map_fold(list(KY), LA, fun((LA, KY) -> {LA, LB})) -> {LA, list(LB)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec do_index_map(list(LD), fun((integer(), LD) -> LF), integer(), list(LF)) -> list(LF).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(LI), fun((integer(), LI) -> LK)) -> list(LK).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(LM), fun((LM) -> {ok, LO} | {error, LP}), list(LO)) -> {ok,
        list(LO)} |
    {error, LP}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(LW), fun((LW) -> {ok, LY} | {error, LZ})) -> {ok, list(LY)} |
    {error, LZ}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(MF), integer()) -> list(MF).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_@1 | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(MI), integer(), list(MI)) -> list(MI).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(MM), integer()) -> list(MM).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(MR), list(MR)) -> list(MR).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(MZ), MZ) -> list(MZ).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(NC), list(NC)) -> list(NC).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [Head | Tail] ->
            reverse_and_prepend(Tail, [Head | Suffix])
    end.

-spec do_flatten(list(list(NG)), list(NG)) -> list(NG).
do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            reverse(Acc);

        [List | Further_lists] ->
            do_flatten(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec flatten(list(list(NL))) -> list(NL).
flatten(Lists) ->
    do_flatten(Lists, []).

-spec flat_map(list(NP), fun((NP) -> list(NR))) -> list(NR).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold(list(NU), NW, fun((NW, NU) -> NW)) -> NW.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec fold_right(list(NX), NZ, fun((NZ, NX) -> NZ)) -> NZ.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(list(OA), OC, fun((OC, OA, integer()) -> OC), integer()) -> OC.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(OD), OF, fun((OF, OD, integer()) -> OF)) -> OF.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(OG), OI, fun((OI, OG) -> {ok, OI} | {error, OJ})) -> {ok,
        OI} |
    {error, OJ}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {error, _try} -> {error, _try};
                {ok, Accumulator@1} ->
                    try_fold(Rest, Accumulator@1, Fun)
            end
    end.

-spec fold_until(list(OO), OQ, fun((OQ, OO) -> continue_or_stop(OQ))) -> OQ.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(OS), fun((OS) -> boolean())) -> {ok, OS} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _@1 ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(OW), fun((OW) -> {ok, OY} | {error, any()})) -> {ok, OY} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _@1 ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(PE), fun((PE) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    all(Tail, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(PG), fun((PG) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    true;

                false ->
                    any(Tail, Predicate)
            end
    end.

-spec do_zip(list(PI), list(PK), list({PI, PK})) -> list({PI, PK}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_@1, _@2} ->
            reverse(Acc)
    end.

-spec zip(list(PO), list(PQ)) -> list({PO, PQ}).
zip(Xs, Ys) ->
    do_zip(Xs, Ys, []).

-spec strict_zip(list(PT), list(PV)) -> {ok, list({PT, PV})} |
    {error, length_mismatch()}.
strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({QE, QF}), list(QE), list(QF)) -> {list(QE), list(QF)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({QE, QF})) -> {list(QE), list(QF)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(QJ), QJ, list(QJ)) -> list(QJ).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(QN), QN) -> list(QN).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_@1] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(QQ), integer()) -> {ok, QQ} | {error, nil}.
at(List, Index) ->
    case Index >= 0 of
        true ->
            _pipe = List,
            _pipe@1 = drop(_pipe, Index),
            first(_pipe@1);

        false ->
            {error, nil}
    end.

-spec unique(list(QU)) -> list(QU).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_up(
    integer(),
    integer(),
    list(QX),
    list(QX),
    list(QX),
    fun((QX, QX) -> gleam@order:order())
) -> list(QX).
merge_up(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _@1, _@2} ->
            Acc;

        {_@3, 0, [Ax | Ar], _@4} ->
            merge_up(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _@5, _@6, [Bx | Br]} ->
            merge_up(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_@7, _@8, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Ax@1, Bx@1) of
                gt ->
                    merge_up(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare);

                _@9 ->
                    merge_up(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare)
            end
    end.

-spec merge_down(
    integer(),
    integer(),
    list(RC),
    list(RC),
    list(RC),
    fun((RC, RC) -> gleam@order:order())
) -> list(RC).
merge_down(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _@1, _@2} ->
            Acc;

        {_@3, 0, [Ax | Ar], _@4} ->
            merge_down(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _@5, _@6, [Bx | Br]} ->
            merge_down(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_@7, _@8, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Bx@1, Ax@1) of
                lt ->
                    merge_down(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare);

                _@9 ->
                    merge_down(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare)
            end
    end.

-spec merge_sort(
    list(RH),
    integer(),
    fun((RH, RH) -> gleam@order:order()),
    boolean()
) -> list(RH).
merge_sort(L, Ln, Compare, Down) ->
    N = Ln div 2,
    A = L,
    B = drop(L, N),
    case Ln < 3 of
        true ->
            case Down of
                true ->
                    merge_down(N, Ln - N, A, B, [], Compare);

                false ->
                    merge_up(N, Ln - N, A, B, [], Compare)
            end;

        false ->
            case Down of
                true ->
                    merge_down(
                        N,
                        Ln
                        - N,
                        merge_sort(A, N, Compare, false),
                        merge_sort(B, Ln - N, Compare, false),
                        [],
                        Compare
                    );

                false ->
                    merge_up(
                        N,
                        Ln
                        - N,
                        merge_sort(A, N, Compare, true),
                        merge_sort(B, Ln - N, Compare, true),
                        [],
                        Compare
                    )
            end
    end.

-spec sort(list(RK), fun((RK, RK) -> gleam@order:order())) -> list(RK).
sort(List, Compare) ->
    merge_sort(List, length(List), Compare, true).

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            reverse([Stop | Acc]);

        gt ->
            tail_recursive_range(Start - 1, Stop, [Start | Acc]);

        lt ->
            tail_recursive_range(Start + 1, Stop, [Start | Acc])
    end.

-spec do_repeat(RQ, integer(), list(RQ)) -> list(RQ).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(RT, integer()) -> list(RT).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(RV), integer(), list(RV)) -> {list(RV), list(RV)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(SA), integer()) -> {list(SA), list(SA)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(SE), fun((SE) -> boolean()), list(SE)) -> {list(SE),
    list(SE)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _@1 ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(SJ), fun((SJ) -> boolean())) -> {list(SJ), list(SJ)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({SN, SO}), SN) -> {ok, SO} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(SW), fun((SW) -> boolean()), list(SW)) -> {ok, {SW, list(SW)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(SW), fun((SW) -> boolean())) -> {ok, {SW, list(SW)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(TF), fun((TF) -> {ok, TH} | {error, any()}), list(TF)) -> {ok,
        {TH, list(TF)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _@1} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(TF), fun((TF) -> {ok, TH} | {error, any()})) -> {ok,
        {TH, list(TF)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({TO, TP}), TO) -> {ok, {TP, list({TO, TP})}} | {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _@1 ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({TU, TV}), TU, TV) -> list({TU, TV}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _@1} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(TY), fun((TY) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec do_partition(list(UG), fun((UG) -> boolean()), list(UG), list(UG)) -> {list(UG),
    list(UG)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(UG), fun((UG) -> boolean())) -> {list(UG), list(UG)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(UK)) -> list(list(UK)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _@1 ->
            _pipe = L,
            _pipe@5 = index_map(
                _pipe,
                fun(I_idx, I) ->
                    _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end)
                end
            ),
            flatten(_pipe@5)
    end.

-spec do_window(list(list(UO)), list(UO), integer()) -> list(list(UO)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(UU), integer()) -> list(list(UU)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(UY)) -> list({UY, UY}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(VB), fun((VB) -> boolean())) -> list(VB).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(VE), fun((VE) -> boolean()), list(VE)) -> list(VE).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    do_take_while(Tail, Predicate, [Head | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(VI), fun((VI) -> boolean())) -> list(VI).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(VL), fun((VL) -> VN), VN, list(VL), list(list(VL))) -> list(list(VL)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [Head | Tail] ->
            Key = F(Head),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Tail, F, Key, [Head], New_acc);

                _@1 ->
                    do_chunk(Tail, F, Key, [Head | Current_chunk], Acc)
            end;

        _@2 ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(VT), fun((VT) -> any())) -> list(list(VT)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [Head | Tail] ->
            do_chunk(Tail, F, F(Head), [Head], [])
    end.

-spec do_sized_chunk(list(VY), integer(), integer(), list(VY), list(list(VY))) -> list(list(VY)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
            end;

        [Head | Tail] ->
            Chunk = [Head | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Tail,
                        Count,
                        Count,
                        [],
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Tail, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(WF), integer()) -> list(list(WF)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(WJ), fun((WJ, WJ) -> WJ)) -> {ok, WJ} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [Head | Tail] ->
            {ok, fold(Tail, Head, Fun)}
    end.

-spec do_scan(list(WN), WP, list(WP), fun((WP, WN) -> WP)) -> list(WP).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(WS), WU, fun((WU, WS) -> WU)) -> list(WU).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(WW)) -> {ok, WW} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(XA), integer()) -> list(list(XA)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _@1 ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(XE)) -> list(list({XE, XE})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(XI)) -> list({XI, XI}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec interleave(list(list(XL))) -> list(XL).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec transpose(list(list(XP))) -> list(list(XP)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _@1] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec do_shuffle_pair_unwrap(list({float(), XU}), list(XU)) -> list(XU).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        _@1 ->
            [Elem_pair | Enumerable] = List,
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec do_shuffle_by_pair_indexes(list({float(), XY})) -> list({float(), XY}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec shuffle(list(YB)) -> list(YB).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(
        _pipe,
        [],
        fun(Acc, A) -> [{gleam@float:random(0.0, 1.0), A} | Acc] end
    ),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
