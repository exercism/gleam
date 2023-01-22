-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map_fold/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, flatten/1, flat_map/2, fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, interleave/1, transpose/1, shuffle/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(FR) :: {continue, FR} | {stop, FR}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(FW)) -> list(FW).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(GE), GE) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | _@1] when Head =:= Elem ->
            true;

        [_@2 | Tail] ->
            contains(Tail, Elem)
    end.

-spec first(list(GG)) -> {ok, GG} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _@1] ->
            {ok, X}
    end.

-spec rest(list(GK)) -> {ok, list(GK)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_@1 | Xs] ->
            {ok, Xs}
    end.

-spec do_filter(list(GP), fun((GP) -> boolean()), list(GP)) -> list(GP).
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

-spec filter(list(GT), fun((GT) -> boolean())) -> list(GT).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(GW), fun((GW) -> {ok, GY} | {error, any()}), list(GY)) -> list(GY).
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

-spec filter_map(list(HE), fun((HE) -> {ok, HG} | {error, any()})) -> list(HG).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(HL), fun((HL) -> HN), list(HN)) -> list(HN).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(HQ), fun((HQ) -> HS)) -> list(HS).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec map_fold(list(HU), HW, fun((HW, HU) -> {HW, HX})) -> {HW, list(HX)}.
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

-spec do_index_map(list(HZ), fun((integer(), HZ) -> IB), integer(), list(IB)) -> list(IB).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(IE), fun((integer(), IE) -> IG)) -> list(IG).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(II), fun((II) -> {ok, IK} | {error, IL}), list(IK)) -> {ok,
        list(IK)} |
    {error, IL}.
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

-spec try_map(list(IS), fun((IS) -> {ok, IU} | {error, IV})) -> {ok, list(IU)} |
    {error, IV}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(JB), integer()) -> list(JB).
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

-spec do_take(list(JE), integer(), list(JE)) -> list(JE).
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

-spec take(list(JI), integer()) -> list(JI).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(JN), list(JN)) -> list(JN).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(JV), JV) -> list(JV).
prepend(List, Item) ->
    [Item | List].

-spec reverse_and_prepend(list(JY), list(JY)) -> list(JY).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [Head | Tail] ->
            reverse_and_prepend(Tail, [Head | Suffix])
    end.

-spec do_flatten(list(list(KC)), list(KC)) -> list(KC).
do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            reverse(Acc);

        [List | Further_lists] ->
            do_flatten(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec flatten(list(list(KH))) -> list(KH).
flatten(Lists) ->
    do_flatten(Lists, []).

-spec flat_map(list(KL), fun((KL) -> list(KN))) -> list(KN).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold(list(KQ), KS, fun((KS, KQ) -> KS)) -> KS.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec fold_right(list(KT), KV, fun((KV, KT) -> KV)) -> KV.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(list(KW), KY, fun((KY, KW, integer()) -> KY), integer()) -> KY.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(KZ), LB, fun((LB, KZ, integer()) -> LB)) -> LB.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(LC), LE, fun((LE, LC) -> {ok, LE} | {error, LF})) -> {ok,
        LE} |
    {error, LF}.
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

-spec fold_until(list(LK), LM, fun((LM, LK) -> continue_or_stop(LM))) -> LM.
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

-spec find(list(LO), fun((LO) -> boolean())) -> {ok, LO} | {error, nil}.
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

-spec find_map(list(LS), fun((LS) -> {ok, LU} | {error, any()})) -> {ok, LU} |
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

-spec all(list(MA), fun((MA) -> boolean())) -> boolean().
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

-spec any(list(MC), fun((MC) -> boolean())) -> boolean().
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

-spec do_zip(list(ME), list(MG), list({ME, MG})) -> list({ME, MG}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_@1, _@2} ->
            reverse(Acc)
    end.

-spec zip(list(MK), list(MM)) -> list({MK, MM}).
zip(Xs, Ys) ->
    do_zip(Xs, Ys, []).

-spec strict_zip(list(MP), list(MR)) -> {ok, list({MP, MR})} |
    {error, length_mismatch()}.
strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({NA, NB}), list(NA), list(NB)) -> {list(NA), list(NB)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({NA, NB})) -> {list(NA), list(NB)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(NF), NF, list(NF)) -> list(NF).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(NJ), NJ) -> list(NJ).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_@1] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(NM), integer()) -> {ok, NM} | {error, nil}.
at(List, Index) ->
    case Index >= 0 of
        true ->
            _pipe = List,
            _pipe@1 = drop(_pipe, Index),
            first(_pipe@1);

        false ->
            {error, nil}
    end.

-spec unique(list(NQ)) -> list(NQ).
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
    list(NT),
    list(NT),
    list(NT),
    fun((NT, NT) -> gleam@order:order())
) -> list(NT).
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
    list(NY),
    list(NY),
    list(NY),
    fun((NY, NY) -> gleam@order:order())
) -> list(NY).
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
    list(OD),
    integer(),
    fun((OD, OD) -> gleam@order:order()),
    boolean()
) -> list(OD).
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

-spec sort(list(OG), fun((OG, OG) -> gleam@order:order())) -> list(OG).
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

-spec do_repeat(OM, integer(), list(OM)) -> list(OM).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(OP, integer()) -> list(OP).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(OR), integer(), list(OR)) -> {list(OR), list(OR)}.
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

-spec split(list(OW), integer()) -> {list(OW), list(OW)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(PA), fun((PA) -> boolean()), list(PA)) -> {list(PA),
    list(PA)}.
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

-spec split_while(list(PF), fun((PF) -> boolean())) -> {list(PF), list(PF)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({PJ, PK}), PJ) -> {ok, PK} | {error, nil}.
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

-spec do_pop(list(PS), fun((PS) -> boolean()), list(PS)) -> {ok, {PS, list(PS)}} |
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

-spec pop(list(PS), fun((PS) -> boolean())) -> {ok, {PS, list(PS)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(QB), fun((QB) -> {ok, QD} | {error, any()}), list(QB)) -> {ok,
        {QD, list(QB)}} |
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

-spec pop_map(list(QB), fun((QB) -> {ok, QD} | {error, any()})) -> {ok,
        {QD, list(QB)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({QK, QL}), QK) -> {ok, {QL, list({QK, QL})}} | {error, nil}.
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

-spec key_set(list({QQ, QR}), QQ, QR) -> list({QQ, QR}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _@1} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(QU), fun((QU) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec do_partition(list(RC), fun((RC) -> boolean()), list(RC), list(RC)) -> {list(RC),
    list(RC)}.
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

-spec partition(list(RC), fun((RC) -> boolean())) -> {list(RC), list(RC)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(RG)) -> list(list(RG)).
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

-spec do_window(list(list(RK)), list(RK), integer()) -> list(list(RK)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(RQ), integer()) -> list(list(RQ)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(RU)) -> list({RU, RU}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(RX), fun((RX) -> boolean())) -> list(RX).
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

-spec do_take_while(list(SA), fun((SA) -> boolean()), list(SA)) -> list(SA).
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

-spec take_while(list(SE), fun((SE) -> boolean())) -> list(SE).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(SH), fun((SH) -> SJ), SJ, list(SH), list(list(SH))) -> list(list(SH)).
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

-spec chunk(list(SP), fun((SP) -> any())) -> list(list(SP)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [Head | Tail] ->
            do_chunk(Tail, F, F(Head), [Head], [])
    end.

-spec do_sized_chunk(list(SU), integer(), integer(), list(SU), list(list(SU))) -> list(list(SU)).
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

-spec sized_chunk(list(TB), integer()) -> list(list(TB)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(TF), fun((TF, TF) -> TF)) -> {ok, TF} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [Head | Tail] ->
            {ok, fold(Tail, Head, Fun)}
    end.

-spec do_scan(list(TJ), TL, list(TL), fun((TL, TJ) -> TL)) -> list(TL).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(TO), TQ, fun((TQ, TO) -> TQ)) -> list(TQ).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(TS)) -> {ok, TS} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(TW), integer()) -> list(list(TW)).
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

-spec do_combination_pairs(list(UA)) -> list(list({UA, UA})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(UE)) -> list({UE, UE}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec interleave(list(list(UH))) -> list(UH).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec transpose(list(list(UL))) -> list(list(UL)).
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

-spec do_shuffle_pair_unwrap(list({float(), UQ}), list(UQ)) -> list(UQ).
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

-spec do_shuffle_by_pair_indexes(list({float(), UU})) -> list({float(), UU}).
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

-spec shuffle(list(UX)) -> list(UX).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(
        _pipe,
        [],
        fun(Acc, A) -> [{gleam@float:random(0.0, 1.0), A} | Acc] end
    ),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
