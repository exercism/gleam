-module(gleam@option).
-compile(no_auto_import).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(AMT) :: {some, AMT} | none.

-spec all(list(option(AMU))) -> option(list(AMU)).
all(List) ->
    gleam@list:fold_right(
        List,
        {some, []},
        fun(Acc, Item) -> case {Acc, Item} of
                {{some, Values}, {some, Value}} ->
                    {some, [Value | Values]};

                {_@1, _@2} ->
                    none
            end end
    ).

-spec is_some(option(any())) -> boolean().
is_some(Option) ->
    Option /= none.

-spec is_none(option(any())) -> boolean().
is_none(Option) ->
    Option =:= none.

-spec to_result(option(AND), ANG) -> {ok, AND} | {error, ANG}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _@1 ->
            {error, E}
    end.

-spec from_result({ok, ANJ} | {error, any()}) -> option(ANJ).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _@1 ->
            none
    end.

-spec unwrap(option(ANO), ANO) -> ANO.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-spec lazy_unwrap(option(ANQ), fun(() -> ANQ)) -> ANQ.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-spec map(option(ANS), fun((ANS) -> ANU)) -> option(ANU).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-spec flatten(option(option(ANW))) -> option(ANW).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-spec then(option(AOA), fun((AOA) -> option(AOC))) -> option(AOC).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-spec 'or'(option(AOF), option(AOF)) -> option(AOF).
'or'(First, Second) ->
    case First of
        {some, _@1} ->
            First;

        none ->
            Second
    end.

-spec lazy_or(option(AOJ), fun(() -> option(AOJ))) -> option(AOJ).
lazy_or(First, Second) ->
    case First of
        {some, _@1} ->
            First;

        none ->
            Second()
    end.

-spec values(list(option(AON))) -> list(AON).
values(Options) ->
    gleam@list:filter_map(Options, fun(Op) -> to_result(Op, <<""/utf8>>) end).
