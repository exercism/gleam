-module(gleam@option).
-compile(no_auto_import).

-export([all/1, is_some/1, is_none/1, to_result/2, from_result/1, unwrap/2, lazy_unwrap/2, map/2, flatten/1, then/2, 'or'/2, lazy_or/2, values/1]).
-export_type([option/1]).

-type option(APX) :: {some, APX} | none.

-spec all(list(option(APY))) -> option(list(APY)).
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

-spec to_result(option(AQH), AQK) -> {ok, AQH} | {error, AQK}.
to_result(Option, E) ->
    case Option of
        {some, A} ->
            {ok, A};

        _@1 ->
            {error, E}
    end.

-spec from_result({ok, AQN} | {error, any()}) -> option(AQN).
from_result(Result) ->
    case Result of
        {ok, A} ->
            {some, A};

        _@1 ->
            none
    end.

-spec unwrap(option(AQS), AQS) -> AQS.
unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default
    end.

-spec lazy_unwrap(option(AQU), fun(() -> AQU)) -> AQU.
lazy_unwrap(Option, Default) ->
    case Option of
        {some, X} ->
            X;

        none ->
            Default()
    end.

-spec map(option(AQW), fun((AQW) -> AQY)) -> option(AQY).
map(Option, Fun) ->
    case Option of
        {some, X} ->
            {some, Fun(X)};

        none ->
            none
    end.

-spec flatten(option(option(ARA))) -> option(ARA).
flatten(Option) ->
    case Option of
        {some, X} ->
            X;

        none ->
            none
    end.

-spec then(option(ARE), fun((ARE) -> option(ARG))) -> option(ARG).
then(Option, Fun) ->
    case Option of
        {some, X} ->
            Fun(X);

        none ->
            none
    end.

-spec 'or'(option(ARJ), option(ARJ)) -> option(ARJ).
'or'(First, Second) ->
    case First of
        {some, _@1} ->
            First;

        none ->
            Second
    end.

-spec lazy_or(option(ARN), fun(() -> option(ARN))) -> option(ARN).
lazy_or(First, Second) ->
    case First of
        {some, _@1} ->
            First;

        none ->
            Second()
    end.

-spec values(list(option(ARR))) -> list(ARR).
values(Options) ->
    gleam@list:filter_map(Options, fun(Op) -> to_result(Op, <<""/utf8>>) end).
