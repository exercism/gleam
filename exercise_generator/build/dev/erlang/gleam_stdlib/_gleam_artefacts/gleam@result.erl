-module(gleam@result).
-compile(no_auto_import).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, replace/2, replace_error/2, values/1]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _@1} ->
            false;

        {ok, _@2} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _@1} ->
            false;

        {error, _@2} ->
            true
    end.

-spec map({ok, CBV} | {error, CBW}, fun((CBV) -> CBZ)) -> {ok, CBZ} |
    {error, CBW}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, CCC} | {error, CCD}, fun((CCD) -> CCG)) -> {ok, CCC} |
    {error, CCG}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, CCJ} | {error, CCK}} | {error, CCK}) -> {ok, CCJ} |
    {error, CCK}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec then({ok, CCR} | {error, CCS}, fun((CCR) -> {ok, CCV} | {error, CCS})) -> {ok,
        CCV} |
    {error, CCS}.
then(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec unwrap({ok, CDA} | {error, any()}, CDA) -> CDA.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default
    end.

-spec lazy_unwrap({ok, CDE} | {error, any()}, fun(() -> CDE)) -> CDE.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, CDJ}, CDJ) -> CDJ.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _@1} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, CDM} | {error, CDM}) -> CDM.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, CDP} | {error, any()}) -> {ok, CDP} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, CDV} | {error, CDW}, {ok, CDV} | {error, CDW}) -> {ok, CDV} |
    {error, CDW}.
'or'(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second
    end.

-spec lazy_or({ok, CED} | {error, CEE}, fun(() -> {ok, CED} | {error, CEE})) -> {ok,
        CED} |
    {error, CEE}.
lazy_or(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second()
    end.

-spec all(list({ok, CEL} | {error, CEM})) -> {ok, list(CEL)} | {error, CEM}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, CEU}, CEX) -> {ok, CEX} | {error, CEU}.
replace(Result, Value) ->
    case Result of
        {ok, _@1} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, CFA} | {error, any()}, CFE) -> {ok, CFA} | {error, CFE}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _@1} ->
            {error, Error}
    end.

-spec values(list({ok, CFH} | {error, any()})) -> list(CFH).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).
