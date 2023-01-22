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

-spec map({ok, CDS} | {error, CDT}, fun((CDS) -> CDW)) -> {ok, CDW} |
    {error, CDT}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, CDZ} | {error, CEA}, fun((CEA) -> CED)) -> {ok, CDZ} |
    {error, CED}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, CEG} | {error, CEH}} | {error, CEH}) -> {ok, CEG} |
    {error, CEH}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec then({ok, CEO} | {error, CEP}, fun((CEO) -> {ok, CES} | {error, CEP})) -> {ok,
        CES} |
    {error, CEP}.
then(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec unwrap({ok, CEX} | {error, any()}, CEX) -> CEX.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default
    end.

-spec lazy_unwrap({ok, CFB} | {error, any()}, fun(() -> CFB)) -> CFB.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, CFG}, CFG) -> CFG.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _@1} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, CFJ} | {error, CFJ}) -> CFJ.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, CFM} | {error, any()}) -> {ok, CFM} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, CFS} | {error, CFT}, {ok, CFS} | {error, CFT}) -> {ok, CFS} |
    {error, CFT}.
'or'(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second
    end.

-spec lazy_or({ok, CGA} | {error, CGB}, fun(() -> {ok, CGA} | {error, CGB})) -> {ok,
        CGA} |
    {error, CGB}.
lazy_or(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second()
    end.

-spec all(list({ok, CGI} | {error, CGJ})) -> {ok, list(CGI)} | {error, CGJ}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, CGR}, CGU) -> {ok, CGU} | {error, CGR}.
replace(Result, Value) ->
    case Result of
        {ok, _@1} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, CGX} | {error, any()}, CHB) -> {ok, CGX} | {error, CHB}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _@1} ->
            {error, Error}
    end.

-spec values(list({ok, CHE} | {error, any()})) -> list(CHE).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).
