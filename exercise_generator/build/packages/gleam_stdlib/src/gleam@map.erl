-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(AUK, AUL)) -> list({AUK, AUL}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({AUU, AUV})) -> map_(AUU, AUV).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(AVE, any()), AVE) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(AVU, AVV), AVU) -> {ok, AVV} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(AWG, AWH), AWG, AWH) -> map_(AWG, AWH).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(AWS, AWT), fun((AWS, AWT) -> AWW)) -> map_(AWS, AWW).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(AXG, any())) -> list(AXG).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), AXR)) -> list(AXR).
values(Map) ->
    maps:values(Map).

-spec filter(map_(AYA, AYB), fun((AYA, AYB) -> boolean())) -> map_(AYA, AYB).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(AYM, AYN), list(AYM)) -> map_(AYM, AYN).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(AZA, AZB), map_(AZA, AZB)) -> map_(AZA, AZB).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(AZQ, AZR), AZQ) -> map_(AZQ, AZR).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(BAC, BAD), list(BAC)) -> map_(BAC, BAD).
drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun delete/2).

-spec update(map_(BAJ, BAK), BAJ, fun((gleam@option:option(BAK)) -> BAK)) -> map_(BAJ, BAK).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({BAQ, BAR}), BAT, fun((BAT, BAQ, BAR) -> BAT)) -> BAT.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(BAU, BAV), BAY, fun((BAY, BAU, BAV) -> BAY)) -> BAY.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
