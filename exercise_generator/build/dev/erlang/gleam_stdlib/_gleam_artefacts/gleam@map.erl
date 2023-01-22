-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(ARV, ARW)) -> list({ARV, ARW}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({ASF, ASG})) -> map_(ASF, ASG).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(ASP, any()), ASP) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(ATF, ATG), ATF) -> {ok, ATG} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(ATR, ATS), ATR, ATS) -> map_(ATR, ATS).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(AUD, AUE), fun((AUD, AUE) -> AUH)) -> map_(AUD, AUH).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(AUR, any())) -> list(AUR).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), AVC)) -> list(AVC).
values(Map) ->
    maps:values(Map).

-spec filter(map_(AVL, AVM), fun((AVL, AVM) -> boolean())) -> map_(AVL, AVM).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(AVX, AVY), list(AVX)) -> map_(AVX, AVY).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(AWL, AWM), map_(AWL, AWM)) -> map_(AWL, AWM).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(AXB, AXC), AXB) -> map_(AXB, AXC).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(AXN, AXO), list(AXN)) -> map_(AXN, AXO).
drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun delete/2).

-spec update(map_(AXU, AXV), AXU, fun((gleam@option:option(AXV)) -> AXV)) -> map_(AXU, AXV).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({AYB, AYC}), AYE, fun((AYE, AYB, AYC) -> AYE)) -> AYE.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(AYF, AYG), AYJ, fun((AYJ, AYF, AYG) -> AYJ)) -> AYJ.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
