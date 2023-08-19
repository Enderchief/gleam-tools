-module(gleam@map).
-compile([no_auto_import, nowarn_unused_vars]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(KP, KQ) :: any() | {gleam_phantom, KP, KQ}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(KZ, LA)) -> list({KZ, LA}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({LJ, LK})) -> map_(LJ, LK).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(LT, any()), LT) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(MJ, MK), MJ) -> {ok, MK} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(MV, MW), MV, MW) -> map_(MV, MW).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(NH, NI), fun((NH, NI) -> NL)) -> map_(NH, NL).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(NV, any())) -> list(NV).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), OG)) -> list(OG).
values(Map) ->
    maps:values(Map).

-spec filter(map_(OP, OQ), fun((OP, OQ) -> boolean())) -> map_(OP, OQ).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(PB, PC), list(PB)) -> map_(PB, PC).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(PP, PQ), map_(PP, PQ)) -> map_(PP, PQ).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(QF, QG), QF) -> map_(QF, QG).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(QR, QS), list(QR)) -> map_(QR, QS).
drop(Map, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Map;

        [X | Xs] ->
            drop(delete(Map, X), Xs)
    end.

-spec update(map_(QY, QZ), QY, fun((gleam@option:option(QZ)) -> QZ)) -> map_(QY, QZ).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({RF, RG}), RI, fun((RI, RF, RG) -> RI)) -> RI.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Rest] ->
            do_fold(Rest, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(RJ, RK), RN, fun((RN, RJ, RK) -> RN)) -> RN.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
