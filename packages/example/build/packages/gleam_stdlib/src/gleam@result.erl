-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BFF} | {error, BFG}, fun((BFF) -> BFJ)) -> {ok, BFJ} |
    {error, BFG}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BFM} | {error, BFN}, fun((BFN) -> BFQ)) -> {ok, BFM} |
    {error, BFQ}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BFT} | {error, BFU}} | {error, BFU}) -> {ok, BFT} |
    {error, BFU}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BGB} | {error, BGC}, fun((BGB) -> {ok, BGF} | {error, BGC})) -> {ok,
        BGF} |
    {error, BGC}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BGK} | {error, BGL}, fun((BGK) -> {ok, BGO} | {error, BGL})) -> {ok,
        BGO} |
    {error, BGL}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BGT} | {error, any()}, BGT) -> BGT.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BGX} | {error, any()}, fun(() -> BGX)) -> BGX.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BHC}, BHC) -> BHC.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BHF} | {error, BHF}) -> BHF.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BHI} | {error, any()}) -> {ok, BHI} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BHO} | {error, BHP}, {ok, BHO} | {error, BHP}) -> {ok, BHO} |
    {error, BHP}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BHW} | {error, BHX}, fun(() -> {ok, BHW} | {error, BHX})) -> {ok,
        BHW} |
    {error, BHX}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BIE} | {error, BIF})) -> {ok, list(BIE)} | {error, BIF}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BIT} | {error, BIU}), list(BIT), list(BIU)) -> {list(BIT),
    list(BIU)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BIM} | {error, BIN})) -> {list(BIM), list(BIN)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BJC}, BJF) -> {ok, BJF} | {error, BJC}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BJI} | {error, any()}, BJM) -> {ok, BJI} | {error, BJM}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BJP} | {error, any()})) -> list(BJP).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BJV} | {error, BJW},
    fun((BJW) -> {ok, BJV} | {error, BJZ})
) -> {ok, BJV} | {error, BJZ}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
