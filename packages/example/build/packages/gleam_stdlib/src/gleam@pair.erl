-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({FJ, any()}) -> FJ.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), FM}) -> FM.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({FN, FO}) -> {FO, FN}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({FP, FQ}, fun((FP) -> FR)) -> {FR, FQ}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({FS, FT}, fun((FT) -> FU)) -> {FS, FU}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(FV, FW) -> {FV, FW}.
new(First, Second) ->
    {First, Second}.
