-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EMP) -> EMQ), fun((EMQ) -> EMR)) -> fun((EMP) -> EMR).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EMS, EMT) -> EMU)) -> fun((EMS) -> fun((EMT) -> EMU)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EMW, EMX, EMY) -> EMZ)) -> fun((EMW) -> fun((EMX) -> fun((EMY) -> EMZ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((ENB, ENC, END, ENE) -> ENF)) -> fun((ENB) -> fun((ENC) -> fun((END) -> fun((ENE) -> ENF)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((ENH, ENI, ENJ, ENK, ENL) -> ENM)) -> fun((ENH) -> fun((ENI) -> fun((ENJ) -> fun((ENK) -> fun((ENL) -> ENM))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((ENO, ENP, ENQ, ENR, ENS, ENT) -> ENU)) -> fun((ENO) -> fun((ENP) -> fun((ENQ) -> fun((ENR) -> fun((ENS) -> fun((ENT) -> ENU)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((ENW, ENX) -> ENY)) -> fun((ENX, ENW) -> ENY).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(ENZ) -> ENZ.
identity(X) ->
    X.

-spec constant(EOA) -> fun((any()) -> EOA).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EOC, fun((EOC) -> any())) -> EOC.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EOE) -> EOF), EOE) -> EOF.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EOG, EOH) -> EOI), EOG, EOH) -> EOI.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EOJ, EOK, EOL) -> EOM), EOJ, EOK, EOL) -> EOM.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
