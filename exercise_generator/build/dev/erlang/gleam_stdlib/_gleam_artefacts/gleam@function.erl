-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((DWE) -> DWF), fun((DWF) -> DWG)) -> fun((DWE) -> DWG).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((DWH, DWI) -> DWJ)) -> fun((DWH) -> fun((DWI) -> DWJ)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((DWL, DWM, DWN) -> DWO)) -> fun((DWL) -> fun((DWM) -> fun((DWN) -> DWO))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((DWQ, DWR, DWS, DWT) -> DWU)) -> fun((DWQ) -> fun((DWR) -> fun((DWS) -> fun((DWT) -> DWU)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((DWW, DWX, DWY, DWZ, DXA) -> DXB)) -> fun((DWW) -> fun((DWX) -> fun((DWY) -> fun((DWZ) -> fun((DXA) -> DXB))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((DXD, DXE, DXF, DXG, DXH, DXI) -> DXJ)) -> fun((DXD) -> fun((DXE) -> fun((DXF) -> fun((DXG) -> fun((DXH) -> fun((DXI) -> DXJ)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((DXL, DXM) -> DXN)) -> fun((DXM, DXL) -> DXN).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(DXO) -> DXO.
identity(X) ->
    X.

-spec constant(DXP) -> fun((any()) -> DXP).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(DXR, fun((DXR) -> any())) -> DXR.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((DXT) -> DXU), DXT) -> DXU.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((DXV, DXW) -> DXX), DXV, DXW) -> DXX.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((DXY, DXZ, DYA) -> DYB), DXY, DXZ, DYA) -> DYB.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
