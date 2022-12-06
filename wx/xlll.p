DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando1 AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(132)" NO-UNDO.
kommando1 = "d:\delad\pro9\guru\wx\tp.w".
kommando2 = "d:\delad\pro9\guru\wx\lll.w".
ccc
kommando = SUBSTITUTE("d:\delad\pro9\wrk\co.bat &1 &2", kommando1,kommando2).
   OS-COMMAND VALUE(kommando).
