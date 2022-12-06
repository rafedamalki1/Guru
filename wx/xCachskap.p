/*CACHSKAP.P*/


                              
DEFINE OUTPUT PARAMETER cguruvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.
RUN PROVAG.P.
cguruvar = guruvar.
 prognamn = guruvar + "t.txt".
 OUTPUT TO VALUE(prognamn).
 PUT "ttttttt" AT 1 SKIP
 guruvar  AT 1 SKIP
 LDBNAME(1) AT 1.
 OUTPUT CLOSE.


IF CONNECTED("granadm9") THEN DO:
   prognamn = guruvar + "granadm9.CSH".
   RUN cache_UI.
END.              

PROCEDURE cache_UI:
   SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).
END PROCEDURE.
