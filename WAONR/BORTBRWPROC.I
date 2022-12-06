/*BORTBRWPROC.I*/

DEFINE VARIABLE i AS INTEGER NO-UNDO INITIAL 1.
DO WHILE i LE 30:
    
   IF VALID-HANDLE(brwproc[i]) THEN DO:
      RUN ostempdel_UI IN brwproc[i].
      DELETE PROCEDURE brwproc[i] NO-ERROR.
   END.   
   brwproc[i] = ?.
   i = i + 1.
END.
IF VALID-HANDLE(framesizeh) THEN DELETE PROCEDURE framesizeh NO-ERROR.
framesizeh = ?.
