/*SEEKPATH.P körs inte*/
DEFINE VARIABLE dlcvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE wtidvar AS CHARACTER NO-UNDO.

DEFINE VARIABLE ednum AS INTEGER NO-UNDO.
IF Guru.Konstanter:globforetag = "ELPA" THEN wtidvar = SEARCH("GURU.W").
ELSE wtidvar = SEARCH("GURU.r").  
IF OPSYS = "unix" THEN dlcvar = SEARCH("QUOTER").
ELSE dlcvar = SEARCH("QUOTER.EXE").
ASSIGN
ednum = LENGTH(wtidvar)
ednum = ednum - 6
wtidvar = SUBSTRING(wtidvar,1,ednum).

ASSIGN
ednum = LENGTH(dlcvar)
ednum = ednum - 10
dlcvar = SUBSTRING(dlcvar,1,ednum).

IF SUBSTRING(wtidvar,1,1) = "." THEN DO:
   Guru.Konstanter:guruvar = "..\".
END.

ELSE DO:
   ASSIGN
   ednum = LENGTH(wtidvar)
   ednum = ednum - 5
   Guru.Konstanter:guruvar = SUBSTRING(wtidvar,1,ednum) + "\".   
END.
IF OPSYS = "unix" THEN DO:
   Guru.Konstanter:guruvar = REPLACE(Guru.Konstanter:guruvar,"\","/").
END.

