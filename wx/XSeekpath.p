/*XSEEKPATH.P*/
DEFINE VARIABLE dlcvar AS CHARACTER FORMAT "X(40)" NO-UNDO.


DEFINE VARIABLE ednum AS INTEGER NO-UNDO.
wtidvar = SEARCH("GURU.W").
/*
wtidvar = SEARCH("GURU.r").  
*/
dlcvar = SEARCH("QUOTER.EXE").

ASSIGN
ednum = LENGTH(wtidvar)
ednum = ednum - 6
wtidvar = SUBSTRING(wtidvar,1,ednum).

ASSIGN
ednum = LENGTH(dlcvar)
ednum = ednum - 10
dlcvar = SUBSTRING(dlcvar,1,ednum).

IF SUBSTRING(wtidvar,1,1) = "." THEN DO:
   guruvar = "..\".
END.

ELSE DO:
   ASSIGN
   ednum = LENGTH(wtidvar)
   ednum = ednum - 5
   guruvar = SUBSTRING(wtidvar,1,ednum).   
END.
MESSAGE guruvar
dlcvar  VIEW-AS ALERT-BOX.
