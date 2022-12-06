DEFINE SHARED VARIABLE wtidvar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE SHARED VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE ednum AS INTEGER NO-UNDO.
DEFINE VARIABLE ednum2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kolonvar AS INTEGER NO-UNDO.
DEFINE VARIABLE pathvar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE sokvar AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE strangvar AS CHARACTER FORMAT "X(15)" NO-UNDO.
   
   IF OPSYS = "WIN32" THEN ASSIGN sokvar = "\WTID" strangvar = "\priskat.q".
   ELSE IF OPSYS = "MS-DOS" THEN ASSIGN sokvar = "\WTID" strangvar = "\priskat.q".
   ELSE ASSIGN sokvar = "/WTID" strangvar = "/priskat.q".
   
   ASSIGN
   pathvar = PROPATH.
   pathvar = TRIM(pathvar).     
   ednum = INDEX(pathvar,sokvar). 
   IF ednum = 0 THEN DO:
      felvar = TRUE.
   END.
   ELSE DO:
      ednum2 = INDEX(pathvar,":").
      IF ednum2 = 0 THEN DO:
         felvar = TRUE.
      END.
      ELSE DO:
         IF ednum2 < ednum THEN DO:
            DO WHILE ednum2 < ednum:
               kolonvar = ednum2.
               ednum2 = ednum2 + 1.
               ednum2 = INDEX(pathvar,":",ednum2).
               IF ednum2 = 0 THEN LEAVE.
            END.
            wtidvar = SUBSTRING(pathvar,(kolonvar - 1),((ednum - kolonvar) + 6))
            + strangvar.
         END.
         ELSE DO:   
            felvar = TRUE.
         END. 
      END.   
   END.        
