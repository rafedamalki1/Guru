DEFINE SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE ednum AS INTEGER NO-UNDO.
DEFINE VARIABLE ednum2 AS INTEGER NO-UNDO.
DEFINE VARIABLE kolonvar AS INTEGER NO-UNDO.
DEFINE VARIABLE pathvar AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE sokvar AS CHARACTER FORMAT "X(4)" NO-UNDO.
DEFINE VARIABLE strangvar AS CHARACTER FORMAT "X(15)" NO-UNDO.
   
   IF OPSYS = "WIN32" THEN ASSIGN sokvar = "\DLC" strangvar = "\BIN\quoter".
   ELSE IF OPSYS = "MS-DOS" THEN ASSIGN sokvar = "\DLC" strangvar = "\BIN\quoter".
   ELSE ASSIGN sokvar = "/DLC" strangvar = "/BIN/quoter".
   
   ASSIGN
   pathvar = "F:\PRO8\GURU\KOMP, F:\PRO8\GURU\WTID, F:\PRO8\DLC\BIN"  /*PROPATH*/
   pathvar = TRIM(pathvar).     
   ednum = INDEX(pathvar,sokvar). 
   IF ednum = 0 THEN DO:
      MESSAGE "Ett programfel har uppstått. Kontakta Elpool: Niklas Johnson 090 - 14 10 50."
      VIEW-AS ALERT-BOX TITLE "Programfel".
   END.
   ELSE DO:
      ednum2 = INDEX(pathvar,":").
      IF ednum2 = 0 THEN DO:
         MESSAGE "Ett programfel har uppstått. Kontakta Elpool: Niklas Johnson 090 - 14 10 50."
         VIEW-AS ALERT-BOX TITLE "Programfel".
      END.
      ELSE DO:
         IF ednum2 < ednum THEN DO:
            DO WHILE ednum2 < ednum:
               kolonvar = ednum2.
               ednum2 = ednum2 + 1.
               ednum2 = INDEX(pathvar,":",ednum2).
               IF ednum2 = 0 THEN LEAVE.
            END.
            quotervar = SUBSTRING(pathvar,(kolonvar - 1),((ednum - kolonvar) + 5))
            + strangvar.
         END.
         ELSE DO:   
            MESSAGE "Ett programfel har uppstått. Kontakta Elpool: Niklas Johnson 090 - 14 10 50."
            VIEW-AS ALERT-BOX TITLE "Programfel".
         END. 
      END.   
   END.      
   message quotervar.
