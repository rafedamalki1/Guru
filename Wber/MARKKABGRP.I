/*MARKKABGRP.I*/
   DEFINE VARIABLE enval AS LOGICAL NO-UNDO.
   enval = FALSE.
   IF AVAILABLE konstgrptemp THEN DO:
      IF konstgrptemp.KONSKOD = 0 THEN enval = TRUE.   
      ELSE IF LOOKUP(STRING(konstgrptemp.KONSKOD),Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.
   END.
