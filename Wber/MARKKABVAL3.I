   /*MARKKABVAL3.I*/
   DEFINE VARIABLE enval AS LOGICAL NO-UNDO.
   enval = FALSE.
   IF uppl_temp.GRUPP = 0 THEN enval = TRUE.   
   ELSE IF LOOKUP(STRING(uppl_temp.GRUPP),Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.
   