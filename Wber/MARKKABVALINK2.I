/*MARKKABVALINK2.I*/
   DEFINE VARIABLE enval AS LOGICAL NO-UNDO.
   enval = FALSE.
   IF ink_val.GRUPP = 0 THEN enval = TRUE.   
   ELSE IF LOOKUP(STRING(ink_val.GRUPP),Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.
   