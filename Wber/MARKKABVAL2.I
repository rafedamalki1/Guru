   /*MARKKABVAL2.I Guru.Konstanter:varforetypchar[5] = MARKSTATIONSGRUPP*/
   
   enval = FALSE.
   IF AVAILABLE kon_val THEN DO:
      IF kon_val.GRUPP = 0 THEN enval = TRUE.   
      ELSE IF LOOKUP(STRING(kon_val.GRUPP), Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.
   END.