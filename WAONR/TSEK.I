/*TSEK.I*/  
IF Guru.Konstanter:globniv NE 0 THEN DO:
  
   FIND FIRST TIDSEK WHERE TIDSEK.ANVANDARE = Guru.Konstanter:globanv AND
   TIDSEK.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX TIDSEK NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE TIDSEK THEN DO: 
      musz = TRUE.
   END.   
   ELSE DO:
      IF TIDSEK.PANDRA = FALSE THEN DO:
         musz = TRUE.
      END. 
      ELSE DO:
         musz = FALSE.
      END.  	
   END.   
END.      
