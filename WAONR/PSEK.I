/*PSEK.I*/

IF Guru.Konstanter:globniv NE 0 THEN DO:
   FIND FIRST PERSEK WHERE PERSEK.ANVANDARE = Guru.Konstanter:globanv AND
   PERSEK.PERSONALKOD = PERSONALTAB.PERSONALKOD 
   USE-INDEX PERSEK NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSEK THEN DO: 
      musz = TRUE.
   END.   
   ELSE DO:
      IF PERSEK.PANDRA = FALSE THEN DO:
         musz = TRUE.
      END.   	
   END.
   IF musz = TRUE THEN DO:
      MESSAGE "Du är inte behörig att ändra på denna person." VIEW-AS ALERT-BOX.
   END.
END.      
