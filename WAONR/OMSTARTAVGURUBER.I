/*OMSTARTAVGURUBER.I*/   
/*I  BEREDNING*/

   IF Guru.Konstanter:globanvbyt = {LOSENKOLLFEL10.I} THEN DO:
      RUN avsluta_UI IN huvprogh.
      RETURN. 
   END. 
