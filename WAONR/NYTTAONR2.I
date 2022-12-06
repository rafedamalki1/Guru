/*NYTTAONR2.I*/
  IF musz = TRUE THEN DO:
      musz = FALSE.      
      RETURN.
   END.
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:          
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "BESTÄLLT").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "BESTÄLLT").                  
      END.  
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
      (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "AONRAVSL").
   END.
   ELSE DO:
      RUN TLAGAUTO.P  
      (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "AONRAVSL").                  
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN"  THEN musz = musz.
   ELSE DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 43
      soktemp.SOKINT[1] = INTEGER(aonrrec).
      {SOKANROP.I}        
   END.
