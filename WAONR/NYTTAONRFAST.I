   /*NYTTAONRFAST.I*/
   {FORETIDL.I}   

   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      /*20130521 ENLIGT SAMTAL MED INGRID  TIDSTOPP ÄVEN PÅ DELNR 99*/
      IF delnrvar = 0 OR delnrvar = 99 /*OR delnrvar = 5 OR delnrvar = 6*/ THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
           (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").                  
         END.
      END.
      
   END.
