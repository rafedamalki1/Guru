/*NYTTAONR1.I*/
IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MALDATUM").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
        (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MALDATUM").                  
      END.
   END.
