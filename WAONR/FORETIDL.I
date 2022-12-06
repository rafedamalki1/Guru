/*FORETIDL.I*/
   {NYTTAONR1.I} 
   {NYTTAONR2.I}
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:          
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
      (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "AOUPPLAGT").
   END.
   ELSE DO:
      RUN TLAGAUTO.P  
      (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "AOUPPLAGT").                  
   END.              
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
      (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "AONRAVSL").
   END.
   ELSE DO:
      RUN TLAGAUTO.P  
      (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "AONRAVSL").                  
   END.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:          
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN MEDDAONR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT aonrrec, INPUT 1,INPUT "",INPUT "",INPUT "",OUTPUT skick,OUTPUT efel).
      END.
   END.
   /*aonrsund*/
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" THEN DO:  
      /*GG 2007-07-02 - Alla nya projektnummer ska ha tidplan automatiskt upplagt*/
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDPLAN").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
        (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDPLAN").                  
      END.
      
      /* Borttaget 20130121 efter beslut av Johnny Smedman och Ingrid Eriksson
      TIDSTOPP FORTFARANDE KVAR PÅ DELNR 0 se NYTTAONRFAST.I körs i underfasta_UI i AONRMENY.P
      IF aonrvar BEGINS "34" THEN DO:         
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").
         END.
         ELSE DO:
            RUN TLAGAUTO.P  
           (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDSTOPP").                  
         END.
      END.*/
      
   END.
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDPLAN").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
        (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "TIDPLAN").                  
      END.
   END.
   
   
