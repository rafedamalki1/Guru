/*GODKFIN.I  */   
   IF Guru.Konstanter:appcon THEN DO:              
      RUN APGODKF.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar,
      INPUT regmnr, INPUT regdatum, INPUT regvnr, INPUT Guru.Konstanter:globanv).      
   END.
   ELSE DO:
      RUN APGODKF.P 
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar,
      INPUT regmnr, INPUT regdatum, INPUT regvnr, INPUT Guru.Konstanter:globanv).
   END.
   
