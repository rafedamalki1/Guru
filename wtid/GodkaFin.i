/*GODKAFIN.I  */   
   IF Guru.Konstanter:appcon THEN DO:     
      musz = musz.
      RUN APFAGOD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar, 
       INPUT regmnr, INPUT regdatum, INPUT regvnr, INPUT Guru.Konstanter:globanv).      
   END.
   ELSE DO:
      RUN APFAGOD.P
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar, 
      INPUT regmnr, INPUT regdatum, INPUT regvnr, INPUT Guru.Konstanter:globanv).
      
   END.
