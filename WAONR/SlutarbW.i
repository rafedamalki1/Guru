   /*SLUTARBW.I*/
   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN SLUTARBW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT-OUTPUT pkod,INPUT-OUTPUT regstart,INPUT-OUTPUT regslut, 
      INPUT-OUTPUT regvnr,INPUT-OUTPUT regdagnamn,INPUT-OUTPUT regdatum, 
      INPUT-OUTPUT regtotalt,INPUT-OUTPUT frustarten,INPUT-OUTPUT fruslutet, 
      INPUT-OUTPUT kaffestart,INPUT-OUTPUT kaffeslut,INPUT-OUTPUT lunchstarten,
      INPUT-OUTPUT lunchslutet,INPUT-OUTPUT nytid,INPUT-OUTPUT sekunder).
   END. 
   ELSE DO:
      RUN SLUTARBW.P 
      (INPUT-OUTPUT pkod,INPUT-OUTPUT regstart,INPUT-OUTPUT regslut, 
      INPUT-OUTPUT regvnr,INPUT-OUTPUT regdagnamn,INPUT-OUTPUT regdatum, 
      INPUT-OUTPUT regtotalt,INPUT-OUTPUT frustarten,INPUT-OUTPUT fruslutet, 
      INPUT-OUTPUT kaffestart,INPUT-OUTPUT kaffeslut,INPUT-OUTPUT lunchstarten,
      INPUT-OUTPUT lunchslutet,INPUT-OUTPUT nytid,INPUT-OUTPUT sekunder). 
   END.
