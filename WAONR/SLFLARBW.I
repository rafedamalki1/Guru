/*SLFLARBW.I*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN SLFLARBW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT regstart,INPUT-OUTPUT regslut, 
      INPUT-OUTPUT regvnr,INPUT-OUTPUT regdagnamn,INPUT-OUTPUT regdatum, 
      INPUT-OUTPUT regtotalt,INPUT-OUTPUT frustarten,INPUT-OUTPUT fruslutet, 
      INPUT-OUTPUT kaffestart,INPUT-OUTPUT kaffeslut,INPUT-OUTPUT lunchstarten,
      INPUT-OUTPUT lunchslutet,INPUT-OUTPUT nytid,INPUT-OUTPUT sekunder).
   END.
   ELSE DO:
      RUN SLFLARBW.P 
      (INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT regstart,INPUT-OUTPUT regslut, 
      INPUT-OUTPUT regvnr,INPUT-OUTPUT regdagnamn,INPUT-OUTPUT regdatum, 
      INPUT-OUTPUT regtotalt,INPUT-OUTPUT frustarten,INPUT-OUTPUT fruslutet, 
      INPUT-OUTPUT kaffestart,INPUT-OUTPUT kaffeslut,INPUT-OUTPUT lunchstarten,
      INPUT-OUTPUT lunchslutet,INPUT-OUTPUT nytid,INPUT-OUTPUT sekunder).
   END.
