/*SPROJ.I*/      
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN SPROJAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE kalaonr).
   END.
   ELSE DO:
      RUN SPROJAPP.P
      (OUTPUT TABLE kalaonr).
   END.   
