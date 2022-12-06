/*FLRPRIIN.I*/       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN FLRDEB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT-OUTPUT TABLE debpris).
      END.
      ELSE DO:
         RUN FLRDEB.P 
         (INPUT-OUTPUT TABLE debpris).
      END.
