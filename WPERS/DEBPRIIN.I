/*DEBPRIIN.I*/       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PRISDEB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT-OUTPUT TABLE debpris, INPUT forsta, INPUT visstart, INPUT visslut).
      END.
      ELSE DO:
         RUN PRISDEB.P 
         (INPUT-OUTPUT TABLE debpris, INPUT forsta, INPUT visstart, INPUT visslut).
      END.
