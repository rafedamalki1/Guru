/*AOUTFIN.I*/   
   IF Guru.Konstanter:appcon THEN DO:                           
      musz = musz.
      RUN APAOUTF.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE uppfoltemp,OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN APAOUTF.P 
      (INPUT TABLE uppfoltemp,OUTPUT TABLE tidut).
   END.
