/*markIN.I*/   
   IF Guru.Konstanter:appcon THEN DO:                           
      musz = musz.
      RUN SKAPMARK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE uppfoltemp,INPUT TABLE markval,OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN SKAPMARK.P 
      (INPUT TABLE uppfoltemp,INPUT TABLE markval,OUTPUT TABLE tidut).
   END.
