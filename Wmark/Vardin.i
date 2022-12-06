/*VARDIN.I*/   
   IF Guru.Konstanter:appcon THEN DO:                           
      musz = musz.
      RUN SKAPPROT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE uppfoltemp,INPUT TABLE markval,OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN SKAPPROT.P 
      (INPUT TABLE uppfoltemp,INPUT TABLE markval,OUTPUT TABLE tidut).
   END.
