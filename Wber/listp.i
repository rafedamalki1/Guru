/*LISTP.I*/   
   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN LISTPROGD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, OUTPUT TABLE mtrl_temp, OUTPUT TABLE lin_upp, 
      OUTPUT TABLE lin_temp).
   END.
   ELSE DO:
      RUN LISTPROGD.P
      (INPUT valaonr, INPUT valomrade, OUTPUT TABLE mtrl_temp, OUTPUT TABLE lin_upp, 
      OUTPUT TABLE lin_temp).
   END.
