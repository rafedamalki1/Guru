/*RESRENIN.I*/    
           

   IF Guru.Konstanter:appcon THEN DO:                           
      RUN SKAPAREN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE resapptemp,INPUT TABLE respersextra,
      INPUT TABLE maltidfil,INPUT TABLE kostfil,
      INPUT TABLE okost,INPUT TABLE kosters,OUTPUT musz).
   END.
   ELSE DO:
      RUN SKAPAREN.P 
      (INPUT TABLE resapptemp,INPUT TABLE respersextra,
      INPUT TABLE maltidfil,INPUT TABLE kostfil,
      INPUT TABLE okost,INPUT TABLE kosters,OUTPUT musz).                  
   END.
