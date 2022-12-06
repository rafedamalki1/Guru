/*SKABERIN.I*/      
      
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN SKAPABERN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT TABLE bertemp,OUTPUT musz).
         END.
         ELSE DO:
            RUN SKAPABERN.P 
            (INPUT TABLE bertemp,OUTPUT musz).
         END.   
      /*END.      */
