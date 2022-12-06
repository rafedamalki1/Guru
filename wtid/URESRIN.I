/*URESRIN.I*/               
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN SKAPURES.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT TABLE uresapptemp,INPUT TABLE respersextra,
                  INPUT TABLE maltidfil,INPUT TABLE kostfil,
                  INPUT TABLE okost,INPUT TABLE kosters,OUTPUT musz).
               END.
               ELSE DO:
                  RUN SKAPURES.P 
                  (INPUT TABLE uresapptemp,INPUT TABLE respersextra,
                  INPUT TABLE maltidfil,INPUT TABLE kostfil,
                  INPUT TABLE okost,INPUT TABLE kosters,OUTPUT musz).                  
               END.
