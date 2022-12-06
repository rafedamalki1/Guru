/*VALDBDEMOF.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
           
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ELPA"
         valdbtemp.GFORETAG = "FFELPA"
         valdbtemp.DBNAMN = "RT9"
         valdbtemp.ORDNING = 1      
            
         valdbtemp.DBCON = "-db RT9 -H 192.168.1.202 -S 2553 -N TCP -U ELPAO -P KAGGEN"
         /*   
         valdbtemp.DBCON = "-db RT9 -H 89.160.51.202 -S 2775 -N TCP -U ELPAO -P KAGGEN"
         */
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.APPCON = "-AppService APPDEMOF -H 89.160.51.202 -S 2555'
         valdbtemp.VALDB = "Guru Demo Fortum".    
         valdbtemp.WWWFTP = FALSE.             
   END.
