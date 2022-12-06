/*VALDBSEKG.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SEKG"
         valdbtemp.GFORETAG = "SEKG"
         valdbtemp.DBNAMN = "SEKG"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SEKG -H 192.168.7.23 -S 2801 -N TCP"
         valdbtemp.APPCON = "-AppService APPSEKG -H 31.216.227.30 -S 2667'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "Svensk Energi - EKG".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   