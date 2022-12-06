/*VALDBSKOGSK.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SKOK"
         valdbtemp.GFORETAG = "SKOGSK"
         valdbtemp.DBNAMN = "SKOGSK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SKOGSK -H 192.168.7.23 -S 2653 -N TCP"
         valdbtemp.APPCON = "-AppService APPSKOGSK -H 31.216.227.30 -S 2655'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Skogskonsult och Kraft".     
         valdbtemp.WWWFTP = TRUE.                                           
   END.
   
    