/*VALDBORBI.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ORBI"
         valdbtemp.GFORETAG = "ORBI"
         valdbtemp.DBNAMN = "ORBI"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ORBI -H 192.168.7.23 -S 2753 -N TCP"
         valdbtemp.APPCON = "-AppService APPORBI -H 31.216.227.30 -S 2755'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Orbion Consulting AB".    
         valdbtemp.WWWFTP = TRUE.                                            
   END.
   