/*VALDBNSKOG.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SKOG"
         valdbtemp.GFORETAG = "SKOG"
         valdbtemp.DBNAMN = "NSKOG"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db NSKOG -H 192.168.7.23 -S 2789 -N TCP"
         valdbtemp.APPCON = "-AppService APPSINGEL -H 31.216.227.30 -S 2665'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "Nätsruktur Skogaholm AB".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   