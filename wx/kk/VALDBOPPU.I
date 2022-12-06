/*VALDBOPPU.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "OPPU"
         valdbtemp.GFORETAG = "OPPU"
         valdbtemp.DBNAMN = "OPPU"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db OPPU -H 192.168.7.23 -S 2787 -N TCP"
         valdbtemp.APPCON = "-AppService APPSINGEL -H 31.216.227.30 -S 2665'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "Oppunda Kraftkonsult AB".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   