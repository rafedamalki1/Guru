/*VALDBELKB.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ELKB"
         valdbtemp.GFORETAG = "ELKB"
         valdbtemp.DBNAMN = "ELKB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ELKB -H 192.168.7.23 -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPELKB -H 31.216.227.30 -S 2555'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Elkraftberedning i Blekinge".  
         valdbtemp.WWWFTP = TRUE.  
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "GRAN"
         valdbtemp.GFORETAG = "ELKO"
         valdbtemp.DBNAMN = "ELKO"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db ELKO -H 192.168.7.23 -S 2791 -N TCP"
         valdbtemp.APPCON = "-AppService APPELKB -H 31.216.227.30 -S 2555'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Elkraftberedning One Nordic ".  
         valdbtemp.WWWFTP = TRUE.   
                                                     
   END.
   