/*VALDBETS.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ETSA"
         valdbtemp.GFORETAG = "ETSA"
         valdbtemp.DBNAMN = "ETSA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ETSA -H " + {www2db.I} + " -S 2812 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Energy Technical Service Sweden AB".
         valdbtemp.WWWFTP = TRUE.  
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ETSA"
         valdbtemp.GFORETAG = "ETSAONE"
         valdbtemp.DBNAMN = "ETSAONE"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db ETSAONE -H " + {www2db.I} + " -S 3815 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "ETSA-One Nordic".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   