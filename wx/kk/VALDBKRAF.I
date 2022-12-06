/*VALDBKRAF.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "KRAF"
         valdbtemp.GFORETAG = "KRAF"
         valdbtemp.DBNAMN = "KRAF"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db KRAF -H " + {www2db.I} + " -S 2837 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "KraftTeknik i Stockholm AB".   
         valdbtemp.WWWFTP = TRUE.                                             
   END.
   