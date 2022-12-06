/*VALDBSKEK.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SKEK"
         valdbtemp.GFORETAG = "SKEK"
         valdbtemp.DBNAMN = "SKEK"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db SKEK -H " + {www2db.I} + " -S 3805 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"      
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "Skandinavisk Elkraft AB".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   