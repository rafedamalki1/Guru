/*VALDBPOLA.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "POLA"
         valdbtemp.GFORETAG = "POLA"
         valdbtemp.DBNAMN = "POLA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db POLA -H " + {www2db.I} + " -S 2833 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Pola Consulting AB".   
         valdbtemp.WWWFTP = TRUE.                                             
   END.
   