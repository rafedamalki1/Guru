/*VALDBHANA.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "HANA"
         valdbtemp.GFORETAG = "HANA"
         valdbtemp.DBNAMN = "HANA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db HANA -H " + {www2db.I} + " -S 2811 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Hagström i Nås AB".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   