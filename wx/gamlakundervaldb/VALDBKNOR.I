/*VALDBKNOR.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "KNOR"
         valdbtemp.GFORETAG = "KNOR"
         valdbtemp.DBNAMN = "KNOR"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db KNOR -H " + {www2db.I} + " -S 2835 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Kraftteknik i norr AB". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   