/*VALDBELPC.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ELPC"
         valdbtemp.GFORETAG = "ELPC"
         valdbtemp.DBNAMN = "ELPC"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ELPC -H " + {www2db.I} + " -S 2829 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Elpedro Power Consultant AB".   
         valdbtemp.WWWFTP = TRUE.                                             
   END.
   