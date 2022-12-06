/*VALDBSWECO.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SWEO"
         valdbtemp.GFORETAG = "SWEO"
         valdbtemp.DBNAMN = "SWEO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SWEO -H " + {www2db.I} + " -S 2813 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Sweco". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   