/*VALDBATS.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ATS"
         valdbtemp.GFORETAG = "ATS"
         valdbtemp.DBNAMN = "ATS"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ATS -H " + {www2db.I} + " -S 2810 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "ATS Kraftservice".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   