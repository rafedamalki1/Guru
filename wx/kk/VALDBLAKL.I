/*VALDBLAKL.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "LAKL"
         valdbtemp.GFORETAG = "LAKL"
         valdbtemp.DBNAMN = "LAKL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db LAKL -H " + {www2db.I} + " -S 2814 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Landers Kraft & ljusteknik AB". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   