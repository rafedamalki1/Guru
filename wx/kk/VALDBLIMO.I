/*VALDBLIMO.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "LIMO"
         valdbtemp.GFORETAG = "LIMO"
         valdbtemp.DBNAMN = "LIMO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db LIMO -H " + {www2db.I} + " -S 2817 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Linjemontage i Grästorp AB". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   
   