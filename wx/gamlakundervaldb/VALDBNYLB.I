/*VALDBNYLB.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "NYLB"
         valdbtemp.GFORETAG = "NYLB"
         valdbtemp.DBNAMN = "NYLB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db NYLB -H " + {www2db.I} + " -S 2827 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Nyströms Linjeberedning". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   