/*VALDBRAMB.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "RAMB"
         valdbtemp.GFORETAG = "RAMB"
         valdbtemp.DBNAMN = "RAMB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db RAMB -H " + {www2db.I} + " -S 3813 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "Ramböll Sverige AB".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   