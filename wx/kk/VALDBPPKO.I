/*VALDBPPKO.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "PPKO"
         valdbtemp.GFORETAG = "PPKO"
         valdbtemp.DBNAMN = "PPKO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db PPKO -H " + {www2db.I} + " -S 2825 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Projekt och Planerings Konsult".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   