/*VALDBYSEN.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "YSEN"
         valdbtemp.GFORETAG = "YSEN"
         valdbtemp.DBNAMN = "YSEN"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db YSEN -H " + {www2db.I} + " -S 3809 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "Ystad Energi AB/Elnät".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   