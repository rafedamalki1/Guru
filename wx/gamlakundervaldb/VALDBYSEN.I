/*VALDBYSEN.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
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
   