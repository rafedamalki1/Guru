/*VALDBGURUUNI.I*/

 /*KÖR AppEgen*/
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ELPA"
         valdbtemp.GFORETAG = "classELPA"
         valdbtemp.DBNAMN = "RT9kalk" /*democ*/
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db RT9kalk -H " + {egnadb.I} + " -S 3553 -N tcp"
         valdbtemp.DBCACHE = ""  
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.VALDB = "Guru Universitet On Web". 
         valdbtemp.WWWFTP = TRUE.
         valdbtemp.APPCON = {AppEgna.i}. 
       
   END.
