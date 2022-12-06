/*VALDBDARPLU.I*/

 
   DO TRANSACTION:
      
         
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  "http://www2.guruonweb.se/EKGonWeb116/"
         valdbtemp.WWWSTART10 = "http://www2.guruonweb.se/EKGonWeb10/"
         valdbtemp.FORETAG = "DARPLU"
         valdbtemp.GFORETAG = "DARPLU"
         valdbtemp.DBNAMN = "DARPLU"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db DARPLU -H " + {egnadb.I} + " -S 2803 -N TCP"
         valdbtemp.APPCON = {PasoeEgna.i} 
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "DarwinPlus".
         valdbtemp.WWWFTP = TRUE.
                                                       
   END.
   