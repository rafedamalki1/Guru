/*VALDBPOLA.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "POLA"
         valdbtemp.GFORETAG = "POLA"
         valdbtemp.DBNAMN = "POLA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db POLA -H " + {www2db.I} + " -S 2833 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "Pola Consulting AB".   
         valdbtemp.WWWFTP = TRUE.
                                                      
   END.
   