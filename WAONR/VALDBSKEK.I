/*VALDBSKEK.I*/

 
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SKEK"
         valdbtemp.GFORETAG = "SKEK"
         valdbtemp.DBNAMN = "SKEK"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db SKEK -H " + {www2db.I} + " -S 3805 -N TCP"
         valdbtemp.APPCON = {Appasp.i}      
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "Skandinavisk Elkraft AB".
         valdbtemp.WWWFTP = TRUE. 
                                                        
   END.
   