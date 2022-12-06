/*VALDBELPC.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ELPC"
         valdbtemp.GFORETAG = "ELPC"
         valdbtemp.DBNAMN = "ELPC"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ELPC -H " + {www2db.I} + " -S 2829 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Elpedro Power Consultant AB".   
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {APPWWW2.i}.
                                                     
   END.
   