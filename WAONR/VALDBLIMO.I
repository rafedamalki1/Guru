/*VALDBLIMO.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "LIMO"
         valdbtemp.GFORETAG = "LIMO"
         valdbtemp.DBNAMN = "LIMO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db LIMO -H " + {www2db.I} + " -S 2817 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Linjemontage i Grästorp AB". 
         valdbtemp.WWWFTP = TRUE. 
                                                       
   END.
   
   