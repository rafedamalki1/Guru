/*VALDBKEWA.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "KEWA"
         valdbtemp.GFORETAG = "KEWA"
         valdbtemp.DBNAMN = "KEWA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db KEWA -H " + {www2db.I} + " -S 2831 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Axeda Entreprenad AB".  
         valdbtemp.WWWFTP = TRUE. 
                                                      
   END.
   