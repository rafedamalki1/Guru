/*VALDBKRAF.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "KRAF"
         valdbtemp.GFORETAG = "KRAF"
         valdbtemp.DBNAMN = "KRAF"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db KRAF -H " + {www2db.I} + " -S 2837 -N TCP"
                            
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "KraftTeknik i Stockholm AB".   
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
         
         
                                                 
   END.
   