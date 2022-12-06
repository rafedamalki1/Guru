/*VALDBWIGA.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "WIGA"
         valdbtemp.GFORETAG = "WIGA"
         valdbtemp.DBNAMN = "WIGA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db WIGA -H " + {www2db.I} + " -S 2839 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Wisegate AB".   
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
          
                                                 
   END.
   