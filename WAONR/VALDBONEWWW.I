/*VALDBONEWWW.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "GRAN"
         valdbtemp.GFORETAG = "ONEWWW"
         valdbtemp.DBNAMN = "ONEWWW"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db ONEWWW -H " + {www2db.I} + " -S 3845 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "One Nordic On Web".  
         valdbtemp.WWWFTP = TRUE.    
         valdbtemp.APPCON = {APPWWW2.i}.
        
                                                  
   END.
   