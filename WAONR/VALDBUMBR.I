/*VALDBUMBR.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "UMBR"
         valdbtemp.GFORETAG = "UMBR"
         valdbtemp.DBNAMN = "UMBR"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db UMBR -H " + {www2db.I} + " -S 2857 -N TCP"
                           
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Umeå Energi Umenet AB Bredband".   
         valdbtemp.WWWFTP = TRUE.   
         valdbtemp.APPCON = {PasOewww2.i}.
        
                                                   
   END.
   