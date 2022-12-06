/*VALDBNOSS.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "NOSS"
         valdbtemp.GFORETAG = "NOSS"
         valdbtemp.DBNAMN = "NOSS"
         valdbtemp.ORDNING = 0      
         valdbtemp.DBCON = "-db NOSS -H " + {www2db.I} + " -S 2853 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Nossebro Energi".   
         valdbtemp.WWWFTP = TRUE.   
         valdbtemp.APPCON = {PasOewww2.i}.
                                              
   END.
   