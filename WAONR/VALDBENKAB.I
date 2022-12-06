/*VALDBENKAB.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ENKA"
         valdbtemp.GFORETAG = "ENKA"
         valdbtemp.DBNAMN = "ENKA"
         valdbtemp.ORDNING = 0      
         valdbtemp.DBCON = "-db ENKA -H " + {www2db.I} + " -S 2859 -N TCP"
         
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "ENKAB Elnätkonsult i norr AB".   
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.  
                                            
   END.
   