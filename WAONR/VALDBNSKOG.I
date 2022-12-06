/*VALDBNSKOG.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SKOG"
         valdbtemp.GFORETAG = "SKOG"
         valdbtemp.DBNAMN = "NSKOG"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db NSKOG -H " + {egnadb.I} + " -S 2789 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}                  
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "Nätstruktur Skogaholm AB".
         valdbtemp.WWWFTP = TRUE.  
             
                                                        
   END.
   