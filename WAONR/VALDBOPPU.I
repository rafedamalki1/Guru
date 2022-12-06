/*VALDBOPPU.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "OPPU"
         valdbtemp.GFORETAG = "OPPU"
         valdbtemp.DBNAMN = "OPPU"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db OPPU -H " + {egnadb.I} + " -S 2787 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "Oppunda Kraftkonsult AB".
         valdbtemp.WWWFTP = TRUE.
                                                        
   END.
   