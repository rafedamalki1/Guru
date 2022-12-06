/*VALDBESKOG.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ESKO"
         valdbtemp.GFORETAG = "ESKO"
         valdbtemp.DBNAMN = "ESKOG"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db ESKOG -H " + {egnadb.I} + " -S 2785 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "El & Skog AB".
         valdbtemp.WWWFTP = TRUE.   
                                                    
   END.
   