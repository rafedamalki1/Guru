/*VALDBGREL.I*/

 
   DO TRANSACTION:
     
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "GREL"
         valdbtemp.GFORETAG = "GREL"
         valdbtemp.DBNAMN = "GREL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db GREL -H " + {egnadb.I} + " -S 2795 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "GR Elkonsult AB ".
         valdbtemp.WWWFTP = TRUE.
        
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "GRAN"
         valdbtemp.GFORETAG = "GRELONE"
         valdbtemp.DBNAMN = "GRELONE"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db GRELONE -H " + {egnadb.I} + " -S 2797 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}                  
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "GR Elkonsult AB - One Nordic ".
         valdbtemp.WWWFTP = TRUE.   
         
                                                                  
   END.
   