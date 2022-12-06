/*VALDBUTBIWWW.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SKOL"
         valdbtemp.GFORETAG = "SKOL"
         valdbtemp.DBNAMN = "SKOL"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db SKOL -H " + {www2db.I} + " -S 2869 -N TCP"
                            
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Utbildning On Web"   
         valdbtemp.WWWFTP = TRUE.  
         valdbtemp.APPCON = {PasOewww2.i}.
         
                                                   
   END.
   