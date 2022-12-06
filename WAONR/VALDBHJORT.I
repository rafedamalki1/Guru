/*VALDBHJORT.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "HJEL"
         valdbtemp.GFORETAG = "HJEL"
         valdbtemp.DBNAMN = "HJEL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db HJEL -H " + {www2db.I} + " -S 2815 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "Hjorts Entreprenad och Linjebyggnad". 
         valdbtemp.WWWFTP = TRUE.
                                                        
   END.
   