/*VALDBKEKR.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "KEKR"
         valdbtemp.GFORETAG = "KEKR"
         valdbtemp.DBNAMN = "KEKR"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db KEKR -H " + {www2db.I} + " -S 2867 -N TCP"
                            
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "KE Kraft AB"   
         valdbtemp.WWWFTP = TRUE.  
         valdbtemp.APPCON = {PasOewww2.i}.
         
                                                   
   END.
   