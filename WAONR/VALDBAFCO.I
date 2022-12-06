/*VALDBAFCON.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "AFCO"
         valdbtemp.GFORETAG = "AFCO"
         valdbtemp.DBNAMN = "AFCO"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db AFCO -H " + {www2db.I} + " -S 2823 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""          
         valdbtemp.VALDB = "ÅF-Industry AB"
         valdbtemp.WWWFTP = TRUE.  
         valdbtemp.APPCON = {APPWWW2.i}.
        
   END.
   