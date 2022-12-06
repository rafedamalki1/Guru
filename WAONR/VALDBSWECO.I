/*VALDBSWECO.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SWEO"
         valdbtemp.GFORETAG = "SWEO"
         valdbtemp.DBNAMN = "SWEO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SWEO -H " + {www2db.I} + " -S 2813 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Sweco". 
         valdbtemp.WWWFTP = TRUE.   
        
         
         
                                             
   END.
   