/*VALDBPOFO.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "POFO"
         valdbtemp.GFORETAG = "POFO"
         valdbtemp.DBNAMN = "POFO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db POFO -H " + {www2db.I} + " -S 2891 -N TCP"
                             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Power Forze AB".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
          
     END.
   