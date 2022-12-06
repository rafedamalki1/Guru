/*VALDBAKEA.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "AKEA"
         valdbtemp.GFORETAG = "AKEA"
         valdbtemp.DBNAMN = "AKEA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db AKEA -H " + {www2db.I} + " -S 2883 -N TCP"
                             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "AKEA".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
        
         
     END.
   