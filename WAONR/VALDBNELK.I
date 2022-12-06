/*VALDBNELK.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "NELK"
         valdbtemp.GFORETAG = "NELK"
         valdbtemp.DBNAMN = "NELK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db NELK -H " + {www2db.I} + " -S 2893 -N TCP"
                             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Nelkon AB".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
         
     END.
   