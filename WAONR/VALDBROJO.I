/*VALDBROJO.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ROJO"
         valdbtemp.GFORETAG = "ROJO"
         valdbtemp.DBNAMN = "ROJO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ROJO -H " + {www2db.I} + " -S 2889 -N TCP"
                             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Rolf Johansson".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.
          
     END.
   