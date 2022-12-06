/*VALDBDUTA.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "DUTA"
         valdbtemp.GFORETAG = "DUTA"
         valdbtemp.DBNAMN = "DUTA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db DUTA -H " + {www2db.I} + " -S 2873 -N TCP"
                           
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Dutab AB".  
         valdbtemp.WWWFTP = TRUE. 
         valdbtemp.APPCON = {PasOewww2.i}.  
         
                                                      
   END.
   