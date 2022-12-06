/*VALDBKRIN.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "KRIN"
         valdbtemp.GFORETAG = "KRIN"
         valdbtemp.DBNAMN = "KRIN"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db KRIN -H " + {www2db.I} + " -S 3821 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "Kraftringen".  
         valdbtemp.WWWFTP = TRUE.
         valdbtemp.APPCON = {PasOewww2.i}.
           
        
        
        
                                
   END.
   