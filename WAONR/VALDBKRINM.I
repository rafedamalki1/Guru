/*VALDBKRINM.I*/

 
   DO TRANSACTION:
      
         
          CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "KRIN"
         valdbtemp.GFORETAG = "KRINMTRL"
         valdbtemp.DBNAMN = "KRINMTRL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db " + valdbtemp.DBNAMN + " -H " + {www2db.I} + " -S 2895 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "Kraftringen Matriel".  
         valdbtemp.WWWFTP = TRUE.
         valdbtemp.APPCON = {PasOewww2.i}. 
          
        
        
                                
   END.
   