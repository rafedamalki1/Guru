/*VALDBSTAMDEMO.I*/          
    
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SNAT"
         valdbtemp.GFORETAG = "SNEUTBI".
         valdbtemp.DBNAMN = valdbtemp.GFORETAG.
         ASSIGN 
         valdbtemp.DBCON = "-db " + valdbtemp.DBNAMN + " -H " + {egnadb.I} + " -S 2526 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\" 
         valdbtemp.DBCACHE = ""   
         valdbtemp.ORDNING = 1     
         valdbtemp.APPCON = {AppEgna.i}.
         valdbtemp.VALDB = "Stamford Demo".  
  

 
         