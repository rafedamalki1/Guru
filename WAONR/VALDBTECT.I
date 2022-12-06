  /*VALDBTECT.I*/

 
  DO TRANSACTION:
     
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "TECT"
      valdbtemp.GFORETAG = "TECT"
      valdbtemp.DBNAMN = "TECT"      
      valdbtemp.DBCON = "-db TECT -H " + {egnadb.I} + " -S 2793 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = {AppSingel.i}
      valdbtemp.DBCACHE = ""  
      valdbtemp.VALDB = "Tectel AB".
      valdbtemp.WWWFTP = TRUE.
      
      
      
   END.
