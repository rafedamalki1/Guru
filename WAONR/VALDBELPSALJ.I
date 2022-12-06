/*VALDBELPSALJ.I*/

 
     
      
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "SALJELPA"
      valdbtemp.DBNAMN = "ST"
      valdbtemp.ORDNING = 3      
      valdbtemp.DBPLATS = "c:\DELAD\PRO11S\DB\"
      valdbtemp.DBCACHE = "" 
      valdbtemp.VALDB = "Elpool Sälj".      
      valdbtemp.WWWFTP = FALSE.
      
     valdbtemp.APPCON = {App11elpool.i}.  
     valdbtemp.DBCON = "-db ST -S 2563 -H " + {pc122db.I} + " -N TCP".