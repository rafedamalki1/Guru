/*VALDBPICA.I*/

 

   
   
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "PICA"
      valdbtemp.GFORETAG = "PICAn"
      valdbtemp.DBNAMN = "PICA"
      valdbtemp.DBCON = "-db PICA -H " + {egnadb.I} + " -S 2775 -N tcp"
      valdbtemp.ORDNING = 3      
      valdbtemp.DBCACHE = ""
     
      valdbtemp.APPCON = {AppSingel.i}
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.VALDB = "PICAB".
      valdbtemp.WWWFTP = TRUE.
      
     
   END.
  