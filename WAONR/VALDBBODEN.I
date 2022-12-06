/*VALDBBODEN.I*/


   DO TRANSACTION:
         
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "BODE"
      valdbtemp.GFORETAG = "BODEN"
      valdbtemp.DBNAMN  = "BODE"      
      valdbtemp.DBCON = "-db BODE -H " + {egnadb.I} + " -S 2799 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.ORDNING = 1   
      valdbtemp.DBCACHE = ""     
      valdbtemp.APPCON = {AppEgna.i} .
      valdbtemp.VALDB = "Bodens Energi Nät AB".
      valdbtemp.WWWFTP = TRUE.
         
                       
   END.
   
