/*VALDBELPOOLTKW.I*/

 
   DO TRANSACTION:
      
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "BMELPOOL"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 5      
      valdbtemp.DBPLATS = "C:\elpool\pro117s\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Guru BM utveckling OE11".
      valdbtemp.WWWFTP = FALSE.  
      /*
      valdbtemp.APPCON = "-AppService app11elpool -H " + {pc122db.I} + " -S 2755".
      */
      valdbtemp.APPCON = "-AppService App116 -H KD-HP840-BRIOLS -S 2755".
      valdbtemp.DBCON =  "-db RT9 -S 3327 -H KD-HP840-BRIOLS -N TCP".
      
      
      
      
   END.
