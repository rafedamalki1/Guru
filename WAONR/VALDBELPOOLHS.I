/*VALDBELPOOLHS.I*/

 
   DO TRANSACTION:
      
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "HSELPOOLClassic"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 4      
      valdbtemp.DBPLATS = "D:\DELAD\pro117s\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Guru HS Classic  OE11".
      valdbtemp.WWWFTP = FALSE.  
      /*
      valdbtemp.APPCON = "-AppService app11elpool -H " + {pc122db.I} + " -S 2755".
      */
      valdbtemp.APPCON = "-AppService App117 -H W2019-ELPO -S 2755".
      valdbtemp.DBCON =  "-db RT9 -S 3327 -H W2019-ELPO -N TCP".
      
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "HSELPOOLPasoe"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 4      
      valdbtemp.DBPLATS = "D:\DELAD\pro117s\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Guru HS PASOE OE11"
      valdbtemp.WWWFTP = FALSE  
      valdbtemp.APPCON = "-URL https://egna.guruonweb.se:8445/apsv"
      valdbtemp.DBCON =  "-db RT9 -S 3327 -H W2019-ELPO -N TCP".
      
      
   END.
