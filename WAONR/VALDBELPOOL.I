/*VALDBELPOOL.I*/

 
   DO TRANSACTION:
     
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "fELPA"
      valdbtemp.DBNAMN = "flextid"
      valdbtemp.ORDNING = 2      
      valdbtemp.DBPLATS = "c:\DELAD\PRO11S\DB\"
      valdbtemp.DBCACHE = "" 
       
      valdbtemp.VALDB = "Elpool Flextid".
      valdbtemp.WWWFTP = FALSE.     
      
      valdbtemp.APPCON = {App11elpool.i}.  
      valdbtemp.DBCON =  "-db flextid -S 2551 -H " + {pc122db.I} + " -N TCP".
      
       
      {VALDBELPSALJ.I}
      
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "OEELPAclas"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 4     
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.VALDB = "Guru utveckling OE11".
      valdbtemp.WWWFTP = FALSE.  
      /*
      valdbtemp.APPCON = {App11elpool.i}.
      */
      valdbtemp.APPCON = "-AppService App116 -H " + {pc122db.I} + " -S 2755".
      valdbtemp.DBCON =  "-db RT9 -S 3327 -H " + {pc122db.I} + " -N TCP".
      
      {VALDBANDERS.I}
      {VALDBLENA.I}  
      
      
      
      
      
      
   END.
