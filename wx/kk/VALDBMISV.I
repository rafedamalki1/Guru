/*VALDBMISV.I*/          
   /*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "misv"
      valdbtemp.GFORETAG = "misv"
      valdbtemp.DBNAMN = "misv"
      valdbtemp.DBCON = "-db misv -S 2510 -H msvguru -N TCP"
      valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
      valdbtemp.DBCACHE = ""   
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService appnmisv -H msvguru -S 2512'
      valdbtemp.VALDB = "Mittsverige Vatten AB". 
      valdbtemp.WWWFTP = FALSE.
      /*    
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "misv"
      valdbtemp.GFORETAG = "Umisv"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -S 2610 -H msvguru -N TCP"
      valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
      valdbtemp.DBCACHE = ""   
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService appnmisv -H msvguru -S 2512'
      valdbtemp.VALDB = "UTBILDNING Mittsverige Vatten AB".
      */  
   END.
   
