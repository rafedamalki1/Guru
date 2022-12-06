  /*VALDBTECT.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
  DO TRANSACTION:
     /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "TECT"
      valdbtemp.GFORETAG = "TECT"
      valdbtemp.DBNAMN = "TECT"      
      valdbtemp.DBCON = "-db TECT -H tectelarp -S 2501 -N tcp"
      valdbtemp.DBPLATS = "D:\SHARED\elpool\PRO9S\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPTECT -H 10.1.1.11 -S 2503'
      valdbtemp.VALDB = "Tectel AB".
      valdbtemp.WWWFTP = FALSE.
      */
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "TECT"
      valdbtemp.GFORETAG = "TECT"
      valdbtemp.DBNAMN = "TECT"      
      valdbtemp.DBCON = "-db TECT -H 192.168.7.23 -S 2793 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPSINGEL -H 31.216.227.30 -S 2665'
      valdbtemp.DBCACHE = ""  
      valdbtemp.VALDB = "Tectel AB".
      valdbtemp.WWWFTP = TRUE.
      
      
      
      
   END.
