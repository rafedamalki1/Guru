
  /*VALDBLAPP.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
  DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "LAPP"
      valdbtemp.GFORETAG = "LAPP"
      valdbtemp.DBNAMN  = "LAPP"      
      valdbtemp.DBCON = "-db LAPP -H 81.8.169.2 -S 2501 -N tcp"
      valdbtemp.DBPLATS = "c:\elpool\delad\pro10s\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPLAPP -H 81.8.169.2 -S 2503'
      valdbtemp.VALDB = "Lapplands Eltjänst AB".
      valdbtemp.WWWFTP = FALSE.
      
     /* CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "LAPP"
      valdbtemp.GFORETAG = "UTBILAPP"
      valdbtemp.DBNAMN = "UTBI"      
      valdbtemp.DBCON = "-db UTBI -H 192.168.111.52 -S 2511 -N tcp"
      valdbtemp.DBPLATS = "G:\DELAD\PRO9S\DBTILLBAKA\"
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService APPLAPP -H 192.168.111.52 -S 2503'
      valdbtemp.VALDB = "UTBILDNING".
      */
   END.

