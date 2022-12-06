/*VALDBUMEA.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "UMEA"
      valdbtemp.GFORETAG = "UMEA"
      valdbtemp.DBNAMN = "UMEA"
      /*valdbtemp.DBCON = "-db umea -H 172.20.16.18 -S 2721 -N tcp"*/
      valdbtemp.DBCON = "-db umea -H balder -S 2721 -N tcp"
      valdbtemp.DBPLATS = "D:\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPUMEA -H balder -S 2723'
      valdbtemp.VALDB = "Umeå Energi".
      valdbtemp.WWWFTP = FALSE.
   END.
   /*
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "UMEA"
      valdbtemp.GFORETAG = "UTBIUMEA"
      valdbtemp.DBNAMN = "UTBI"
      /*valdbtemp.DBCON = "-db umea -H 172.20.16.18 -S 2721 -N tcp"*/
      valdbtemp.DBCON = "-db UTBI -H balder -S 2737 -N tcp"
      valdbtemp.DBPLATS = "D:\DELAD\PRO9S\DBTILLBAKA\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService APPUMEA -H balder -S 2723'
      valdbtemp.VALDB = "UTBILDNING Umeå Energi".
   END.

   */
   
