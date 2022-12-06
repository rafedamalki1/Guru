/*VALDBBORL.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 

   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "BORL"
      valdbtemp.GFORETAG = "BORL"
      valdbtemp.DBNAMN = "BORL"
      valdbtemp.DBCON = "-db borl -H 10.50.1.5 -S 2701 -N tcp"
      valdbtemp.DBPLATS = "D:\GURU\PRO9S\DB\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPBORL -H 10.50.1.5 -S 2703'
      valdbtemp.VALDB = "AB Borlänge Energi".
      valdbtemp.WWWFTP = FALSE.
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "BORL"
      valdbtemp.GFORETAG = "UTBIBORL"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -H 10.50.1.5 -S 2704 -N tcp"
      valdbtemp.DBPLATS = "D:\GURU\PRO9S\DBTILLBAKA\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.ORDNING = 99      
      valdbtemp.APPCON = "-AppService UTBI -H 10.50.1.5 -S 2706'
      valdbtemp.VALDB = "UTBILDNING".  
      */
   END.
