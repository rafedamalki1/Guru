/*VALDBELTEL.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
/*
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELTE"
      valdbtemp.GFORETAG = "ELTE"
      valdbtemp.DBNAMN = "ELTEL"
      valdbtemp.DBCON = "-db ELTEL -H 217.212.19.57 -S 2774 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\DELAD\PRO9S\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService APPELTEL -H 217.212.19.57 -S 2776'
      valdbtemp.VALDB = "Eltel Networks".
     
   END.
   */
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELTE"
      valdbtemp.GFORETAG = "nELTE"
      valdbtemp.DBNAMN = "ELTEL"
      valdbtemp.DBCON = "-db ELTEL -H sebro1033asguru -S 2774 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\DELAD\PRO9s\DB\"
      valdbtemp.ORDNING = 1      
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService APPELTEL -H sebro1033asguru -S 2776'
      valdbtemp.VALDB = "Eltel Networks nyaserver".
      valdbtemp.WWWFTP = FALSE.
   END.
/*  
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELTE"
      valdbtemp.GFORETAG = "UELTE"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -H 217.212.19.57 -S 2785 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\DELAD\PRO9S\DB\"
      valdbtemp.ORDNING = 99      
      valdbtemp.DBCACHE = ""
         /*
      valdbtemp.APPCON = "-AppService UAPPELTEL -H 217.212.19.57 -S 2787'
      */
      valdbtemp.APPCON = "-AppService APPELTEL -H 217.212.19.57 -S 2776'
      valdbtemp.VALDB = "Eltel Networks utbildning".    
   END.
   */
