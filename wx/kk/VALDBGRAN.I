/*VALDBGRAN.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER  VALDBALL.I +
    
    
   DO TRANSACTION:
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "GRANES"
      valdbtemp.DBNAMN = "granes"
      valdbtemp.DBCON = "-db granes -S 2501 -H srv00938 -N tcp"
      valdbtemp.DBPLATS = "/u01/guru/db9/"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService GRANESAPP -H srv00938 -S 2531'
      valdbtemp.VALDB = "E.on.ES Välj detta!".
      */
       CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "GRANES"
      valdbtemp.DBNAMN = "granes"
      valdbtemp.DBCON = "-db granes -S 2501 -H srv01449 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService GRANESAPP -H srv01449 -S 2531'
      valdbtemp.VALDB = "One Nordic".
      valdbtemp.WWWFTP = FALSE.
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "ESUTBI"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -S 2511 -H srv01449 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService GRANESAPP -H srv01449 -S 2531'
      valdbtemp.VALDB = "Utbildning".
      valdbtemp.WWWFTP = FALSE.  
   
   END.
   
  
