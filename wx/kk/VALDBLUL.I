/*VALDBLUL.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER  VALDBALL.I +
    
   DO TRANSACTION:
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "LULE"
      valdbtemp.GFORETAG = "LULE"
      valdbtemp.DBNAMN = "LULE"
      valdbtemp.ORDNING = 1      
      valdbtemp.DBCON = "-db LULE -S 2761 -H 195.196.19.8 -N tcp"
      valdbtemp.DBPLATS = "D:\ELPOOL\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService APPLULE -H 195.196.19.8 -S 2763'
      valdbtemp.VALDB = "Lule? Energikoncernen".
      */
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "LULE"
      valdbtemp.GFORETAG = "LULE"
      valdbtemp.DBNAMN = "LULE"
      valdbtemp.DBCON = "-db LULE -S 2761 -H 195.196.19.21 -N tcp"
      valdbtemp.DBPLATS = "D:\ELPOOL\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPLULE -H 195.196.19.21 -S 2763'
      valdbtemp.VALDB = "Lule? Energikoncern".
      valdbtemp.WWWFTP = FALSE.
      
   END.
  
