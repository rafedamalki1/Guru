/*VALDBPICA.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 

   
   
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "PICA"
      valdbtemp.GFORETAG = "PICAn"
      valdbtemp.DBNAMN = "PICA"
      valdbtemp.DBCON = "-db PICA -H 192.168.7.23 -S 2775 -N tcp"
      valdbtemp.ORDNING = 3      
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService APPPICA -H 31.216.227.30 -S 2455'                   
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.VALDB = "PICAB".
      valdbtemp.WWWFTP = TRUE.
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "PICA"
      valdbtemp.GFORETAG = "UPICA"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -H 192.168.7.23 -S 2785 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DBkopia\"
      valdbtemp.ORDNING = 99      
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService APPPICA -H 31.216.227.30 -S 2455'
      valdbtemp.VALDB = "PICAB utbildning".  
        */       
     
   END.
  