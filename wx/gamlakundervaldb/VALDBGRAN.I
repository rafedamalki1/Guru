/*VALDBGRAN.I*/

    
    
   DO TRANSACTION:
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  "http://srv01449/guruonweb11/"           /*ONE*/
      valdbtemp.WWWSTART10 =  "http://srv01449/guruonweb10/"           /*ONE*/
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "GRANES"
      valdbtemp.DBNAMN = "granes"
      valdbtemp.DBCON = "-db granes -S 2501 -H srv01449 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService GRANESAPP -H srv01449 -S 2531"
      valdbtemp.VALDB = "One Nordic".
      valdbtemp.WWWFTP = FALSE.
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  "http://srv01449/guruonweb11/"           /*ONE*/
      valdbtemp.WWWSTART10 =  "http://srv01449/guruonweb10/"           /*ONE*/
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "ESUTBI"
      valdbtemp.DBNAMN = "UPMTRL"
      valdbtemp.DBCON = "-db UPMTRL -S 2511 -H srv01449 -N tcp"
      valdbtemp.DBPLATS = "d:\elpool\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService GRANESAPP -H srv01449 -S 2531"
      valdbtemp.VALDB = "One Nordic MATERIEL ADM".
      valdbtemp.WWWFTP = FALSE.  
   
   END.
   
  
