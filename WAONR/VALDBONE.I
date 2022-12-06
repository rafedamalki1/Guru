/*VALDBONE.I*/

    
    
   DO TRANSACTION:
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  "http://srv01449/guruonweb11/"           /*ONE*/
      valdbtemp.WWWSTART10 =  "http://srv01449/guruonweb10/"           /*ONE*/
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "ONENO"
      valdbtemp.DBNAMN = "granes"
      valdbtemp.DBCON = "-db granes -S 2501 -H ONEGURU01 -N tcp"
      /*sql 2502*/
      valdbtemp.DBPLATS = "d:\elpool\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 1     
      valdbtemp.APPCON = "-AppService GRANESAPP -H ONEGURU01 -S 2531"
      valdbtemp.VALDB = "One Nordic AB".
      valdbtemp.WWWFTP = FALSE.
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  "http://srv01449/guruonweb11/"           /*ONE*/
      valdbtemp.WWWSTART10 =  "http://srv01449/guruonweb10/"           /*ONE*/
      valdbtemp.FORETAG = "GRAN"
      valdbtemp.GFORETAG = "ONENOUTBI"
      valdbtemp.DBNAMN = "UPMTRL"
      valdbtemp.DBCON = "-db UPMTRL -S 2511 -H ONEGURU01 -N tcp"
      /*sql 2512*/
      valdbtemp.DBPLATS = "d:\elpool\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService GRANESAPP -H ONEGURU01 -S 2531"
      valdbtemp.VALDB = "One Nordic AB  MATERIEL ADM ".
      valdbtemp.WWWFTP = FALSE.  
   
   END.
   
  
