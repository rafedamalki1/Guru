/*VALDBVESAB.I*/
   DO TRANSACTION:
      
        
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "VSAB"
      valdbtemp.GFORETAG = "VSAB"
      valdbtemp.DBNAMN = "VSAB"
      valdbtemp.DBCON = "-db VSAB -S 2631 -H 151.156.177.197 -N tcp"
      valdbtemp.DBPLATS = "e:\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 11      
      valdbtemp.APPCON = "-AppService appvsab -H 151.156.177.197 -S 2633"
      valdbtemp.VALDB = "Vattenfall Service AB".
      valdbtemp.WWWFTP = FALSE.
     
      
     
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "VSAB"
      valdbtemp.GFORETAG = "VUTBI"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBPLATS = "e:\delad\pro9s\dbtillbaka\"
      valdbtemp.DBCON = "-db UTBI -S 2601 -H 151.156.177.197 -N tcp"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 99      
      valdbtemp.APPCON = "-AppService appvutbi -H 151.156.177.197 -S 2615"
      valdbtemp.VALDB = "Vattenfall Utbildning". 
      valdbtemp.WWWFTP = FALSE.     
      
      
      
      
       /*151.156.177.197
       from 151.156.176.205: bytes=32 time<1ms TTL=127
 from sagurup0118        151.156.176.205: bytes=32 time<1ms TTL=127
 from 151.156.176.205: bytes=32 time<1ms TTL=127
 from 151.156.176.205: bytes=32 time<1ms TTL=127
  */    
       CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "VSAB"
      valdbtemp.GFORETAG = "VSAB"
      valdbtemp.DBNAMN = "VSAB"
      valdbtemp.DBCON = "-db VSAB -S 2631 -H 151.156.176.205 -N tcp"
      valdbtemp.DBPLATS = "e:\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 11      
      valdbtemp.APPCON = "-AppService appvsab -H 151.156.176.205 -S 2633"
      valdbtemp.VALDB = "Nya Vattenfall Service AB".
      valdbtemp.WWWFTP = FALSE.
      
   END.
