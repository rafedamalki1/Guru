/*VALDBSUNDNAT.I*/          
   /*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
  
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "SNAT"
      valdbtemp.GFORETAG = "SUNDNAT"
      valdbtemp.DBNAMN = "SUNDNAT"
      valdbtemp.DBCON = "-db SUNDNAT -S 2526 -H seguru -N TCP"
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO10S\DB\"
      valdbtemp.DBCACHE = ""   
      valdbtemp.ORDNING = 2      
      valdbtemp.APPCON = "-AppService appnsund -H seguru -S 2518'
      valdbtemp.VALDB = "Sundsvall Elnät AB".  
      valdbtemp.WWWFTP = FALSE.
   /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "SNAT"
      valdbtemp.GFORETAG = "USUNDNAT"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -S 2528 -H seguru -N TCP"
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO10S\DB\"
      valdbtemp.DBCACHE = ""   
      valdbtemp.ORDNING = 99      
      valdbtemp.APPCON = "-AppService appnsund -H seguru -S 2518'
      valdbtemp.VALDB = "Utbildning Sundsvall Elnät AB". 
      valdbtemp.WWWFTP = FALSE. 
   */