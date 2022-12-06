/*VALDBSUND9.I*/          
   /*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
  
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "SUND"
      valdbtemp.GFORETAG = "SUNDN9"
      valdbtemp.DBNAMN = "SUNDn9"
      valdbtemp.DBCON = "-db sundn9 -S 2516 -H seguru -N TCP"
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO10S\DB\"
      valdbtemp.DBCACHE = ""   
      valdbtemp.ORDNING = 1     
      valdbtemp.APPCON = "-AppService appnsund -H seguru -S 2518'
      valdbtemp.VALDB = "Sundsvall Energi AB".
      valdbtemp.WWWFTP = FALSE.
      
   END.
   
/*   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "SUND"
      valdbtemp.GFORETAG = "UTBISUND"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -S 2506 -H seguru  -N TCP"
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO9S\DBTILLBAKA\"
      /*valdbtemp.DBCACHE = ""*/
      valdbtemp.ORDNING = 99      
      valdbtemp.APPCON = "-AppService apputbi -H seguru -S 2508'
      valdbtemp.VALDB = "Sundsvall Utbildning".
      
   END.*/
   
    
