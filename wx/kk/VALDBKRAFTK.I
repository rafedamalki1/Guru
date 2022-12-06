/*VALDBKRAFTK.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "GETB"
         valdbtemp.GFORETAG = "GETB"
         valdbtemp.DBNAMN = "GETB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db GETB -H kraftdc01 -S 2801 -N TCP"
         valdbtemp.APPCON = "-AppService APPKRAFT -H kraftdc01 -S 2802"                   
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Kraftkonsult i Skandinavien AB".  
         valdbtemp.WWWFTP = FALSE.
            /*
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "GETB"
         valdbtemp.GFORETAG = "uGETB"
         valdbtemp.DBNAMN = "utbi"
         valdbtemp.ORDNING = 99      
             
         valdbtemp.DBCON = "-db utbi -H kraftdc01 -S 2903 -N TCP"
         valdbtemp.APPCON = "-AppService APPKRAFT -H kraftdc01 -S 2802"                   
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Utbildning".   
         */                                          
   END.
   