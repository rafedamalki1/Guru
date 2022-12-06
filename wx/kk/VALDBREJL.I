/*VALDBREJL.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "REJI"
         valdbtemp.GFORETAG = "REJI"
         valdbtemp.DBNAMN = "REJL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db REJL -H srv-guru1 -S 2553 -N TCP"
         
         valdbtemp.APPCON = "-AppService APPREJL -H srv-guru1 -S 2555'                   
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = " Rejlers".
         valdbtemp.WWWFTP = FALSE.
         /*
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "REJI"
         valdbtemp.GFORETAG = "utbiREJI"
         valdbtemp.DBNAMN = "UTBI"
         valdbtemp.ORDNING = 99      
             
         valdbtemp.DBCON = "-db UTBI -H srv-guru1 -S 2563 -N TCP"
         valdbtemp.APPCON = "-AppService APPREJL -H 192.168.142.249 -S 2555'                   
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DBTILLBAKA\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "UTBILDNING Rejlers".
         valdbtemp.WWWFTP = FALSE.
         */
         /*
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "REJI"
         valdbtemp.GFORETAG = "utbiEREJI"
         valdbtemp.DBNAMN = "UTBI"
         valdbtemp.ORDNING = 100      
         
         valdbtemp.DBCON = "-db rejl -H " + {www2db.I} + " -S 3811 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"      
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "UTBILDNING ELPOOL Rejlers".
         valdbtemp.WWWFTP = FALSE.
         */
        /*
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "REJI"
         valdbtemp.GFORETAG = "RUTBI"
         valdbtemp.DBNAMN = "UTBI"
         valdbtemp.ORDNING = 99      
             
         valdbtemp.DBCON = "-db UTBI -H srv-guru1 -S 2563 -N TCP"
         valdbtemp.APPCON = "-AppService APPUTBI -H srv-guru1 -S 2565'                   
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "UTBILDNING Rejlers".   
          */             
   END.
   
