/*VALDBINFRAOMEXOM.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
/*
Servrarna är i dagsläget medlem i en arbetsgrupp: workgroup

Elpool-SRV01  192.168.7.21    ->                         89.160.54.6:3328 infra
Elpool-SRV02  " + {www2db.I} + "    ->                         89.160.54.6:3329 www2 
Elpool-SRV03  192.168.7.23    ->                         89.160.54.6:3330  egna
*/
   DO TRANSACTION:
       
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "FORS"
         valdbtemp.GFORETAG = "FORS"
         valdbtemp.DBNAMN = "FORS"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db FORS -H 192.168.7.21 -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPFORS -H 89.160.54.6 -S 2555'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "INFRATEK".
         valdbtemp.WWWFTP = TRUE.
        /*
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "FORS"
         valdbtemp.GFORETAG = "FORSUTBI"
         valdbtemp.DBNAMN = "UTBI"
         valdbtemp.ORDNING = 2      
         valdbtemp.DBCON = "-db UTBI -H 192.168.7.21 -S 2558 -N TCP"   
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\dbtillbaka\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.APPCON = "-AppService APPFORS -H 89.160.54.6 -S 2555'
         valdbtemp.VALDB = "Utbildning Service".   
          */
         
                       
   END.
   /*
   DO TRANSACTION:
            
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "FORS"
         valdbtemp.GFORETAG = "FORSUTBI"
         valdbtemp.DBNAMN = "UTBI"
         valdbtemp.ORDNING = 2      
         valdbtemp.DBCON = "-db UTBI -H 192.168.7.10 -S 2558 -N TCP"   
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\dbtillbaka\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.APPCON = "-AppService APPFORS -H 89.160.54.6 -S 2555'
         valdbtemp.VALDB = "Utbildning Service".                 
   END.
   */
   
     /*
            http://89.160.54.6:8080/wselpool/wsinfra
            http://localhost:8080/wselpool/wsinfra
            valdbtemp.APPCON = "-AppService APPFORS -H 89.160.54.6 -S 2555'  
           valdbtemp.APPCON = "-URL http://89.160.54.6:8080/aia/aiaf?AppService=APPFORS'                  
       http://89.160.54.6/aia/APPFORS?GetServletStatus                  
          */

       
            
         
         /*lligo
         valdbtemp.DBCON = "-db FORS -H 192.168.1.202 -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPFORS -H 89.160.51.202 -S 2555'
         */
   
   /*
 CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "FORS"
         valdbtemp.GFORETAG = "FORS"
         valdbtemp.DBNAMN = "FORS"
         valdbtemp.ORDNING = 3      
             
         valdbtemp.DBCON = "-db FORS -H 192.168.7.10 -S 2553 -N TCP"
         valdbtemp.APPCON = "-URL HTTP://89.160.54.6:8080/aia/aiaf?AppService=appft'                  
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Fortum Service-URL".   
     */

     