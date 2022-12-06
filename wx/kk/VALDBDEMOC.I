/*VALDBDEMOC.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ELPA"
         valdbtemp.GFORETAG = "classELPA"
         valdbtemp.DBNAMN = "RT9kalk"
         valdbtemp.ORDNING = 1      
         
         valdbtemp.DBCON = "-db RT9kalk -H 192.168.7.23 -S 3553 -N tcp"
         valdbtemp.DBCACHE = ""  
         
         valdbtemp.APPCON = "-AppService APPPICA -H 31.216.227.30 -S 2455'                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.VALDB = "Guru Demo Classer". 
         valdbtemp.WWWFTP = FALSE.
         /* 
         valdbtemp.DBCON = "-db RT9kalk -H 192.168.95.101 -S 3553 -N TCP -U ELPAO -P KAGGEN"
         valdbtemp.APPCON = "-AppService appdemo -H www.guruonweb.se -S 2555'   
         valdbtemp.APPCON = "-AppService appdemo -H www.guruonweb.se -S 2555'
         */  
                    
            /*
            valdbtemp.APPCON = "-URL http://www.guruonweb.se:8080/aia/aiaf?AppService=appdemo'
            valdbtemp.APPCON = "-URL http://www.guruonweb.se:8080/aia/aiaf?AppService=appdemo'              
            
            */
      
      
   END.
