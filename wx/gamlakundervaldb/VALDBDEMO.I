/*VALDBDEMO.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
      FIND FIRST valdbtemp WHERE valdbtemp.VALDB = "Guru Demo" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valdbtemp THEN DO:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "ELPA"
         valdbtemp.GFORETAG = "dELPA"
         valdbtemp.DBNAMN = "RT9"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db RT9 -H 192.168.95.101 -S 2553 -N TCP -U ELPAO -P KAGGEN"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""  
         valdbtemp.APPCON = "-AppService appdemo -H www.guruonweb.se -S 2555'
         /*    
         valdbtemp.APPCON = "-AppService appdemo -H www.guruonweb.se -S 2555'
         */  
         valdbtemp.VALDB = "Guru Demo".  
         valdbtemp.WWWFTP = FALSE.          
            /*
            valdbtemp.APPCON = "-URL http://www.guruonweb.se:8080/aia/aiaf?AppService=appdemo'
            valdbtemp.APPCON = "-URL http://www.guruonweb.se:8080/aia/aiaf?AppService=appdemo'              
            
            */
      END.
      
   END.
