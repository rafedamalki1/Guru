/*VALDBELPOOL.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
     
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "fELPA"
      valdbtemp.DBNAMN = "flextid"
      valdbtemp.ORDNING = 2      
      valdbtemp.DBCON = "-db flextid -S 2551 -H pc112 -N TCP"
      valdbtemp.DBPLATS = "c:\DELAD\PRO11S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService app11elpool -H PC112 -S 2755'      
      valdbtemp.VALDB = "Elpool Flextid".
      valdbtemp.WWWFTP = FALSE.      
      {VALDBELPSALJ.I}
      
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "UTBIELPA"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.ORDNING = 4      
      valdbtemp.DBCON = "-db UTBI -S 2509 -H pc112 -N TCP"
      valdbtemp.DBPLATS = "c:\DELAD\PRO10S\DBTILLBAKA\"
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService apputbi -H pc112 -S 2510'
      valdbtemp.VALDB = "Elpool utbildning".
      valdbtemp.WWWFTP = FALSE.
      */ 
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "OEELPAclas"
      valdbtemp.DBNAMN = "RT9"
      valdbtemp.ORDNING = 5      
      valdbtemp.DBCON = "-db RT9 -S 3327 -H pc112 -N TCP"
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService app11elpool -H PC112 -S 2755'
      valdbtemp.VALDB = "Classer test Guru utveckling OE11".
      valdbtemp.WWWFTP = FALSE.   
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "SEKG"
      valdbtemp.GFORETAG = "ELPSEKG"
      valdbtemp.DBNAMN = "SEKG"
      valdbtemp.ORDNING = 6      
      valdbtemp.DBCON = "-db SEKG -S 3540 -H pc112 -N TCP"
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService app11elpool -H PC112 -S 2755'
      valdbtemp.VALDB = "Elpools EKG utveckling OE11".
      valdbtemp.WWWFTP = FALSE.   
      
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "INGEN"
      valdbtemp.DBNAMN = ""
      valdbtemp.ORDNING = 99      
      valdbtemp.DBCON = ""
      valdbtemp.DBPLATS = ""
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "'
      valdbtemp.VALDB = "LADDA WTID".
      valdbtemp.WWWFTP = FALSE.   
      
      /* 
       CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SEKG"
         valdbtemp.GFORETAG = "ELPSEKG"
         valdbtemp.DBNAMN = "SEKG"
         valdbtemp.ORDNING = 6      
             
          valdbtemp.DBCON = "-db SEKG -S 3540 -H pc112 -N TCP"
      valdbtemp.DBPLATS = "C:\DELAD\PRO11S\db\"
      valdbtemp.DBCACHE = ""      
      valdbtemp.APPCON = "-AppService app11elpool -H PC112 -S 2755'
      valdbtemp.VALDB = "Svensk Energi - EKG utveckling OE11".
      valdbtemp.WWWFTP = FALSE. 
                 
       */
       
          /*
          FUNKAR
          valdbtemp.APPCON = "-URL http://192.168.95.2:8080/aia/aiaflex?AppService=fapp'
          
          valdbtemp.APPCON = "-URL http://192.168.95.2:8080/aia/Aia?AppService=fapp'                
      valdbtemp.APPCON = "-AppService fapp -H server04 -S 2559'
      valdbtemp.APPCON= ("-URL AppServer://192.168.95.2:2559/fapp").
      */
      
      
      /*
      valdbtemp.APPCON= "-URL http://192.168.95.2/aia/aiaflex?AppService=fapp".
      valdbtemp.APPCON= ("-URL AppServer://192.168.95.2:2559/fapp").
      valdbtemp.APPCON= '-URL http://192.168.95.2/Aia/aiaflex?AppService=fapp'      
      valdbtemp.APPCON= '-URL https://192.168.95.2/aia/appe1?AppService=fapp'
      valdbtemp.APPCON = "-AppService fapp -H server04 -S 2559'
      http://localhost/aia/appe1?GetServletStatus  in a web browser.)
      valdbtemp.APPCON= '-URL AppServer://pc012/aia/appe1?AppService=appelpool9'
         
         valdbtemp.APPCON= '-URL http://pc012/aia/appe1?AppService=appelpool9'
      
      
      */
       
/*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.FORETAG = "ELPA"
      valdbtemp.GFORETAG = "fELPA"
      valdbtemp.DBNAMN = "flextid"
      valdbtemp.ORDNING = 5      
      valdbtemp.DBCON = "-db flextid -S 2551 -H server04 -N TCP -U ELPAO -P KAGGEN"
      valdbtemp.DBPLATS = "D:\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""
      valdbtemp.APPCON = "-AppService fapp -H server04 -S 2559'
          /*
          valdbtemp.APPCON = "-URL http://192.168.95.2:8080/aia/Aia?AppService=fapp'                
      valdbtemp.APPCON = "-AppService fapp -H server04 -S 2559'
      valdbtemp.APPCON= ("-URL AppServer://192.168.95.2:2559/fapp").
      */
      
      valdbtemp.VALDB = "Elpool Flextid org".      
*/
   END.
