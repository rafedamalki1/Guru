/*VALDBAFCON.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "AFCO"
         valdbtemp.GFORETAG = "AFCO"
         valdbtemp.DBNAMN = "AFCO"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db AFCO -H " + {www2db.I} + " -S 2823 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "?F-Industry AB"
         valdbtemp.WWWFTP = TRUE.                                                 
   END.
   