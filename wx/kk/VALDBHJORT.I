/*VALDBHJORT.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "HJEL"
         valdbtemp.GFORETAG = "HJEL"
         valdbtemp.DBNAMN = "HJEL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db HJEL -H " + {www2db.I} + " -S 2815 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Hjorts Entreprenad och Linjebyggnad". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   