/*VALDBNYLB.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "NYLB"
         valdbtemp.GFORETAG = "NYLB"
         valdbtemp.DBNAMN = "NYLB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db NYLB -H " + {www2db.I} + " -S 2827 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Nystr?ms Linjeberedning". 
         valdbtemp.WWWFTP = TRUE.                                               
   END.
   