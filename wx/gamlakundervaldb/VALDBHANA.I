/*VALDBHANA.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "HANA"
         valdbtemp.GFORETAG = "HANA"
         valdbtemp.DBNAMN = "HANA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db HANA -H " + {www2db.I} + " -S 2811 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Hagstr?m i N?s AB".
         valdbtemp.WWWFTP = TRUE.                                                
   END.
   