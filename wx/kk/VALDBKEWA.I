/*VALDBKEWA.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "KEWA"
         valdbtemp.GFORETAG = "KEWA"
         valdbtemp.DBNAMN = "KEWA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db KEWA -H " + {www2db.I} + " -S 2831 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Kenneth Wahlstr?m AB".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   