/*VALDBRAMB.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "RAMB"
         valdbtemp.GFORETAG = "RAMB"
         valdbtemp.DBNAMN = "RAMB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db RAMB -H " + {www2db.I} + " -S 3813 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "Ramb?ll Sverige AB".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   