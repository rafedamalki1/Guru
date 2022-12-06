/*VALDBJSBF.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "JSBF"
         valdbtemp.GFORETAG = "JSBF"
         valdbtemp.DBNAMN = "JSBF"
         valdbtemp.ORDNING = 4      
             
         valdbtemp.DBCON = "-db JSBF -H " + {www2db.I} + " -S 2819 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Jukkasjärvi sockens belysningsförening".     
         valdbtemp.WWWFTP = TRUE.                                           
   END.
   