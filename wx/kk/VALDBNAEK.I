/*VALDBNAEK.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "NAEK"
         valdbtemp.GFORETAG = "NAEK"
         valdbtemp.DBNAMN = "NAEK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db NAEK -H " + {www2db.I} + " -S 3807 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "N?ssj? Elkraftberedning".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   