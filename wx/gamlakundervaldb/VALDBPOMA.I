/*VALDBPOMA.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "POMA"
         valdbtemp.GFORETAG = "POMA"
         valdbtemp.DBNAMN = "POMA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db POMA -H " + {www2db.I} + " -S 3811 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "PO M?rtensson".  
         valdbtemp.WWWFTP = TRUE.                                              
   END.
   