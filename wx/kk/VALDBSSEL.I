/*VALDBSSEL.I*/
/*VID ?NDRINGA AV N?GOT KOMPILER ALLA SOM INNEH?LLER CONAPP.I +
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SSEL"
         valdbtemp.GFORETAG = "SSEL"
         valdbtemp.DBNAMN = "SSEL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SSEL -H " + {www2db.I} + " -S 2821 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         /*
         valdbtemp.DBNAMN = "EKSK"
         valdbtemp.VALDB = "Ekstr?ms Skogsservice AB".
         */                                           
         valdbtemp.VALDB = "Sm?lanska Skog & Elkraft AB". 
         valdbtemp.WWWFTP = TRUE.    
   END.
   