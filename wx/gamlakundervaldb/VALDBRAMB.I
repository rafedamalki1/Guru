/*VALDBRAMB.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
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
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE IF SESSION:CLIENT-TYPE = "xWEBCLIENT" THEN DO:
            valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}.
         END.                                             
   END.
   