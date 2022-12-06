/*VALDBBYNS.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "BYNS"
         valdbtemp.GFORETAG = "BYNS"
         valdbtemp.DBNAMN = "BYNS"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db BYNS -H " + {www2db.I} + " -S 2855 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"           
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "BML Kraftservice Byns ".   
         valdbtemp.WWWFTP = TRUE.  
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
            valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}.
         END.                                   
                                                   
   END.
   