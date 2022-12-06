/*VALDBPITE.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "PITE"
         valdbtemp.GFORETAG = "PITE"
         valdbtemp.DBNAMN = "PITE"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db PITE -H " + {www2db.I} + " -S 2843 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "PITE ENERGI ".   
         valdbtemp.WWWFTP = TRUE.   
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE DO:
            valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}.
         END.                                          
   END.
   