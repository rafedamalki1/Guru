/*VALDBMOLN.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "MOLN"
         valdbtemp.GFORETAG = "MOLN"
         valdbtemp.DBNAMN = "MOLN"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db MOLN -H " + {www2db.I} + " -S 3817 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""        
         valdbtemp.VALDB = "Mölndal Energi".  
         valdbtemp.WWWFTP = TRUE.   
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE DO:
            valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}.
         END.                                           
   END.
   