/*VALDBORBI.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ORBI"
         valdbtemp.GFORETAG = "ORBI"
         valdbtemp.DBNAMN = "ORBI"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ORBI -H " + {egnadb.I} + " -S 2753 -N TCP"
         valdbtemp.APPCON = "-AppService APPORBI -H " + {egnaapp.I} + " -S 2755"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Orbion Consulting AB".    
         valdbtemp.WWWFTP = TRUE.
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE IF SESSION:CLIENT-TYPE = "xWEBCLIENT" THEN DO:
            valdbtemp.APPCON = "-AppService AppEgen -H " + {egna.I} + {guruonweb.i} + " " + {ssl.I}. 
         END.
                                                  
   END.
   