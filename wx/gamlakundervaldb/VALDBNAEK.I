/*VALDBNAEK.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "NAEK"
         valdbtemp.GFORETAG = "NAEK"
         valdbtemp.DBNAMN = "NAEK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db NAEK -H " + {www2db.I} + " -S 3807 -N TCP"
         valdbtemp.APPCON = "-AppService APPASP -H " + {www2app.I} + " -S 2802"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Niklas Plath Teknikkonsult AB".  
         valdbtemp.WWWFTP = TRUE.  
         IF PROVERSION BEGINS "11.2" THEN.
         ELSE IF SESSION:CLIENT-TYPE = "xWEBCLIENT" THEN DO:
            valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}.
         END.                                            
   END.
   