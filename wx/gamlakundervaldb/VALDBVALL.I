/*VALDBVALL.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "VALL"
         valdbtemp.GFORETAG = "VALL"
         valdbtemp.DBNAMN = "VALL"
         valdbtemp.ORDNING = 0      
         valdbtemp.DBCON = "-db VALL -H " + {www2db.I} + " -S 2849 -N TCP"
         valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Elverket Vallentuna".   
         valdbtemp.WWWFTP = TRUE.   
                                                   
   END.
   