/*VALDBETS.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ETSA"
         valdbtemp.GFORETAG = "ETSA"
         valdbtemp.DBNAMN = "ETSA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ETSA -H " + {www2db.I} + " -S 2812 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Energy Technical Service Sweden AB".
         valdbtemp.WWWFTP = TRUE.  
         valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}.
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "JAMT"
         valdbtemp.GFORETAG = "JAMT"
         valdbtemp.DBNAMN = "JAMT"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db JAMT -H " + {www2db.I} + " -S 3815 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Jämtkraft".
         valdbtemp.WWWFTP = TRUE.
         valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}.
                                                      
   END.
   