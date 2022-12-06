/*VALDBALAK.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ALAK"
         valdbtemp.GFORETAG = "ALAK"
         valdbtemp.DBNAMN = "ALAK"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db ALAK -H " + {www2db.I} + " -S 2879 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "ALAKUIJALA OY".  
         valdbtemp.WWWFTP = TRUE. 
     END.
   