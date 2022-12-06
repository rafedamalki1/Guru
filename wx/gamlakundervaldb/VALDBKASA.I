/*VALDBKASA.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "KASA"
         valdbtemp.GFORETAG = "KASA"
         valdbtemp.DBNAMN = "KASA"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db KASA -H " + {www2db.I} + " -S 2851 -N TCP"
        
         valdbtemp.APPCON = "-AppService APPWWW2 -H " + {pas.I} + {guruonweb.i} + " " + {ssl.I}    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "KASAB".   
         valdbtemp.WWWFTP = TRUE.   
                                                   
   END.
   