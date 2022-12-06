/*VALDBLABE.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "LABE"
         valdbtemp.GFORETAG = "LABE"
         valdbtemp.DBNAMN = "LABE"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db LABE -H " + {www2db.I} + " -S 2885 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Lars Beronius AB"   
         valdbtemp.WWWFTP = TRUE.  
         
                                                   
   END.
   