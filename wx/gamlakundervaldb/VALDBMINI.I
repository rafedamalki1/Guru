/*VALDBMINI.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "MINI"
         valdbtemp.GFORETAG = "MINI"
         valdbtemp.DBNAMN = "MINI"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db MINI -H " + {www2db.I} + " -S 2861 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Mini Entreprenad AB ".   
         valdbtemp.WWWFTP = TRUE.  
         
                                                   
   END.
   