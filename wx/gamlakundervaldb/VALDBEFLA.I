/*VALDBEFLA.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "EFLA"
         valdbtemp.GFORETAG = "EFLA"
         valdbtemp.DBNAMN = "EFLA"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db EFLA -H " + {www2db.I} + " -S 2881 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "EFLA".  
         valdbtemp.WWWFTP = TRUE. 
     END.
   