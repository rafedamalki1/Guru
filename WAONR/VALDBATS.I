/*VALDBATS.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ATS"
         valdbtemp.GFORETAG = "ATS"
         valdbtemp.DBNAMN = "ATS"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ATS -H " + {www2db.I} + " -S 2810 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "ATS Kraftservice".
         valdbtemp.WWWFTP = TRUE.
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ATS"
         valdbtemp.GFORETAG = "ATSMTR"
         valdbtemp.DBNAMN = "ATSMTR"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ATSMTR -H " + {www2db.I} + " -S 3857 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "ATS Kraftservice MTRL".
         valdbtemp.WWWFTP = TRUE.
        
                          
   END.
  