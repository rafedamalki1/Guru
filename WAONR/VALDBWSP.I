/*VALDBWSP.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "WSP"
         valdbtemp.GFORETAG = "WSP"
         valdbtemp.DBNAMN = "WSP"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db WSP -H " + {www2db.I} + " -S 2841 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "WSP Sverige AB ".   
         valdbtemp.WWWFTP = TRUE.  
         valdbtemp.APPCON =  {PasOewww2.i}.
          
                                                   
   END.
   