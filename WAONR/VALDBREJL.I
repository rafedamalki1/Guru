/*VALDBREJL.I*/

 
   DO TRANSACTION:
     CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "REJI"
         valdbtemp.GFORETAG = "REJI"
         valdbtemp.DBNAMN = "REJL"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db REJL -H guru2 -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPREJL -H guru2 -S 2555"                  
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Rejlers Sverige".
         valdbtemp.WWWFTP = FALSE.
     
                 
   END.
   
