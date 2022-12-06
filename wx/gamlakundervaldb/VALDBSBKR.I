/*VALDBSBKR.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SBKR"
         valdbtemp.GFORETAG = "SBKR"
         valdbtemp.DBNAMN = "SBKR"
         valdbtemp.ORDNING = 0      
             
         valdbtemp.DBCON = "-db SBKR -H " + {www2db.I} + " -S 2865 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "SB Kraft"   
         valdbtemp.WWWFTP = TRUE.  
         
                                                   
   END.
   