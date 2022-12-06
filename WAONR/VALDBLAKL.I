/*VALDBLAKL.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "LAKL"
         valdbtemp.GFORETAG = "LAKL"
         valdbtemp.DBNAMN = "LAKL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db LAKL -H " + {www2db.I} + " -S 2814 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
         valdbtemp.VALDB = "Landers Kraft & ljusteknik AB". 
         valdbtemp.WWWFTP = TRUE.
                                                       
   END.
   