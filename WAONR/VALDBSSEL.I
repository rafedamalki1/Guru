/*VALDBSSEL.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SSEL"
         valdbtemp.GFORETAG = "SSEL"
         valdbtemp.DBNAMN = "SSEL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SSEL -H " + {www2db.I} + " -S 2821 -N TCP"
         valdbtemp.APPCON = {Appasp.i}                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""       
                                                   
         valdbtemp.VALDB = "Smålanska Skog & Elkraft AB". 
         valdbtemp.WWWFTP = TRUE.
             
   END.
   