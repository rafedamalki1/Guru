/*VALDBTECMAR.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "TECM"
         valdbtemp.GFORETAG = "TECM"
         valdbtemp.DBNAMN = "TECM"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db TECM -H " + {www2db.I} + " -S 2877 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "TECMAR AB".  
         valdbtemp.WWWFTP = TRUE. 
         
                                                      
   END.
   