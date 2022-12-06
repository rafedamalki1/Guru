/*VALDBOTER.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "OTER"
         valdbtemp.GFORETAG = "OTER"
         valdbtemp.DBNAMN = "OTER"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db OTER -H " + {www2db.I} + " -S 2875 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Otera Ratel AB".  
         valdbtemp.WWWFTP = TRUE. 
         
                                                      
   END.
   