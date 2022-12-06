/*VALDBPFBK.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "PFBK"
         valdbtemp.GFORETAG = "PFBK"
         valdbtemp.DBNAMN = "PFBK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db PFBK -H " + {www2db.I} + " -S 2887 -N TCP"
         valdbtemp.APPCON = "-URL https://" + {pas.I} +  {guruonweb.I} + {apsv.I}                    
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Beredningskonsult".  
         valdbtemp.WWWFTP = TRUE. 
     END.
   