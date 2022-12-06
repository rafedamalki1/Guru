/*VALDBOXEL.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "OXEL"
         valdbtemp.GFORETAG = "OXEL"
         valdbtemp.DBNAMN = "OXEL"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db OXEL -H " + {egnadb.I} + " -S 2781 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}             
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Oxelö Energi AB".
         valdbtemp.WWWFTP = TRUE.   
                                                  
   END.
   