/*VALDBINFRAOMEXOM.I*/

 


   DO TRANSACTION:
       
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "FORS"
         valdbtemp.GFORETAG = "FORS"
         valdbtemp.DBNAMN = "FORS"
         valdbtemp.ORDNING = 1      
         valdbtemp.DBCON = "-db FORS -H " + {forsdb.I} + " -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPFORS -H " + {forsapp.I} + " -S 2555" 
                                           
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Infratek/Omexom".
         valdbtemp.WWWFTP = TRUE.
                       
   END.
   
     