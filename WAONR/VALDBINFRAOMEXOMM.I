/*VALDBINFRAOMEXOMM.I*/

 
/*
Servrarna är i dagsläget medlem i en arbetsgrupp: workgroup


*/
   DO TRANSACTION:
       CREATE valdbtemp.
         
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "FORS"
         valdbtemp.GFORETAG = "FORSM"
         valdbtemp.DBNAMN = "FORSM"
         valdbtemp.ORDNING = 2      
         valdbtemp.DBCON = "-db FORSM -H " + {forsdb.I} + " -S 2563 -N TCP"
         valdbtemp.APPCON = "-AppService APPFORS -H " + {forsapp.I} + " -S 2555" 
                                           
         valdbtemp.DBPLATS = "C:\DELAD\PRO9S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Infratek/Omexom MATERIEL ADM".
         valdbtemp.WWWFTP = TRUE.
         
         
                       
   END.
   
        