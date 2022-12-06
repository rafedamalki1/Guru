/*VALDBSKOGSK.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SKOK"
         valdbtemp.GFORETAG = "SKOGSK"
         valdbtemp.DBNAMN = "SKOGSK"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SKOGSK -H " + {egnadb.I} + " -S 2653 -N TCP"
         valdbtemp.APPCON = "-AppService APPSKOGSK -H " + {egnaapp.I} + " -S 2655"                   
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Skogskonsult och Kraft".     
         valdbtemp.WWWFTP = TRUE.     
         
                                           
   END.
   
    