/*VALDBELKB.I*/

 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "ELKB"
         valdbtemp.GFORETAG = "ELKB"
         valdbtemp.DBNAMN = "ELKB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db ELKB -H " + {egnadb.I} + " -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPELKB -H " + {egnaapp.I} + " -S 2555"                
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""    
         valdbtemp.VALDB = "Elkraftberedning i Blekinge".  
         valdbtemp.WWWFTP = TRUE.  
        
                                
   END.
   