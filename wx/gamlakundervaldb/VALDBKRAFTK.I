/*VALDBKRAFTK.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "GETB"
         valdbtemp.GFORETAG = "GETB"
         valdbtemp.DBNAMN = "GETB"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db GETB -H kraftdc01 -S 2801 -N TCP"
         valdbtemp.APPCON = "-AppService APPKRAFT -H kraftdc01 -S 2802"                   
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Kraftkonsult i Skandinavien AB".  
         valdbtemp.WWWFTP = FALSE.
         
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "GRAN"
         valdbtemp.GFORETAG = "GETBO"
         valdbtemp.DBNAMN = "GETBO"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db GETBO -H kraftdc01 -S 2903 -N TCP"
         valdbtemp.APPCON = "-AppService APPKRAFT -H kraftdc01 -S 2802"                  
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Kraftkonsult- One Nordic".  
         valdbtemp.WWWFTP = false.   
         
                      
   END.
   