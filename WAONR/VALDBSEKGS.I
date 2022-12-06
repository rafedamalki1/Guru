/*VALDBSEKGS.I*/
/*SKA BARA FINNAS I WC-CHECK.P*/
 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  "http://www2.guruonweb.se/EKGonWeb116/"
         valdbtemp.WWWSTART10 = "http://www2.guruonweb.se/EKGonWeb10/"
         valdbtemp.FORETAG = "SEKG"
         valdbtemp.GFORETAG = "SEKG"
         valdbtemp.DBNAMN = "SEKG"
         valdbtemp.ORDNING = 2      
             
         valdbtemp.DBCON = "-db SEKG -H " + {egnadb.I} + " -S 2801 -N TCP"
         valdbtemp.APPCON = {AppSingel.i}  
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "Energiföretagen - EKG AppSingel".
         valdbtemp.WWWFTP = TRUE. 
         
        
                                                        
   END.
   