/*VALDBSEKG.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  "http://www2.guruonweb.se/EKGonWeb116/"
         valdbtemp.WWWSTART10 = "http://www2.guruonweb.se/EKGonWeb10/"
         valdbtemp.FORETAG = "SEKG"
         valdbtemp.GFORETAG = "SEKG"
         valdbtemp.DBNAMN = "SEKG"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db SEKG -H " + {egnadb.I} + " -S 2801 -N TCP"
         
         valdbtemp.APPCON = {PasoeEgna.i}
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""     
         valdbtemp.VALDB = "Energiföretagen - EKG".
         valdbtemp.WWWFTP = TRUE. 
         
        
                                                       
   END.
   /*{PasoeEgna.i}*/