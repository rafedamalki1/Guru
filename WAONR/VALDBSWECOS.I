/*VALDBSWECOS.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "SWEO"
         valdbtemp.GFORETAG = "SWEOS"
         valdbtemp.DBNAMN = "SWEO"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "C:\ELPOOL\PRO11S\DB\SWEO -1"
         valdbtemp.APPCON = "'                   
         valdbtemp.DBPLATS = "C:\ELPOOL\PRO11S\DB"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Sweco Singel". 
         valdbtemp.WWWFTP = TRUE.   
   END.
   