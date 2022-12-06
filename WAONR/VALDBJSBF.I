/*VALDBJSBF.I*/

 
   DO TRANSACTION:
      
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  {WWWGURU11.I}
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "JSBF"
         valdbtemp.GFORETAG = "JSBF"
         valdbtemp.DBNAMN = "JSBF"
         valdbtemp.ORDNING = 4      
             
         valdbtemp.DBCON = "-db JSBF -H " + {www2db.I} + " -S 2819 -N TCP"
         valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Jukkasjärvi sockens belysningsförening".     
         valdbtemp.WWWFTP = TRUE.  
         valdbtemp.APPCON = {PasOewww2.i}.
                                                   
   END.
   