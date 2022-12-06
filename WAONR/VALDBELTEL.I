/*VALDBELTEL.I*/

 /*KÖR AppEgen*/

   DO TRANSACTION:


      
       CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "ELTE"
      valdbtemp.GFORETAG = "BELTE"
      valdbtemp.DBNAMN = "ELTEL"
      valdbtemp.DBCON = "-db ELTEL -H " + {egnadb.I} + " -S 2774 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.ORDNING = 1  
      valdbtemp.DBCACHE = "2022-11-01"  
      valdbtemp.APPCON = {AppEgna.i}
      valdbtemp.VALDB = "Eltel Networks ".
      valdbtemp.WWWFTP = TRUE.
      
    
      
   END.
 