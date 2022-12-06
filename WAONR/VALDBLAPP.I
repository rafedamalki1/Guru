
  /*VALDBLAPP.I*/
/*KÖR AppEgen*/
 
  DO TRANSACTION:
     
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "LAPP"
      valdbtemp.GFORETAG = "LAPPB"
      valdbtemp.DBNAMN  = "LAPP"      
      valdbtemp.DBCON = "-db LAPP -H " + {egnadb.I} + " -S 2501 -N tcp"
      valdbtemp.DBPLATS = "C:\DELAD\PRO10S\DB\"
      valdbtemp.ORDNING = 1   
      valdbtemp.DBCACHE = ""     
      valdbtemp.APPCON = {AppEgna.i}
      valdbtemp.VALDB = "Lapplands Elnät AB Berget".
      valdbtemp.WWWFTP = TRUE.
      
      
   END.

