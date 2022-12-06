/*VALDBSUNDNAT.I*/          
   
   
    CREATE valdbtemp.
         ASSIGN
         valdbtemp.WWWSTART =  "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(253)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(198)) + "/guruonwebElnat116/"
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SNAT"
         valdbtemp.GFORETAG = "SUNDNAT"
         valdbtemp.DBNAMN = "SUNDNAT"
         valdbtemp.DBCON = "-db SUNDNAT -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(253)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(198)) + " -S 2526 -N TCP"
         valdbtemp.DBPLATS = "D:\DELAD\PRO10S\DB\" 
         valdbtemp.DBCACHE = ""   
         valdbtemp.ORDNING = 2      
         valdbtemp.APPCON = "-AppService AppEgen -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(253)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(198)) + " -S 2671" 
         valdbtemp.VALDB = "Sundsvall Elnät AB Berget ".  
         valdbtemp.WWWFTP = FALSE.
            /*obs ej {AppEgna.i}*/               
         CREATE valdbtemp.
         ASSIGN
         
         valdbtemp.WWWSTART =  "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(253)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(198)) + "/guruonwebElnat116/"
         valdbtemp.WWWSTART10 = {WWWGURU10.I}
         valdbtemp.FORETAG = "SNAT"
         valdbtemp.GFORETAG = "USUNDNAT"
         valdbtemp.DBNAMN = "UTBI"
         valdbtemp.DBCON = "-db UTBI -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(253)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(198)) + " -S 2529 -N TCP"
         valdbtemp.DBPLATS = "D:\DELAD\PRO10S\DB\" 
         valdbtemp.DBCACHE = ""   
         valdbtemp.ORDNING = 2      
         valdbtemp.APPCON = "-AppService AppEgen -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(253)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(198)) + " -S 2671"
         valdbtemp.VALDB = "Sundsvall Elnät AB Utbildning ".  
  
 /*snatberget*/
 
 
         