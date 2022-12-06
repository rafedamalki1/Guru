/*VALDBMISV.I*/          
   
   /*Guru.Konstanter:AppSpringSet[1] = "misvstb"*/
   
   DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(62)) + "/guruonweb11/"        /*MITT*/
      valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(62)) + "/guruonweb11/"        /*MITT*/
      valdbtemp.FORETAG = "misv"
      valdbtemp.GFORETAG = "misvstb"
      valdbtemp.DBNAMN = "misv"
      valdbtemp.DBCON = "-db misv -S 2510 -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(62)) + " -N TCP"
      valdbtemp.DBPLATS = "d:\elpool\DELAD\PRO10S\DB\"
      valdbtemp.DBCACHE = "" 
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService appnmisv -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(62)) + " -S 2512"
      valdbtemp.VALDB = "Mittsverige Vatten AB Stadsb". 
      valdbtemp.WWWFTP = FALSE.
   END.
   
