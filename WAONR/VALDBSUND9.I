/*VALDBSUND9.I*/          
   
    DO TRANSACTION:
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(63)) + "/guruonweb11/" .         /*SUND*/
     valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(63)) + "/guruonweb11/" .         /*SUND*/
              /*SUND*/
      ASSIGN 
      valdbtemp.FORETAG = "SUND"
      valdbtemp.GFORETAG = "SUNDN9STB"
      valdbtemp.DBNAMN = "SUNDn9"
      valdbtemp.DBCON = "-db sundn9 -S 2516 -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(63)) + " -N TCP"
      valdbtemp.DBPLATS = "D:\DELAD\SERVER\PRO10S\DB\"
      valdbtemp.DBCACHE = "" 
      valdbtemp.ORDNING = 1     
      valdbtemp.APPCON = "-AppService appnsund -H " + Guru.Konstanter:InternNr(STRING(172)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(16)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(254)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(63)) + " -S 2518"
      valdbtemp.VALDB = "Sundsvall Energi AB STADS".
      valdbtemp.WWWFTP = FALSE.
   END. 
    
