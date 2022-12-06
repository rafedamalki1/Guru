/*VALDBVESABUTBIPRO.I*/
   DO TRANSACTION:
      
        
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(53)) + "/guruonweb11/"     /*VATTEN*/
      valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(53)) + "/guruonweb10/"     /*VATTEN*/
      valdbtemp.FORETAG = "VAST"
      valdbtemp.GFORETAG = "VSABUTBIPRO"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBCON = "-db UTBI -S 2601 -H " + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(53)) + " -N tcp"
      valdbtemp.DBPLATS = "e:\delad\pro9s\dbtillbaka\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 99      
      valdbtemp.APPCON = "-AppService appvsab -H " + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(53)) + " -S 2633"
      valdbtemp.VALDB = "Vattenfall Utbildning".
      valdbtemp.WWWFTP = FALSE.
      
   END.
