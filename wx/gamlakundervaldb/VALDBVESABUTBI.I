/*VALDBVESABUTBI.I*/
   DO TRANSACTION:
      
     CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb11/"     /*VATTEN*/
      valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb10/"     /*VATTEN*/
      valdbtemp.FORETAG = "VSAB"
      valdbtemp.GFORETAG = "VUTBI"
      valdbtemp.DBNAMN = "UTBI"
      valdbtemp.DBPLATS = "e:\delad\pro9s\dbtillbaka\"
      valdbtemp.DBCON = "-db UTBI -S 2601 -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -N tcp"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 99      
      valdbtemp.APPCON = "-AppService appvsab -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -S 2633"
      valdbtemp.VALDB = "Vattenfall Utbildning". 
      valdbtemp.WWWFTP = FALSE.     
            
      
      
   END.
