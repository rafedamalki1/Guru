/*VALDBVESABDP.I*/
   DO TRANSACTION:
      
        
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(53)) + "/guruonweb11/"     /*VATTEN*/
      valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(53)) + "/guruonweb10/"     /*VATTEN*/
      valdbtemp.FORETAG = "VAST"
      valdbtemp.GFORETAG = "VSABDP"
      valdbtemp.DBNAMN = "VSAB"                                                
      
      valdbtemp.DBCON = "-db VSAB -S 2631 -H localhost -U " + CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) + " -P " + CHR(75) + CHR(65) + CHR(71) + CHR(71) + CHR(69) + CHR(78) + " -N tcp"
      valdbtemp.DBPLATS = "e:\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 11      
      valdbtemp.APPCON = "-AppService appvsab -H " + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(222)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(144)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(62)) + " -S 2633"
      valdbtemp.VALDB = "Vattenfall Service AB DP".
      valdbtemp.WWWFTP = FALSE.
      
      
   END.
