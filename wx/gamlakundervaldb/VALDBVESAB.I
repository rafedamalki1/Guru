/*VALDBVESAB.I*/
   DO TRANSACTION:
      
        
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb11/"     /*VATTEN*/
      valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb10/"     /*VATTEN*/
      valdbtemp.FORETAG = "VAST"
      valdbtemp.GFORETAG = "VSAB"
      valdbtemp.DBNAMN = "VSAB"
      valdbtemp.DBCON = "-db VSAB -S 2631 -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -N tcp"
      valdbtemp.DBPLATS = "e:\delad\pro9s\db\"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 11      
      valdbtemp.APPCON = "-AppService appvsab -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -S 2633"
      valdbtemp.VALDB = "Vattenfall Service AB".
      valdbtemp.WWWFTP = FALSE.
     
   
      
      /*
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb11/"     /*VATTEN*/
      valdbtemp.WWWSTART10 = "http://" + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + "/guruonweb10/"     /*VATTEN*/
      valdbtemp.FORETAG = "VSAB"
      valdbtemp.GFORETAG = "VTOR"
      valdbtemp.DBNAMN = "VSABTOR"
      valdbtemp.DBPLATS = "e:\delad\pro9s\dbKOPIA\"
      valdbtemp.DBCON = "-db VSABTOR -S 2601 -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -N tcp"
      valdbtemp.DBCACHE = ""
      valdbtemp.ORDNING = 101     
      valdbtemp.APPCON = "-AppService appvutbi -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -S 2615"
      /*
      valdbtemp.APPCON = "-AppService appvsab -H " + Guru.Konstanter:InternNr(STRING(151)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(156)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(177)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(197)) + " -S 2633"
      */
      valdbtemp.VALDB = "Vattenfall TOR". 
      valdbtemp.WWWFTP = FALSE.  
      */
            
      
      
   END.
