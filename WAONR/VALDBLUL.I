/*VALDBLUL.I*/

    
   DO TRANSACTION:
     
      CREATE valdbtemp.
      ASSIGN
      valdbtemp.WWWSTART =  {WWWGURU11.I}
      valdbtemp.WWWSTART10 = {WWWGURU10.I}
      valdbtemp.FORETAG = "LULE"
      valdbtemp.GFORETAG = "LULE"
      valdbtemp.DBNAMN = "LULE"
      
      valdbtemp.DBCON = "-db LULE -S 2761 -H " + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(20)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(153)) + " -N tcp"
      valdbtemp.DBPLATS = "D:\ELPOOL\DELAD\PRO9S\DB\"
      valdbtemp.DBCACHE = ""  
      valdbtemp.ORDNING = 1      
      valdbtemp.APPCON = "-AppService APPLULE -H " + Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(20)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(10)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(153)) + " -S 2763"
      valdbtemp.VALDB = "Luleå Energikoncern".
      valdbtemp.WWWFTP = FALSE.
      
   END.
   /*
  valdbtemp.DBCON = "-db LULE -S 2761 -H " + Guru.Konstanter:InternNr(STRING(195)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(196)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(19)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(21)) + " -N tcp"
  valdbtemp.APPCON = "-AppService APPLULE -H " + Guru.Konstanter:InternNr(STRING(195)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(196)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(19)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(21)) + " -S 2763"    
*/