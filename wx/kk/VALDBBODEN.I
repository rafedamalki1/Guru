/*VALDBBODEN.I*/
/*VID ÄNDRINGA AV NÅGOT KOMPILER ALLA SOM INNEHÅLLER CONAPP.I +
 
   DO TRANSACTION:
         CREATE valdbtemp.
         ASSIGN
         valdbtemp.FORETAG = "BODE"
         valdbtemp.GFORETAG = "BODE"
         valdbtemp.DBNAMN = "BODE"
         valdbtemp.ORDNING = 1      
             
         valdbtemp.DBCON = "-db BODE -H " + Guru.Konstanter:InternNr(STRING(192) + CHR(46) +  Guru.Konstanter:InternNr(STRING(168)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(60)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(22)) + " -S 2553 -N TCP"
         valdbtemp.APPCON = "-AppService APPBODE -H " + Guru.Konstanter:InternNr(STRING(192) + CHR(46) +  Guru.Konstanter:InternNr(STRING(168)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(60)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(22)) + " -S 2555"                  
         valdbtemp.DBPLATS = "C:\elpool\DELAD\PRO9S\DB\"
         valdbtemp.DBCACHE = ""      
         valdbtemp.VALDB = "Bodens Energi".   
         valdbtemp.WWWFTP = FALSE.              
   END.
   
