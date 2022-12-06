/*PROGRAMMET DUMPAR UT BEREDNINGSUPPLÄGGET*/
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
   {AMERICANEUROPEAN.I}
   IF  
   globforetag = "VAST"  THEN DO:
      OUTPUT TO e:\delad\pro9\guru\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY kq FOR EACH KONSTGRUPP
   NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(KONSTGRUPP):
      EXPORT KONSTGRUPP.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY ordq FOR EACH BBENAMNING NO-LOCK.
   GET FIRST ordq NO-LOCK.
   DO WHILE AVAILABLE(BBENAMNING):
      EXPORT BBENAMNING.
      GET NEXT ordq NO-LOCK.
   END.
   CLOSE QUERY ordq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY ediq FOR EACH EDIGRUPP NO-LOCK.
   GET FIRST ediq NO-LOCK.
   DO WHILE AVAILABLE(EDIGRUPP):
      EXPORT EDIGRUPP.
      GET NEXT ediq NO-LOCK.
   END.
   CLOSE QUERY ediq.
   OUTPUT CLOSE.
  
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN"  THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY friq FOR EACH KONSTRUKTION NO-LOCK.
   GET FIRST friq NO-LOCK.
   DO WHILE AVAILABLE(KONSTRUKTION):
      EXPORT KONSTRUKTION.
      GET NEXT friq NO-LOCK.
   END.
   CLOSE QUERY friq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY berqid FOR EACH KONSTVAL NO-LOCK.
   GET FIRST berqid NO-LOCK.
   DO WHILE AVAILABLE(KONSTVAL):
      EXPORT KONSTVAL.
      GET NEXT berqid NO-LOCK.
   END.
   CLOSE QUERY berqid.
   OUTPUT CLOSE.
  
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY mq2 FOR EACH MTRLBER NO-LOCK.
   GET FIRST mq2 NO-LOCK.
   DO WHILE AVAILABLE(MTRLBER):
      EXPORT MTRLBER.
      GET NEXT mq2 NO-LOCK.
   END.
   CLOSE QUERY mq2.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO berstolp.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY bersq FOR EACH BERSTOLP NO-LOCK.
   GET FIRST bersq NO-LOCK.
   DO WHILE AVAILABLE(BERSTOLP):
      EXPORT BERSTOLP.
      GET NEXT bersq NO-LOCK.
   END.
   CLOSE QUERY bersq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY skapq FOR EACH BERSKAP NO-LOCK.
   GET FIRST skapq NO-LOCK.
   DO WHILE AVAILABLE(BERSKAP):
      EXPORT BERSKAP.
      GET NEXT skapq NO-LOCK.
   END.
   CLOSE QUERY skapq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      OUTPUT TO kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   OPEN QUERY kalkq FOR EACH KALKBER NO-LOCK.  
   GET FIRST kalkq NO-LOCK.
   DO WHILE AVAILABLE(KALKBER):
      EXPORT KALKBER.       
      GET NEXT kalkq NO-LOCK.
   END.
   CLOSE QUERY kalkq.
   OUTPUT CLOSE.
   
   IF  
   globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\koll.txt APPEND.
      PUT "Dumpallt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
      PUT "Dumpallt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
      PUT "Dumpallt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      OUTPUT TO E:\DELAD\PRO9\koll.txt APPEND.
      PUT "Dumpallt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\koll.txt APPEND.
      PUT "Dumpallt klart. " substring(globforetag,1,10) SKIP.
      OUTPUT CLOSE.
   END.
{EUROPEANAMERICAN.I}