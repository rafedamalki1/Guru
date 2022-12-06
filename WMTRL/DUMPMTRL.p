/*DUMPMTRL.P DUMPAR MTRL FÖR LEVERANTÖR I MTRL.D*/
DEFINE VARIABLE dumpplats AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
   {muswait.i}      
   IF globforetag = "VAST"   THEN DO:
      OUTPUT TO e:\delad\pro9\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      OUTPUT TO D:\DELAD\klient\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      OUTPUT TO d:\elpool\delad\pro9\wrk\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      OUTPUT TO E:\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      OUTPUT TO D:\ELPOOL\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" or globforetag = "ppko" OR globforetag = "KEWA" OR globforetag = "POLA" OR globforetag = "KNOR"  THEN DO:
      OUTPUT TO \\pc112\DELAD\mtrl.d convert target "iso8859-1" source "iso8859-1" APPEND.
   END.      
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      OUTPUT TO D:\DELAD\KLIENT\PRO10\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      OUTPUT TO C:\DELAD\PRO10\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "lapp" THEN DO:
      OUTPUT TO C:\ELPOOL\DELAD\PRO10\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "FORS"  THEN DO:      
      OUTPUT TO C:\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "ORBI" OR globforetag = "EKSK" OR globforetag = "SSEL" OR globforetag = "JSBF" OR globforetag = "LIMO" OR globforetag = "HJEL" OR globforetag = "SWECO"
   OR globforetag = "ETSA" OR globforetag = "ATS" OR globforetag = "LAKL" OR globforetag = "KRAF"  THEN DO:
      OUTPUT TO C:\delad\pro10\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SKEK" THEN DO:
      OUTPUT TO C:\delad\pro10\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.   
   ELSE IF globforetag = "KRIN" THEN DO:
      OUTPUT TO C:\delad\pro10\guru\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.   
   ELSE DO:
      OUTPUT TO mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.   
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.LEVKOD = leverant AND 
   MTRL.KALKNR = 0 NO-LOCK.   
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(MTRL):
      EXPORT MTRL.
      GET NEXT mtrlq NO-LOCK.
   END.                  
   CLOSE QUERY mtrlq.
   OUTPUT CLOSE.  
{EUROPEANAMERICAN.I}