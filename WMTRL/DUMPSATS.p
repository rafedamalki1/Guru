/*DUMPSATS.P DUMPAR SATSER FÖR LEVERANTÖR I SATS.D*/
DEFINE VARIABLE dumpplats AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.   
DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
   {muswait.i}
   IF globforetag = "VAST"  THEN DO:
      dumpplats = "e:\delad\pro9\guru\sats.d". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      dumpplats = "D:\DELAD\klient\PRO9\sats.d".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      dumpplats = "d:\elpool\delad\pro9\wrk\sats.d".
   END.
   ELSE DO:
      dumpplats = "\\pc112\DELAD\sats.d".
   END.
   OUTPUT TO VALUE(dumpplats) convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY mtrlq FOR EACH SATS WHERE 
   SATS.LEVKOD = leverant NO-LOCK.
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(SATS):
      EXPORT SATS.
      GET NEXT mtrlq NO-LOCK.
   END.                  
   CLOSE QUERY mtrlq.
   OUTPUT CLOSE.  
{EUROPEANAMERICAN.I}