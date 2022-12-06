/*LADDSATS.p TAR BORT SATS FÖR LEVERANTÖR OCH LADDAR SATS FRÅN .D FIL*/

DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.   
DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
   {muswait.i}
   OPEN QUERY mtrlq FOR EACH SATS WHERE  
   SATS.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE SATS THEN DO:
         DELETE SATS.
         GET NEXT mtrlq EXCLUSIVE-LOCK.
      END.  
   END.      
   DO WHILE AVAILABLE(SATS):
      DO TRANSACTION:
         DELETE SATS.
         GET NEXT mtrlq EXCLUSIVE-LOCK.
      END.
   END.                  
   CLOSE QUERY mtrlq.
   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\sats.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN"  THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\sats.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\sats.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM sats.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE SATS.
      ASSIGN.
      IMPORT SATS.
   END.
   INPUT CLOSE.
{EUROPEANAMERICAN.I}