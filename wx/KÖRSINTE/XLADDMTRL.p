/*XLADDMTRL.P TAR BORT MTRL FÖR LEVERANTÖR OCH LADDAR MTRL FRÅN .D FIL*/

DEFINE VAR globforetag LIKE FORETAG.FORETAG NO-UNDO.  
{AMERICANEUROPEAN.I} 
FIND FIRST FORETAG NO-LOCK.
globforetag = FORETAG.FORETAG.
DEFINE VAR leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
leverant = "1".
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
   MTRL.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         DELETE MTRL.
         GET NEXT mtrlq EXCLUSIVE-LOCK.
      END.  
   END.      
   DO WHILE AVAILABLE(MTRL):
      DO TRANSACTION:
         DELETE MTRL.
         GET NEXT mtrlq EXCLUSIVE-LOCK.
      END.
   END.                  
   CLOSE QUERY mtrlq.
   IF  
   globforetag = "VAST"  THEN DO:
      INPUT FROM /guru/wtid/mtrl.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE DO:
      IF globforetag = "GRAN"  THEN DO:
         INPUT FROM d:\elpool\delad\pro9\wrk\mtrl.d convert target "iso8859-1" source "iso8859-1". 
      END.
      IF globforetag = "GKAL" THEN DO:
         INPUT FROM D:\DELAD\KLIENT\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1". 
      END.
      ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
         INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1". 
      END.
      ELSE DO:
         INPUT FROM mtrl.d convert target "iso8859-1" source "iso8859-1".
      END.      
   END.   
   REPEAT: 
      CREATE MTRL.  
      ASSIGN.     
      IMPORT MTRL. 
      {MTRLCREATE.I}  
   END.
   INPUT CLOSE.
{EUROPEANAMERICAN.I}