/*DUMPAR MTRL FÖR LEVERANTÖR I MTRL.D*/
DEFINE VARIABLE dumpplats AS CHARACTER NO-UNDO.

     
   dumpplats = "/guru/wtid/mtrl.d".   
   OUTPUT TO VALUE(dumpplats) convert target "iso8859-1" source "iso8859-1" append.
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.LEVKOD = "3" AND
   MTRL.KALKNR = 0 NO-LOCK.
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(MTRL):
      EXPORT MTRL.
      GET NEXT mtrlq NO-LOCK.
   END.                  
   CLOSE QUERY mtrlq.
   OUTPUT CLOSE.  
