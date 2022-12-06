DEFINE NEW SHARED VARIABLE fastrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
OPEN QUERY kq FOR EACH KALKNATT NO-LOCK.
DO TRANSACTION:
   GET FIRST kq EXCLUSIVE-LOCK.
   IF AVAILABLE KALKNATT THEN DELETE KALKNATT.
END.
REPEAT:
   DO TRANSACTION:
      GET NEXT kq EXCLUSIVE-LOCK.
      IF AVAILABLE KALKNATT THEN DELETE KALKNATT.
      ELSE LEAVE.
   END.
END.
CLOSE QUERY kq.   

OPEN QUERY specq FOR EACH FASTSPEC NO-LOCK.
GET FIRST specq NO-LOCK.
DO WHILE AVAILABLE(FASTSPEC):  
   fastrec = RECID(FASTSPEC).
   RUN SUMKALK.P (INPUT fastrec, INPUT FORETAG.FORETAG).
   GET NEXT specq NO-LOCK.
END.        
CLOSE QUERY specq.


OPEN QUERY specq2 FOR EACH KALKSPEC NO-LOCK.
GET FIRST specq2 NO-LOCK.
DO WHILE AVAILABLE(KALKSPEC):  
   fastrec = RECID(KALKSPEC).
   RUN FRINATTG.P.
   GET NEXT specq2 NO-LOCK.
END.        
CLOSE QUERY specq2.