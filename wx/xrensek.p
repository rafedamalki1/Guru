OPEN QUERY PQ FOR EACH PERSEK WHERE PERSEK.PANDRA = FALSE NO-LOCK.
DO TRANSACTION:
   GET FIRST PQ EXCLUSIVE-LOCK.
   DELETE PERSEK.
END.
repeat:
   DO TRANSACTION:
      GET NEXT PQ EXCLUSIVE-LOCK.
      IF AVAILABLE PERSEK THEN DO:
         DELETE PERSEK.
      END.
      else leave.
   END.   
END.

OPEN QUERY TQ FOR EACH TIDSEK WHERE TIDSEK.PANDRA = FALSE NO-LOCK.
DO TRANSACTION:
   GET FIRST TQ EXCLUSIVE-LOCK.
   DELETE TIDSEK.
END.
repeat:
   DO TRANSACTION:
      GET NEXT TQ EXCLUSIVE-LOCK.
      IF AVAILABLE TIDSEK THEN DO:
         DELETE TIDSEK.
      END.
      else leave.
   END.   
END.