/* ordkonstval.p pROGRAMMET FIXAR TILL ORDNINGEN PÅ TABELLEN KONSTVAL*/
DEFINE TEMP-TABLE ktemp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD KVALKOD AS CHARACTER
   FIELD BB AS CHARACTER
   FIELD KOPP AS LOGICAL
   FIELD ORDNING AS INTEGER.
DEFINE VARIABLE bbvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ordvar AS INTEGER NO-UNDO.
FOR EACH konstgrupp /*WHERE konstgrupp.konskod NE 0 */ NO-LOCK:
   EMPTY TEMP-TABLE ktemp NO-ERROR. 
   FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = konstgrupp.konskod NO-LOCK.
      FIND FIRST ktemp WHERE ktemp.KVALKOD = KONSTVAL.KVALKOD AND ktemp.BB = KONSTVAL.BB
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ktemp THEN DO:   
         CREATE ktemp.
         ASSIGN
         ktemp.konskod = KONSTVAL.KONSKOD
         ktemp.KVALKOD = KONSTVAL.KVALKOD
         ktemp.BB = KONSTVAL.BB
         ktemp.KOPP = KONSTVAL.KOPP
         ktemp.ORDNING = KONSTVAL.ORDNING.
      END.
   END.
   bbvar = "".
   FOR EACH ktemp BY ktemp.BB BY ktemp.ORDNING:
      IF bbvar NE ktemp.BB THEN DO:
         ASSIGN
         bbvar = ktemp.BB
         ordvar = 1.
      END.
      ELSE ordvar = ordvar + 1.
      OPEN QUERY konstq FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = ktemp.konskod
      AND KONSTVAL.BB = bbvar AND KONSTVAL.KVALKOD = ktemp.KVALKOD NO-LOCK.
      GET FIRST konstq NO-LOCK.
      DO WHILE AVAILABLE(KONSTVAL):
         DO TRANSACTION:         
            GET CURRENT konstq EXCLUSIVE-LOCK.
            konstval.ordning = ordvar.   
            /*MESSAGE KONSTVAL.KONSKOD KONSTVAL.BB KONSTVAL.KVALKOD.*/
         END.
         GET NEXT konstq NO-LOCK.
      END.
      CLOSE QUERY konstq.
   END.   

END.
