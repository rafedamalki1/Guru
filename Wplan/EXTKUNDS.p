/*EXTKUNDS.P HAMTAR ALLA EXTKUND*/
DEFINE TEMP-TABLE tempkund
   FIELD KUNDNR AS INTEGER
   FIELD KUNDNAMN AS CHARACTER
   FIELD ADRESS AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD TEL AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD KONTAKT AS CHARACTER
   INDEX KUNDNR KUNDNR ASCENDING.

DEFINE BUFFER extbuff FOR EXTKUND.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tempkund.
DEFINE INPUT PARAMETER prog AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kundnrvar AS INTEGER NO-UNDO.

IF prog = 1 THEN DO:
   OPEN QUERY oq FOR EACH EXTKUND USE-INDEX KUNDNR NO-LOCK. 
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(EXTKUND):
      CREATE tempkund.
      ASSIGN
      tempkund.KUNDNR = EXTKUND.KUNDNR
      tempkund.KUNDNAMN = EXTKUND.KUNDNAMN
      tempkund.ADRESS = EXTKUND.ADRESS
      tempkund.PNR = EXTKUND.PNR
      tempkund.ORT = EXTKUND.ORT
      tempkund.TEL = EXTKUND.TEL
      tempkund.KONTAKT = EXTKUND.KONTAKT.    
      GET NEXT oq NO-LOCK.
   END.
END.
ELSE IF prog = 2 THEN DO:
   IF kundnrvar = ? THEN DO:
      FIND LAST EXTKUND USE-INDEX KUNDNR NO-LOCK NO-ERROR.
      IF AVAILABLE EXTKUND THEN DO:
         DO TRANSACTION:
            FIND tempkund WHERE tempkund.KUNDNR = ? EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN
            kundnrvar = EXTKUND.KUNDNR + 1
            tempkund.KUNDNR = EXTKUND.KUNDNR + 1.
            CREATE extbuff.
            ASSIGN
            extbuff.KUNDNR = EXTKUND.KUNDNR + 1
            extbuff.KUNDNAMN = tempkund.KUNDNAMN
            extbuff.ADRESS = tempkund.ADRESS
            extbuff.TEL = tempkund.TEL
            extbuff.ORT = tempkund.ORT
            extbuff.PNR = tempkund.PNR
            extbuff.KONTAKT = tempkund.KONTAKT.
         END.         
      END.
      ELSE DO:
         DO TRANSACTION:
            FIND tempkund WHERE tempkund.KUNDNR = ? EXCLUSIVE-LOCK NO-ERROR.
            ASSIGN
            kundnrvar = 1
            tempkund.KUNDNR = 1.
            CREATE EXTKUND.
            ASSIGN
            EXTKUND.KUNDNR = 1
            EXTKUND.KUNDNAMN = tempkund.KUNDNAMN
            EXTKUND.ADRESS = tempkund.ADRESS
            EXTKUND.TEL = tempkund.TEL
            EXTKUND.ORT = tempkund.ORT
            EXTKUND.PNR = tempkund.PNR
            EXTKUND.KONTAKT = tempkund.KONTAKT.
         END.         
      END.
   END.
   ELSE DO:
      DO TRANSACTION:
         FIND tempkund WHERE tempkund.KUNDNR = kundnrvar NO-LOCK NO-ERROR.
         FIND FIRST EXTKUND WHERE EXTKUND.KUNDNR = kundnrvar EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE EXTKUND THEN DO:
            ASSIGN
            EXTKUND.KUNDNAMN = tempkund.KUNDNAMN
            EXTKUND.ADRESS = tempkund.ADRESS
            EXTKUND.TEL = tempkund.TEL
            EXTKUND.ORT = tempkund.ORT
            EXTKUND.PNR = tempkund.PNR
            EXTKUND.KONTAKT = tempkund.KONTAKT.
         END.
      END.
   END.
END.
ELSE DO:
   DO TRANSACTION:
      FIND tempkund WHERE tempkund.KUNDNR = kundnrvar EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST EXTKUND WHERE EXTKUND.KUNDNR = kundnrvar EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE EXTKUND THEN DO:
         DELETE EXTKUND.
      END.
      DELETE tempkund.
   END.
END.
