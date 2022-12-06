/* PRISUPP.P
UPPDATERAR PRISER FÖR TABELLER SOM ANVÄNDS I ADMINISTRATION BEREDNING*/

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
   
   /*MTRL KOPPLAT TILL KONSTRUKTIONER*/
   
   OPEN QUERY qmtrl FOR EACH MTRLBER WHERE MTRLBER.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl EXCLUSIVE-LOCK.
      FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
      MTRL.KALKNR = 0 AND MTRL.ENR = MTRLBER.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:                           
         ASSIGN         
         MTRLBER.PRIS = MTRL.NPRIS
         MTRLBER.BENAMNING = MTRL.BENAMNING
         MTRLBER.ENHET = MTRL.ENHET.
      END.                                      
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl EXCLUSIVE-LOCK.
         IF NOT AVAILABLE MTRLBER THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = MTRLBER.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:                               
               ASSIGN               
               MTRLBER.PRIS = MTRL.NPRIS
               MTRLBER.BENAMNING = MTRL.BENAMNING
               MTRLBER.ENHET = MTRL.ENHET.
            END.
         END.
      END.
   END.
   CLOSE QUERY qmtrl.
   
   /*STOLPAR OCH STATIONER*/
   
   OPEN QUERY qmtrl2 FOR EACH BERSTOLP WHERE BERSTOLP.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl2 EXCLUSIVE-LOCK.
      IF AVAILABLE BERSTOLP THEN DO:
         FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
         MTRL.KALKNR = 0 AND MTRL.ENR = BERSTOLP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:                           
            ASSIGN         
            BERSTOLP.PRIS = MTRL.NPRIS
            BERSTOLP.BENAMNING = MTRL.BENAMNING
            BERSTOLP.ENHET = MTRL.ENHET.
         END.                                      
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl2 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE BERSTOLP THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = BERSTOLP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:                               
               ASSIGN               
               BERSTOLP.PRIS = MTRL.NPRIS
               BERSTOLP.BENAMNING = MTRL.BENAMNING
               BERSTOLP.ENHET = MTRL.ENHET.
            END.
         END.
      END.
   END.
   CLOSE QUERY qmtrl2.
      
   /*KABELSKÅP*/
   
   OPEN QUERY qmtrl3 FOR EACH BERSKAP WHERE BERSKAP.LEVKOD = leverant 
   AND BERSKAP.ENR NE "" NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl3 EXCLUSIVE-LOCK.
      IF AVAILABLE BERSKAP THEN DO:
         FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
         MTRL.KALKNR = 0 AND MTRL.ENR = BERSKAP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:                           
            ASSIGN         
            BERSKAP.PRIS = MTRL.NPRIS
            BERSKAP.BENAMNING = MTRL.BENAMNING
            BERSKAP.ENHET = MTRL.ENHET.
         END.                                      
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl3 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE BERSKAP THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = BERSKAP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:                               
               ASSIGN               
               BERSKAP.PRIS = MTRL.NPRIS
               BERSKAP.BENAMNING = MTRL.BENAMNING
               BERSKAP.ENHET = MTRL.ENHET.
            END.
         END.
      END.
   END.
   CLOSE QUERY qmtrl3.
   
   /*KABELSKYDD OCH -RÖR*/
   
   OPEN QUERY qmtrl4 FOR EACH KSKYDD WHERE KSKYDD.LEVKOD = leverant 
   AND KSKYDD.BERED = FALSE NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl4 EXCLUSIVE-LOCK.
      IF AVAILABLE KSKYDD THEN DO:
         FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
         MTRL.KALKNR = 0 AND MTRL.ENR = KSKYDD.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:                           
            ASSIGN         
            KSKYDD.PRIS = MTRL.NPRIS
            KSKYDD.BENAMNING = MTRL.BENAMNING
            KSKYDD.ENHET = MTRL.ENHET.
         END.                                      
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl4 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE KSKYDD THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = KSKYDD.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:                               
               ASSIGN               
               KSKYDD.PRIS = MTRL.NPRIS
               KSKYDD.BENAMNING = MTRL.BENAMNING
               KSKYDD.ENHET = MTRL.ENHET.
            END.
         END.
      END.
   END.
   CLOSE QUERY qmtrl4.
   
   /*MATERIELSATSER*/
   
   OPEN QUERY qmtrl5 FOR EACH SATS WHERE SATS.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl5 EXCLUSIVE-LOCK.
      IF AVAILABLE SATS THEN DO:
         IF SATS.SATS = TRUE THEN DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND
            MTRL.KALKNR = 0 AND MTRL.ENR = SATS.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:
               ASSIGN
               SATS.PRIS = MTRL.NPRIS
               SATS.BENAMNING = MTRL.BENAMNING
               SATS.ENHET = MTRL.ENHET.
            END.
         END.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND
            MTRL.KALKNR = 0 AND MTRL.ENR = SATS.ENR2 USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:
               ASSIGN
               SATS.PRIS2 = MTRL.NPRIS
               SATS.BENAMNING2 = MTRL.BENAMNING
               SATS.ENHET2 = MTRL.ENHET.
            END.
         END.
      END.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl5 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE SATS THEN LEAVE.
         ELSE DO:
            IF SATS.SATS = TRUE THEN DO:
               FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND
               MTRL.KALKNR = 0 AND MTRL.ENR = SATS.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:
                  ASSIGN
                  SATS.PRIS = MTRL.NPRIS
                  SATS.BENAMNING = MTRL.BENAMNING
                  SATS.ENHET = MTRL.ENHET.
               END.
            END.
            ELSE DO:
               FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND
               MTRL.KALKNR = 0 AND MTRL.ENR = SATS.ENR2 USE-INDEX LEV NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:
                  ASSIGN
                  SATS.PRIS2 = MTRL.NPRIS
                  SATS.BENAMNING2 = MTRL.BENAMNING
                  SATS.ENHET2 = MTRL.ENHET.
               END.
            END.
         END.
      END.
   END.
   CLOSE QUERY qmtrl5.
     
     
