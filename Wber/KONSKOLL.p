DEFINE TEMP-TABLE kons_mtrl
    FIELD KTYPKOD LIKE MTRLBER.KTYPKOD
    FIELD KONSKOD LIKE KONSTRUKTION.KONSKOD     
    FIELD ENR LIKE MTRLBER.ENR
    FIELD BENAMNING LIKE MTRLBER.BENAMNING
    FIELD ENHET LIKE MTRLBER.ENHET
    FIELD ANTAL LIKE MTRLBER.ANTAL
    FIELD LINKAB LIKE MTRLBER.LINKAB
    FIELD MODUL LIKE MTRLBER.MODUL
    FIELD TYPBER LIKE MTRLBER.TYPBER
    FIELD DIAMETER LIKE MTRLBER.DIAMETER
    INDEX KOD KONSKOD KTYPKOD ENR
    INDEX ENR ENR.

DEFINE INPUT PARAMETER lev1 LIKE LEVERANTOR.LEVKOD NO-UNDO.
DEFINE INPUT PARAMETER lev2 LIKE LEVERANTOR.LEVKOD NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR kons_mtrl.

   OPEN QUERY kq FOR EACH MTRLBER WHERE MTRLBER.LEVKOD = lev1
   NO-LOCK.
   DO TRANSACTION:
      GET FIRST kq EXCLUSIVE-LOCK.
      FIND FIRST MTRL WHERE MTRL.LEVKOD = lev2 AND MTRL.KALKNR = 0
      AND MTRL.ENR = MTRLBER.ENR /*SUBSTRING(MTRLBER.ENR,2,8)*/ USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:         
         
         ASSIGN
         MTRLBER.ENR = MTRL.ENR
         MTRLBER.BENAMNING = MTRL.BENAMNING
         MTRLBER.ENHET = MTRL.ENHET
         MTRLBER.PRIS = MTRL.NPRIS
         MTRLBER.LEVKOD = MTRL.LEVKOD.         
         
      END.
      ELSE DO:
         CREATE kons_mtrl.
         ASSIGN
         kons_mtrl.KTYPKOD = MTRLBER.KTYPKOD
         kons_mtrl.ENR = MTRLBER.ENR
         kons_mtrl.BENAMNING = MTRLBER.BENAMNING
         kons_mtrl.ENHET = MTRLBER.ENHET
         kons_mtrl.ANTAL = MTRLBER.ANTAL
         kons_mtrl.LINKAB = MTRLBER.LINKAB
         kons_mtrl.MODUL = MTRLBER.MODUL
         kons_mtrl.TYPBER = MTRLBER.TYPBER
         kons_mtrl.DIAMETER = MTRLBER.DIAMETER.
      END.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT kq EXCLUSIVE-LOCK.
         IF NOT AVAILABLE MTRLBER THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = lev2 AND MTRL.KALKNR = 0
            AND MTRL.ENR = MTRLBER.ENR /*SUBSTRING(MTRLBER.ENR,2,8)*/ USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:               
               
               ASSIGN
               MTRLBER.ENR = MTRL.ENR
               MTRLBER.BENAMNING = MTRL.BENAMNING
               MTRLBER.ENHET = MTRL.ENHET
               MTRLBER.PRIS = MTRL.NPRIS
               MTRLBER.LEVKOD = MTRL.LEVKOD.               
               
            END.
            ELSE DO:
               CREATE kons_mtrl.
               ASSIGN
               kons_mtrl.KTYPKOD = MTRLBER.KTYPKOD
               kons_mtrl.ENR = MTRLBER.ENR
               kons_mtrl.BENAMNING = MTRLBER.BENAMNING
               kons_mtrl.ENHET = MTRLBER.ENHET
               kons_mtrl.ANTAL = MTRLBER.ANTAL
               kons_mtrl.LINKAB = MTRLBER.LINKAB
               kons_mtrl.MODUL = MTRLBER.MODUL
               kons_mtrl.TYPBER = MTRLBER.TYPBER
               kons_mtrl.DIAMETER = MTRLBER.DIAMETER.
            END.
         END.
      END.
   END.         
   OPEN QUERY kq2 FOR EACH BERSKAP WHERE BERSKAP.LEVKOD = lev1
   NO-LOCK.
   DO TRANSACTION:
      GET FIRST kq2 EXCLUSIVE-LOCK.
      IF AVAILABLE BERSKAP THEN DO:      
         FIND FIRST MTRL WHERE MTRL.LEVKOD = lev2 AND MTRL.KALKNR = 0
         AND MTRL.ENR = BERSKAP.ENR /*SUBSTRING(BERSKAP.ENR,2,8)*/ USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:            
            
            ASSIGN
            BERSKAP.ENR = MTRL.ENR
            BERSKAP.BENAMNING = MTRL.BENAMNING
            BERSKAP.ENHET = MTRL.ENHET
            BERSKAP.PRIS = MTRL.NPRIS
            BERSKAP.LEVKOD = MTRL.LEVKOD.            
            
         END.
         ELSE DO:
            CREATE kons_mtrl.
            ASSIGN
            kons_mtrl.KTYPKOD = BERSKAP.KTYPKOD + " " + BERSKAP.KOD
            kons_mtrl.ENR = BERSKAP.ENR
            kons_mtrl.BENAMNING = BERSKAP.BENAMNING
            kons_mtrl.ENHET = BERSKAP.ENHET
            kons_mtrl.ANTAL = BERSKAP.ANTAL         
            kons_mtrl.MODUL = BERSKAP.MODUL.         
         END.
      END.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT kq2 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE BERSKAP THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = lev2 AND MTRL.KALKNR = 0
            AND MTRL.ENR = BERSKAP.ENR /*SUBSTRING(BERSKAP.ENR,2,8)*/ USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:               
               
               ASSIGN
               BERSKAP.ENR = MTRL.ENR
               BERSKAP.BENAMNING = MTRL.BENAMNING
               BERSKAP.ENHET = MTRL.ENHET
               BERSKAP.PRIS = MTRL.NPRIS
               BERSKAP.LEVKOD = MTRL.LEVKOD.               
               
            END.
            ELSE DO:
               CREATE kons_mtrl.
               ASSIGN
               kons_mtrl.KTYPKOD = BERSKAP.KTYPKOD + " " + BERSKAP.KOD
               kons_mtrl.ENR = BERSKAP.ENR
               kons_mtrl.BENAMNING = BERSKAP.BENAMNING
               kons_mtrl.ENHET = BERSKAP.ENHET
               kons_mtrl.ANTAL = BERSKAP.ANTAL               
               kons_mtrl.MODUL = BERSKAP.MODUL.               
            END.
         END.
      END.
   END.         
   OPEN QUERY kq3 FOR EACH BERSTOLP WHERE BERSTOLP.LEVKOD = lev1
   NO-LOCK.
   DO TRANSACTION:
      GET FIRST kq3 EXCLUSIVE-LOCK.
      IF AVAILABLE BERSTOLP THEN DO:      
         FIND FIRST MTRL WHERE MTRL.LEVKOD = lev2 AND MTRL.KALKNR = 0
         AND MTRL.ENR = BERSTOLP.ENR /*SUBSTRING(BERSTOLP.ENR,2,8)*/ USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:            
            ASSIGN
            BERSTOLP.ENR = MTRL.ENR
            BERSTOLP.BENAMNING = MTRL.BENAMNING
            BERSTOLP.ENHET = MTRL.ENHET
            BERSTOLP.PRIS = MTRL.NPRIS
            BERSTOLP.LEVKOD = MTRL.LEVKOD.            
            
         END.
         ELSE DO:
            CREATE kons_mtrl.
            ASSIGN
            kons_mtrl.KTYPKOD = STRING(BERSTOLP.STOLPE)
            kons_mtrl.ENR = BERSTOLP.ENR
            kons_mtrl.BENAMNING = BERSTOLP.BENAMNING
            kons_mtrl.ENHET = BERSTOLP.ENHET.                 
         END.
      END.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT kq3 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE BERSTOLP THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = lev2 AND MTRL.KALKNR = 0
            AND MTRL.ENR = BERSTOLP.ENR /*SUBSTRING(BERSTOLP.ENR,2,8)*/ USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:               
               ASSIGN
               BERSTOLP.ENR = MTRL.ENR
               BERSTOLP.BENAMNING = MTRL.BENAMNING
               BERSTOLP.ENHET = MTRL.ENHET
               BERSTOLP.PRIS = MTRL.NPRIS
               BERSTOLP.LEVKOD = MTRL.LEVKOD.               
               
            END.
            ELSE DO:
               CREATE kons_mtrl.
               ASSIGN
               kons_mtrl.KTYPKOD = STRING(BERSTOLP.STOLPE)
               kons_mtrl.ENR = BERSTOLP.ENR
               kons_mtrl.BENAMNING = BERSTOLP.BENAMNING
               kons_mtrl.ENHET = BERSTOLP.ENHET.               
            END.
         END.
      END.
   END.         
   /*
   FOR EACH kons_mtrl:
      FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = kons_mtrl.KTYPKOD
      NO-LOCK NO-ERROR.
      IF AVAILABLE KONSTRUKTION THEN kons_mtrl.KONSKOD = KONSTRUKTION.KONSKOD.
   END.      
   */
