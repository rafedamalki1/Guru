/*IMPKON2.P*/
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamndat AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER FORMAT "X(20)" NO-UNDO.                
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE satsvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE melvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO. 
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD KTYPKOD            AS CHARACTER  
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD LEVKOD             AS CHARACTER
   FIELD ANTAL              AS INTEGER 
   FIELD F1                 AS CHARACTER 
   FIELD F2                 AS CHARACTER
   FIELD F3                 AS CHARACTER
   FIELD F4                 AS CHARACTER
   FIELD F5                 AS CHARACTER   
   FIELD LINKAB             AS CHARACTER
   FIELD DIAMETER           AS INTEGER
   FIELD MODULER            AS INTEGER
   FIELD TYPBER             AS CHARACTER
   FIELD DUMMY1             AS CHARACTER
   FIELD DUMMY2             AS CHARACTER
   FIELD DUMMY3             AS CHARACTER
   FIELD FEL                AS LOGICAL INITIAL FALSE
   FIELD LINKAB2            AS LOGICAL
   FIELD TYPBER2            AS LOGICAL.   
DEFINE TEMP-TABLE bbtemp NO-UNDO
   FIELD B1         AS CHARACTER 
   FIELD B2         AS CHARACTER 
   FIELD B3         AS CHARACTER 
   FIELD B4         AS CHARACTER 
   FIELD B5         AS CHARACTER 
   FIELD B6         AS CHARACTER 
   FIELD ID1        AS CHARACTER 
   FIELD ID2        AS CHARACTER 
   FIELD KONSKOD    AS INTEGER.

DEFINE TEMP-TABLE tidinfel NO-UNDO LIKE tidin.
PROCEDURE skapaenrkoll_UI:      
   DEFINE INPUT PARAMETER ktypval AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER TABLE FOR tidin.
   DEFINE OUTPUT PARAMETER okimp AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidinfel.
   typkod = ktypval.
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = ktypval 
   NO-LOCK NO-ERROR.
   FIND FIRST BBENAMNING WHERE BBENAMNING.KONSKOD = KONSTRUKTION.KONSKOD
   NO-LOCK NO-ERROR.
   FOR EACH tidin NO-LOCK:
      IF tidin.F1 NE "" THEN DO:         
         FIND FIRST KONSTVAL WHERE 
         KONSTVAL.KONSKOD = KONSTRUKTION.KONSKOD AND KONSTVAL.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         KONSTVAL.KOPP = TRUE AND KONSTVAL.BB = BBENAMNING.B2 AND
         TRIM(KONSTVAL.KVALKOD) = TRIM(tidin.F1) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KONSTVAL THEN DO:
            tidin.FEL = TRUE.
         END.
         ELSE DO:
            ASSIGN
            tidin.F2 = "" 
            tidin.F3 = "" 
            tidin.F4 = "" 
            tidin.F5 = "".           
         END.
      END.  
      ELSE IF tidin.F2 NE "" THEN DO:
         FIND FIRST KONSTVAL WHERE 
         KONSTVAL.KONSKOD = KONSTRUKTION.KONSKOD AND KONSTVAL.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         KONSTVAL.KOPP = TRUE AND KONSTVAL.BB = BBENAMNING.B3 AND
         TRIM(KONSTVAL.KVALKOD) = TRIM(tidin.F2) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KONSTVAL THEN DO:
            tidin.FEL = TRUE.
         END.
         ELSE DO:
            ASSIGN
            tidin.F1 = "" 
            tidin.F3 = "" 
            tidin.F4 = "" 
            tidin.F5 = "".            
         END.
      END.  
      ELSE IF tidin.F3 NE "" THEN DO:
         FIND FIRST KONSTVAL WHERE 
         KONSTVAL.KONSKOD = KONSTRUKTION.KONSKOD AND KONSTVAL.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         KONSTVAL.KOPP = TRUE AND KONSTVAL.BB = BBENAMNING.B4 AND
         TRIM(KONSTVAL.KVALKOD) = TRIM(tidin.F3) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KONSTVAL THEN DO:            
            tidin.FEL = TRUE.
         END.
         ELSE DO:
            ASSIGN
            tidin.F1 = "" 
            tidin.F2 = "" 
            tidin.F4 = "" 
            tidin.F5 = "".
         END.
      END.  
      ELSE IF tidin.F4 NE "" THEN DO:
         FIND FIRST KONSTVAL WHERE 
         KONSTVAL.KONSKOD = KONSTRUKTION.KONSKOD AND KONSTVAL.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         KONSTVAL.KOPP = TRUE AND KONSTVAL.BB = BBENAMNING.B5 AND
         TRIM(KONSTVAL.KVALKOD) = TRIM(tidin.F4) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KONSTVAL THEN DO:
            tidin.FEL = TRUE.
         END.
         ELSE DO:
            ASSIGN
            tidin.F1 = "" 
            tidin.F2 = "" 
            tidin.F3 = "" 
            tidin.F5 = "".            
         END.
      END.  
      ELSE IF tidin.F5 NE "" THEN DO:
         FIND FIRST KONSTVAL WHERE 
         KONSTVAL.KONSKOD = KONSTRUKTION.KONSKOD AND KONSTVAL.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         KONSTVAL.KOPP = TRUE AND KONSTVAL.BB = BBENAMNING.B6 AND
         TRIM(KONSTVAL.KVALKOD) = TRIM(tidin.F5) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KONSTVAL THEN DO:
            tidin.FEL = TRUE.
         END.
         ELSE DO:
            ASSIGN
            tidin.F1 = "" 
            tidin.F2 = "" 
            tidin.F3 = "" 
            tidin.F4 = "".            
         END.
      END.
      IF TRIM(tidin.LINKAB) = "Ja" THEN tidin.LINKAB2 = TRUE.
      ELSE tidin.LINKAB2 = FALSE.
      IF TRIM(tidin.TYPBER) = "Ja" THEN tidin.TYPBER2 = TRUE.
      ELSE tidin.TYPBER2 = FALSE.
   END.
   FOR EACH  tidin WHERE tidin.fel = TRUE:
      CREATE tidinfel.
      BUFFER-COPY tidin TO tidinfel.
   END.
   FIND FIRST tidin WHERE tidin.fel = FALSE NO-LOCK NO-ERROR.
   IF AVAILABLE tidin THEN okimp = TRUE.
   ELSE okimp = FALSE.
END PROCEDURE.

PROCEDURE impstart_UI :
   DEFINE OUTPUT PARAMETER messimport AS CHARACTER NO-UNDO.
   FOR EACH tidin WHERE tidin.FEL = FALSE: 
      IF tidin.F1 NE "" THEN DO:      
         ASSIGN
         var2 = tidin.F1
         var2 = TRIM(var2)
         langd = LENGTH(var2).
         IF langd = 8 THEN DO:
            tidin.F1 = var2.
         END.
         ELSE DO:
            DO WHILE langd < 8:
               ASSIGN
               var2 = " " + var2
               langd = langd + 1.
            END.
            tidin.F1 = var2.
         END.
      END.
      ELSE IF tidin.F2 NE "" THEN DO:      
         ASSIGN
         var2 = tidin.F2
         var2 = TRIM(var2)
         langd = LENGTH(var2).
         IF langd = 8 THEN DO:
            tidin.F2 = var2.
         END.
         ELSE DO:
            DO WHILE langd < 8:
               ASSIGN
               var2 = " " + var2
               langd = langd + 1.
            END.
            tidin.F2 = var2.
         END.
      END.
      ELSE IF tidin.F3 NE "" THEN DO:      
         ASSIGN
         var2 = tidin.F3
         var2 = TRIM(var2)
         langd = LENGTH(var2).
         IF langd = 8 THEN DO:
            tidin.F3 = var2.
         END.
         ELSE DO:
            DO WHILE langd < 8:
               ASSIGN
               var2 = " " + var2
               langd = langd + 1.
            END.
            tidin.F3 = var2.
         END.
      END.
      ELSE IF tidin.F4 NE "" THEN DO:      
         ASSIGN
         var2 = tidin.F4
         var2 = TRIM(var2)
         langd = LENGTH(var2).
         IF langd = 8 THEN DO:
            tidin.F4 = var2.
         END.
         ELSE DO:
            DO WHILE langd < 8:
               ASSIGN
               var2 = " " + var2
               langd = langd + 1.
            END.
            tidin.F4 = var2.
         END.
      END.
      ELSE IF tidin.F5 NE "" THEN DO:      
         ASSIGN
         var2 = tidin.F5
         var2 = TRIM(var2)
         langd = LENGTH(var2).
         IF langd = 8 THEN DO:
            tidin.F5 = var2.
         END.
         ELSE DO:
            DO WHILE langd < 8:
               ASSIGN
               var2 = " " + var2
               langd = langd + 1.
            END.
            tidin.F5 = var2.
         END.
      END.
      RUN mtrlber_UI. 
   END.
END PROCEDURE.   



PROCEDURE mtrlber_UI:
   
   OPEN QUERY dq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = typkod NO-LOCK.
   DO TRANSACTION:
      GET FIRST dq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT dq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
         ELSE LEAVE.
      END.
   END.
   FIND FIRST HUVUDLEV WHERE HUVUDLEV.DEP-NR = 999 NO-LOCK NO-ERROR.
   FOR EACH tidin WHERE tidin.FEL = FALSE:
      DO TRANSACTION:
         CREATE MTRLBER.
         ASSIGN
         MTRLBER.KTYPKOD = tidin.KTYPKOD
         MTRLBER.ENR = tidin.ENR
         MTRLBER.BENAMNING = tidin.BENAMNING
         MTRLBER.ENHET = tidin.ENHET
         MTRLBER.LEVKOD = tidin.LEVKOD
         MTRLBER.ANTAL = tidin.ANTAL
         MTRLBER.F1 = tidin.F1
         MTRLBER.F2 = tidin.F2
         MTRLBER.F3 = tidin.F3
         MTRLBER.F4 = tidin.F4
         MTRLBER.F5 = tidin.F5
         MTRLBER.PRIS = 0
         MTRLBER.LINKAB = tidin.LINKAB2
         MTRLBER.DIAMETER = tidin.DIAMETER
         MTRLBER.MODUL = tidin.MODUL
         MTRLBER.TYPBER = tidin.TYPBER2
         MTRLBER.SATS = FALSE
         MTRLBER.LEVKOD = tidin.LEVKOD.
         
         FIND FIRST MTRL WHERE MTRL.ENR =  tidin.ENR AND
         MTRL.LEVKOD = tidin.LEVKOD AND MTRL.KALKNR = 0 USE-INDEX LEV
         NO-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:
            ASSIGN
            MTRLBER.BENAMNING = MTRL.BENAMNING
            MTRLBER.ENHET = MTRL.ENHET
            MTRLBER.PRIS = MTRL.NPRIS.
         END.
      END.
   END.
END PROCEDURE.


PROCEDURE bbenmning_UI :
   DEFINE INPUT PARAMETER ktypval AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER TABLE FOR bbtemp.
   EMPTY TEMP-TABLE bbtemp NO-ERROR. 
   FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD = typkod
   NO-LOCK NO-ERROR.
   FIND FIRST BBENAMNING WHERE BBENAMNING.KONSKOD = KONSTRUKTION.KONSKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE BBENAMNING THEN DO:
      CREATE bbtemp.
      BUFFER-COPY BBENAMNING TO bbtemp.
   END.
END PROCEDURE.
