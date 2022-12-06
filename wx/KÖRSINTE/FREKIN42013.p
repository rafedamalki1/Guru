/* FREKIN4.P INLÄSNING AV FREKVENSTABELL FRÅN .PDF P2 TILL P3 VITA KATALOGEN  */
/* SÄTT VARIABELN ARVAR TILL RÄTT KATALOG ÅR                        */

DEFINE NEW SHARED VARIABLE quotervar AS CHARACTER FORMAT "X(256)" NO-UNDO.



DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

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
DEFINE VARIABLE langd2 AS INTEGER NO-UNDO.
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE arvar AS INTEGER NO-UNDO.
DEFINE VARIABLE stopvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE decimalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE testinteger AS INTEGER NO-UNDO.
DEFINE VARIABLE radrak AS INTEGER NO-UNDO.
DEFINE VARIABLE radrak2 AS INTEGER NO-UNDO.

/*DEFINE TEMP-TABLE tidin
   FIELD A1                 AS CHARACTER FORMAT "X(6)" 
   FIELD B1                 AS CHARACTER FORMAT "X(10)" 
   FIELD C1                 AS CHARACTER    
   FIELD D1                 AS CHARACTER    
   FIELD E1                 AS CHARACTER    
   FIELD F1                 AS CHARACTER    
   FIELD G1                 AS CHARACTER    
   FIELD H1                 AS CHARACTER    
   FIELD I1                 AS CHARACTER    
   FIELD J1                 AS CHARACTER    
   FIELD K1                 AS CHARACTER    
   FIELD L1                 AS CHARACTER    
   FIELD M1                 AS CHARACTER    
   FIELD N1                 AS CHARACTER    
   FIELD O1                 AS CHARACTER    
   FIELD P1                 AS CHARACTER    
   FIELD Q1                 AS CHARACTER    
   FIELD R1                 AS CHARACTER    
   FIELD S1                 AS CHARACTER    
   FIELD T1                 AS CHARACTER    
   FIELD U1                 AS CHARACTER    
   FIELD V1                 AS CHARACTER    
   FIELD X1                 AS CHARACTER    
   FIELD Y1                 AS CHARACTER    
   FIELD Z1                 AS CHARACTER.


DEFINE TEMP-TABLE tidin2
   FIELD ARBKOD             AS CHARACTER 
   FIELD LOPNR              AS INTEGER
   FIELD ARBKOD2            AS CHARACTER
   FIELD LOPNR2             AS INTEGER
   FIELD BENAMNING          AS CHARACTER.*/
   
DEFINE TEMP-TABLE tidin
 FIELD UT             AS CHARACTER.

DEFINE TEMP-TABLE tidin2
   FIELD ARBKOD             AS CHARACTER
   FIELD LOPNR              AS INTEGER      
   FIELD ARBKOD2            AS CHARACTER
   FIELD LOPNR2              AS INTEGER   
   FIELD ANTAL              AS DECIMAL.

   

DEFINE TEMP-TABLE tidin3
   FIELD ARBKOD             AS CHARACTER 
   FIELD LOPNR              AS INTEGER
   FIELD ARBKOD2            AS CHARACTER
   FIELD LOPNR2             AS INTEGER
   FIELD ANTAL              AS DECIMAL.


DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
   

{muswait.i}        
   ASSIGN
   /*arvar = 2012
   filnamn = "X:\kalk\2011\skv\vp2p311.skv".*/
   arvar = 2014
   filnamn = "\\server05\d\elpool\elplo\kalk\2013\vp2p313.txt".

   /*filnamn = "\\server04\d\elpool\elpnj\kalk\2010\skv\vp2p310.skv".
   arvar = 2010
   filnamn = "\\server04\d\elpool\elpnj\kalk\2009\vp2p309.skv".*/
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.    
   {AMERICANEUROPEAN.I}   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT tidin   NO-ERROR.
      END.               
   END.
   
   RUN skapasats2_UI.
   /*FOR EACH tidin2:
      DISP tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2 TIDIN2.ANTAL.
   END.*/
   
   FOR EACH tidin2 NO-LOCK:
      DO TRANSACTION:
         CREATE FREKVENS.
         ASSIGN         
         FREKVENS.ARBKOD = " " + SUBSTRING(tidin2.ARBKOD,1,2)
         FREKVENS.LOPNR = tidin2.LOPNR
         FREKVENS.FREKOD = tidin2.ARBKOD2
         FREKVENS.FREKNR = tidin2.LOPNR2
         FREKVENS.ANTAL = tidin2.ANTAL
         FREKVENS.KATAR = arvar.
         FIND FIRST LOP3 WHERE LOP3.ARBKOD = tidin2.ARBKOD2 AND
         LOP3.LOPNR = tidin2.LOPNR2 AND LOP3.KATAR = arvar NO-LOCK NO-ERROR.
         IF AVAILABLE LOP3 THEN DO:
            ASSIGN
            FREKVENS.BENAMNING = LOP3.BENAMNING
            FREKVENS.ENHET = LOP3.ENHET.
         END.        
      END.
   END.   
   
   /*stopvar = stopvar.   
   OUTPUT TO c:\kalk.txt.
   FOR EACH tidin2:
      PUT tidin2.arbkod tidin2.lopnr tidin2.arbkod2 tidin2.lopnr2 tidin2.benamning SKIP.
      CREATE tidin3.
      ASSIGN
      tidin3.ARBKOD = tidin2.ARBKOD
      tidin3.LOPNR = tidin2.LOPNR
      tidin3.ARBKOD2 = tidin2.ARBKOD2
      tidin3.LOPNR2 = tidin2.LOPNR2.
      ASSIGN
      stopvar = FALSE
      langd = LENGTH(tidin2.BENAMNING).
      REPEAT WHILE stopvar = FALSE:
         IF SUBSTRING(tidin2.BENAMNING,langd - 1,1) = " " THEN DO:
            langd = langd - 1.
         END.
         ELSE DO:
            stopvar = TRUE.
         END.         
      END.      
      langd2 = langd.
      stopvar = FALSE.
      
      REPEAT WHILE stopvar = FALSE:
         IF SUBSTRING(tidin2.BENAMNING,langd - 1,1) = " " THEN DO:
            decimalvar = INDEX(SUBSTRING(tidin2.BENAMNING,langd),",").
            IF decimalvar = 0 THEN DO:
               tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN MESSAGE tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2
               VIEW-AS ALERT-BOX.
            END.
            ELSE DO:
               IF langd2 - langd  - decimalvar = 1 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 10 NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN MESSAGE tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2
                  VIEW-AS ALERT-BOX.
               END.
               ELSE IF langd2 - langd  - decimalvar = 2 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 100 NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN MESSAGE tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2
                  VIEW-AS ALERT-BOX.
               END.
               ELSE IF langd2 - langd  - decimalvar = 3 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 1000 NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN MESSAGE tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2
                  VIEW-AS ALERT-BOX.
               END.
               ELSE IF langd2 - langd  - decimalvar = 4 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 10000 NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN MESSAGE tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2
                  VIEW-AS ALERT-BOX.
               END.
            END.
            stopvar = TRUE.
         END.
         ELSE DO:
            langd = langd - 1.
         END.
      END.                 
   END. 
   OUTPUT CLOSE.
   
   FOR EACH tidin3 NO-LOCK:
      DO TRANSACTION:
         CREATE FREKVENS.
         ASSIGN
         FREKVENS.ARBKOD = tidin3.ARBKOD
         FREKVENS.LOPNR = tidin3.LOPNR
         FREKVENS.FREKOD = tidin3.ARBKOD2
         FREKVENS.FREKNR = tidin3.LOPNR2
         FREKVENS.ANTAL = tidin3.ANTAL
         FREKVENS.KATAR = arvar.
         FIND FIRST LOP3 WHERE LOP3.ARBKOD = tidin3.ARBKOD2 AND
         LOP3.LOPNR = tidin3.LOPNR2 AND LOP3.KATAR = arvar NO-LOCK NO-ERROR.
         IF AVAILABLE LOP3 THEN DO:
            ASSIGN
            FREKVENS.BENAMNING = LOP3.BENAMNING
            FREKVENS.ENHET = LOP3.ENHET.
         END.
      END.
   END.*/
        

/*    FOR EACH tidin3 WHERE tidin3.arbkod = "109" AND tidin3.lopnr = 12:                              */
/*       MESSAGE tidin3.arbkod ":" tidin3.lopnr ":" tidin3.arbkod2 ":" tidin3.lopnr2 ":" tidin3.antal */
/*       VIEW-AS ALERT-BOX.                                                                           */
/*    END.                                                                                            */
{musarrow.i}
{EUROPEANAMERICAN.I}

PROCEDURE skapasats2_UI:
   radrak = 1.
   radrak2 = 1.
   FOR EACH tidin:
      IF tidin.ut NE "" THEN DO:         
         IF radrak = 1 THEN DO:            
            CREATE tidin2.
            ASSIGN
            tidin2.ARBKOD =  SUBSTRING(tidin.ut,1,2).
            tidin2.LOPNR =  INTEGER(SUBSTRING(tidin.ut,3,2)).
            
         END.   
         IF radrak = 3 THEN DO:
            IF LENGTH(tidin.ut) = 5 THEN DO:
               tidin2.ARBKOD2 = SUBSTRING(tidin.ut,1,3).
               tidin2.LOPNR2 =  INTEGER(SUBSTRING(tidin.ut,4,2)).
            END.
            ELSE radrak = radrak - 1.                
         END.
         IF radrak = 5 THEN DO:
            
            tidin.ut = REPLACE(tidin.ut,",",".").
            tidin2.antal = DECIMAL(tidin.ut) .
            radrak = 0.            
         END.   
         radrak = radrak + 1.
      END.
      radrak2 = radrak2 + 1.
   END.      
   
END PROCEDURE.


/*PROCEDURE skapasats_UI: 
   FOR EACH tidin:
      CREATE tidin2.
      ASSIGN
      tidin2.ARBKOD = " " + SUBSTRING(tidin.A1,1,2)
      tidin2.LOPNR = INTEGER(SUBSTRING(tidin.A1,3,2))
      stopvar = FALSE.
      testinteger = INTEGER(SUBSTRING(tidin.C1,1,5)) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN musz = musz.
      ELSE DO:         
         FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.C1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.C1,4,2)) NO-LOCK NO-ERROR.
         IF AVAILABLE LOP3 THEN DO:
            ASSIGN            
            tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.C1,1,3)
            tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.C1,4,2))
            stopvar = TRUE.
         END.
         ELSE DO:
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.C1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.C1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.C1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.C1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.      
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.D1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.D1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.D1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.D1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.D1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.D1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.D1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.D1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.D1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.D1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.E1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.E1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.E1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.E1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.E1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.E1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.E1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.E1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.E1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.E1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.F1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.F1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.F1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.F1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.F1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.F1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.F1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.F1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.F1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.F1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.G1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.G1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.G1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.G1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.G1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.G1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.G1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.G1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.G1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.G1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.H1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.H1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.H1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.H1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.H1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.H1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.H1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.H1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.H1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.H1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.I1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.I1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.I1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.I1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.I1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.I1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.I1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.I1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.I1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.I1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.J1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.J1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.J1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.J1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.J1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.J1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.J1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.J1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.J1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.J1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.K1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.K1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.K1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.K1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.K1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.K1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.K1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.K1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.K1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.K1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.L1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.L1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.L1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.L1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.L1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.L1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.L1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.L1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.L1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.L1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.M1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.M1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.M1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.M1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.M1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.M1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.M1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.M1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.M1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.M1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.N1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.N1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.N1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.N1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.N1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.N1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.N1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.N1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.N1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.N1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.O1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.O1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.O1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.O1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.O1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.O1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.O1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.O1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.O1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.O1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.P1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.P1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.P1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.P1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.P1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.P1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.P1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.P1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.P1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.P1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.Q1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.Q1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.Q1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.Q1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.Q1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Q1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.Q1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.Q1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.Q1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Q1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.R1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.R1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.R1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.R1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.R1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.R1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.R1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.R1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.R1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.R1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.S1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.S1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.S1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.S1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.S1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.S1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.S1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.S1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.S1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.S1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.T1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.T1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.T1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.T1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.T1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.T1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.T1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.T1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.T1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.T1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.U1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.U1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.U1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.U1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.U1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.U1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.U1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.U1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.U1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.U1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.V1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.V1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE  LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.V1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.V1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.V1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.V1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.V1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.V1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.V1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.V1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.X1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.X1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.X1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.X1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.X1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.X1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.X1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.X1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.X1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.X1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.Y1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.Y1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.Y1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.Y1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.Y1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Y1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.Y1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.Y1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.Y1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Y1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.Z1.
      END.
      ELSE DO:
         testinteger = INTEGER(SUBSTRING(tidin.Z1,1,5)) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN musz = musz.
         ELSE DO:         
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.Z1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.Z1,4,2)) NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = "R" + SUBSTRING(tidin.Z1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Z1,4,2))
               stopvar = TRUE.
            END.
            ELSE DO:
               FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.Z1,1,3) AND LOP3.LOPNR = INTEGER(SUBSTRING(tidin.Z1,4,2)) NO-LOCK NO-ERROR.
               IF AVAILABLE LOP3 THEN DO:
                  ASSIGN            
                  tidin2.ARBKOD2 = SUBSTRING(tidin.Z1,1,3)
                  tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Z1,4,2))
                  stopvar = TRUE.
               END.
            END.
         END. 
      END.
   END.
END PROCEDURE.*/   

                
