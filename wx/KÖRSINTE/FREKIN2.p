/* FREKIN2.P INLÄSNING AV FREKVENSTABELL FRÅN .PDF P2 TILL P3 GRÖNA KATALOGEN */
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

DEFINE TEMP-TABLE tidin
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
   FIELD Z1                 AS CHARACTER
   FIELD AA1                 AS CHARACTER
   FIELD AB1                 AS CHARACTER   
   FIELD AC1                 AS CHARACTER
   FIELD AD1                 AS CHARACTER
   FIELD AE1                 AS CHARACTER
   FIELD AF1                 AS CHARACTER
   FIELD AG1                 AS CHARACTER
   FIELD AH1                 AS CHARACTER
   FIELD AI1                 AS CHARACTER
   FIELD AJ1                 AS CHARACTER
   FIELD AK1                 AS CHARACTER
   FIELD AL1                 AS CHARACTER
   FIELD AM1                 AS CHARACTER
   FIELD AN1                 AS CHARACTER
   FIELD AO1                 AS CHARACTER.



DEFINE TEMP-TABLE tidin2
   FIELD ARBKOD             AS CHARACTER 
   FIELD LOPNR              AS INTEGER
   FIELD ARBKOD2            AS CHARACTER
   FIELD LOPNR2             AS INTEGER
   FIELD BENAMNING          AS CHARACTER.

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
   /*arvar = 2010
   filnamn = "\\server04\d\elpool\elpnj\kalk\2009\p2p309.skv".
   arvar = 2011
   filnamn = "\\server04\d\elpool\elpnj\kalk\2010\skv\p2p310.skv".*/
   arvar = 2012
   filnamn = "X:\kalk\2011\skv\p2p311.skv".
 
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.  
   {AMERICANEUROPEAN.I}  
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.
   RUN skapasats_UI.
   
   stopvar = stopvar.   
   FOR EACH tidin2 WHERE tidin2.ARBKOD = "":
      DELETE tidin2.
   END.
   FOR EACH tidin2:
      FIND FIRST lop2 WHERE lop2.arbkod = tidin2.ARBKOD AND lop2.LOPNR = tidin2.LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LOp2 THEN DO:
         DISP "LOp2" tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2.
      END.
      FIND FIRST LOp3 WHERE LOp3.arbkod = tidin2.ARBKOD2 AND LOp3.LOPNR = tidin2.LOPNR2 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LOp3 THEN DO:
         DISP "LOp3" tidin2.ARBKOD tidin2.LOPNR tidin2.ARBKOD2 tidin2.LOPNR2.
      END.
   END.
   FOR EACH tidin2:
      CREATE tidin3.
      ASSIGN
      tidin3.ARBKOD = tidin2.ARBKOD
      tidin3.LOPNR = tidin2.LOPNR
      tidin3.ARBKOD2 = tidin2.ARBKOD2
      tidin3.LOPNR2 = tidin2.LOPNR2.
      /*IF  tidin2.ARBKOD = "138" AND tidin2.LOPNR = 17 THEN DO:
         
      END.*/
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
               tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)).
            END.
            ELSE DO:
               IF langd2 - langd  - decimalvar = 1 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 10.
               END.
               ELSE IF langd2 - langd  - decimalvar = 2 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 100.
               END.
               ELSE IF langd2 - langd  - decimalvar = 3 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 1000.
               END.
               ELSE IF langd2 - langd  - decimalvar = 4 THEN DO:
                  tidin3.ANTAL = DECIMAL(SUBSTRING(tidin2.BENAMNING,langd)) / 10000.
               END.
            END.
            stopvar = TRUE.
         END.
         ELSE DO:
            langd = langd - 1.
            
         END.
      END.          
   END.
   
   
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
   END.
   

/*    FOR EACH tidin3 WHERE tidin3.arbkod = "109" AND tidin3.lopnr = 12:                              */
/*       MESSAGE tidin3.arbkod ":" tidin3.lopnr ":" tidin3.arbkod2 ":" tidin3.lopnr2 ":" tidin3.antal */
/*       VIEW-AS ALERT-BOX.                                                                           */
/*    END.                                                                                            */
{musarrow.i}
{EUROPEANAMERICAN.I}
PROCEDURE skapasats_UI: 
   FOR EACH tidin:
      CREATE tidin2.
      ASSIGN
      tidin2.ARBKOD = SUBSTRING(tidin.A1,1,3)
      tidin2.LOPNR = INTEGER(SUBSTRING(tidin.A1,4,2))
      stopvar = FALSE.
      
      IF SUBSTRING(tidin.C1,1,1) = "8" OR SUBSTRING(tidin.C1,1,1) = "9" THEN DO:
         FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.C1,1,3) NO-LOCK NO-ERROR.
         IF AVAILABLE P3 THEN DO:
            ASSIGN            
            tidin2.ARBKOD2 = SUBSTRING(tidin.C1,1,3)
            tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.C1,4,2))
            stopvar = TRUE.
         END.
      END.
      
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.D1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.D1,1,1) = "8" OR SUBSTRING(tidin.D1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.D1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.D1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.D1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.E1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.E1,1,1) = "8" OR SUBSTRING(tidin.E1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.E1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.E1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.E1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.F1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.F1,1,1) = "8" OR SUBSTRING(tidin.F1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.F1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.F1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.F1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.G1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.G1,1,1) = "8" OR SUBSTRING(tidin.G1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.G1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.G1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.G1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.H1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.H1,1,1) = "8" OR SUBSTRING(tidin.H1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.H1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.H1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.H1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.I1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.I1,1,1) = "8" OR SUBSTRING(tidin.I1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.I1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.I1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.I1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.J1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.J1,1,1) = "8" OR SUBSTRING(tidin.J1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.J1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.J1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.J1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.K1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.K1,1,1) = "8" OR SUBSTRING(tidin.K1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.K1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.K1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.K1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.L1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.L1,1,1) = "8" OR SUBSTRING(tidin.L1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.L1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.L1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.L1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.M1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.M1,1,1) = "8" OR SUBSTRING(tidin.M1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.M1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.M1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.M1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.N1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.N1,1,1) = "8" OR SUBSTRING(tidin.N1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.N1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.N1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.N1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.O1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.O1,1,1) = "8" OR SUBSTRING(tidin.O1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.O1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.O1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.O1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.P1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.P1,1,1) = "8" OR SUBSTRING(tidin.P1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.P1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.P1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.P1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.Q1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.Q1,1,1) = "8" OR SUBSTRING(tidin.Q1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.Q1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.Q1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Q1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.R1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.R1,1,1) = "8" OR SUBSTRING(tidin.R1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.R1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.R1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.R1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.S1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.S1,1,1) = "8" OR SUBSTRING(tidin.S1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.S1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.S1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.S1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.T1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.T1,1,1) = "8" OR SUBSTRING(tidin.T1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.T1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.T1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.T1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.U1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.U1,1,1) = "8" OR SUBSTRING(tidin.U1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.U1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.U1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.U1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.V1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.V1,1,1) = "8" OR SUBSTRING(tidin.V1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.V1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.V1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.V1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.X1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.X1,1,1) = "8" OR SUBSTRING(tidin.X1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.X1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.X1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.X1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.Y1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.Y1,1,1) = "8" OR SUBSTRING(tidin.Y1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.Y1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.Y1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Y1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.Z1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.Z1,1,1) = "8" OR SUBSTRING(tidin.Z1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.Z1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.Z1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.Z1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AA1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AA1,1,1) = "8" OR SUBSTRING(tidin.AA1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AA1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AA1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AA1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AB1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AB1,1,1) = "8" OR SUBSTRING(tidin.AB1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AB1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AB1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AB1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AC1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AC1,1,1) = "8" OR SUBSTRING(tidin.AC1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AC1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AC1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AC1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AD1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AD1,1,1) = "8" OR SUBSTRING(tidin.AD1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AD1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AD1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AD1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AE1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AE1,1,1) = "8" OR SUBSTRING(tidin.AE1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AE1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AE1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AE1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AF1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AF1,1,1) = "8" OR SUBSTRING(tidin.AF1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AF1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AF1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AF1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AG1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AG1,1,1) = "8" OR SUBSTRING(tidin.AG1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AG1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AG1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AG1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AH1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AH1,1,1) = "8" OR SUBSTRING(tidin.AH1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AH1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AH1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AH1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AI1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AI1,1,1) = "8" OR SUBSTRING(tidin.AI1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AI1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AI1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AI1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AJ1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AJ1,1,1) = "8" OR SUBSTRING(tidin.AJ1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AJ1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AJ1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AJ1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AK1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AK1,1,1) = "8" OR SUBSTRING(tidin.AK1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AK1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AK1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AK1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AL1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AL1,1,1) = "8" OR SUBSTRING(tidin.AL1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AL1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AL1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AL1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AM1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AM1,1,1) = "8" OR SUBSTRING(tidin.AM1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AM1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AM1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AM1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AN1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AN1,1,1) = "8" OR SUBSTRING(tidin.AN1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AN1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AN1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AN1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.
      IF stopvar = TRUE THEN DO:
         tidin2.BENAMNING = tidin2.BENAMNING + " " + tidin.AO1.
      END.
      ELSE DO:
         IF SUBSTRING(tidin.AO1,1,1) = "8" OR SUBSTRING(tidin.AO1,1,1) = "9" THEN DO:
            FIND FIRST P3 WHERE P3.KATAR = arvar AND P3.ARBKOD = SUBSTRING(tidin.AO1,1,3) NO-LOCK NO-ERROR.
            IF AVAILABLE P3 THEN DO:
               ASSIGN            
               tidin2.ARBKOD2 = SUBSTRING(tidin.AO1,1,3)
               tidin2.LOPNR2 = INTEGER(SUBSTRING(tidin.AO1,4,2))
               stopvar = TRUE.
            END.
         END.
      END.

      
   END.
END PROCEDURE.   

                
