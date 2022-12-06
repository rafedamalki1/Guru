/*JORDTAG.P*/
DEFINE VARIABLE valvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tidinah
   FIELD KONSKOD            AS INTEGER
   FIELD ENR                AS CHARACTER       
   FIELD BENAMNING          AS CHARACTER       
   FIELD ANTAL              AS INTEGER
   FIELD ENHET              AS CHARACTER       
   FIELD PRIS               AS DECIMAL.
DEFINE TEMP-TABLE tgrupp
   FIELD KONSKOD            AS INTEGER.


DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.

DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   

DEFINE BUFFER mbuff FOR MTRLBER.
{AMERICANEUROPEAN.I}  
{muswait.i} 

ASSIGN
/*filnamn = "\\server04\d\elpool\elpnj\esgraninge\JORDTAG2.skv".*/
filnamn = "d:\elpool\delad\pro9\wrk\jordtag2.skv".
EMPTY TEMP-TABLE intid NO-ERROR.
EMPTY TEMP-TABLE tidinah NO-ERROR.

INPUT FROM VALUE(filnamn) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE tidinah.
      ASSIGN.
      IMPORT DELIMITER ";" tidinah   NO-ERROR.
   END.               
END.   
FOR EACH tidinah:
   FIND FIRST tgrupp WHERE tgrupp.KONSKOD = tidinah.KONSKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tgrupp THEN DO:
      CREATE tgrupp.
      ASSIGN
      tgrupp.KONSKOD = tidinah.KONSKOD.
   END.
END.
FOR EACH tgrupp:
   OPEN QUERY kq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = tgrupp.KONSKOD NO-LOCK.
   GET FIRST KQ NO-LOCK.
   DO  WHILE AVAILABLE(KONSTRUKTION):
      IF KONSTRUKTION.KONSKOD = 19 OR KONSTRUKTION.KONSKOD = 20 OR KONSTRUKTION.KONSKOD = 21 OR 
      KONSTRUKTION.KONSKOD = 22 OR KONSTRUKTION.KONSKOD = 24 OR KONSTRUKTION.KONSKOD = 26 THEN DO:               
         valvar = " Jordtag".
      END.
      ELSE IF KONSTRUKTION.KONSKOD = 25 OR KONSTRUKTION.KONSKOD = 27 THEN DO:
         valvar = "Jordning".
      END.
      ELSE IF KONSTRUKTION.KONSKOD = 31 OR KONSTRUKTION.KONSKOD = 32 THEN DO:
         valvar = "      JT".
      END.
      ELSE IF KONSTRUKTION.KONSKOD = 28 THEN DO:
         valvar = "     JT1".
      END.
      IF KONSTRUKTION.KONSKOD = 28 THEN DO:
         OPEN QUERY DQ FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         MTRLBER.F3 = valvar NO-LOCK.
         DO TRANSACTION:
            GET FIRST dq EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
         END.
         REPEAT:
            DO TRANSACTION:
               GET NEXT DQ EXCLUSIVE-LOCK.
               IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
               ELSE LEAVE.
            END.
         END.
         CLOSE QUERY DQ.         
      END.
      ELSE DO:      
         OPEN QUERY DQ FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD AND 
         MTRLBER.F4 = valvar NO-LOCK.
         DO TRANSACTION:
            GET FIRST dq EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
         END.
         REPEAT:
            DO TRANSACTION:
               GET NEXT DQ EXCLUSIVE-LOCK.
               IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
               ELSE LEAVE.
            END.
         END.
         CLOSE QUERY DQ.         
      END.
      FOR EACH tidinah WHERE tidinah.KONSKOD = tgrupp.KONSKOD:
         DO TRANSACTION:
            CREATE mbuff.
            ASSIGN
            mbuff.KTYPKOD = KONSTRUKTION.KTYPKOD
            mbuff.ENR = tidinah.ENR
            mbuff.BENAMNING = tidinah.BENAMNING
            mbuff.ENHET = tidinah.ENHET
            mbuff.PRIS = tidinah.PRIS
            mbuff.LEVKOD = "2"
            mbuff.ANTAL = tidinah.ANTAL
            mbuff.SATS = NO
            mbuff.TYPBER = NO
            mbuff.DIAMETER = 0
            mbuff.MODUL = 0.
            IF KONSTRUKTION.KONSKOD = 28 THEN DO:
               mbuff.F3 = valvar.
            END.
            ELSE DO:
               mbuff.F4 = valvar.
            END.
         END.
      END.
      GET NEXT KQ NO-LOCK.
   END.
   CLOSE QUERY KQ.
END.
{EUROPEANAMERICAN.I}
