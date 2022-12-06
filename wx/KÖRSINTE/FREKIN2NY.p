/* FREKIN.p INLÄSNING AV FREKVENSTABELL P2 TILL P3 GRÖNA KATALOGEN             */
/*4 decimaler på antal                                                */ 
/* TÄNK PÅ ATT DET ÄR MINUS PÅ ANTAL FÖR RETURER I FILEN              */
/* GÖR ANTALSKOLUMNEN UTAN - TECKEN GENOM ATT VÄLJA MINUS MED RÖD FÄRG*/
/* SÄTT VARIABELN ARVAR TILL RÄTT KATALOG ÅR                          */

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
DEFINE VARIABLE pos1 AS INTEGER NO-UNDO.
DEFINE VARIABLE arvar AS INTEGER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD P2                 AS CHARACTER FORMAT "X(6)" 
   FIELD B1                 AS CHARACTER FORMAT "X(10)" 
   FIELD P3                 AS CHARACTER FORMAT "X(5)"
   FIELD B2                 AS CHARACTER FORMAT "X(10)" 
   FIELD ANTAL              AS DECIMAL FORMAT ">>>>>9.9999".

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

{muswait.i}        
   ASSIGN
   /*arvar = 2008.
   filnamn = "\\server04\d\elpool\elpnj\kalk\2007\p1p207.skv".
   arvar = 2009.
   filnamn = "\\server04\d\elpool\elpnj\kalk\2008\p1p208.skv".
   arvar = 2010.
   filnamn = "\\server04\d\elpool\elpnj\kalk\2009\p1p209.skv".
   arvar = 2011.
   filnamn = "\\server04\d\elpool\elpnj\kalk\2010\skv\p1p210.skv".
   arvar = 2012.
   filnamn = "X:\kalk\2011\skv\p1p211.skv".
   arvar = 2012.
   filnamn = "\\server05\d\elpool\elplo\kalk\2012\skv\p1p212.skv".*/
   arvar = 2013.
   filnamn = "\\server05\d\elpool\elplo\kalk\2012\skv\p2p312.skv".
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
   {EUROPEANAMERICAN.I}           
{musarrow.i}

PROCEDURE skapasats_UI:   
   FOR EACH tidin NO-LOCK:                                
      DO TRANSACTION:
         CREATE FREKVENS.
         ASSIGN
         FREKVENS.ARBKOD = SUBSTRING(tidin.P2,1,3)
         FREKVENS.LOPNR = INTEGER(SUBSTRING(tidin.P2,4,2))
         FREKVENS.FREKOD = SUBSTRING(tidin.P3,1,3)
         FREKVENS.FREKNR = INTEGER(SUBSTRING(tidin.P3,4,2))
         FREKVENS.ANTAL = tidin.ANTAL / 10000         
         FREKVENS.KATAR = arvar.                  
         FIND FIRST LOP3 WHERE LOP3.ARBKOD = SUBSTRING(tidin.P3,1,3) AND
         LOP3.LOPNR = INTEGER(SUBSTRING(tidin.P3,4,2)) AND LOP3.KATAR = arvar NO-LOCK NO-ERROR.
         IF AVAILABLE LOP3 THEN DO:
            ASSIGN
            FREKVENS.BENAMNING = LOP3.BENAMNING
            FREKVENS.ENHET = LOP3.ENHET.            
         END. 
         ELSE DO:
            ASSIGN
            FREKVENS.BENAMNING = tidin.B2.
         END.
      END.                           
   END.      
END PROCEDURE.   

                
