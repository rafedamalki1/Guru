/* FREKIN4NY.p INLÄSNING AV FREKVENSTABELL P2 TILL P3 VITA KATALOGEN EXCEL             */
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
   FIELD P2                 AS CHARACTER  
   FIELD L2                 AS integer 
   FIELD B1                 AS CHARACTER FORMAT "X(10)" 
   FIELD P3                 AS CHARACTER 
   FIELD L3                 AS integer 
   FIELD B2                 AS CHARACTER FORMAT "X(10)" 
   FIELD ANTAL              AS DECIMAL FORMAT ">>>>>9.9999".

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .

{muswait.i}        
   ASSIGN   
   /*arvar = 2013.
   filnamn = "\\server05\d\elpool\elplo\kalk\2012\skv\vp2p312.skv".*/
   /*arvar = 2015.
   filnamn = "\\server05\d\elpool\elplo\kalk\2014\KLG2\skv\Vp2p3 2014.skv".
   arvar = 2016.
   filnamn = "\\SERVER05\d\elpool\elplo\kalk\2015\KLG2\SKV\Vp2p32015.skv".*/
   arvar = 2017.
   filnamn = "\\SERVER05\d\elpool\elplo\kalk\2016\Excel KLG2\SKV\Vp2p32016.skv".
   
   
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
      IF tidin.P2 NE "" THEN DO  TRANSACTION:                                    
         
         CREATE FREKVENS.
         ASSIGN
         FREKVENS.ARBKOD = " " +  SUBSTRING(tidin.P2,1,2)
         FREKVENS.LOPNR = tidin.L2
         FREKVENS.FREKOD = SUBSTRING(tidin.P3,1,3)
         FREKVENS.FREKNR = tidin.L3
         FREKVENS.ANTAL = tidin.ANTAL / 10000         
         FREKVENS.KATAR = arvar.                  
         
         FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = "R" + SUBSTRING(tidin.P3,1,3) AND LOP3.LOPNR = tidin.L3 NO-LOCK NO-ERROR.
         IF AVAILABLE LOP3 THEN DO:
            ASSIGN            
            FREKVENS.FREKOD = "R" + SUBSTRING(tidin.P3,1,3)
            FREKVENS.FREKNR = tidin.L3            
            FREKVENS.BENAMNING = LOP3.BENAMNING
            FREKVENS.ENHET = LOP3.ENHET.           
         END.
         ELSE DO:
            FIND FIRST LOP3 WHERE LOP3.KATAR = arvar AND LOP3.ARBKOD = SUBSTRING(tidin.P3,1,3) AND LOP3.LOPNR = tidin.L3 NO-LOCK NO-ERROR.
            IF AVAILABLE LOP3 THEN DO:
               ASSIGN
               FREKVENS.BENAMNING = LOP3.BENAMNING
               FREKVENS.ENHET = LOP3.ENHET.
            END.
            ELSE DO:
               FREKVENS.BENAMNING = tidin.B2.
            END.   
         END.
                  
      END.                           
   END.      
END PROCEDURE.   

                
