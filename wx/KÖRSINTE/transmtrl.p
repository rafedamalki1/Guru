/*TRANSMTRL.P INLÄSNING AV EBRMATERIEL*/       
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

DEFINE BUFFER mtrlbuff FOR MTRL.

DEFINE TEMP-TABLE tidin   
   FIELD ENR                AS CHARACTER
   FIELD NUMMER             AS INTEGER
   FIELD BENAMNING          AS CHARACTER         
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO. 
{AMERICANEUROPEAN.I}  
{muswait.i}      
   filnamn = "F:\elpool\elpnj\guru-transelectric\grundsatser3.skv".
   FOR EACH intid:
      DELETE intid.
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.       
   wtidvar = filnamn.
   INPUT FROM VALUE(wtidvar) CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE tidin.
         ASSIGN.
         IMPORT DELIMITER ";" tidin   NO-ERROR.
      END.               
   END.
   FOR EACH tidin WHERE tidin.ENR = "":
      DELETE tidin.
   END.       
   RUN skapaenr_UI.           
/*    OS-DELETE VALUE(wtidvar). */
{EUROPEANAMERICAN.I}

PROCEDURE skapaenr_UI:   
   FOR EACH tidin NO-LOCK:
      IF tidin.NUMMER = 0 THEN DO:                     
          DO TRANSACTION: 
             FIND FIRST mtrlbuff WHERE mtrlbuff.ENR =  tidin.ENR AND
             mtrlbuff.LEVKOD = "100" AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
             EXCLUSIVE-LOCK NO-ERROR.
             IF AVAILABLE mtrlbuff THEN DO:
                MUSZ = MUSZ.
             END.
             ELSE DO:
                CREATE MTRL.
                ASSIGN
                MTRL.ENR = tidin.ENR
                MTRL.LEVKOD = "100"
                MTRL.KALKNR = 0
                MTRL.BENAMNING = TRIM(tidin.BENAMNING).   
                {MTRLCREATE.I}          
             END.
          END.
      END.               
   END.   
END PROCEDURE.   

                
