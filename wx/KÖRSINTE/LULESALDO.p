/*LULESALDO.P INLÄSNING AV PRISFIL ELEF*/       
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
   FIELD BENAMNING          AS CHARACTER   
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL  
   FIELD SALDO              AS INTEGER
   INDEX ENR IS PRIMARY ENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .  
{muswait.i}     
{AMERICANEUROPEAN.I} 
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.       
   wtidvar = "c:\lagersaldo2.skv".
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

{EUROPEANAMERICAN.I}
PROCEDURE skapaenr_UI:     
   /*FIND FIRST tidin.
   MESSAGE TIDIN.ENR TIDIN.BENAMNING TIDIN.ENHET tidin.bPRIS tidin.NPRIS tidin.saldo.   
   */
   /*
   FOR EACH tidin NO-LOCK: 
      FIND FIRST MTRLDEP WHERE MTRLDEP.ENR =  tidin.ENR         
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRLDEP THEN DO:
         MESSAGE TIDIN.ENR TIDIN.BENAMNING TIDIN.ENHET tidin.bPRIS tidin.NPRIS tidin.saldo.
      END.
   END.
   */   
   FOR EACH tidin NO-LOCK:       
         OPEN QUERY dq FOR EACH MTRLDEP WHERE MTRLDEP.ENR =  TRIM(tidin.ENR) NO-LOCK.
         DO TRANSACTION:
            GET FIRST dq EXCLUSIVE-LOCK.
            IF AVAILABLE mtrldep THEN DO:
               ASSIGN               
               MTRLDEP.BPRIS = tidin.BPRIS / 100
               MTRLDEP.NPRIS = tidin.NPRIS / 100.
            END.
         END.
         REPEAT:
            DO TRANSACTION:
               GET NEXT dq EXCLUSIVE-LOCK.
               IF AVAILABLE mtrldep THEN DO:
                  ASSIGN               
                  MTRLDEP.BPRIS = tidin.BPRIS / 100
                  MTRLDEP.NPRIS = tidin.NPRIS / 100.
               END.
               ELSE LEAVE.
            END.
         END.
         /*IF AVAILABLE MTRLDEP THEN DO:
            ASSIGN
            MTRLDEP.BPRIS = tidin.BPRIS / 100
            MTRLDEP.NPRIS = tidin.NPRIS / 100.
            MTRLDEP.SALDO = tidin.SALDO.
         END.*/
         /*ELSE DO:
            CREATE MTRLDEP.
            ASSIGN
            MTRLDEP.ENR = TRIM(tidin.ENR)          
            MTRLDEP.LEVKOD = "40"            
            MTRLDEP.BENAMNING = TRIM(tidin.BENAMNING)
            MTRLDEP.ENHET = TRIM(tidin.ENHET)
            MTRLDEP.NPRIS = tidin.NPRIS
            MTRLDEP.BPRIS = tidin.BPRIS
            MTRLDEP.SALDO = tidin.SALDO.            
         END. */      
   END.      
END PROCEDURE.   

                
