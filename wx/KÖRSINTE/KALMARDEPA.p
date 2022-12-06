/*KALMARDEPA.P INLÄSNING AV KALMAR DEPÅ*/       
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
DEFINE VARIABLE FILNAMN AS CHARACTER.

DEFINE BUFFER mtrlbuff FOR MTRLDEP.

DEFINE TEMP-TABLE tidin
   FIELD ENR                AS CHARACTER
   FIELD NPRIS              AS DECIMAL
   FIELD SALDO              AS INTEGER
   FIELD BENAMNING          AS CHARACTER   
   FIELD ENHET              AS CHARACTER  
   FIELD FACKID             AS CHARACTER
   FIELD BESTPUNKT          AS INTEGER
   INDEX ENR IS PRIMARY ENR.

DEFINE TEMP-TABLE rak_temp
   FIELD BOKSTAV AS CHARACTER
   FIELD RAKNARE AS INTEGER
   FIELD CHARRAK AS CHARACTER
   INDEX RAK BOKSTAV RAKNARE.

DEFINE BUFFER rak_buff FOR rak_temp.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
{muswait.i}      
   FILNAMN = "f:\elpool\elpnj\kalmar\Lagersaldo20021230.SKV".
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.    
 {AMERICANEUROPEAN.I}  
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
      DO TRANSACTION: 
         CREATE MTRLDEP.
         ASSIGN
         MTRLDEP.ENR = tidin.ENR         
         MTRLDEP.LEVKOD = "1"         
         MTRLDEP.BENAMNING = TRIM(tidin.BENAMNING)
         MTRLDEP.ENHET = tidin.ENHET
         MTRLDEP.NPRIS = tidin.NPRIS / 100
         MTRLDEP.BPRIS = tidin.NPRIS / 100
         MTRLDEP.BESTPUNKT = tidin.BESTPUNKT
         MTRLDEP.DEPNR = 1
         MTRLDEP.LAGER = TRUE
         MTRLDEP.INVDATUM = TODAY
         MTRLDEP.SALDO = tidin.SALDO.
         IF tidin.FACKID NE "" THEN DO:         
            FIND LAST rak_temp WHERE rak_temp.BOKSTAV = tidin.FACKID
            USE-INDEX RAK NO-LOCK NO-ERROR.
            IF AVAILABLE rak_temp THEN DO:
/*                IF rak_temp.bokstav = "k" THEN DO: */
/*                   DISPLAY rak_temp.raknare.       */
/*                END.                               */
               CREATE rak_buff.
               ASSIGN
               rak_buff.BOKSTAV = tidin.FACKID + " "
               rak_buff.RAKNARE = rak_temp.RAKNARE + 1.
               IF rak_buff.RAKNARE >= 10 AND rak_buff.raknare <= 99 THEN rak_buff.CHARRAK = "    " + STRING(rak_buff.RAKNARE).
               ELSE IF rak_buff.RAKNARE >= 100 THEN rak_buff.CHARRAK = "   " + STRING(rak_buff.RAKNARE).
               ELSE rak_buff.CHARRAK = "     " + STRING(rak_buff.RAKNARE).
               MTRLDEP.FACKID = rak_buff.BOKSTAV + rak_buff.CHARRAK.
            END.
            ELSE DO:
               CREATE rak_temp.
               ASSIGN
               rak_temp.BOKSTAV = tidin.FACKID + " "
               rak_temp.RAKNARE = 1
               rak_temp.CHARRAK = "     " + STRING(rak_temp.RAKNARE)
               MTRLDEP.FACKID = rak_temp.BOKSTAV + rak_temp.CHARRAK.
            END.
         END.
      END.            
   END.   
END PROCEDURE.   

                
