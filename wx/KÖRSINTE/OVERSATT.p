/*OVERSATT.P*/
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
   FIELD GAMENR                AS CHARACTER
   FIELD A                     AS CHARACTER
   FIELD A1                    AS CHARACTER
   FIELD NYTTENR               AS CHARACTER      
   INDEX GAMENR IS PRIMARY GAMENR.
   

DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN.
DEFINE TEMP-TABLE intid
   FIELD TIN AS CHARACTER FORMAT "X(78)" .
   
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.   
DEFINE VARIABLE leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{AMERICANEUROPEAN.I}
{muswait.i}      
   EMPTY TEMP-TABLE intid NO-ERROR. 
   EMPTY TEMP-TABLE tidin NO-ERROR.       
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   IF FORETAG.FORETAG = "ELPA" THEN DO:
      filnamn = "\\server04\d\elpool\elpnj\borl\ESTILLONN.skv".
   END. 
   ELSE IF FORETAG.FORETAG = "BORL" THEN DO:
      filnamn = "D:\GURU\PRO9\GURU\WTID\ESTILLONN.skv".      
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
   FOR EACH tidin WHERE tidin.GAMENR = "":
      DELETE tidin.
   END.       
   RUN skapaenr_UI.           
   IF FORETAG.FORETAG = "BORL" THEN DO:
      OUTPUT TO D:\GURU\PRO9\koll.txt APPEND.
      PUT "Oversatt klart. " substring(FORETAG.FORETAG,1,10) SKIP.
      OUTPUT CLOSE.
   END.
{EUROPEANAMERICAN.I}
PROCEDURE skapaenr_UI:    
   /*
   FOR EACH tidin NO-LOCK: 
      tidin.GAMENR = REPLACE(tidin.GAMENR," ",""). 
      tidin.NYTTENR = REPLACE(tidin.NYTTENR," ","").
   END.   
   */ 
  OUTPUT TO D:\GURU\PRO9\GURU\KALLE.TXT.
  FOR EACH tidin NO-LOCK:   
      /*PUT TIDIN.NYTTENR SKIP.*/
      OPEN QUERY MQ FOR EACH MTRLBER WHERE MTRLBER.ENR = tidin.GAMENR AND MTRLBER.LEVKOD = "1" NO-LOCK.
      DO TRANSACTION:
         GET FIRST MQ EXCLUSIVE-LOCK.
         IF AVAILABLE MTRLBER THEN DO:
            FIND FIRST MTRL WHERE MTRL.ENR = SUBSTRING(tidin.NYTTENR,2) AND MTRL.KALKNR = 0 AND 
            MTRL.LEVKOD = "5" NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:
               ASSIGN
               MTRLBER.ENR = SUBSTRING(tidin.NYTTENR,2)
               MTRLBER.BENAMNING = MTRL.BENAMNING
               MTRLBER.ENHET = MTRL.ENHET
               MTRLBER.PRIS = MTRL.NPRIS
               MTRLBER.LEVKOD = MTRL.LEVKOD.
            END.
            ELSE DO:                         
               PUT TIDIN.NYTTENR FORMAT "X(15)" SKIP.
            END.                             
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT MQ EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DO:                             
               FIND FIRST MTRL WHERE MTRL.ENR = SUBSTRING(tidin.NYTTENR,2) AND MTRL.KALKNR = 0 AND 
               MTRL.LEVKOD = "5" NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:
                  ASSIGN
                  MTRLBER.ENR = SUBSTRING(tidin.NYTTENR,2)
                  MTRLBER.BENAMNING = MTRL.BENAMNING
                  MTRLBER.ENHET = MTRL.ENHET
                  MTRLBER.PRIS = MTRL.NPRIS
                  MTRLBER.LEVKOD = MTRL.LEVKOD.
               END.
               ELSE DO:                         
                  PUT TIDIN.NYTTENR FORMAT "X(15)" SKIP.
               END.                              
            END.
            ELSE LEAVE.
         END.
      END.      
      CLOSE QUERY MQ.      

      /*STOLPAR OCH TRANSFORMATORER*/      
      OPEN QUERY stolpq FOR EACH BERSTOLP WHERE BERSTOLP.ENR = tidin.GAMENR AND
      BERSTOLP.LEVKOD = "1" NO-LOCK.            
      DO TRANSACTION:
         GET FIRST stolpq EXCLUSIVE-LOCK.
         IF AVAILABLE BERSTOLP THEN DO:
            FIND FIRST MTRL WHERE MTRL.ENR = SUBSTRING(tidin.NYTTENR,2) AND MTRL.KALKNR = 0 AND 
            MTRL.LEVKOD = "5" NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:
               ASSIGN
               BERSTOLP.ENR = SUBSTRING(tidin.NYTTENR,2)
               BERSTOLP.BENAMNING = MTRL.BENAMNING
               BERSTOLP.ENHET = MTRL.ENHET
               BERSTOLP.PRIS = MTRL.NPRIS            
               BERSTOLP.LEVKOD = MTRL.LEVKOD.
            END.
         END.
      END.      
      REPEAT:
         DO TRANSACTION:
            GET NEXT stolpq EXCLUSIVE-LOCK. 
            IF AVAILABLE BERSTOLP THEN DO:
               FIND FIRST MTRL WHERE MTRL.ENR = SUBSTRING(tidin.NYTTENR,2) AND MTRL.KALKNR = 0 AND 
               MTRL.LEVKOD = "5" NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:
                  ASSIGN
                  BERSTOLP.ENR = SUBSTRING(tidin.NYTTENR,2)
                  BERSTOLP.BENAMNING = MTRL.BENAMNING
                  BERSTOLP.ENHET = MTRL.ENHET
                  BERSTOLP.PRIS = MTRL.NPRIS            
                  BERSTOLP.LEVKOD = MTRL.LEVKOD.
               END.
            END.
            ELSE LEAVE.
         END.         
      END.      
      CLOSE QUERY stolpq.   
      /*KABELSKÅP*/      
      OPEN QUERY skapq FOR EACH BERSKAP WHERE BERSKAP.ENR = tidin.GAMENR AND
      BERSKAP.LEVKOD = "1" NO-LOCK.            
      DO TRANSACTION:
         GET FIRST skapq EXCLUSIVE-LOCK.
         IF AVAILABLE BERSKAP THEN DO:
            FIND FIRST MTRL WHERE MTRL.ENR = SUBSTRING(tidin.NYTTENR,2) AND MTRL.KALKNR = 0 AND 
            MTRL.LEVKOD = "5" NO-LOCK NO-ERROR.
            IF AVAILABLE MTRL THEN DO:
               ASSIGN
               BERSKAP.ENR = SUBSTRING(tidin.NYTTENR,2)
               BERSKAP.BENAMNING = MTRL.BENAMNING
               BERSKAP.ENHET = MTRL.ENHET
               BERSKAP.PRIS = MTRL.NPRIS            
               BERSKAP.LEVKOD = MTRL.LEVKOD.
            END.
         END.
      END.      
      REPEAT:
         DO TRANSACTION:
            GET NEXT skapq EXCLUSIVE-LOCK. 
            IF AVAILABLE BERSKAP THEN DO:
               FIND FIRST MTRL WHERE MTRL.ENR = SUBSTRING(tidin.NYTTENR,2) AND MTRL.KALKNR = 0 AND 
               MTRL.LEVKOD = "5" NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:
                  ASSIGN
                  BERSKAP.ENR = SUBSTRING(tidin.NYTTENR,2)
                  BERSKAP.BENAMNING = MTRL.BENAMNING
                  BERSKAP.ENHET = MTRL.ENHET
                  BERSKAP.PRIS = MTRL.NPRIS            
                  BERSKAP.LEVKOD = MTRL.LEVKOD.
               END.
            END.
            ELSE LEAVE.
         END.         
      END.      
      CLOSE QUERY skapq. 
   END.    
   OUTPUT CLOSE.
END PROCEDURE.   

                
