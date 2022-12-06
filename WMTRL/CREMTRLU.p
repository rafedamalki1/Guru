/*CREMTRLU.p*/

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

DEFINE INPUT PARAMETER decivar AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER pos2 AS INTEGER NO-UNDO.

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
DEFINE INPUT PARAMETER e1 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER e2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER b1 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER b2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER en1 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER en2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER bp1 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER bp2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER np1 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER np2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER svar5 AS LOGICAL NO-UNDO. 


DEFINE INPUT PARAMETER TABLE FOR tidin.

DEFINE VARIABLE leverant2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE enrrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO. 
DEFINE BUFFER mtrlbuff FOR MTRL.

{TIDUTTT.I}

FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "VAST" THEN DO:
    leverant2 = "3".
    prognamnque = "e:\delad\pro9s\MTRLKOLL.txt".
    prognamnque2 = "e:\delad\pro9s\ENRKOLL.txt".
END.    
ELSE DO:
    leverant2 = "".
    prognamnque = "".
    prognamnque2 = "".
END.    

   

IF svar5 = TRUE THEN DO:
   FOR EACH MTRL WHERE MTRL.LEVKOD = leverant AND  MTRL.KALKNR = 0  EXCLUSIVE-LOCK:
      DELETE MTRL.
   END.
END.   


FIND FIRST tidin NO-LOCK NO-ERROR.
IF AVAILABLE tidin THEN DO TRANSACTION:    
   raknare = 1.   
   FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND MTRL.Enr = SUBSTRING(tidin.TIN,e1,(e2 - e1) + 1) EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE MTRL THEN DO:
      CREATE MTRL.
   END.     
   {MTRLCREATE.I} 
   ASSIGN 
   MTRL.LEVKOD = leverant
   MTRL.ENR = SUBSTRING(tidin.TIN,e1,
   (e2 - e1) + 1)
   MTRL.BENAMNING = SUBSTRING(tidin.TIN,b1,
   (b2 - b1) + 1)
   MTRL.ENHET = SUBSTRING(tidin.TIN,en1,
   (en2 - en1) + 1).                                                      
   /*Sundsvall tar med inledande "E" men tex UZ får då inledande blank*/ 
   IF SUBSTRING(MTRL.ENR,1,1) = " " THEN MTRL.ENR = SUBSTRING(MTRL.ENR,2).
   IF SUBSTRING(tidin.TIN,(pos2 - decivar),1) = "." THEN DO:
      IF np1 = 0 THEN DO: 
         MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
         (bp2 - bp1) + 1)).
      END.
      ELSE DO:          
         MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
         (np2 - np1) + 1)).
      END.
      IF bp1 = 0 THEN DO:
         MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
         (np2 - np1) + 1)).
      END.
      ELSE DO:      
         MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
         (bp2 - bp1) + 1)). 
      END.      
   END. 
   ELSE DO:  
      IF np1 = 0 THEN DO: 
         MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
         (bp2 - bp1) + 1)) / EXP(10,decivar).
      END.
      ELSE DO:          
          MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
          (np2 - np1) + 1)) / EXP(10,decivar).
      END.
      IF bp1 = 0 THEN DO:
         MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
         (np2 - np1) + 1)) / EXP(10,decivar).
      END.
      ELSE DO:      
         MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
         (bp2 - bp1) + 1)) / EXP(10,decivar). 
      END.
   END.
                                    
   IF FORETAG.FORETAG = "VAST" THEN.    
   ELSE IF FORETAG.FORETAG = "GKAL" THEN.
   ELSE DO:                 
   /* Bara Vattenfall och Kalmar har priset i filen*/   
      ASSIGN
      MTRL.NPRIS = 0
      MTRL.BPRIS = 0.
   END.
END.
DEBUGGER:SET-BREAK().
REPEAT: 
   FIND NEXT tidin NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidin THEN LEAVE.
   ELSE DO TRANSACTION:      
      raknare = raknare + 1.
      FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND MTRL.Enr = SUBSTRING(tidin.TIN,e1,(e2 - e1) + 1) EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:
         CREATE MTRL.
      END.        
      {MTRLCREATE.I} 
      ASSIGN 
      MTRL.LEVKOD = leverant
      MTRL.ENR = SUBSTRING(tidin.TIN,e1,
      (e2 - e1) + 1)
      MTRL.BENAMNING = SUBSTRING(tidin.TIN,b1,
      (b2 - b1) + 1)
      MTRL.ENHET = SUBSTRING(tidin.TIN,en1,
      (en2 - en1) + 1).                                                                         
      IF SUBSTRING(tidin.TIN,(pos2 - decivar),1) = "." THEN DO:
         IF np1 = 0 THEN DO: 
            MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)).
         END.
         ELSE DO:          
            MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)).
         END.
         IF bp1 = 0 THEN DO:
            MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)).
         END.
         ELSE DO:      
            MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)). 
         END.      
      END. 
      ELSE DO:  
         IF np1 = 0 THEN DO: 
            MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)) / EXP(10,decivar).
         END.
         ELSE DO:          
            MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)) / EXP(10,decivar).
         END.
         IF bp1 = 0 THEN DO:
            MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)) / EXP(10,decivar).
         END.
         ELSE DO:      
            MTRL.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)) / EXP(10,decivar). 
         END.
      END.
      IF FORETAG.FORETAG = "cVAST"  THEN DO:
         /*Borttaget 20140110 lena*/
         MTRL.NPRIS = 0.99 * MTRL.NPRIS .
         MTRL.BPRIS = 0.99 * MTRL.BPRIS.
      END.         
      IF FORETAG.FORETAG = "VAST" THEN.    
      ELSE IF FORETAG.FORETAG = "GKAL" THEN.
      ELSE DO:                 
      /* Bara Vattenfall och Kalmar har priset i filen    */         
         ASSIGN
         MTRL.NPRIS = 0
         MTRL.BPRIS = 0.
      END.
   END. 
END.
RELEASE MTRL NO-ERROR.
IF FORETAG.FORETAG = "VAST"  THEN DO:         
   FOR EACH MTRL WHERE MTRL.LEVKOD = leverant AND MTRL.NPRIS = 0 AND KALKNR = 0 EXCLUSIVE-LOCK:
      DELETE MTRL.
   END.  
END.

IF FORETAG.FORETAG = "VAST"  THEN DO:   
   
   RUN textut_UI (INPUT "Delete extramateriel start" , TRUE).
   /*Kopiera in extramateriel OM DET INTE REDAN FINNS I KATALOGEN FÖR DÅ TAR VI BORT DEN*/
   IF leverant2 NE "" THEN DO:
      OPEN QUERY mtrlq1 FOR EACH MTRL WHERE MTRL.LEVKOD = leverant2 AND
      MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.
      DO TRANSACTION:
         GET FIRST mtrlq1 EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DO:
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND mtrlbuff.LEVKOD = leverant
            AND mtrlbuff.KALKNR = 0 NO-LOCK NO-ERROR.
            IF AVAILABLE mtrlbuff THEN DO:
               RUN textutENr_UI (INPUT MTRL.LEVKOD + " " + MTRL.Enr , FALSE).
               DELETE MTRL.
            END.
            ELSE DO:
               CREATE mtrlbuff.
               ASSIGN
               mtrlbuff.ENR = MTRL.ENR
               mtrlbuff.BENAMNING = MTRL.BENAMNING
               mtrlbuff.ENHET = MTRL.ENHET
               mtrlbuff.LEVKOD = leverant
               mtrlbuff.NPRIS = MTRL.NPRIS
               mtrlbuff.BPRIS = MTRL.BPRIS
               mtrlbuff.KALKNR = 0.
               mtrlbuff.INDATETIME = NOW.
   mtrlbuff.INANVPROG = THIS-PROCEDURE:NAME   + " " + Guru.Konstanter:globanv.
            END.                  
         END.
      END.   
      REPEAT:
         DO TRANSACTION:
            GET NEXT mtrlq1 EXCLUSIVE-LOCK.
            IF AVAILABLE MTRL THEN DO:
               FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND mtrlbuff.LEVKOD = leverant
               AND mtrlbuff.KALKNR = 0 NO-LOCK NO-ERROR.
               IF AVAILABLE mtrlbuff THEN DO:
                  RUN textutENr_UI (INPUT MTRL.LEVKOD + " " + MTRL.Enr , FALSE).
                  DELETE MTRL.
               END.
               ELSE DO:
                  CREATE mtrlbuff.
                  ASSIGN
                  mtrlbuff.ENR = MTRL.ENR
                  mtrlbuff.BENAMNING = MTRL.BENAMNING
                  mtrlbuff.ENHET = MTRL.ENHET
                  mtrlbuff.LEVKOD = leverant
                  mtrlbuff.NPRIS = MTRL.NPRIS
                  mtrlbuff.BPRIS = MTRL.BPRIS
                  mtrlbuff.KALKNR = 0.
                  mtrlbuff.INDATETIME = NOW.
   mtrlbuff.INANVPROG = THIS-PROCEDURE:NAME   + " " + Guru.Konstanter:globanv.
               END.                  
            END.
            ELSE LEAVE.
         END.
      END.   
      CLOSE QUERY mtrlq1.
      RUN textut_UI (INPUT "Delete extramateriel SLUT" , TRUE).
   END.   
END.
RUN textut_UI (INPUT "Kopi enr slut" , TRUE).

RUN textut_UI (INPUT "priser start" , TRUE).         
/*Uppdatera priser i adm.bered. för vald lev.*/
RUN BERKOLL2.P (INPUT leverant, INPUT-OUTPUT TABLE tidut).
/*Uppdatera satsers priser i bered. för vald lev.*/
RUN PRISUPP6.P (INPUT leverant, INPUT FORETAG.FORETAG).
/*Priser i depåer*/
RUN textut_UI (INPUT "priser slut" , TRUE).



{EUROPEANAMERICAN.I}

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lagagtill AS LOGICAL NO-UNDO.
   IF prognamnque = "" THEN RETURN.
   IF lagagtill = TRUE THEN OUTPUT TO VALUE(prognamnque) APPEND NO-ECHO.
   ELSE OUTPUT TO VALUE(prognamnque) NO-ECHO.
   PUT UNFORMATTED meddvar " " FORETAG.FORETAG  " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
PROCEDURE textutENr_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lagagtill AS LOGICAL NO-UNDO.
   IF prognamnque2 = "" THEN RETURN.
   enrrakn = enrrakn + 1.
   IF lagagtill = TRUE THEN OUTPUT TO VALUE(prognamnque2) APPEND.
   ELSE OUTPUT TO VALUE(prognamnque2) NO-ECHO.
   PUT UNFORMATTED meddvar " " FORETAG.FORETAG " " enrrakn " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE. 
