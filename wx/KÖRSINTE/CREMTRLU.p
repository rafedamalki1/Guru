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

DEFINE INPUT PARAMETER TABLE FOR tidin. 
FIND FIRST FORETAG NO-LOCK NO-ERROR.
FIND FIRST tidin NO-LOCK NO-ERROR.
IF AVAILABLE tidin THEN DO TRANSACTION: 
   /*FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 
 *       AND MTRL.ENR = SUBSTRING(tidin.TIN,e1,
 *       (e2 - e1) + 1) USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.
 *       IF AVAILABLE MTRL THEN DO:
 *          MTRL.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
 *             (np2 - np1) + 1)).
 *       END.
 *       ELSE DO:*/
   FIND FIRST MTRL WHERE MTRL.ENR =  SUBSTRING(tidin.TIN,e1,
      (e2 - e1) + 1) AND
   MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE MTRL THEN DO:
       {MTRLCREATE.I}            
      /*CREATE MTRL.  
      ASSIGN 
      MTRL.LEVKOD = leverant
      MTRL.ENR = SUBSTRING(tidin.TIN,e1,
      (e2 - e1) + 1)
      MTRL.BENAMNING = SUBSTRING(tidin.TIN,b1,
      (b2 - b1) + 1)
      MTRL.ENHET = SUBSTRING(tidin.TIN,en1,
      (en2 - en1) + 1).                                                      
      /*Sundsvall tar med inledande "E" men tex UZ får då inledande blank*/ 
      IF SUBSTRING(MTRL.ENR,1,1) = " " THEN MTRL.ENR = SUBSTRING(MTRL.ENR,2).*/
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
      IF FORETAG.FORETAG = "VAST" AND TODAY = 08/05/2011 THEN DO:
         MTRL.NPRIS = 0.99 * MTRL.NPRIS .
         MTRL.BPRIS = 0.99 * MTRL.BPRIS.
      END.                                                      
      IF FORETAG.FORETAG = "ELPA" THEN DO:
         /*SINGEL*/
         ASSIGN
         MTRL.NPRIS = 0
         MTRL.BPRIS = 0.
      END.   
   END.
   
END.
REPEAT: 
   FIND NEXT tidin NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidin THEN LEAVE.
   ELSE DO TRANSACTION:
      FIND FIRST MTRL WHERE MTRL.ENR =  SUBSTRING(tidin.TIN,e1,
      (e2 - e1) + 1) AND
      MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO:
         {MTRLCREATE.I} 
         /*CREATE MTRL.  
         ASSIGN 
         MTRL.LEVKOD = leverant
         MTRL.ENR = SUBSTRING(tidin.TIN,e1,
         (e2 - e1) + 1)
         MTRL.BENAMNING = SUBSTRING(tidin.TIN,b1,
         (b2 - b1) + 1)
         MTRL.ENHET = SUBSTRING(tidin.TIN,en1,
         (en2 - en1) + 1).*/                                                                         
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
         IF FORETAG.FORETAG = "VAST" AND TODAY = 08/05/2011 THEN DO:
            MTRL.NPRIS = 0.99 * MTRL.NPRIS .
            MTRL.BPRIS = 0.99 * MTRL.BPRIS.
         END.                                                   
         IF FORETAG.FORETAG = "ELPA" THEN DO:
            /*SINGEL*/
            ASSIGN
            MTRL.NPRIS = 0
            MTRL.BPRIS = 0.
         END.
      END.
      
   END. 
END.
RELEASE MTRL NO-ERROR.
/*IF FORETAG.FORETAG = "VAST"  THEN DO:         
   FOR EACH MTRL WHERE MTRL.LEVKOD = leverant AND MTRL.NPRIS = 0 AND KALKNR = 0 EXCLUSIVE-LOCK:
      DELETE MTRL.
   END.  
END.*/
