DEFINE VARIABLE kalle AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE mtrl_temp
   FIELD ENR LIKE MTRL.ENR
   FIELD BENAMNING LIKE MTRL.BENAMNING
   FIELD ENHET LIKE MTRL.ENHET
   FIELD NPRIS LIKE MTRL.NPRIS
   FIELD BPRIS LIKE MTRL.BPRIS
   INDEX ENR ENR ASCENDING.
   
DEFINE TEMP-TABLE mtrl_temp2
  {MTRLTEMP2TT.I}
   
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

DEFINE INPUT PARAMETER deci AS INTEGER NO-UNDO. 
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
DEFINE OUTPUT PARAMETER TABLE FOR mtrl_temp2. 
   
   FIND FIRST tidin NO-LOCK NO-ERROR.
   IF AVAILABLE tidin THEN DO TRANSACTION:      
      CREATE mtrl_temp.  
      ASSIGN 
      mtrl_temp.ENR = SUBSTRING(tidin.TIN,e1,
      (e2 - e1) + 1)
      mtrl_temp.BENAMNING = SUBSTRING(tidin.TIN,b1,
      (b2 - b1) + 1)
      mtrl_temp.ENHET = SUBSTRING(tidin.TIN,en1,
      (en2 - en1) + 1).                                                                         
      IF SUBSTRING(tidin.TIN,(pos2 - deci),1) = "." THEN DO:
         IF np1 = 0 THEN DO: 
            mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)).
         END.
         ELSE DO:          
            mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)).
         END.
         IF bp1 = 0 THEN DO:
            mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)).
         END.
         ELSE DO:      
            mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)). 
         END.      
      END. 
      ELSE DO:  
         IF np1 = 0 THEN DO: 
            mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)) / EXP(10,deci).
         END.
         ELSE DO:          
             mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
             (np2 - np1) + 1)) / EXP(10,deci).
         END.
         IF bp1 = 0 THEN DO:
            mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
            (np2 - np1) + 1)) / EXP(10,deci).
         END.
         ELSE DO:      
            mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
            (bp2 - bp1) + 1)) / EXP(10,deci). 
         END.
      END.                                                       
   END.
   REPEAT: 
      FIND NEXT tidin NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidin THEN LEAVE.
      ELSE DO TRANSACTION:         
         CREATE mtrl_temp.  
         ASSIGN 
         mtrl_temp.ENR = SUBSTRING(tidin.TIN,e1,
         (e2 - e1) + 1)
         mtrl_temp.BENAMNING = SUBSTRING(tidin.TIN,b1,
         (b2 - b1) + 1)
         mtrl_temp.ENHET = SUBSTRING(tidin.TIN,en1,
         (en2 - en1) + 1).                                                                         
         IF SUBSTRING(tidin.TIN,(pos2 - deci),1) = "." THEN DO:
            IF np1 = 0 THEN DO: 
               mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
               (bp2 - bp1) + 1)).
            END.
            ELSE DO:          
               mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
               (np2 - np1) + 1)).
            END.
            IF bp1 = 0 THEN DO:
               mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
               (np2 - np1) + 1)).
            END.
            ELSE DO:      
               mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
               (bp2 - bp1) + 1)). 
            END.      
         END. 
         ELSE DO:  
            IF np1 = 0 THEN DO: 
               mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
               (bp2 - bp1) + 1)) / EXP(10,deci).
            END.
            ELSE DO:          
               mtrl_temp.NPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
               (np2 - np1) + 1)) / EXP(10,deci).
            END.
            IF bp1 = 0 THEN DO:
               mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,np1,
               (np2 - np1) + 1)) / EXP(10,deci).
            END.
            ELSE DO:      
               mtrl_temp.BPRIS = DECIMAL(SUBSTRING(tidin.TIN,bp1,
               (bp2 - bp1) + 1)) / EXP(10,deci). 
            END.
         END.                                                            
      END. 
   END.
   
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
   MTRL.LEVKOD = leverant USE-INDEX LEV NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq NO-LOCK.
      IF AVAILABLE MTRL THEN DO:
         FIND FIRST mtrl_temp WHERE mtrl_temp.ENR = mtrl.ENR USE-INDEX ENR
         NO-ERROR.
         IF NOT AVAILABLE mtrl_temp THEN DO:
            CREATE mtrl_temp2.
            ASSIGN
            mtrl_temp2.ENR = MTRL.ENR
            mtrl_temp2.BENAMNING = MTRL.BENAMNING
            mtrl_temp2.ENHET = MTRL.ENHET
            mtrl_temp2.PRIS = MTRL.NPRIS.
         END.
      END.  
   END. 
   REPEAT:     
      DO TRANSACTION:
         GET NEXT mtrlq NO-LOCK.         
         IF NOT AVAILABLE MTRL THEN LEAVE.
         ELSE DO:
            FIND FIRST mtrl_temp WHERE mtrl_temp.ENR = mtrl.ENR USE-INDEX ENR
            NO-ERROR.
            IF NOT AVAILABLE mtrl_temp THEN DO:
               CREATE mtrl_temp2.
               ASSIGN
               mtrl_temp2.ENR = MTRL.ENR
               mtrl_temp2.BENAMNING = MTRL.BENAMNING
               mtrl_temp2.ENHET = MTRL.ENHET
               mtrl_temp2.PRIS = MTRL.NPRIS.
            END.            
         END.
      END.
   END.                  
   CLOSE QUERY mtrlq.              
