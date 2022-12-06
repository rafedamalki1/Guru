/*IMPMTRLSKV2.P*/
DEFINE TEMP-TABLE tidin   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.

DEFINE INPUT PARAMETER TABLE FOR tidin.
DEFINE INPUT PARAMETER leverant AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER svar5 AS LOGICAL NO-UNDO.
IF svar5 = TRUE THEN DO:
   OPEN QUERY dq FOR EACH MTRL WHERE MTRL.LEVKOD = leverant AND 
   MTRL.KALKNR = 0 NO-LOCK.
   DO TRANSACTION:
      GET FIRST dq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DELETE MTRL.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT dq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DELETE MTRL.
         ELSE LEAVE.
      END.
   END.
   FOR EACH tidin:
      DO TRANSACTION:
         CREATE MTRL.
         ASSIGN      
         MTRL.ENR = tidin.ENR
         MTRL.BENAMNING = tidin.BENAMNING
         MTRL.ENHET = tidin.ENHET
         MTRL.LEVKOD = leverant
         MTRL.KALKNR = 0
         MTRL.BPRIS = tidin.BPRIS 
         MTRL.NPRIS = tidin.NPRIS. 
         {MTRLCREATE.I}                 
      END.
   END.
END.
ELSE DO:
   FOR EACH tidin:
      DO TRANSACTION:         
         FIND FIRST MTRL WHERE MTRL.ENR =  tidin.ENR AND
         MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE MTRL THEN DO:
            ASSIGN 
            MTRL.BENAMNING = tidin.BENAMNING 
            MTRL.BPRIS = tidin.BPRIS 
            MTRL.NPRIS = tidin.NPRIS 
            MTRL.ENHET = tidin.ENHET.
            {MTRLCREATE.I} 
         END.
         ELSE DO:                               
            CREATE MTRL.
            ASSIGN      
            MTRL.ENR = tidin.ENR
            MTRL.LEVKOD = leverant 
            MTRL.KALKNR = 0        
            MTRL.BENAMNING = tidin.BENAMNING 
            MTRL.BPRIS = tidin.BPRIS       
            MTRL.NPRIS = tidin.NPRIS 
            MTRL.ENHET = tidin.ENHET.
            {MTRLCREATE.I}             
         END.   
      END.
   END.
END.
