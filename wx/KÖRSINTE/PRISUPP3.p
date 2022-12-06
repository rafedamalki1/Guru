DEFINE BUFFER mtrlbuff FOR MTRL.
DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
     
   
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
   MTRL.LEVKOD = leverant USE-INDEX LEV NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = mtrl.ENR 
         AND mtrlbuff.LEVKOD = "5" AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
         NO-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:            
            ASSIGN
            MTRL.BENAMNING = mtrlbuff.BENAMNING.            
         END.         
      END.  
   END. 
   REPEAT:     
      DO TRANSACTION:
         GET NEXT mtrlq EXCLUSIVE-LOCK.         
         IF NOT AVAILABLE MTRL THEN LEAVE.
         ELSE DO:
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = mtrl.ENR 
            AND mtrlbuff.LEVKOD = "5" AND mtrlbuff.KALKNR = 0 USE-INDEX LEV
            NO-LOCK NO-ERROR.
            IF AVAILABLE mtrlbuff THEN DO:            
               ASSIGN
               MTRL.BENAMNING = mtrlbuff.BENAMNING.            
            END.
         END.
      END.
   END.                  
   CLOSE QUERY mtrlq.   
