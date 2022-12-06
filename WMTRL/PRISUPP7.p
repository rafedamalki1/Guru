/* Programmet uppdaterar leverantör 12 Ericson kabel för VSAB                */

DEFINE BUFFER mtrlbuff FOR MTRL.   
   
    
   /*ericsson kablar*/
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
   MTRL.LEVKOD = "12" USE-INDEX LEV NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrlq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND 
         mtrlbuff.LEVKOD = "1" AND mtrlbuff.KALKNR = 0 
         USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:
            ASSIGN
            MTRL.BENAMNING = mtrlbuff.BENAMNING
            MTRL.ENHET = mtrlbuff.ENHET
            MTRL.NPRIS = mtrlbuff.NPRIS
            MTRL.BPRIS = mtrlbuff.BPRIS.
         END.
         ELSE DO:
            DELETE MTRL.
         END.
      END.  
   END. 
   REPEAT:     
      DO TRANSACTION:
         GET NEXT mtrlq EXCLUSIVE-LOCK.         
         IF NOT AVAILABLE MTRL THEN LEAVE.
         ELSE DO:
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND 
            mtrlbuff.LEVKOD = "1" AND mtrlbuff.KALKNR = 0 
            USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE mtrlbuff THEN DO:
               ASSIGN
               MTRL.BENAMNING = mtrlbuff.BENAMNING
               MTRL.ENHET = mtrlbuff.ENHET
               MTRL.NPRIS = mtrlbuff.NPRIS
               MTRL.BPRIS = mtrlbuff.BPRIS.
            END.
            ELSE DO:
               DELETE MTRL.
            END.
         END.
      END.
   END.                  
   CLOSE QUERY mtrlq.    
   RELEASE MTRL NO-ERROR.
   /*skall inte beställas från Onninen 2007-12-10 lena via Handfast
   /*Transf stn Transfix*/
   OPEN QUERY mtrltq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND 
   MTRL.LEVKOD = "70" USE-INDEX LEV NO-LOCK.
   DO TRANSACTION:
      GET FIRST mtrltq EXCLUSIVE-LOCK.
      IF AVAILABLE MTRL THEN DO:
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND 
         mtrlbuff.LEVKOD = "16" AND mtrlbuff.KALKNR = 0 
         USE-INDEX LEV NO-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN DO:
            ASSIGN
            MTRL.NPRIS = mtrlbuff.NPRIS
            MTRL.BPRIS = mtrlbuff.BPRIS.
         END.
         ELSE DO:
            DELETE MTRL.
         END.
      END.  
   END. 
   REPEAT:     
      DO TRANSACTION:
         GET NEXT mtrltq EXCLUSIVE-LOCK.         
         IF NOT AVAILABLE MTRL THEN LEAVE.
         ELSE DO:
            FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND 
            mtrlbuff.LEVKOD = "16" AND mtrlbuff.KALKNR = 0 
            USE-INDEX LEV NO-LOCK NO-ERROR.
            IF AVAILABLE mtrlbuff THEN DO:
               ASSIGN
               MTRL.NPRIS = mtrlbuff.NPRIS
               MTRL.BPRIS = mtrlbuff.BPRIS.
            END.
            ELSE DO:
               DELETE MTRL.
            END.
         END.
      END.
   END.                  
   CLOSE QUERY mtrltq.    
 */
