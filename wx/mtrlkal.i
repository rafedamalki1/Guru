/*MTRLKAL.I}*/   
   FIND FIRST MTRL WHERE MTRL.KALKNR = FASTSPEC.KALKNR AND 
   MTRL.OFFERT = TRUE AND MTRL.KUND = FALSE USE-INDEX KALK NO-LOCK NO-ERROR.
   IF AVAILABLE MTRL THEN kontroll = TRUE.                           
   ELSE kontroll = FALSE.      
   totmtrl = 0.
   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = FASTSPEC.KALKNR AND 
   MTRL.OFFERT = FALSE AND MTRL.KUND = FALSE USE-INDEX KALK NO-LOCK.
   GET FIRST mtrlq NO-LOCK.
   DO WHILE AVAILABLE(MTRL):
      IF kontroll = TRUE THEN DO:
         FIND FIRST mtrlbuff WHERE mtrlbuff.KALKNR = FASTSPEC.KALKNR AND 
         mtrlbuff.OFFERT = TRUE AND mtrlbuff.KUND = FALSE 
         AND mtrlbuff.LEVKOD = MTRL.LEVKOD USE-INDEX KALK NO-LOCK NO-ERROR.
         IF NOT AVAILABLE mtrlbuff THEN
         totmtrl = totmtrl + (MTRL.NPRIS * MTRL.BERKVANT).          
      END.
      ELSE DO:
         totmtrl = totmtrl + (MTRL.NPRIS * MTRL.BERKVANT).
      END.   
      GET NEXT mtrlq NO-LOCK.                            
   END.
   IF kontroll = TRUE THEN DO:
      OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = FASTSPEC.KALKNR AND 
      MTRL.OFFERT = TRUE AND MTRL.KUND = FALSE USE-INDEX KALK NO-LOCK.
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRL):        
         totmtrl = totmtrl + MTRL.NPRIS.
         GET NEXT mtrlq NO-LOCK.
      END.                      
      CLOSE QUERY mtrlq.        
   END.                 
