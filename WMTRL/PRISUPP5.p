/*PRISUPP5.P
UPPDATERAR PRISER FÖR TABELLER SOM ANVÄNDS I BEREDNING*/

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
   
   
   OPEN QUERY qmtrl0 FOR EACH MTRL WHERE MTRL.LEVKOD = leverant AND
   MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl0 EXCLUSIVE-LOCK.
      ASSIGN
      MTRL.NPRIS = 0
      MTRL.BPRIS = 0
      MTRL.KUND = FALSE.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl0 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE MTRL THEN LEAVE.
         ELSE DO:
            ASSIGN
            MTRL.NPRIS = 0
            MTRL.BPRIS = 0
            MTRL.KUND = FALSE.
         END.
      END.
   END.
   CLOSE QUERY qmtrl0.
   
   /*MTRL KOPPLAT TILL KONSTRUKTIONER*/      
   
   OPEN QUERY qmtrl FOR EACH MTRLBER WHERE MTRLBER.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl EXCLUSIVE-LOCK. 
      IF AVAILABLE MTRLBER THEN DO:                                
         ASSIGN         
         MTRLBER.PRIS = 0.                       
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl EXCLUSIVE-LOCK.
         IF NOT AVAILABLE MTRLBER THEN LEAVE.
         ELSE DO:                                        
            ASSIGN               
            MTRLBER.PRIS = 0.             
         END.
      END.
   END.
   CLOSE QUERY qmtrl.
   
   /*STOLPAR OCH STATIONER*/
   
   OPEN QUERY qmtrl2 FOR EACH BERSTOLP WHERE BERSTOLP.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl2 EXCLUSIVE-LOCK.
      IF AVAILABLE BERSTOLP THEN DO:                                   
         ASSIGN         
         BERSTOLP.PRIS = 0.                                       
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl2 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE BERSTOLP THEN LEAVE.
         ELSE DO:                                         
            ASSIGN               
            BERSTOLP.PRIS = 0.          
         END.
      END.
   END.
   CLOSE QUERY qmtrl2.
      
   /*KABELSKÅP*/
   
   OPEN QUERY qmtrl3 FOR EACH BERSKAP WHERE BERSKAP.LEVKOD = leverant 
   AND BERSKAP.ENR NE "" NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl3 EXCLUSIVE-LOCK.
      IF AVAILABLE BERSKAP THEN DO:                                
         ASSIGN         
         BERSKAP.PRIS = 0.                                  
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl3 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE BERSKAP THEN LEAVE.
         ELSE DO:                                 
            ASSIGN               
            BERSKAP.PRIS = 0.
         END.
      END.
   END.
   CLOSE QUERY qmtrl3.
   
   /*KABELSKYDD OCH -RÖR*/
   
   OPEN QUERY qmtrl4 FOR EACH KSKYDD WHERE KSKYDD.LEVKOD = leverant 
   AND KSKYDD.BERED = FALSE NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl4 EXCLUSIVE-LOCK.
      IF AVAILABLE KSKYDD THEN DO:                              
         ASSIGN         
         KSKYDD.PRIS = 0.                        
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl4 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE KSKYDD THEN LEAVE.
         ELSE DO:                                     
            ASSIGN               
            KSKYDD.PRIS = 0.
         END.
      END.
   END.
   CLOSE QUERY qmtrl4.
   
   /*MATERIELSATSER*/
   
   OPEN QUERY qmtrl5 FOR EACH SATS WHERE SATS.LEVKOD = leverant NO-LOCK.
   DO TRANSACTION:
      GET FIRST qmtrl5 EXCLUSIVE-LOCK.
      IF AVAILABLE SATS THEN DO:
         IF SATS.SATS = TRUE THEN DO:                                    
            ASSIGN         
            SATS.PRIS = 0.                                  
         END.
         ELSE DO:
            ASSIGN         
            SATS.PRIS2 = 0.
         END.   
      END.   
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl5 EXCLUSIVE-LOCK.
         IF NOT AVAILABLE SATS THEN LEAVE.
         ELSE DO:
            IF SATS.SATS = TRUE THEN DO:                                     
               ASSIGN         
               SATS.PRIS = 0.                                                  
            END.
            ELSE DO:                               
               ASSIGN         
               SATS.PRIS2 = 0.
            END.
         END.
      END.
   END.
   CLOSE QUERY qmtrl5.
     
     
