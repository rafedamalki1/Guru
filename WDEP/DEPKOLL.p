     
DEFINE VARIABLE raknare AS INTEGER NO-UNDO. 

DEFIN QUERY qmtrl FOR MTRLDEP.

{TIDUTTT.I}

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
DEFINE INPUT PARAMETER vald_depa AS INTEGER.

DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tidut.  
  
  
   raknare = 0.     
   FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = leverant NO-LOCK NO-ERROR.                
   FIND FIRST FORETAG NO-LOCK NO-ERROR.
   Guru.Konstanter:globforetag = FORETAG.FORETAG.
   /*IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
      IF leverant = "1" THEN DO:
         OPEN QUERY qmtrl FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.IBDATUM = ? 
         AND (MTRLDEP.LEVKOD = leverant OR MTRLDEP.LEVKOD = "11") NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY qmtrl FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.IBDATUM = ? 
         AND MTRLDEP.LEVKOD = leverant NO-LOCK.
      END.
   END.
   ELSE DO:
      OPEN QUERY qmtrl FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.IBDATUM = ? 
      AND MTRLDEP.LEVKOD = leverant NO-LOCK.  
   END.*/
   OPEN QUERY qmtrl FOR EACH MTRLDEP WHERE MTRLDEP.DEPNR = vald_depa AND MTRLDEP.IBDATUM = ? 
   AND MTRLDEP.LEVKOD = leverant NO-LOCK. 
   DO TRANSACTION:
      GET FIRST qmtrl EXCLUSIVE-LOCK.
      IF AVAILABLE MTRLDEP THEN DO:
         FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
         MTRL.KALKNR = 0 AND MTRL.ENR = MTRLDEP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MTRL THEN DO:                               
            CREATE tidut.      
            ASSIGN         
            SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
            SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
            SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
            SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
            raknare = raknare + 1.
            /*IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
               IF MTRLDEP.LEVKOD = "1" THEN DO:
                  FIND FIRST MTRL WHERE MTRL.LEVKOD = "11" AND 
                  MTRL.KALKNR = 0 AND MTRL.ENR = MTRLDEP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
                  IF AVAILABLE MTRL THEN DO:
                     ASSIGN
                     MTRLDEP.BENAMNING = MTRL.BENAMNING
                     MTRLDEP.NPRIS = MTRL.NPRIS
                     MTRLDEP.BPRIS = MTRL.BPRIS
                     MTRLDEP.ENHET = MTRL.ENHET.
                  END.
                  ELSE DO:
                     CREATE tidut.      
                     ASSIGN         
                     SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
                     SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
                     SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
                     SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
                     raknare = raknare + 1.
                  END.
               END.
               ELSE DO:
                  CREATE tidut.      
                  ASSIGN         
                  SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
                  SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
                  SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
                  SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
                  raknare = raknare + 1.
               END.
            END.
            ELSE DO:
               CREATE tidut.      
               ASSIGN         
               SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
               SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
               SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
               SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
               raknare = raknare + 1.
            END.            
            */
         END.
         ELSE DO:                      
            ASSIGN
            MTRLDEP.BENAMNING = MTRL.BENAMNING
            MTRLDEP.NPRIS = MTRL.NPRIS
            MTRLDEP.BPRIS = MTRL.BPRIS
            MTRLDEP.ENHET = MTRL.ENHET.
         END.
      END.
   END.
   REPEAT:
      DO TRANSACTION:
         GET NEXT qmtrl EXCLUSIVE-LOCK.
         IF NOT AVAILABLE MTRLDEP THEN LEAVE.
         ELSE DO:
            FIND FIRST MTRL WHERE MTRL.LEVKOD = leverant AND 
            MTRL.KALKNR = 0 AND MTRL.ENR = MTRLDEP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
            IF NOT AVAILABLE MTRL THEN DO:                               
               CREATE tidut.      
               ASSIGN         
               SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
               SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
               SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
               SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
               raknare = raknare + 1.
               /*
               IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
                  IF MTRLDEP.LEVKOD = "1" THEN DO:
                     FIND FIRST MTRL WHERE MTRL.LEVKOD = "11" AND 
                     MTRL.KALKNR = 0 AND MTRL.ENR = MTRLDEP.ENR USE-INDEX LEV NO-LOCK NO-ERROR.
                     IF AVAILABLE MTRL THEN DO:
                        ASSIGN
                        MTRLDEP.BENAMNING = MTRL.BENAMNING
                        MTRLDEP.NPRIS = MTRL.NPRIS
                        MTRLDEP.BPRIS = MTRL.BPRIS
                        MTRLDEP.ENHET = MTRL.ENHET.
                     END.
                     ELSE DO:
                        CREATE tidut.      
                        ASSIGN         
                        SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
                        SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
                        SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
                        SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
                        raknare = raknare + 1.
                     END.
                  END.
                  ELSE DO:
                     CREATE tidut.      
                     ASSIGN         
                     SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
                     SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
                     SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
                     SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
                     raknare = raknare + 1.
                  END.
               END.
               ELSE DO:
                  CREATE tidut.      
                  ASSIGN         
                  SUBSTRING(tidut.UT,1) = MTRLDEP.ENR     
                  SUBSTRING(tidut.UT,13) = MTRLDEP.BENAMNING  
                  SUBSTRING(tidut.UT,54) = MTRLDEP.FACKID                          
                  SUBSTRING(tidut.UT,63) = MTRLDEP.ENHET
                  raknare = raknare + 1.
               END.            
               */
            END.
            ELSE DO:                      
               ASSIGN
               MTRLDEP.BENAMNING = MTRL.BENAMNING
               MTRLDEP.NPRIS = MTRL.NPRIS
               MTRLDEP.BPRIS = MTRL.BPRIS
               MTRLDEP.ENHET = MTRL.ENHET.
            END.
         END.
      END.
   END.         
   IF raknare = 0 THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,1) = "ALLA ARTIKLAR FÖR LEVERANTÖR " + LEVERANTOR.LEVNAMN + " UPPLAGDA I DEPÅN FINNS I KATALOGEN".
   END.
   CLOSE QUERY qmtrl.
