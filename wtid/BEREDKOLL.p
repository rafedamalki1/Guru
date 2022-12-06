/*BEREDKOLL.P*/
DEFINE INPUT PARAMETER pkod  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER FILL-IN-START AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER FILL-IN-SLUT AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER bdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER avdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER bertyp AS INTEGER NO-UNDO. 
DEFINE OUTPUT PARAMETER musz AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER utdatum AS CHARACTER NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
RUN beredkoll_UI.
PROCEDURE beredkoll_UI :
   IF ( FORETAG.FORETAG = "SUND" OR FORETAG.FORETAG = "SNAT" ) AND PERSONALTAB.BEREDSKAPSAVTAL = "K" THEN DO:
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = bdatum AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK.
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF TIDREGITAB.BEREDSKAP = ' ' THEN musz = musz.
         ELSE IF bertyp = 1 AND TIDREGITAB.BEREDSKAP NE "4444" THEN musz = musz.
         ELSE IF bertyp = 2 AND TIDREGITAB.BEREDSKAP = "4444" THEN musz = musz.
         ELSE DO:                         
            IF TIDREGITAB.BEREDSKAPSTART >= FILL-IN-START THEN DO:   
               RUN redanmed_UI.
               musz = TRUE.
               RETURN.   
            END.   
            ELSE IF TIDREGITAB.BEREDSKAPSLUT > FILL-IN-START AND
            TIDREGITAB.BEREDSKAPSTART < FILL-IN-START THEN DO:   
               RUN redanmed_UI.
               musz = TRUE.
               RETURN.   
            END. 
         END.            
         GET NEXT tidq NO-LOCK.
      END.  
      CLOSE QUERY tidq.
      regdatumspar = bdatum + 1.
      REPEAT:
         IF regdatumspar >= avdatum THEN LEAVE.
         FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatumspar AND TIDREGITAB.TIDLOG = FALSE     
         USE-INDEX PKOD NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:        
            IF TIDREGITAB.BEREDSKAP = ' ' THEN NEXT. 
            ELSE IF bertyp = 1 AND TIDREGITAB.BEREDSKAP NE "4444" THEN DO:
               NEXT.
            END.
            ELSE IF bertyp = 2 AND TIDREGITAB.BEREDSKAP = "4444" THEN DO:
               NEXT.
            END.
            RUN redanmed_UI.       
            musz = TRUE.
            RETURN.                 
         END.
         regdatumspar = regdatumspar + 1.                  
         IF regdatumspar = avdatum THEN LEAVE.
      END. 
      OPEN QUERY tidq 
      FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = avdatum AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK.
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF TIDREGITAB.BEREDSKAP = ' ' THEN musz = musz.
         ELSE IF bertyp = 1 AND TIDREGITAB.BEREDSKAP = "4444" THEN musz = musz.
         ELSE IF bertyp = 2 AND TIDREGITAB.BEREDSKAP NE "4444" THEN musz = musz.
         ELSE DO:            
            IF FILL-IN-SLUT > TIDREGITAB.BEREDSKAPSTART THEN DO: 
               RUN redanmed_UI.      
               musz = TRUE.
               RETURN.   
            END. 
         END.      
         GET NEXT tidq NO-LOCK.
      END.  
      CLOSE QUERY tidq.        
   END.
   ELSE DO:   
      OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      TIDREGITAB.DATUM = bdatum AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK.
      GET FIRST tidq NO-LOCK.
      DO WHILE AVAILABLE(TIDREGITAB):
         IF TIDREGITAB.BEREDSKAP = ' ' THEN musz = musz.
         ELSE DO:                
            IF TIDREGITAB.BEREDSKAPSTART >= FILL-IN-START THEN DO:   
               RUN redanmed_UI.
               musz = TRUE.
               RETURN.   
            END.   
            ELSE IF TIDREGITAB.BEREDSKAPSLUT > FILL-IN-START AND
            TIDREGITAB.BEREDSKAPSTART < FILL-IN-START THEN DO:   
               RUN redanmed_UI.
               musz = TRUE.
               RETURN.   
            END. 
         END.            
         GET NEXT tidq NO-LOCK.
      END.  
      CLOSE QUERY tidq.
      regdatumspar = bdatum + 1.
      REPEAT:
         IF regdatumspar >= avdatum THEN LEAVE.
         FIND NEXT TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = regdatumspar AND TIDREGITAB.TIDLOG = FALSE     
         USE-INDEX PKOD NO-LOCK NO-ERROR.
         IF AVAILABLE TIDREGITAB THEN DO:
            IF TIDREGITAB.BEREDSKAP = ' ' THEN NEXT. 
            RUN redanmed_UI.       
            musz = TRUE.
            RETURN.                 
         END.
         regdatumspar = regdatumspar + 1.                  
         IF regdatumspar = avdatum THEN LEAVE.
      END.
      IF avdatum > bdatum THEN DO: 
         OPEN QUERY tidq 
         FOR EACH TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
         TIDREGITAB.DATUM = avdatum AND TIDREGITAB.TIDLOG = FALSE USE-INDEX PKOD NO-LOCK.
         GET FIRST tidq NO-LOCK.
         DO WHILE AVAILABLE(TIDREGITAB):
            IF TIDREGITAB.BEREDSKAP = ' ' THEN musz = musz.
            ELSE DO:
               IF FILL-IN-SLUT > TIDREGITAB.BEREDSKAPSTART THEN DO: 
                  RUN redanmed_UI.      
                  musz = TRUE.
                  RETURN.   
               END. 
            END.      
            GET NEXT tidq NO-LOCK.
         END.  
         CLOSE QUERY tidq.
      END.           
   END.
END PROCEDURE.
PROCEDURE redanmed_UI :
   musz = TRUE.
   utdatum = STRING(TIDREGITAB.DATUM) + " klockan " + STRING(TIDREGITAB.BEREDSKAPSTART,"99.99").      
END PROCEDURE.

