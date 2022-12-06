/*SOKSATS.I SOKNING AV SATS*/

   ASSIGN
   sok = FALSE  
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   IF FILL-IN-KOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-KOD,"*",1).
   IF stjarnvar = 0 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD + "*"
      begvar = FALSE.
   END.
   ELSE DO:
      IF SUBSTRING(FILL-IN-KOD,1,1) = "*" THEN DO:
         stjarnvar = INDEX(FILL-IN-KOD,"*",2).
         IF stjarnvar = 0 THEN DO:
            ASSIGN
            posok = FILL-IN-KOD
            begvar = FALSE.
         END.
         ELSE DO:
            MESSAGE "Ni kan endast använda en '*' i början eller slutet av en sökning"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.  
      ELSE DO:
         soklangd = LENGTH(FILL-IN-KOD).
         IF SUBSTRING(FILL-IN-KOD,soklangd,1) = "*" THEN DO:
            stjarnvar = INDEX(FILL-IN-KOD,"*",1).
            IF stjarnvar = soklangd THEN DO:
               ASSIGN
               posok = SUBSTRING(FILL-IN-KOD,1,soklangd - 1)
               begvar = TRUE.
            END.
            ELSE DO:
               MESSAGE "Ni kan endast använda en '*' i början eller slutet av en sökning"
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
         END.
         ELSE DO:
            ASSIGN
            posok = "*" + FILL-IN-KOD + "*"
            begvar = FALSE.
         END.
      END.    
   END.
  
   IF AVAILABLE SATS THEN mtrl_recid = RECID(SATS).
   status-ok = BRW_SATS:DESELECT-ROWS().   
   FIND SATS WHERE RECID(SATS) = mtrl_recid NO-LOCK NO-ERROR.
   IF begvar = FALSE THEN
   FIND NEXT SATS WHERE SATS.KOD MATCHES posok AND SATS.LEVKOD = vald_lev
   AND SATS.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR. 
   ELSE
   FIND NEXT SATS WHERE SATS.KOD BEGINS posok AND SATS.LEVKOD = vald_lev
   AND SATS.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SATS THEN DO:
      IF begvar = FALSE THEN
      FIND FIRST SATS WHERE SATS.KOD MATCHES posok AND SATS.LEVKOD = vald_lev
      AND SATS.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR. 
      ELSE
      FIND FIRST SATS WHERE SATS.KOD BEGINS posok AND SATS.LEVKOD = vald_lev
      AND SATS.SATS = TRUE USE-INDEX KOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SATS THEN DO:
         MESSAGE "Det finns ingen på sökbegreppet."
         VIEW-AS ALERT-BOX
         TITLE "Sökning".                       
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.                          
      END.
   END.              
   IF AVAILABLE SATS THEN DO:                     
      RUN repo_UI (INPUT 4, INPUT RECID(SATS)).
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW().
   END.
