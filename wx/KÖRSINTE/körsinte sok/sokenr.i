/*SOKENR.I SOKNING AV ENR*/

   ASSIGN
   sok = FALSE  
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   IF FILL-IN-ENR = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-ENR,"*",1).
   IF stjarnvar = 0 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR + "*"
      begvar = FALSE.
   END.
   ELSE DO:
      IF SUBSTRING(FILL-IN-ENR,1,1) = "*" THEN DO:
         stjarnvar = INDEX(FILL-IN-ENR,"*",2).
         IF stjarnvar = 0 THEN DO:
            ASSIGN
            posok = FILL-IN-ENR
            begvar = FALSE.
         END.
         ELSE DO:
            MESSAGE "Ni kan endast använda en '*' i början eller slutet av en sökning"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.  
      ELSE DO:
         soklangd = LENGTH(FILL-IN-ENR).
         IF SUBSTRING(FILL-IN-ENR,soklangd,1) = "*" THEN DO:
            stjarnvar = INDEX(FILL-IN-ENR,"*",1).
            IF stjarnvar = soklangd THEN DO:
               ASSIGN
               posok = SUBSTRING(FILL-IN-ENR,1,soklangd - 1)
               begvar = TRUE.
            END.
            ELSE DO:
               MESSAGE "Ni kan endast använda en '*' i början eller slutet av en sökning"
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
         END.
         ELSE DO:
            ASSIGN
            posok = "*" + FILL-IN-ENR + "*"
            begvar = FALSE.
         END.
      END.    
   END.
   IF val = FALSE THEN DO:
      IF AVAILABLE MTRL THEN mtrl_recid = RECID(MTRL).               
      status-ok = BRW_HLEV:DESELECT-ROWS().        
      FIND MTRL WHERE RECID(MTRL) = mtrl_recid NO-LOCK NO-ERROR.
      IF begvar = FALSE THEN
      FIND NEXT MTRL WHERE MTRL.ENR MATCHES posok AND MTRL.LEVKOD = vald_kundlev
      AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR. 
      ELSE
      FIND NEXT MTRL WHERE MTRL.ENR BEGINS posok AND MTRL.LEVKOD = vald_kundlev
      AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:  
         IF begvar = FALSE THEN    
         FIND FIRST MTRL WHERE MTRL.ENR MATCHES posok AND MTRL.LEVKOD = vald_kundlev
         AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.   
         ELSE
         FIND FIRST MTRL WHERE MTRL.ENR BEGINS posok AND MTRL.LEVKOD = vald_kundlev
         AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MTRL THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör. Vill du utföra sökning hos övriga leverantörer?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Sökning" UPDATE svar.         
            IF NOT svar THEN DO:        
               APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               ASSIGN 
               BTN_LEV:HIDDEN = FALSE
               BTN_LEV2:HIDDEN = TRUE 
               FILL-IN-BEN:HIDDEN = TRUE
               FILL-IN-ENR:HIDDEN = TRUE
               CMB_LEV:HIDDEN = TRUE
               BRW_VLEV:HIDDEN = TRUE                            
               BRW_HLEV:HIDDEN = TRUE
               BRW_LEV:HIDDEN = FALSE. 
               IF begvar = FALSE THEN                                                                
               OPEN QUERY sok FOR EACH MTRL WHERE MTRL.ENR MATCHES posok AND MTRL.LEVKOD NE
               vald_kundlev AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.                   
               ELSE 
               OPEN QUERY sok FOR EACH MTRL WHERE MTRL.ENR BEGINS posok AND MTRL.LEVKOD NE
               vald_kundlev AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.
               GET FIRST sok NO-LOCK.   
               DO WHILE AVAILABLE(MTRL):                  
                  CREATE sok_mtrl.   
                  ASSIGN
                  sok_mtrl.ENR = MTRL.ENR
                  sok_mtrl.BENAMNING = MTRL.BENAMNING
                  sok_mtrl.ENHET = MTRL.ENHET 
                  sok_mtrl.NPRIS = MTRL.NPRIS
                  sok_mtrl.BPRIS = MTRL.BPRIS
                  sok_mtrl.LEVKOD = MTRL.LEVKOD.                                           
                  GET NEXT sok NO-LOCK.                   
               END.               
               IF NOT AVAILABLE sok_mtrl THEN DO:         
                  MESSAGE "Det finns inget på sökbegreppet hos övriga leverantörer."
                  VIEW-AS ALERT-BOX. 
                  ASSIGN   
                  BTN_LEV:HIDDEN = TRUE 
                  BTN_LEV2:HIDDEN = TRUE
                  FILL-IN-BEN:HIDDEN = FALSE 
                  FILL-IN-ENR:HIDDEN = FALSE               
                  BRW_HLEV:HIDDEN = FALSE 
                  CMB_LEV:HIDDEN = FALSE
                  BRW_VLEV:HIDDEN = TRUE     
                  BRW_LEV:HIDDEN = TRUE.  
                  APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
                  RETURN NO-APPLY.   
               END.                
               IF AVAILABLE sok_mtrl THEN DO:  
                  sok = TRUE.             
                  OPEN QUERY BRW_LEV FOR EACH sok_mtrl NO-LOCK BY sok_mtrl.ENR BY sok_mtrl.LEV.
               END.                  
            END.
         END.
      END.              
      IF AVAILABLE MTRL AND sok = FALSE THEN DO:               
         RUN repo_UI (INPUT 1, INPUT RECID(MTRL)).
         status-ok = BRW_HLEV:SELECT-FOCUSED-ROW().
      END.
   END.
   ELSE DO:     
      IF AVAILABLE MTRL THEN mtrl_recid = RECID(MTRL).                  
      status-ok = BRW_VLEV:DESELECT-ROWS().      
      FIND MTRL WHERE RECID(MTRL) = mtrl_recid NO-LOCK NO-ERROR.
      IF begvar = FALSE THEN
      FIND NEXT MTRL WHERE MTRL.ENR MATCHES posok AND MTRL.LEVKOD = vald_lev
      AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR. 
      ELSE
      FIND NEXT MTRL WHERE MTRL.ENR BEGINS posok AND MTRL.LEVKOD = vald_lev
      AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MTRL THEN DO:
         IF begvar = FALSE THEN      
         FIND FIRST MTRL WHERE MTRL.ENR MATCHES posok AND MTRL.LEVKOD = vald_lev
         AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
         ELSE   
         FIND FIRST MTRL WHERE MTRL.ENR BEGINS posok AND MTRL.LEVKOD = vald_lev
         AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MTRL THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör. Vill du utföra sökning hos övriga leverantörer?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Sökning" UPDATE svar.         
            IF NOT svar THEN DO:  
               APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               ASSIGN 
               BTN_LEV:HIDDEN = TRUE 
               BTN_LEV2:HIDDEN = FALSE
               FILL-IN-BEN:HIDDEN = TRUE
               FILL-IN-ENR:HIDDEN = TRUE
               CMB_LEV:HIDDEN = TRUE
               BRW_VLEV:HIDDEN = TRUE                            
               BRW_HLEV:HIDDEN = TRUE
               BRW_LEV:HIDDEN = FALSE.
               IF begvar = FALSE THEN                                                            
               OPEN QUERY sok FOR EACH MTRL WHERE MTRL.ENR MATCHES posok AND MTRL.LEVKOD NE
               vald_lev AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.                   
               ELSE 
               OPEN QUERY sok FOR EACH MTRL WHERE MTRL.ENR BEGINS posok AND MTRL.LEVKOD NE
               vald_lev AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.
               GET FIRST sok NO-LOCK.   
               DO WHILE AVAILABLE(MTRL):                  
                  CREATE sok_mtrl.   
                  ASSIGN
                  sok_mtrl.ENR = MTRL.ENR
                  sok_mtrl.BENAMNING = MTRL.BENAMNING
                  sok_mtrl.ENHET = MTRL.ENHET 
                  sok_mtrl.NPRIS = MTRL.NPRIS
                  sok_mtrl.BPRIS = MTRL.BPRIS
                  sok_mtrl.LEVKOD = MTRL.LEVKOD.                                           
                  GET NEXT sok NO-LOCK.                   
               END.               
               IF NOT AVAILABLE sok_mtrl THEN DO:         
                  MESSAGE "Det finns inget på sökbegreppet hos övriga leverantörer."
                  VIEW-AS ALERT-BOX. 
                  ASSIGN   
                  BTN_LEV:HIDDEN = TRUE
                  BTN_LEV2:HIDDEN = TRUE 
                  FILL-IN-BEN:HIDDEN = FALSE 
                  FILL-IN-ENR:HIDDEN = FALSE               
                  BRW_HLEV:HIDDEN = TRUE 
                  CMB_LEV:HIDDEN = FALSE
                  BRW_VLEV:HIDDEN = FALSE     
                  BRW_LEV:HIDDEN = TRUE.  
                  APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
                  RETURN NO-APPLY.   
               END.                
               IF AVAILABLE sok_mtrl THEN DO:  
                  sok = TRUE.             
                  OPEN QUERY BRW_LEV FOR EACH sok_mtrl NO-LOCK BY sok_mtrl.ENR BY sok_mtrl.LEV.
               END.                                        
            END.
         END.
      END.              
      IF AVAILABLE MTRL AND sok = FALSE THEN DO:               
         RUN repo_UI (INPUT 2, INPUT RECID(MTRL)).
         status-ok = BRW_VLEV:SELECT-FOCUSED-ROW().
      END.
   END.
