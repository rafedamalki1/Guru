/*SOKBEN2V.I SOKNING AV BENAMNING VALD LEVERANTÖR*/

 
   IF AVAILABLE mtrltemp THEN mtrl_rowid = ROWID(mtrltemp).        
   /*status-ok = BRW_VLEV:DESELECT-ROWS().       */
   FIND mtrltemp WHERE ROWID(mtrltemp) = mtrl_rowid NO-LOCK NO-ERROR.
   IF begvar = FALSE THEN
   FIND NEXT mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND mtrltemp.LEVKOD = vald_lev
   AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR.
   ELSE
   FIND NEXT mtrltemp WHERE mtrltemp.BENAMNING BEGINS aosok AND mtrltemp.LEVKOD = vald_lev
   AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE mtrltemp THEN DO:      
      IF begvar = FALSE THEN
      FIND FIRST mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND mtrltemp.LEVKOD = vald_lev
      AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR.
      ELSE
      FIND FIRST mtrltemp WHERE mtrltemp.BENAMNING BEGINS aosok AND mtrltemp.LEVKOD = vald_lev
      AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE mtrltemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet hos vald leverantör. Vill du utföra sökning hos övriga leverantörer?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Sökning" UPDATE svar.         
         IF NOT svar THEN DO:       
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END. 
         ELSE DO:
            ASSIGN 
            BTN_LEV:HIDDEN = TRUE
            BTN_LEV2:HIDDEN = FALSE
            FILL-IN-BEN:HIDDEN = TRUE 
            FILL-IN-ENR:HIDDEN = TRUE 
            CMB_LEV:HIDDEN = TRUE
           /* BRW_VLEV:HIDDEN = TRUE         
            BRW_HLEV:HIDDEN = TRUE*/
            BRW_LEV:HIDDEN = FALSE.              
            RUN sokhmt_UI IN mtrlbapph (INPUT begvar, INPUT aosok,INPUT vald_kundlev, OUTPUT TABLE sok_mtrl).                   
            /*IF begvar = FALSE THEN                                               
            OPEN QUERY sok FOR EACH mtrltemp WHERE mtrltemp.BENAMNING MATCHES aosok AND 
            mtrltemp.LEVKOD NE vald_lev AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK.
            ELSE 
            OPEN QUERY sok FOR EACH mtrltemp WHERE mtrltemp.BENAMNING BEGINS aosok AND 
            mtrltemp.LEVKOD NE vald_lev AND mtrltemp.KALKNR = 0 USE-INDEX BEN NO-LOCK.                   
            GET FIRST sok NO-LOCK.   
            DO WHILE AVAILABLE(mtrltemp):                  
               CREATE sok_mtrl.   
               ASSIGN
               sok_mtrl.ENR = mtrltemp.ENR
               sok_mtrl.BENAMNING = mtrltemp.BENAMNING
               sok_mtrl.ENHET = mtrltemp.ENHET 
               sok_mtrl.NPRIS = mtrltemp.NPRIS
               sok_mtrl.BPRIS = mtrltemp.BPRIS
               sok_mtrl.LEVKOD = mtrltemp.LEVKOD.                                           
               GET NEXT sok NO-LOCK.                   
            END.               */
            FIND FIRST sok_mtrl NO-ERROR.
            IF NOT AVAILABLE sok_mtrl THEN DO:         
               MESSAGE "Det finns inget på sökbegreppet hos övriga leverantörer."
               VIEW-AS ALERT-BOX. 
               ASSIGN   
               BTN_LEV:HIDDEN = FALSE
               BTN_LEV2:HIDDEN = TRUE
               FILL-IN-BEN:HIDDEN = FALSE 
               FILL-IN-ENR:HIDDEN = FALSE               
               /*BRW_HLEV:HIDDEN = TRUE */
               CMB_LEV:HIDDEN = TRUE
               /*BRW_VLEV:HIDDEN = FALSE     */
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
   IF AVAILABLE mtrltemp AND sok = FALSE THEN DO:      
      /*RUN repo_UI (INPUT 2, INPUT RECID(mtrltemp)).
      status-ok = BRW_VLEV:SELECT-FOCUSED-ROW().*/
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(mtrltemp)).
      RUN lastselectdyn_UI IN brwproc[4].        
   END.
