/*AVVBEFW.I    {AVVBEFW.I}*/
   
   IF Guru.Konstanter:varforetypval[3] >= 1 OR Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      IF Guru.Konstanter:varforetypval[3] = 4 THEN DO:
         ASSIGN
         FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FILL-IN_VIBEFATTNING:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         CMB_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      ELSE DO:
         CMB_BEF:LABEL = "Avvik. bef.". 
         FILL-IN-VALFA = "Välj befattning".
         OPEN QUERY bbfq FOR EACH befvaltemp NO-LOCK BY befvaltemp.VIBEFATTNING.
         GET FIRST bbfq NO-LOCK.
         IF AVAILABLE befvaltemp THEN DO:       
            status-ok = CMB_BEF:ADD-LAST("Återställ bef").
            status-ok = CMB_BEF:ADD-LAST(befvaltemp.VIBEFATTNING).      
            ASSIGN CMB_BEF:SCREEN-VALUE = befvaltemp.VIBEFATTNING.       
         END.   
         DO WHILE AVAILABLE(befvaltemp):     
            GET NEXT bbfq NO-LOCK.
            IF AVAILABLE befvaltemp THEN DO:        
               status-ok = CMB_BEF:ADD-LAST(befvaltemp.VIBEFATTNING).         
            END. 
         END.
         IF AVAILABLE extratidallt THEN DO:
            IF extratidallt.OVERTIDTILL NE "" THEN DO:
               FIND FIRST befvaltemp WHERE befvaltemp.BEFATTNING = extratidallt.OVERTIDTILL
               NO-LOCK NO-ERROR.  
               IF AVAILABLE befvaltemp THEN DO:
                  ASSIGN CMB_BEF:SCREEN-VALUE = befvaltemp.VIBEFATTNING.
                  CMB_BEF = INPUT CMB_BEF.   
                  FILL-IN_VIBEFATTNING = CMB_BEF.
               END.
            END.
            ELSE DO:
               IF AVAILABLE personaltemp THEN DO:
                  FIND FIRST befvaltemp WHERE befvaltemp.BEFATTNING = personaltemp.BEFATTNING
                  NO-LOCK NO-ERROR.  
                  IF AVAILABLE befvaltemp THEN DO:
                     ASSIGN CMB_BEF:SCREEN-VALUE = befvaltemp.VIBEFATTNING.
                     CMB_BEF = INPUT CMB_BEF.   
                     FILL-IN_VIBEFATTNING = CMB_BEF.
                  END.
               END.
            END.
         END.   
         ELSE DO:
            FIND FIRST befvaltemp WHERE befvaltemp.BEFATTNING = personaltemp.BEFATTNING
            NO-LOCK NO-ERROR.  
            IF AVAILABLE befvaltemp THEN DO:
               ASSIGN CMB_BEF:SCREEN-VALUE = befvaltemp.VIBEFATTNING.
               CMB_BEF = INPUT CMB_BEF.   
               FILL-IN_VIBEFATTNING = CMB_BEF.
            END.
         END.
         ENABLE CMB_BEF WITH FRAME {&FRAME-NAME}.      
         ASSIGN
         FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FILL-IN_VIBEFATTNING:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         DISPLAY FILL-IN_VIBEFATTNING FILL-IN-VALFA FILL-IN_VIBEFATTNING 
         WITH FRAME {&FRAME-NAME}.       
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DISABLE CMB_PRISTYP WITH FRAME {&FRAME-NAME}.       
      END.
   END.        
   ELSE DO:
      ASSIGN
      FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_VIBEFATTNING:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      CMB_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      ASSIGN
      FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_VIBEFATTNING:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      CMB_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   

