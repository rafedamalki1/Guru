   /*VIEWID1.I IDENTITET*/
   /*FOREID*/
   ASSIGN
   FILL-IN-SUND:HIDDEN = TRUE 
   FILL-IN-A:HIDDEN = TRUE 
   FILL-IN-B:HIDDEN = TRUE 
   FILL-IN-C:HIDDEN = TRUE 
   TOG_A:HIDDEN = TRUE 
   TOG_B:HIDDEN = TRUE 
   TOG_C:HIDDEN = TRUE 
   BRW_TAB1:HIDDEN = TRUE 
   BRW_TAB2:HIDDEN = TRUE 
   BRW_TAB3:HIDDEN = TRUE 
   FILL-IN-TAB1:HIDDEN = TRUE 
   FILL-IN-TAB2:HIDDEN = TRUE 
   FILL-IN-TAB3:HIDDEN = TRUE 
   FILL-IN-ID1:HIDDEN = TRUE 
   FILL-IN-ID2:HIDDEN = TRUE 
   FILL-IN-NR1:HIDDEN = TRUE 
   FILL-IN-NR2:HIDDEN = TRUE 
   FILL-IN-NR3:HIDDEN = TRUE 
   BTN_ANL    :HIDDEN = TRUE.
 
   /*FOREID*/
   
   IF Guru.Konstanter:varforetypval[32] = 1 THEN DO:
      ASSIGN
      FILL-IN-ID2:HIDDEN = FALSE
      FILL-IN-IDNR2:HIDDEN = FALSE
      FILL-IN-SUND:HIDDEN = FALSE
      BTN_KOPPID:HIDDEN = FALSE
      FILL-IN-A:HIDDEN = FALSE
      FILL-IN-B:HIDDEN = FALSE
      FILL-IN-C:HIDDEN = FALSE
      TOG_A:HIDDEN = FALSE
      TOG_B:HIDDEN = FALSE
      TOG_C:HIDDEN = FALSE.        
   END.
   ELSE IF Guru.Konstanter:varforetypval[32] = 2 THEN DO:
      ASSIGN
      FILL-IN-ID2:HIDDEN = FALSE
      FILL-IN-IDNR2:HIDDEN = FALSE
      FILL-IN-SUND:HIDDEN = FALSE
      BTN_KOPPID:HIDDEN = FALSE.
   END.
   ELSE IF Guru.Konstanter:varforetypval[32] = 4 THEN DO:
      ASSIGN
      FILL-IN-TAB1:HIDDEN = FALSE
      FILL-IN-TAB2:HIDDEN = FALSE
      FILL-IN-TAB3:HIDDEN = FALSE
      FILL-IN-NR1:HIDDEN = FALSE
      FILL-IN-NR2:HIDDEN = FALSE
      FILL-IN-NR3:HIDDEN = FALSE.
      
      ASSIGN
      FILL-IN-ID2:HIDDEN = FALSE
      FILL-IN-IDNR2:HIDDEN = FALSE
      FILL-IN-SUND:HIDDEN = FALSE
      BTN_KOPPID:HIDDEN = FALSE.
   END.
   ELSE DO:
      ASSIGN
      BRW_TAB1:HIDDEN = FALSE
      BRW_TAB2:HIDDEN = FALSE
      BRW_TAB3:HIDDEN = FALSE
      FILL-IN-TAB1:HIDDEN = FALSE
      FILL-IN-TAB2:HIDDEN = FALSE
      FILL-IN-TAB3:HIDDEN = FALSE
      FILL-IN-ID1:HIDDEN = FALSE
      FILL-IN-ID2:HIDDEN = FALSE
      FILL-IN-NR1:HIDDEN = FALSE
      FILL-IN-NR2:HIDDEN = FALSE
      FILL-IN-NR3:HIDDEN = FALSE
      FILL-IN-IDNR1:HIDDEN = FALSE
      FILL-IN-IDNR2:HIDDEN = FALSE      
      BTN_KOPPID:HIDDEN = FALSE
      FILL-IN-A:HIDDEN = FALSE
      FILL-IN-B:HIDDEN = FALSE
      FILL-IN-C:HIDDEN = FALSE
      TOG_A:HIDDEN = FALSE
      TOG_B:HIDDEN = FALSE
      TOG_C:HIDDEN = FALSE
      BTN_ANL:HIDDEN = FALSE
      FILL-IN-SUND:HIDDEN = FALSE.
   END.      
