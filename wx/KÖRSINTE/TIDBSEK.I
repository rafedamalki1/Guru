/*TIDBSEK.I*/     
xhop = "TIDB".          
IF tidblogvar = FALSE THEN DO:
   tidblogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tidbsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidbsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidbsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidbsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidbsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidbsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidbsekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidbsekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidbsekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidbsekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidbsekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidbsekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidbsekvar[13] = XSEK.SEK[13].
END.
IF Guru.Konstanter:tidbsekvar[1] = TRUE THEN ENABLE BTN_REG WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_Registrera_tid:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
IF Guru.Konstanter:tidbsekvar[2] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
IF Guru.Konstanter:tidbsekvar[3] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndra_tidregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.                 
