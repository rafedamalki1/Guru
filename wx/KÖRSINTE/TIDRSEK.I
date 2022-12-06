/*TIDRSEK.I*/     
xhop = "TIDR".            
IF tidrlogvar = FALSE THEN DO:
   tidrlogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tidrsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidrsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidrsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidrsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidrsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidrsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidrsekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidrsekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidrsekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidrsekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidrsekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidrsekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidrsekvar[13] = XSEK.SEK[13].
END. 
IF Guru.Konstanter:tidrsekvar[1] = TRUE THEN ENABLE BTN_REG WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_Registrera_tid:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidrsekvar[2] = TRUE THEN ENABLE BTN_VISA WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_senaste_tidregistreringen:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:tidrsekvar[3] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.
IF Guru.Konstanter:tidrsekvar[4] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndra_tidregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.          
