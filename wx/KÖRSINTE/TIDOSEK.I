/*TIDOSEK.I*/     
xhop = "TIDO".    
IF tidologvar = FALSE THEN DO:
   tidologvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tidosekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidosekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidosekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidosekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidosekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidosekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidosekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidosekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidosekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidosekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidosekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidosekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidosekvar[13] = XSEK.SEK[13].
END. 
IF Guru.Konstanter:tidosekvar[1] = TRUE THEN ENABLE BTN_REG WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_Registrera_otid:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidosekvar[2] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:tidosekvar[3] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndra_tidregistreringar:SENSITIVE IN MENU m_Funktioner2 = FALSE.                  
