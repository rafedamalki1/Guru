/*TIDTSEK.I*/     
xhop = "TIDT". 
IF tidtlogvar = FALSE THEN DO:
   tidtlogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tidtsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidtsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidtsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidtsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidtsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidtsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidtsekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidtsekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidtsekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidtsekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidtsekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidtsekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidtsekvar[13] = XSEK.SEK[13].
END.
IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN ENABLE BTN_REG WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_Registrera_res:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidtsekvar[2] = TRUE THEN ENABLE BTN_TBRES WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Radera:SENSITIVE IN MENU m_Funktioner2 = FALSE.           
IF Guru.Konstanter:tidtsekvar[3] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:tidtsekvar[4] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndra_tidregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE. 
IF Guru.Konstanter:tidtsekvar[5] = TRUE THEN ENABLE BTN_ARES WITH FRAME {&FRAME-NAME}.             
   
