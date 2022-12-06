/*TIDSSEK.I*/     
xhop = "TIDS".      
IF tidslogvar = FALSE THEN DO:
   tidslogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tidssekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidssekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidssekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidssekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidssekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidssekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidssekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidssekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidssekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidssekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidssekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidssekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidssekvar[13] = XSEK.SEK[13].
END.
IF Guru.Konstanter:tidssekvar[1] = TRUE THEN ENABLE BTN_SKR WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_skriv:SENSITIVE IN MENU m_Funktioner2 = FALSE. 
IF Guru.Konstanter:tidssekvar[2] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_Visa_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:tidssekvar[3] = TRUE THEN ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndra_tidregistrering:SENSITIVE IN MENU m_Funktioner2 = FALSE.                  
     
