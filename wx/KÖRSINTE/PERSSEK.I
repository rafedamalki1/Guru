/*PERSSEK.I*/     
xhop = "PERS".
IF perlogvar = FALSE THEN DO:
   perlogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:persekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:persekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:persekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:persekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:persekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:persekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:persekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:persekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:persekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:persekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:persekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:persekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:persekvar[13] = XSEK.SEK[13].
END.
IF Guru.Konstanter:persekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_Ny:SENSITIVE IN MENU m_Updatera = FALSE.    
IF Guru.Konstanter:persekvar[2] = TRUE THEN ENABLE BTN_UPP WITH FRAME {&FRAME-NAME}.
ELSE MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = FALSE.    
IF Guru.Konstanter:persekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
ELSE MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = FALSE.   
IF Guru.Konstanter:persekvar[4] = TRUE THEN ENABLE BTN_VISAP WITH FRAME {&FRAME-NAME}.  
ELSE MENU-ITEM m_Visa_personal_register:SENSITIVE IN MENU m_Updatera = FALSE.   
IF Guru.Konstanter:persekvar[5] = TRUE THEN ENABLE BTN_DAGS WITH FRAME {&FRAME-NAME}.
ELSE MENU-ITEM m_Dags:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:persekvar[6] = TRUE THEN ENABLE BTN_VECS WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_Veckos:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:persekvar[7] = TRUE THEN ENABLE BTN_BYTV WITH FRAME {&FRAME-NAME}.              
ELSE MENU-ITEM m_bytevsch:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:persekvar[8] = TRUE THEN ENABLE BTN_ENHARB WITH FRAME {&FRAME-NAME}.              
ELSE MENU-ITEM m_arbenh:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:persekvar[9] = TRUE THEN ENABLE BTN_ALLARB WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_arballa:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:persekvar[10] = TRUE THEN ENABLE BTN_DEBPR WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_prisdeb:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:persekvar[11] = TRUE THEN ENABLE BTN_MAXOM WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_Justera_ndringsintervall:SENSITIVE IN MENU m_Funktioner2 = FALSE.            
     
