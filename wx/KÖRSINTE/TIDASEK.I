/*TIDASEK.I*/     
xhop = "TIDA".  
IF tidalogvar = FALSE THEN DO:
   tidalogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tidasekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tidasekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tidasekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tidasekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tidasekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tidasekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tidasekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tidasekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tidasekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tidasekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tidasekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tidasekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tidasekvar[13] = XSEK.SEK[13].
END.  
IF Guru.Konstanter:tidasekvar[1] = TRUE THEN ENABLE BTN_TID WITH FRAME {&FRAME-NAME}. 
ELSE MENU-ITEM m_ndra_tidregistreringar:SENSITIVE IN MENU m_Funktioner2 = FALSE.    
IF Guru.Konstanter:tidasekvar[2] = TRUE THEN ENABLE BTN_LON WITH FRAME {&FRAME-NAME}.
ELSE MENU-ITEM m_ndra_Lnetillgg:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
IF Guru.Konstanter:tidasekvar[3] = TRUE THEN ENABLE BTN_BER WITH FRAME {&FRAME-NAME}.              
ELSE MENU-ITEM m_ndra_Beredskap:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
IF Guru.Konstanter:tidasekvar[4] = TRUE THEN ENABLE BTN_TRA WITH FRAME {&FRAME-NAME}.              
ELSE MENU-ITEM m_ndra_Traktamente:SENSITIVE IN MENU m_Funktioner2 = FALSE.      
IF Guru.Konstanter:tidasekvar[5] = TRUE THEN ENABLE BTN_OVER WITH FRAME {&FRAME-NAME}.           
ELSE MENU-ITEM m_ndra_vertidstillgg:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
IF Guru.Konstanter:tidasekvar[6] = TRUE THEN ENABLE BTN_VTID WITH FRAME {&FRAME-NAME}.             
ELSE MENU-ITEM m_ndra_Visatid:SENSITIVE IN MENU m_Funktioner2 = FALSE.         
   
