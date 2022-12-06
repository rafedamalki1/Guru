/*PLANSEK.I*/  
xhop = "AONR".
FIND FIRST sekbuff WHERE sekbuff.MENYVART = xhop AND
sekbuff.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.
IF AVAILABLE sekbuff THEN DO:
   IF sekbuff.SEK[1] = TRUE THEN ENABLE BTN_AONR WITH FRAME {&FRAME-NAME}.
END.
xhop = "PLAN".     
IF planlogvar = FALSE THEN DO:
   planlogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:plansekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:plansekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:plansekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:plansekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:plansekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:plansekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:plansekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:plansekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:plansekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:plansekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:plansekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:plansekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:plansekvar[13] = XSEK.SEK[13].
END.   
IF Guru.Konstanter:plansekvar[1] = TRUE THEN DO:
   ENABLE BTN_NY WITH FRAME {&FRAME-NAME}.
   MENU-ITEM m_Ny:SENSITIVE IN MENU m_Updatera = TRUE.
END.   
ELSE DO:
   MENU-ITEM m_Ny:SENSITIVE IN MENU m_Updatera = FALSE.
END.   
IF Guru.Konstanter:plansekvar[2] = TRUE THEN DO:
   ENABLE BTN_UPP WITH FRAME {&FRAME-NAME}.
   MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = TRUE.    
END.   
ELSE DO: 
   MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = FALSE. 
END.   
IF Guru.Konstanter:plansekvar[3] = TRUE THEN DO:
   ENABLE BTN_BORT WITH FRAME {&FRAME-NAME}.  
   MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = TRUE.
END.   
ELSE DO:
   MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = FALSE.    
END.   
IF Guru.Konstanter:plansekvar[4] = TRUE THEN DO:
   ENABLE BTN_VISAO WITH FRAME {&FRAME-NAME}.  
   MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = TRUE.   
END.   
ELSE DO:
   MENU-ITEM m_Visa:SENSITIVE IN MENU m_Updatera = FALSE.     
END.   
IF Guru.Konstanter:plansekvar[5] = TRUE THEN DO:
   ENABLE BTN_UNDER WITH FRAME {&FRAME-NAME}.
   ENABLE BTN_BUNDER WITH FRAME {&FRAME-NAME}.
   MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = TRUE.
END.   
ELSE DO:
   MENU-ITEM m_Uppdela:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
END.   
IF Guru.Konstanter:plansekvar[6] = TRUE THEN DO:
   ENABLE BTN_BUD WITH FRAME {&FRAME-NAME}. 
   MENU-ITEM m_Budgetera:SENSITIVE IN MENU m_Funktioner2 = TRUE.
END.   
ELSE DO:
   MENU-ITEM m_Budgetera:SENSITIVE IN MENU m_Funktioner2 = FALSE.   
END.   
IF Guru.Konstanter:plansekvar[7] = TRUE THEN DO:
   ENABLE BTN_KALK WITH FRAME {&FRAME-NAME}. 
   MENU-ITEM m_Kalkylera:SENSITIVE IN MENU m_Funktioner2 = TRUE.
END.   
ELSE DO:
   MENU-ITEM m_Kalkylera:SENSITIVE IN MENU m_Funktioner2 = FALSE.                 
END.   
IF Guru.Konstanter:plansekvar[8] = TRUE THEN DO:
   ENABLE BTN_KOPP WITH FRAME {&FRAME-NAME}. 
   MENU-ITEM m_Koppla:SENSITIVE IN MENU m_Funktioner2 = TRUE.             
END.   
ELSE DO:
   MENU-ITEM m_Koppla:SENSITIVE IN MENU m_Funktioner2 = FALSE.        
END.   
IF Guru.Konstanter:plansekvar[9] = TRUE THEN DO:
   ENABLE BTN_BORTKOPP WITH FRAME {&FRAME-NAME}. 
   MENU-ITEM m_Bort_Kopp:SENSITIVE IN MENU m_Funktioner2 = TRUE.          
END.      
ELSE DO:
   MENU-ITEM m_Bort_Kopp:SENSITIVE IN MENU m_Funktioner2 = FALSE.     
END.   
IF Guru.Konstanter:plansekvar[10] = TRUE THEN DO:
   ENABLE BTN_RAPP WITH FRAME {&FRAME-NAME}.
   MENU-ITEM m_Rapporter:SENSITIVE IN MENU m_Funktioner2 = TRUE.              
END.   
ELSE DO:
   MENU-ITEM m_Rapporter:SENSITIVE IN MENU m_Funktioner2 = FALSE.    
END.   
IF Guru.Konstanter:plansekvar[11] = TRUE THEN DO:
   ENABLE BTN_AVSAONR WITH FRAME {&FRAME-NAME}. 
   MENU-ITEM m_Avsluta_Plannr:SENSITIVE IN MENU m_Funktioner2 = TRUE.          
END.   
ELSE DO:
   MENU-ITEM m_Avsluta_Plannr:SENSITIVE IN MENU m_Funktioner2 = FALSE.
END.   
IF Guru.Konstanter:plansekvar[12] = TRUE THEN DO:
   ENABLE BTN_VISAV WITH FRAME {&FRAME-NAME}.        
   MENU-ITEM m_Visa_avslutade:SENSITIVE IN MENU m_Funktioner2 = TRUE.     
END.   
ELSE DO:
   MENU-ITEM m_Visa_avslutade:SENSITIVE IN MENU m_Funktioner2 = FALSE.
END.   
  
