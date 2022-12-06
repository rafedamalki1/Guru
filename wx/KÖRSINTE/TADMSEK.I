/*TADMSEK.I  körs inte*/     
xhop = "TADM".
IF tadmlogvar = FALSE THEN DO:
   tadmlogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:tadmsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:tadmsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:tadmsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:tadmsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:tadmsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:tadmsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:tadmsekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:tadmsekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:tadmsekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:tadmsekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:tadmsekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:tadmsekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:tadmsekvar[13] = XSEK.SEK[13].
END.   
IF Guru.Konstanter:tadmsekvar[1] = TRUE THEN ENABLE BTN_KONT WITH FRAME {&FRAME-NAME}. 
/*ELSE MENU-ITEM m_Kontroll_registrerad_tid:SENSITIVE IN MENU m_Funktioner2 = FALSE.*/
IF Guru.Konstanter:tadmsekvar[2] = TRUE THEN ENABLE BTN_GRAN WITH FRAME {&FRAME-NAME}.
/*ELSE MENU-ITEM m_Granska_tillgg:SENSITIVE IN MENU m_Funktioner2 = FALSE.     */
IF Guru.Konstanter:tadmsekvar[3] = TRUE THEN ENABLE BTN_GOD WITH FRAME {&FRAME-NAME}.              
/*ELSE MENU-ITEM m_Godknna_tidsedlar:SENSITIVE IN MENU m_Funktioner2 = FALSE.     */
IF Guru.Konstanter:tadmsekvar[4] = TRUE THEN ENABLE BTN_ANDGOD WITH FRAME {&FRAME-NAME}.              
/*ELSE MENU-ITEM m_ndra_godknd_tidsedel:SENSITIVE IN MENU m_Funktioner2 = FALSE.     */
IF Guru.Konstanter:tadmsekvar[5] = TRUE THEN ENABLE BTN_LGOD WITH FRAME {&FRAME-NAME}.           
/*ELSE MENU-ITEM m_Lista_tidsedlar:SENSITIVE IN MENU m_Funktioner2 = FALSE.     */
IF Guru.Konstanter:tadmsekvar[6] = TRUE THEN ENABLE BTN_VECK WITH FRAME {&FRAME-NAME}.             
/*ELSE MENU-ITEM m_Veckokra_tidsedlar:SENSITIVE IN MENU m_Funktioner2 = FALSE.     */
IF Guru.Konstanter:tadmsekvar[7] = TRUE THEN ENABLE BTN_LON WITH FRAME {&FRAME-NAME}.             
/*ELSE MENU-ITEM m_Lneadministrea_tidsedlar:SENSITIVE IN MENU m_Funktioner2 = FALSE.    */
     
