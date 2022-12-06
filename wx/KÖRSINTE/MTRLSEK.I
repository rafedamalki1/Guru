/*MTRLSEK.I*/     
xhop = "MTRL".
IF mtrllogvar = FALSE THEN DO:
   mtrllogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:mtrlsekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:mtrlsekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:mtrlsekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:mtrlsekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:mtrlsekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:mtrlsekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:mtrlsekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:mtrlsekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:mtrlsekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:mtrlsekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:mtrlsekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:mtrlsekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:mtrlsekvar[13] = XSEK.SEK[13].
END.        
IF Guru.Konstanter:mtrlsekvar[1] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("Materiel - Kalkyl").          
ELSE MENU-ITEM m_Mtrl:SENSITIVE IN MENU m_Funktioner = FALSE.       
IF Guru.Konstanter:mtrlsekvar[2] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("Materiel - Kund").                  
ELSE MENU-ITEM m_Kund:SENSITIVE IN MENU m_Funktioner = FALSE.       
IF Guru.Konstanter:mtrlsekvar[3] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("Materieladministration").                  
ELSE MENU-ITEM m_Materieladministration:SENSITIVE IN MENU m_Funktioner = FALSE.        
IF Guru.Konstanter:mtrlsekvar[4] = TRUE THEN status-ok = SEL_UPP:ADD-LAST("BULA - depåhantering").               
ELSE MENU-ITEM m_Depåhantering:SENSITIVE IN MENU m_Funktioner = FALSE.
/*IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN ENABLE BTN_BER WITH FRAME FRAME-A.               
 * ELSE MENU-ITEM m_Beredning:SENSITIVE IN MENU m_Funktioner = FALSE.*/
  
/*IF Guru.Konstanter:mtrlsekvar[1] = TRUE THEN ENABLE BTN_MTRL WITH FRAME FRAME-A.         
 * ELSE MENU-ITEM m_Mtrl:SENSITIVE IN MENU m_Funktioner = FALSE.       
 * IF Guru.Konstanter:mtrlsekvar[2] = TRUE THEN ENABLE BTN_KUND WITH FRAME FRAME-A.                  
 * ELSE MENU-ITEM m_Kund:SENSITIVE IN MENU m_Funktioner = FALSE.       
 * IF Guru.Konstanter:mtrlsekvar[3] = TRUE THEN ENABLE BTN_ADM WITH FRAME FRAME-A.                  
 * ELSE MENU-ITEM m_Materieladministration:SENSITIVE IN MENU m_Funktioner = FALSE.        
 * IF Guru.Konstanter:mtrlsekvar[4] = TRUE THEN ENABLE BTN_DEP WITH FRAME FRAME-A.               
 * ELSE MENU-ITEM m_Depåhantering:SENSITIVE IN MENU m_Funktioner = FALSE.
 * IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN ENABLE BTN_BER WITH FRAME FRAME-A.               
 * ELSE MENU-ITEM m_Beredning:SENSITIVE IN MENU m_Funktioner = FALSE.*/
  
