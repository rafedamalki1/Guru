/*KALKSEK2.I KÖRS INTE*/ 
  
   DEFINE buffer sekbuff FOR XSEK.
   xhop = "AONR".           
   FIND FIRST sekbuff WHERE sekbuff.MENYVART = xhop AND
   sekbuff.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.
   IF AVAILABLE sekbuff THEN DO:      
      IF sekbuff.SEK[1] = TRUE THEN ENABLE BTN_AONR WITH FRAME {&FRAME-NAME}.
   END.   
xhop = "KALK".  
IF kalk2logvar = FALSE THEN DO:
   kalk2logvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   Guru.Konstanter:kalk2sekvar[1] = XSEK.SEK[1]  /*ny*/
   Guru.Konstanter:kalk2sekvar[2] = XSEK.SEK[2]  /*ändra kalkylhuv*/
   Guru.Konstanter:kalk2sekvar[3] = XSEK.SEK[3]  /*ta bort*/
   Guru.Konstanter:kalk2sekvar[4] = XSEK.SEK[4]  /*kalkylera*/
   Guru.Konstanter:kalk2sekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:kalk2sekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:kalk2sekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:kalk2sekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:kalk2sekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:kalk2sekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:kalk2sekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:kalk2sekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:kalk2sekvar[13] = XSEK.SEK[13].
END.
IF Guru.Konstanter:kalk2sekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME FRAME-A.
ELSE MENU-ITEM m_Ny:SENSITIVE IN MENU m_Updatera = FALSE.       
IF Guru.Konstanter:kalk2sekvar[2] = TRUE THEN ENABLE BTN_UPP BTN_KOPI BTN_KONV WITH FRAME FRAME-A.
ELSE MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = FALSE.          
IF Guru.Konstanter:kalk2sekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME FRAME-A.  
ELSE MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = FALSE.       
IF Guru.Konstanter:kalk2sekvar[4] = TRUE THEN ENABLE BTN_KALK WITH FRAME FRAME-A.
ELSE MENU-ITEM m_Kalkylera:SENSITIVE IN MENU m_Funktioner2 = FALSE.    
IF Guru.Konstanter:kalk2sekvar[5] = TRUE THEN ENABLE BTN_SKR WITH FRAME FRAME-A.              
ELSE MENU-ITEM m_Skriva_ut_kalkyl:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF Guru.Konstanter:kalk2sekvar[6] = TRUE THEN ENABLE BTN_VISKAL WITH FRAME FRAME-A.              
ELSE MENU-ITEM m_Visa_kalkyl:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF Guru.Konstanter:kalk2sekvar[7] = TRUE THEN ENABLE BTN_KOPKAL BTN_BORTKOP WITH FRAME FRAME-A.           
ELSE MENU-ITEM m_kopplaonr:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF Guru.Konstanter:kalk2sekvar[8] = TRUE THEN ENABLE BTN_INAKTIV WITH FRAME FRAME-A.             
ELSE MENU-ITEM m_Gra_kalyl_aktiv_resp_inaktiv:SENSITIVE IN MENU m_Funktioner2 = FALSE.
IF Guru.Konstanter:kalk2sekvar[9] = TRUE THEN ENABLE BTN_BORTKOP WITH FRAME FRAME-A.             
ELSE MENU-ITEM m_Bort_Kopp:SENSITIVE IN MENU m_Funktioner2 = FALSE.  
IF Guru.Konstanter:kalk2sekvar[10] = TRUE THEN ENABLE BTN_ADM WITH FRAME FRAME-A.             
ELSE MENU-ITEM m_Adm_Kalk:SENSITIVE IN MENU m_Funktioner2 = FALSE.         
     
