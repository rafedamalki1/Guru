/*KALKSEK.I KÖRS INTE*/ 
 
    /*ANDVÄNDS EJ*/    
IF Guru.Konstanter:hoppsekvar[2] = TRUE THEN DO:
   IF (globforetag = "GADM"  OR globforetag = "GRAN" 
     OR globforetag = "GKAL") THEN globforetag = globforetag. 
   ELSE ENABLE BTN_MTRL WITH FRAME FRAME-A.
END. 
xhop = "KALK".  
IF kalklogvar = FALSE THEN DO:
   kalklogvar = TRUE.
   RUN nextguru_UI.
   ASSIGN
   kalksekvar[1] = XSEK.SEK[1]
   kalksekvar[2] = XSEK.SEK[2]
   kalksekvar[3] = XSEK.SEK[3]
   kalksekvar[4] = XSEK.SEK[4]
   kalksekvar[5] = XSEK.SEK[5]
   kalksekvar[6] = XSEK.SEK[6]
   kalksekvar[7] = XSEK.SEK[7]
   kalksekvar[8] = XSEK.SEK[8]
   kalksekvar[9] = XSEK.SEK[9]
   kalksekvar[10] = XSEK.SEK[10]
   kalksekvar[11] = XSEK.SEK[11]
   kalksekvar[12] = XSEK.SEK[12]
   kalksekvar[13] = XSEK.SEK[13].
END.   
IF kalksekvar[1] = TRUE THEN ENABLE BTN_NY WITH FRAME FRAME-A.
ELSE MENU-ITEM m_Ny:SENSITIVE IN MENU m_Updatera = FALSE.       
IF kalksekvar[2] = TRUE THEN ENABLE BTN_UPP WITH FRAME FRAME-A.
ELSE MENU-ITEM m_ndra:SENSITIVE IN MENU m_Updatera = FALSE.          
IF kalksekvar[3] = TRUE THEN ENABLE BTN_BORT WITH FRAME FRAME-A.  
ELSE MENU-ITEM m_Ta_Bort:SENSITIVE IN MENU m_Updatera = FALSE.       
IF kalksekvar[4] = TRUE THEN ENABLE BTN_KALK WITH FRAME FRAME-A.
ELSE MENU-ITEM m_Kalkylera:SENSITIVE IN MENU m_Funktioner2 = FALSE.    
IF kalksekvar[5] = TRUE THEN ENABLE BTN_SKR WITH FRAME FRAME-A.              
ELSE MENU-ITEM m_Skriva_ut_kalkyl:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF kalksekvar[6] = TRUE THEN ENABLE BTN_VISKAL WITH FRAME FRAME-A.              
ELSE MENU-ITEM m_Visa_kalkyl:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF kalksekvar[7] = TRUE THEN ENABLE BTN_KOPKAL WITH FRAME FRAME-A.           
ELSE MENU-ITEM m_kopplaonr:SENSITIVE IN MENU m_Funktioner2 = FALSE.       
IF kalksekvar[8] = TRUE THEN ENABLE BTN_INAKTIV WITH FRAME FRAME-A.             
ELSE MENU-ITEM m_Gra_kalyl_aktiv_resp_inaktiv:SENSITIVE IN MENU m_Funktioner2 = FALSE.           
IF kalksekvar[10] = TRUE THEN ENABLE BTN_ADM WITH FRAME FRAME-A.                  
