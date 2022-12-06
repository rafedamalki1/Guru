/*BULASEK.I*/  
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO.   
DEFINE SHARED VARIABLE bulalogvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLEbulasekvar AS LOGICAL EXTENT 50 NO-UNDO.
xhop = "BULA".   
IF globforetag = "ELPA" OR globforetag = "SOLE"   
OR globforetag = "GKAL" OR globforetag = "LULE" THEN DO:
   ENABLE BTN_UT WITH FRAME FRAME-A. 
   ENABLE BTN_RET WITH FRAME FRAME-A.
END.
ELSE IF globforetag = "VAST"  THEN DO:
   ENABLE BTN_UT WITH FRAME FRAME-A. 
END.   
IF bulalogvar = FALSE THEN DO:
   bulalogvar = TRUE.
   /*RUN xsek_UI IN depaapph (INPUT xhop, OUTPUT Guru.Konstanter:bulasekvar[]).             */
   FIND FIRST XSEK WHERE XSEK.MENYVART = xhop AND
   XSEK.AV-LEVEL = Guru.Konstanter:globniv USE-INDEX XSEK NO-LOCK NO-ERROR.     
   
   ASSIGN
   Guru.Konstanter:bulasekvar[1] = XSEK.SEK[1]
   Guru.Konstanter:bulasekvar[2] = XSEK.SEK[2]
   Guru.Konstanter:bulasekvar[3] = XSEK.SEK[3]
   Guru.Konstanter:bulasekvar[4] = XSEK.SEK[4]
   Guru.Konstanter:bulasekvar[5] = XSEK.SEK[5]
   Guru.Konstanter:bulasekvar[6] = XSEK.SEK[6]
   Guru.Konstanter:bulasekvar[7] = XSEK.SEK[7]
   Guru.Konstanter:bulasekvar[8] = XSEK.SEK[8]
   Guru.Konstanter:bulasekvar[9] = XSEK.SEK[9]
   Guru.Konstanter:bulasekvar[10] = XSEK.SEK[10]
   Guru.Konstanter:bulasekvar[11] = XSEK.SEK[11]
   Guru.Konstanter:bulasekvar[12] = XSEK.SEK[12]
   Guru.Konstanter:bulasekvar[13] = XSEK.SEK[13].
END.   
IF Guru.Konstanter:bulasekvar[1] = TRUE THEN ENABLE btn_best WITH FRAME FRAME-A.
/*ELSE MENU-ITEM m_Bestllningar:SENSITIVE IN MENU m_Funktioner = FALSE.       */
IF Guru.Konstanter:bulasekvar[2] = TRUE THEN ENABLE btn_lager WITH FRAME FRAME-A.
/*ELSE MENU-ITEM m_Lager:SENSITIVE IN MENU m_Funktioner = FALSE.          */
IF Guru.Konstanter:bulasekvar[3] = TRUE THEN ENABLE btn-Rapporter WITH FRAME FRAME-A.  
/*ELSE MENU-ITEM m_Rapporter:SENSITIVE IN MENU m_Funktioner = FALSE.       */
IF Guru.Konstanter:bulasekvar[4] = TRUE THEN ENABLE BTN_LEV WITH FRAME FRAME-A.
/*ELSE MENU-ITEM m_Leveranser:SENSITIVE IN MENU m_Funktioner = FALSE.    */
IF Guru.Konstanter:bulasekvar[5] = TRUE THEN ENABLE btn_inventering WITH FRAME FRAME-A.              
/*ELSE MENU-ITEM m_Inventering:SENSITIVE IN MENU m_Funktioner = FALSE.       */
IF Guru.Konstanter:bulasekvar[6] = TRUE THEN ENABLE BTN_ADM WITH FRAME FRAME-A.              
/*ELSE MENU-ITEM m_Administration:SENSITIVE IN MENU m_Funktioner = FALSE.         */
IF Guru.Konstanter:bulasekvar[7] = TRUE THEN ENABLE BTN_BSTAT WITH FRAME FRAME-A.


     
