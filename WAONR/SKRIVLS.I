/*SKRIVLS.I*/
DEFINE VARIABLE hDevMode   AS INTEGER NO-UNDO.
DEFINE VARIABLE lpPrintDlg AS INTEGER NO-UNDO.
DEFINE VARIABLE lpDevMode  AS INTEGER NO-UNDO.
DEFINE VARIABLE nCopies    AS INTEGER NO-UNDO.
DEFINE VARIABLE dmCopies   AS INTEGER NO-UNDO.
DEFINE VARIABLE bResult    AS INTEGER NO-UNDO.
DEFINE VARIABLE PrintDlg   AS MEMPTR  NO-UNDO.
DEFINE VARIABLE DevMode    AS MEMPTR  NO-UNDO.


IF liggande = TRUE  THEN DO:
   IF System.Environment:OSVersion:Version:Major <= 5 THEN SYSTEM-DIALOG PRINTER-SETUP NUM-COPIES 1 LANDSCAPE UPDATE utskriv.
   ELSE SYSTEM-DIALOG PRINTER-SETUP NUM-COPIES 1 LANDSCAPE UPDATE utskriv.
END.
ELSE DO:                                                  
   IF System.Environment:OSVersion:Version:Major <= 5 THEN SYSTEM-DIALOG PRINTER-SETUP NUM-COPIES 1 PORTRAIT UPDATE utskriv.
   ELSE SYSTEM-DIALOG PRINTER-SETUP NUM-COPIES 1 PORTRAIT UPDATE utskriv.
END.

/* STRING(System.Environment:OSVersion:Version:Major) 
OM 5 = winxp 
Om mer än 6 = vista och frammåt
STRING(System.Environment:OSVersion:Version:Minor) ger delsiffran
enligt http://msdn.microsoft.com/en-us/library/windows/desktop/ms724832%28v=vs.85%29.aspx

*/

  
 /* INTEGER(myint64 - 333000000000).*/
IF utskriv = TRUE THEN DO:
   IF System.Environment:OSVersion:Version:Major <= 5 THEN DO:
      RUN GlobalLock IN Guru.Konstanter:hpApi (SESSION:PRINTER-CONTROL-HANDLE, OUTPUT lpPrintDlg).
      SET-POINTER-VALUE(PrintDlg) = lpPrintDlg.
      hDevMode = GET-LONG(PrintDlg, 9).
      RUN GlobalLock IN Guru.Konstanter:hpApi(hDevMode, OUTPUT lpDevMode).
      SET-POINTER-VALUE(DevMode) = lpDevMode.
      dmCopies = GET-SHORT(DevMode, 87).
      IF dmCopies > 1 THEN  nCopies = dmCopies.
      RUN GlobalUnlock IN Guru.Konstanter:hpApi (hDevMode, OUTPUT bResult).
      RUN GlobalUnlock IN Guru.Konstanter:hpApi (SESSION:PRINTER-CONTROL-HANDLE, OUTPUT bResult).
   
      Guru.SharedVariable:iCopies = nCopies.
      IF Guru.SharedVariable:iCopies < 1 THEN Guru.SharedVariable:iCopies = 1.
   END.   
   ELSE DO:
      RUN GlobalLock IN Guru.Konstanter:hpApi (SESSION:PRINTER-CONTROL-HANDLE, OUTPUT lpPrintDlg).
      SET-POINTER-VALUE(PrintDlg) = lpPrintDlg.
     
      IF Guru.Konstanter:hpApi:NAME = "windows64.p" OR Guru.Konstanter:hpApi:NAME = "windows64.r" THEN  nCopies = GET-SHORT(PrintDlg, 53).
      ELSE nCopies = GET-SHORT(PrintDlg, 33).
     
      RUN GlobalUnlock IN Guru.Konstanter:hpApi (SESSION:PRINTER-CONTROL-HANDLE, OUTPUT bResult).
      Guru.SharedVariable:iCopies = nCopies.
     
      
     
   END.
END.   

/*
RUN GlobalLock  IN Guru.Konstanter:hpApi ( INPUT  SESSION:PRINTER-CONTROL-HANDLE,OUTPUT iPrintDlg ).

/* Now point our local MEMPTR variable to the PRINTDLG structure and
   fetch the nCopies member. */
SET-POINTER-VALUE(lpPrintDlg) = iPrintDlg.
Guru.SharedVariable:iCopies = GET-SHORT(lpPrintDlg,33).  /* For 64Bit use the number 53 */ 
RUN GlobalUnlock IN Guru.Konstanter:hpApi ( INPUT SESSION:PRINTER-CONTROL-HANDLE,OUTPUT iPrintDlg ).
*/

/*
MESSAGE Guru.SharedVariable:iCopies
VIEW-AS ALERT-BOX.
/*xp*/
  hDevMode = GET-LONG(lpPrintDlg, 9).
  RUN GlobalLock IN Guru.Konstanter:hpApi (hDevMode, OUTPUT lpDevMode).
  SET-POINTER-VALUE(DevMode) = lpDevMode.
  dmCopies = GET-SHORT(DevMode, 87).
  IF dmCopies > 1 THEN Guru.SharedVariable:iCopies = dmCopies.
  MESSAGE Guru.SharedVariable:iCopies
  VIEW-AS ALERT-BOX.
/* Now unlock the memory */    
RUN GlobalUnlock IN Guru.Konstanter:hpApi (hDevMode, OUTPUT bResult).
RUN GlobalUnlock IN Guru.Konstanter:hpApi ( INPUT SESSION:PRINTER-CONTROL-HANDLE,OUTPUT iPrintDlg ).

*/

