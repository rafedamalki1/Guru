/*xskrivut32.p*/
DEFINE VARIABLE hDevMode   AS INTEGER NO-UNDO.
DEFINE VARIABLE lpPrintDlg AS INTEGER NO-UNDO.
DEFINE VARIABLE lpDevMode  AS INTEGER NO-UNDO.
DEFINE VARIABLE nCopies    AS INTEGER NO-UNDO.
DEFINE VARIABLE dmCopies   AS INTEGER NO-UNDO.
DEFINE VARIABLE bResult    AS INTEGER NO-UNDO.
DEFINE VARIABLE PrintDlg   AS MEMPTR  NO-UNDO.
DEFINE VARIABLE DevMode    AS MEMPTR  NO-UNDO.
def var utskriv as log.
SESSION:PRINTER-CONTROL-HANDLE = 0.
   SYSTEM-DIALOG PRINTER-SETUP LANDSCAPE UPDATE utskriv.
   
/* STRING(System.Environment:OSVersion:Version:Major) 
OM 5 = winxp 
Om mer än 6 = vista och frammåt
STRING(System.Environment:OSVersion:Version:Minor) ger delsiffran
enligt http://msdn.microsoft.com/en-us/library/windows/desktop/ms724832%28v=vs.85%29.aspx

*/
 


IF utskriv = TRUE THEN DO:
     RUN GlobalLock (SESSION:PRINTER-CONTROL-HANDLE, OUTPUT lpPrintDlg).
     
      SET-POINTER-VALUE(PrintDlg) = lpPrintDlg.
    
      nCopies = GET-SHORT(PrintDlg, 33).
      MESSAGE nCopies 
VIEW-AS ALERT-BOX.
nCopies = GET-SHORT(PrintDlg, 53).
      MESSAGE nCopies 
VIEW-AS ALERT-BOX.
      RUN GlobalUnlock (SESSION:PRINTER-CONTROL-HANDLE, OUTPUT bResult).
MESSAGE nCopies 
VIEW-AS ALERT-BOX.
/*      
   OUTPUT TO PRINTER  NUM-COPIES VALUE(nCopies)  CONVERT TARGET "iso8859-1".
PUT "Hi".
OUTPUT CLOSE.
*/  
END.   


PROCEDURE GlobalLock EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER hMem AS LONG.   /* 64-bit: INT64 */
    DEFINE RETURN PARAMETER hAddr AS LONG. /* 64-bit: INT64 */
END PROCEDURE.
 
PROCEDURE GlobalUnlock EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER hMem AS LONG.   /* 64-bit: INT64 */
    DEFINE RETURN PARAMETER bResult AS LONG.
END PROCEDURE.
