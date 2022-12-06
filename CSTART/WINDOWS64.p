/* ====================================================================
   file      windows64.p
   by        Jurjen Dijkstra, 1997 - 1999
             mailto:jurjen@global-shared.com
             http://www.global-shared.com
   language  Progress 8.2A
   purpose   declarations for windows API procedures
   ==================================================================== */
 
&GLOB DONTDEFINE-HPAPI
{windows.i}
{BTNLINES.I}

PROCEDURE GetCurrentDirectory_UI :
   DEFINE OUTPUT PARAMETER chrDirectoryName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intBufferSize    AS INTEGER   NO-UNDO INITIAL 256.
   DEFINE VARIABLE intResult        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ptrToString      AS MEMPTR    NO-UNDO.
   SET-SIZE(ptrToString) = 256.
   RUN GetCurrentDirectoryA (INPUT        intBufferSize,
                             INPUT-OUTPUT ptrToString,
                             OUTPUT       intResult).
   ASSIGN chrDirectoryName = GET-STRING(ptrToString,1).  
   chrDirectoryName = TRIM(chrDirectoryName).
END PROCEDURE.

PROCEDURE SetCurrentDirectoryA EXTERNAL "kernel32.dll":
    DEFINE INPUT  PARAMETER  lpPathName       AS CHARACTER NO-UNDO.
END PROCEDURE.
PROCEDURE GetCurrentDirectoryA EXTERNAL "KERNEL32.DLL":
    DEFINE INPUT        PARAMETER intBufferSize AS LONG.
    DEFINE INPUT-OUTPUT PARAMETER ptrToString   AS MEMPTR.
    DEFINE RETURN       PARAMETER intResult     AS SHORT.
END PROCEDURE.

PROCEDURE GlobalLock EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER hMem AS LONG.   /* 64-bit: INT64 */
    DEFINE RETURN PARAMETER hAddr AS LONG. /* 64-bit: INT64 */
END PROCEDURE.
 
PROCEDURE GlobalUnlock EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER hMem AS LONG.   /* 64-bit: INT64 */
    DEFINE RETURN PARAMETER bResult AS LONG.
END PROCEDURE.
/*
PROCEDURE GlobalLock EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER hMem AS INT64.   /* 64-bit: INT64 */
    DEFINE RETURN PARAMETER hAddr AS INT64. /* 64-bit: INT64 */
END PROCEDURE.
 
PROCEDURE GlobalUnlock EXTERNAL "kernel32.dll":
    DEFINE INPUT PARAMETER hMem AS INT64.   /* 64-bit: INT64 */
    DEFINE RETURN PARAMETER bResult AS INT64.
END PROCEDURE.
*/


PROCEDURE AdjustWindowRect EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER lpRect       AS LONG. /* get-pointer-value(memptr) */
  DEFINE INPUT  PARAMETER dwstyle      AS LONG.
  DEFINE INPUT  PARAMETER bMenu        AS {&BOOL}.
  DEFINE RETURN PARAMETER ReturnValue  AS {&BOOL}.
END PROCEDURE.

PROCEDURE ClientToScreen EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER win-handle  AS {&HWND}.
  DEFINE INPUT  PARAMETER lppoint     AS LONG.  /* get-pointer-value(memptr) */
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE CreateProcess{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpApplicationName    AS LONG. /* NULL */
  DEFINE INPUT  PARAMETER lpCommandline        AS CHAR.
  DEFINE INPUT  PARAMETER lpProcessAttributes  AS LONG.
  DEFINE INPUT  PARAMETER lpThreadAttributes   AS LONG.
  DEFINE INPUT  PARAMETER bInheritHandles      AS {&BOOL}.
  DEFINE INPUT  PARAMETER dCreationFlags       AS LONG.
  DEFINE INPUT  PARAMETER lpEnvironment        AS LONG.
  DEFINE INPUT  PARAMETER lpCurrentDirectory   AS LONG.
  DEFINE INPUT  PARAMETER lpStartupInfo        AS LONG.
  DEFINE INPUT  PARAMETER lpProcessInformation AS LONG.
  DEFINE RETURN PARAMETER bResult              AS {&BOOL}.
END PROCEDURE.

PROCEDURE CreateWindowExA EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER dwExStyle    AS LONG.
  DEFINE INPUT  PARAMETER lpClassName  AS CHAR.
  DEFINE INPUT  PARAMETER lpWindowName AS CHAR.
  DEFINE INPUT  PARAMETER dwStyle      AS LONG.
  DEFINE INPUT  PARAMETER x            AS LONG.
  DEFINE INPUT  PARAMETER y            AS LONG.
  DEFINE INPUT  PARAMETER nWidth       AS LONG.
  DEFINE INPUT  PARAMETER nHeight      AS LONG.
  DEFINE INPUT  PARAMETER hWndParent   AS LONG.
  DEFINE INPUT  PARAMETER hMenu        AS LONG.
  DEFINE INPUT  PARAMETER hInstance    AS LONG.
  DEFINE INPUT  PARAMETER lpParam      AS LONG.
  DEFINE RETURN PARAMETER hwndCreated  AS LONG.
END PROCEDURE.

PROCEDURE CloseHandle EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER hObject     AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE ClosePrinter EXTERNAL {&WINSPOOL} :
  DEFINE INPUT  PARAMETER VH_PRINTER_HANDLE AS LONG.
  DEFINE RETURN PARAMETER VI_RETURN_VALUE   AS {&BOOL}.
END PROCEDURE.

PROCEDURE CreateMutexA EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpMutexAttributes AS LONG.
  DEFINE INPUT  PARAMETER bInitialOwner     AS LONG.
  DEFINE INPUT  PARAMETER lpName            AS CHAR.
  DEFINE RETURN PARAMETER hMutex            AS LONG.
END PROCEDURE.

PROCEDURE DeleteMenu EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hMenu       AS {&INT}.
  DEFINE INPUT  PARAMETER uPosition   AS {&INT}.
  DEFINE INPUT  PARAMETER uFlags      AS {&INT}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE DestroyCursor EXTERNAL {&USER}  :
   DEFINE INPUT  PARAMETER hCursorName    AS LONG.
   DEFINE RETURN PARAMETER retValue       AS LONG.  
END PROCEDURE.

PROCEDURE DrawMenuBar EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hMenu      AS  LONG.
  DEFINE RETURN PARAMETER iRetCode   AS  LONG.
END PROCEDURE.

PROCEDURE Ellipse EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER hdc         AS LONG.
  DEFINE INPUT  PARAMETER nLeftRect   AS LONG.
  DEFINE INPUT  PARAMETER nTopRect    AS LONG.
  DEFINE INPUT  PARAMETER nRightRect  AS LONG.
  DEFINE INPUT  PARAMETER nBottomRect AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE EndDoc EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER  hdc AS LONG.
  DEFINE RETURN PARAMETER uRet AS LONG.
END PROCEDURE.

PROCEDURE EndPage EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER hdc  AS LONG.
  DEFINE RETURN PARAMETER uRet AS LONG.
END PROCEDURE.

PROCEDURE EnumPrinters{&A} EXTERNAL {&WINSPOOL} :
  DEFINE INPUT  PARAMETER Flags        AS LONG.  /* Local, shared, network,etc.. */
  DEFINE INPUT  PARAMETER Name         AS CHAR.  /* LEAVE AS NULL ie.: 0 */
  DEFINE INPUT  PARAMETER Level        AS LONG.  /* Type of info to return: 1,2,5 on W95 */
  DEFINE INPUT  PARAMETER pPrinterEnum AS LONG.  /* points to PRINTER_INFO_n structures */
  DEFINE INPUT  PARAMETER cbBuf        AS LONG.  /* Tells function the size of pPrinterEnum */
  DEFINE OUTPUT PARAMETER pcbNeeded    AS LONG.  /* Number of bytes copied or required */
  DEFINE OUTPUT PARAMETER pcReturned   AS LONG.  /* Number of PRINTER_INFO_n   structures returned */
  DEFINE RETURN PARAMETER RetValue     AS LONG.  /* A Bool value = zero if failure */
END PROCEDURE.

PROCEDURE FindExecutable{&A} EXTERNAL {&SHELL} :
  DEFINE INPUT        PARAMETER lpFile      AS CHAR.
  DEFINE INPUT        PARAMETER lpDirectory AS CHAR.
  DEFINE INPUT-OUTPUT PARAMETER lpResult    AS CHAR.
  DEFINE RETURN       PARAMETER hInstance   AS {&INT}.
END PROCEDURE.

PROCEDURE FlashWindow EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER bInvert     AS {&BOOL}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE FormatMessage{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER dwFlags      AS LONG.
  DEFINE INPUT  PARAMETER lpSource     AS LONG.
  DEFINE INPUT  PARAMETER dwMessageID  AS LONG.
  DEFINE INPUT  PARAMETER dwLanguageID AS LONG.
  DEFINE OUTPUT PARAMETER lpBuffer     AS CHAR.
  DEFINE INPUT  PARAMETER nSize        AS LONG.
  DEFINE INPUT  PARAMETER lpArguments  AS LONG.
  DEFINE RETURN PARAMETER nTextLength  AS LONG.
END PROCEDURE.

PROCEDURE FreeLibrary EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER hproc       AS {&HINSTANCE}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE GetClientRect EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER lpRect      AS LONG. /* get-pointer-value(memptr) */
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE GetCursorPos EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER  lpPoint     AS LONG. /* memptr */
  DEFINE RETURN PARAMETER  ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE GetDateFormatA EXTERNAL {&KERNEL} :
  DEFINE INPUT        PARAMETER Locale      AS LONG.
  DEFINE INPUT        PARAMETER dwFlags     AS LONG.
  DEFINE INPUT        PARAMETER lpTime      AS LONG.
  DEFINE INPUT        PARAMETER lpFormat    AS LONG.
  DEFINE INPUT-OUTPUT PARAMETER lpDateStr   AS CHAR.
  DEFINE INPUT        PARAMETER cchDate     AS LONG.
  DEFINE RETURN       PARAMETER cchReturned AS LONG.
END PROCEDURE.

PROCEDURE GetDC EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd AS {&HWND}.
  DEFINE RETURN PARAMETER hdc  AS {&INT}.
END PROCEDURE.

PROCEDURE GetDeviceCap :
   DEFINE OUTPUT PARAMETER capability AS INTEGER NO-UNDO.
   DEFINE VARIABLE hdc    AS INTEGER   NO-UNDO INITIAL 256.
   DEFINE VARIABLE nIndex        AS INTEGER   NO-UNDO.
   
   RUN GetDeviceCapsA (INPUT hdc,INPUT  nIndex, OUTPUT capability).
   
END PROCEDURE.

PROCEDURE GetDeviceCapsA EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER  hdc        AS {&INT}.
  DEFINE INPUT  PARAMETER  nIndex     AS {&INT}.
  DEFINE RETURN PARAMETER  capability AS {&INT}.
END PROCEDURE.

PROCEDURE GetLastError EXTERNAL {&KERNEL} :
  DEFINE RETURN PARAMETER dwMessageID AS {&INT}.
END PROCEDURE.

PROCEDURE GetLocaleInfoA EXTERNAL {&KERNEL} :
  DEFINE INPUT        PARAMETER Locale      AS LONG.
  DEFINE INPUT        PARAMETER dwFlags     AS LONG.
  DEFINE INPUT-OUTPUT PARAMETER lpLCData    AS CHAR.
  DEFINE INPUT        PARAMETER cchData     AS LONG.
  DEFINE RETURN       PARAMETER cchReturned AS LONG.
END PROCEDURE.

PROCEDURE GetMenu EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER ProgHwnd AS LONG.
  DEFINE RETURN PARAMETER MenuHnd  AS LONG.
END PROCEDURE.

PROCEDURE GetMenuItemCount EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hMenu    AS  LONG.
  DEFINE RETURN PARAMETER iCount   AS  LONG.
END PROCEDURE.

PROCEDURE GetModuleFileName{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER hInst        AS {&INT}.
  DEFINE OUTPUT PARAMETER lpszFileName AS CHAR.
  DEFINE INPUT  PARAMETER cbFileName   AS {&INT}.
  DEFINE RETURN PARAMETER bSuccess     AS {&INT}.
END PROCEDURE.

PROCEDURE GetParent EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER thishwnd   AS {&HWND}.
  DEFINE RETURN PARAMETER parenthwnd AS {&HWND}.
END PROCEDURE.

PROCEDURE GetPrivateProfileString{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpszSection     AS CHAR.
  DEFINE INPUT  PARAMETER lpszEntry       AS LONG.
  DEFINE INPUT  PARAMETER lpszDefault     AS CHAR.
  DEFINE INPUT  PARAMETER memBuffer       AS LONG. /* memptr */
  DEFINE INPUT  PARAMETER cbReturnBuffer  AS {&INT}.
  DEFINE INPUT  PARAMETER lpszFilename    AS CHAR.
  DEFINE RETURN PARAMETER cbReturnedChars AS {&INT}.
END PROCEDURE.

PROCEDURE GetProfileString{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpAppName        AS CHAR.
  DEFINE INPUT  PARAMETER lpKeyName        AS CHAR.
  DEFINE INPUT  PARAMETER lpDefault        AS CHAR.
  DEFINE OUTPUT PARAMETER lpReturnedString AS CHAR.
  DEFINE INPUT  PARAMETER nSize            AS {&INT}.
  DEFINE RETURN PARAMETER nReturnedChars   AS {&INT}.
END PROCEDURE.

PROCEDURE GetSubMenu EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER MenuHnd    AS LONG.
  DEFINE INPUT  PARAMETER nPos       AS LONG.
  DEFINE RETURN PARAMETER SubMenuHnd AS LONG.
END PROCEDURE.

PROCEDURE GetSysColor EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER nIndex     AS LONG.
  DEFINE RETURN PARAMETER dwRgbValue AS LONG.
END PROCEDURE.

PROCEDURE GetSystemMenu EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd    AS {&HWND}.
  DEFINE INPUT  PARAMETER bRevert AS {&BOOL}.
  DEFINE RETURN PARAMETER hMenu   AS {&INT}.
END PROCEDURE.

PROCEDURE GetTimeFormatA EXTERNAL {&KERNEL} :
  DEFINE INPUT        PARAMETER Locale      AS LONG.
  DEFINE INPUT        PARAMETER dwFlags     AS LONG.
  DEFINE INPUT        PARAMETER lpTime      AS LONG.
  DEFINE INPUT        PARAMETER lpFormat    AS LONG.
  DEFINE INPUT-OUTPUT PARAMETER lpTimeStr   AS CHAR.
  DEFINE INPUT        PARAMETER cchTime     AS LONG.
  DEFINE RETURN       PARAMETER cchReturned AS LONG.
END PROCEDURE.

&IF "{&OPSYS}"="WIN32" &THEN
  PROCEDURE GetUserName{&A} EXTERNAL {&ADVAPI} :
    DEFINE INPUT-OUTPUT PARAMETER lpBuffer    AS CHAR.
    DEFINE INPUT-OUTPUT PARAMETER nSize       AS LONG.
    DEFINE RETURN       PARAMETER ReturnValue AS {&BOOL}.
  END PROCEDURE.
&ELSE
  /* There is no 16-bit equivalent for this function.
     Create a stub OR create a wrapper to a thunked call.
     This would be a stub: */
  PROCEDURE GetUserName{&A} :
    DEFINE INPUT-OUTPUT PARAMETER lpBuffer    AS CHAR.
    DEFINE INPUT-OUTPUT PARAMETER nSize       AS INTEGER.
    DEFINE OUTPUT       PARAMETER ReturnValue AS INTEGER.
    ASSIGN lpBuffer = ""
           ReturnValue = 1.
  END PROCEDURE.
&ENDIF

PROCEDURE GetVersionExA EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpVersionInfo AS LONG.
  DEFINE RETURN PARAMETER ReturnValue   AS {&BOOL}.
END PROCEDURE.

PROCEDURE GetWindowLong{&A} EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER phwnd       AS {&HWND}.
  DEFINE INPUT  PARAMETER cindex      AS {&INT}.
  DEFINE RETURN PARAMETER currentlong AS LONG.
END PROCEDURE.


PROCEDURE GetWindowsDirectoryA EXTERNAL {&KERNEL} :
  DEFINE OUTPUT PARAMETER lpBuffer AS CHAR.
  DEFINE INPUT  PARAMETER uSize    AS LONG.
  DEFINE RETURN PARAMETER uRet     AS LONG.
END PROCEDURE.

PROCEDURE InvalidateRect EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hWnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER lpRect      AS {&INT}.
  DEFINE INPUT  PARAMETER bErase      AS {&BOOL}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE LoadCursorA EXTERNAL {&USER} :
   DEFINE INPUT  PARAMETER hInstance      AS {&HINSTANCE}.
   DEFINE INPUT  PARAMETER lpCursorName   AS LONG.
   DEFINE RETURN PARAMETER hcur           AS LONG.  
END PROCEDURE.

PROCEDURE LoadLibrary{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER libname AS CHAR.
  DEFINE RETURN PARAMETER hproc   AS {&HINSTANCE}.
END PROCEDURE.


PROCEDURE MAPISendMail EXTERNAL {&MAPI} :
  DEFINE INPUT  PARAMETER lhSession  AS LONG.
  DEFINE INPUT  PARAMETER ulUIParam  AS LONG.
  DEFINE INPUT  PARAMETER lpMessage  AS LONG. /* get-pointer-value(memptr) */
  DEFINE INPUT  PARAMETER flFlags    AS LONG.
  DEFINE INPUT  PARAMETER ulReserved AS LONG.
  DEFINE RETURN PARAMETER wretcode   AS {&INT}.
END PROCEDURE.

/* http://support.microsoft.com/kb/839560

 WrapCompressedRTFStreamEx(
  LPSTREAM              lpCompressedRTFStream,
  CONST RTF_WCSINFO *   pWCSInfo,
  LPSTREAM *            lppUncompressedRTFStream,
  RTF_WCSRETINFO *      pRetInfo
*/
PROCEDURE WrapCompressedRTFStreamEx EXTERNAL {&MAPI} :
   DEFINE INPUT PARAMETER lpCompressedRTFStream AS LONG.
   DEFINE INPUT PARAMETER pWCSInfo AS LONG.
   DEFINE OUTPUT PARAMETER lppUncompressedRTFStream AS LONG.
   DEFINE OUTPUT PARAMETER pRetInfo AS LONG.  
  
END PROCEDURE.


PROCEDURE mciGetErrorString{&A} EXTERNAL {&MMEDIA} :
  DEFINE INPUT  PARAMETER mciError       AS {&INT}.
  DEFINE OUTPUT PARAMETER lpszErrorText  AS CHAR.
  DEFINE INPUT  PARAMETER cchErrorText   AS {&INT}.
  DEFINE RETURN PARAMETER ReturnValue    AS {&BOOL}.
END PROCEDURE.

PROCEDURE mciSendCommand{&A} EXTERNAL {&MMEDIA} :
  DEFINE INPUT  PARAMETER IDDevice   AS {&INT}.
  DEFINE INPUT  PARAMETER uMsg       AS {&INT}.
  DEFINE INPUT  PARAMETER fdwCommand AS {&INT}.
  DEFINE INPUT  PARAMETER dwParam    AS LONG.
  DEFINE RETURN PARAMETER mciError   AS {&INT}.
END PROCEDURE.

PROCEDURE OpenPrinter{&A} EXTERNAL {&WINSPOOL} :
  DEFINE INPUT  PARAMETER PC_PRINTER_NAME   AS CHAR.
  DEFINE INPUT  PARAMETER VM_PRINTER_HANDLE AS LONG.
  DEFINE INPUT  PARAMETER VM_DEFAULTS       AS LONG.
  DEFINE RETURN PARAMETER VI_RETURN_VALUE   AS {&BOOL}.
END PROCEDURE.

PROCEDURE PostMessage{&A} EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER umsg        AS {&INT}.
  DEFINE INPUT  PARAMETER wparam      AS {&INT}.
  DEFINE INPUT  PARAMETER lparam      AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE PrinterProperties EXTERNAL {&WINSPOOL} :
  DEFINE INPUT  PARAMETER VH_PARENT         AS LONG.
  DEFINE INPUT  PARAMETER VH_PRINTER_HANDLE AS LONG.
  DEFINE RETURN PARAMETER VI_RETURN_VALUE   AS {&BOOL}.
END PROCEDURE.

PROCEDURE RegOpenKeyA EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey       AS LONG.
  DEFINE INPUT  PARAMETER lpszSubKey AS CHAR.
  DEFINE OUTPUT PARAMETER phkResult  AS LONG.
  DEFINE RETURN PARAMETER lpResult   AS LONG.
END PROCEDURE.

PROCEDURE RegCloseKey EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey     AS LONG.
  DEFINE RETURN PARAMETER lpresult AS LONG.
END PROCEDURE.

PROCEDURE RegEnumKeyA EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hKey        AS LONG.
  DEFINE INPUT  PARAMETER iSubKey     AS LONG.
  DEFINE OUTPUT PARAMETER lpszName    AS CHAR.
  DEFINE INPUT  PARAMETER cchName     AS LONG.
  DEFINE RETURN PARAMETER lpresult    AS LONG.
END PROCEDURE.

PROCEDURE RegQueryValueExA EXTERNAL {&ADVAPI} :
  DEFINE INPUT        PARAMETER hkey         AS LONG.
  DEFINE INPUT        PARAMETER lpValueName  AS CHAR.
  DEFINE INPUT        PARAMETER lpdwReserved AS LONG.
  DEFINE OUTPUT       PARAMETER lpdwType     AS LONG.
  DEFINE INPUT        PARAMETER lpbData      AS LONG. /* memptr */
  DEFINE INPUT-OUTPUT PARAMETER lpcbData     AS LONG.
  DEFINE RETURN       PARAMETER lpresult     AS LONG.
END PROCEDURE.

PROCEDURE RegSetValueExA EXTERNAL {&ADVAPI} :
  DEFINE INPUT  PARAMETER hkey         AS LONG.
  DEFINE INPUT  PARAMETER lpValueName  AS CHAR.
  DEFINE INPUT  PARAMETER Reserved     AS LONG.
  DEFINE INPUT  PARAMETER dwType       AS LONG.
  DEFINE INPUT  PARAMETER lpData       AS LONG. /* memptr */
  DEFINE INPUT  PARAMETER cbData       AS LONG.
  DEFINE RETURN PARAMETER lpresult     AS LONG.
END PROCEDURE.

PROCEDURE ReleaseDC EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd AS {&HWND}.
  DEFINE INPUT  PARAMETER hdc  AS {&INT}.
  DEFINE RETURN PARAMETER ok   AS {&INT}.
END PROCEDURE.

PROCEDURE RemoveMenu EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hMenu      AS  LONG.
  DEFINE INPUT  PARAMETER nPosition  AS  LONG.
  DEFINE INPUT  PARAMETER wFlags     AS  LONG.
  DEFINE RETURN PARAMETER iRetCode   AS  LONG.
END PROCEDURE.

PROCEDURE ScreenToClient EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hWnd        AS LONG.
  DEFINE INPUT  PARAMETER lpPoint     AS LONG. /* memptr */
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE SendMessage{&A} EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER umsg        AS {&INT}.
  DEFINE INPUT  PARAMETER wparam      AS {&INT}.
  DEFINE INPUT  PARAMETER lparam      AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE SetCursorPos EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER x-pos       AS {&INT}.
  DEFINE INPUT  PARAMETER y-pos       AS {&INT}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE SetParent EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER  hwndChild     AS {&HWND}.
  DEFINE INPUT  PARAMETER  hwndNewParent AS {&HWND}.
  DEFINE RETURN PARAMETER hwndOldParent  AS {&HWND}.
END PROCEDURE.

PROCEDURE SetSysColors EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER cDspElements   AS LONG.
  DEFINE INPUT  PARAMETER lpnDspElements AS LONG.
  DEFINE INPUT  PARAMETER lpdwRgbValues  AS LONG.
  DEFINE RETURN PARAMETER ReturnValue    AS {&BOOL}.
END PROCEDURE.

PROCEDURE SetSystemCursor EXTERNAL {&USER} :
   DEFINE INPUT  PARAMETER hcur          AS LONG.
   DEFINE INPUT  PARAMETER id            AS LONG.
   DEFINE RETURN PARAMETER ReturnValue   AS {&BOOL}.  
END PROCEDURE.

PROCEDURE SetWindowContextHelpId EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER ContextID   AS {&INT}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE SetWindowLong{&A} EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER phwnd   AS {&HWND}.
  DEFINE INPUT  PARAMETER cindex  AS {&INT}.
  DEFINE INPUT  PARAMETER newlong AS LONG.
  DEFINE RETURN PARAMETER oldlong AS LONG.
END PROCEDURE.

PROCEDURE SetWindowPos EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd            AS {&HWND}.
  DEFINE INPUT  PARAMETER hwndInsertAfter AS {&HWND}.
  DEFINE INPUT  PARAMETER x               AS {&INT}.
  DEFINE INPUT  PARAMETER y               AS {&INT}.
  DEFINE INPUT  PARAMETER cx              AS {&INT}.
  DEFINE INPUT  PARAMETER cy              AS {&INT}.
  DEFINE INPUT  PARAMETER fuFlags         AS LONG.
  DEFINE RETURN PARAMETER ReturnValue     AS {&BOOL}.
END PROCEDURE.

PROCEDURE SHBrowseForFolder EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER  lpbi         AS LONG.
  DEFINE RETURN PARAMETER  lpItemIDList AS LONG.
END PROCEDURE.

PROCEDURE SHGetPathFromIDList EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER  lpItemIDList AS LONG.
  DEFINE OUTPUT PARAMETER  pszPath      AS CHAR.
  DEFINE RETURN PARAMETER  ReturnValue  AS {&BOOL}.
END PROCEDURE.

PROCEDURE ShellExecute{&A} EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER hwnd          AS {&HWND}.
  DEFINE INPUT  PARAMETER lpOperation   AS CHAR.
  DEFINE INPUT  PARAMETER lpFile        AS CHAR.
  DEFINE INPUT  PARAMETER lpParameters  AS CHAR.
  DEFINE INPUT  PARAMETER lpDirectory   AS CHAR.
  DEFINE INPUT  PARAMETER nShowCmd      AS {&INT}.
  DEFINE RETURN PARAMETER hInstance     AS {&INT}.
END PROCEDURE.

PROCEDURE ShellExecuteExA EXTERNAL {&SHELL} :
  DEFINE INPUT  PARAMETER lpExecInfo  AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE ShowScrollBar EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hWnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER fnBar       AS {&INT}.
  DEFINE INPUT  PARAMETER fShow       AS {&BOOL}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE ShowWindow EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hWnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER nCmdShow    AS {&INT}.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE StartDoc{&A} EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER hdc   AS LONG.
  DEFINE INPUT  PARAMETER lpdi  AS LONG.
  DEFINE RETURN PARAMETER JobId AS LONG.
END PROCEDURE.

PROCEDURE StartPage EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER hdc  AS LONG.
  DEFINE RETURN PARAMETER uRet AS LONG.
END PROCEDURE.

PROCEDURE GetWindowRect EXTERNAL "user32" :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER lpRect      AS INT64. /* get-pointer-value(memptr) */
  DEFINE RETURN PARAMETER ReturnValue AS INT64.
END PROCEDURE.
/*
PROCEDURE GetWindowRect EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwnd        AS {&HWND}.
  DEFINE INPUT  PARAMETER lpRect      AS LONG. /* get-pointer-value(memptr) */
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.
*/
/*
PROCEDURE SystemParametersInfo{&A} EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER uiAction    AS {&INT}.
  DEFINE INPUT  PARAMETER uiParam     AS {&INT}.
  DEFINE INPUT  PARAMETER pvParam     AS LONG.  /* get-pointer-value(memptr) */
  DEFINE INPUT  PARAMETER fWinIni     AS {&INT}.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
 
END PROCEDURE.
*/
PROCEDURE SystemParametersInfoA EXTERNAL "user32.dll":
   DEFINE INPUT PARAMETER uiAction AS INT64.
   DEFINE INPUT PARAMETER uiParam AS LONG.
   DEFINE INPUT PARAMETER pvParam AS INT64.
   DEFINE INPUT PARAMETER fWinIni AS INT64.
   DEFINE RETURN PARAMETER ReturnValue AS INT64 .
END PROCEDURE.


PROCEDURE TextOut{&A} EXTERNAL {&GDI} :
  DEFINE INPUT  PARAMETER hdc      AS LONG.
  DEFINE INPUT  PARAMETER nXstart  AS LONG.
  DEFINE INPUT  PARAMETER nYstart  AS LONG.
  DEFINE INPUT  PARAMETER lpString AS CHAR.
  DEFINE INPUT  PARAMETER cbString AS LONG.
  DEFINE RETURN PARAMETER uRet     AS LONG.
END PROCEDURE.

PROCEDURE WaitForSingleObject EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER hObject     AS {&INT}.
  DEFINE INPUT  PARAMETER dwTimeout   AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE WinExec EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpszCmdLine AS CHAR.
  DEFINE INPUT  PARAMETER fuCmdShow   AS {&INT}.
  DEFINE RETURN PARAMETER nTask       AS {&INT}.
END PROCEDURE.

PROCEDURE WinHelp{&A} EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hwndmain    AS {&HWND}.
  DEFINE INPUT  PARAMETER lpszHelp    AS CHAR.
  DEFINE INPUT  PARAMETER uCommand    AS {&INT}.
  DEFINE INPUT  PARAMETER dwData      AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS {&BOOL}.
END PROCEDURE.

PROCEDURE WritePrivateProfileString{&A} EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER lpszSection  AS CHAR.
  DEFINE INPUT  PARAMETER lpszEntry    AS CHAR.
  DEFINE INPUT  PARAMETER lpszString   AS CHAR.
  DEFINE INPUT  PARAMETER lpszFilename AS CHAR.
  DEFINE RETURN PARAMETER lpszValue    AS {&INT}.
END PROCEDURE.

PROCEDURE Sleep EXTERNAL {&KERNEL} :
  DEFINE INPUT PARAMETER dwMilliseconds AS LONG.
END PROCEDURE.

PROCEDURE OpenProcess EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER dwDesiredAccess AS LONG.
  DEFINE INPUT  PARAMETER bInheritHandle  AS LONG.
  DEFINE INPUT  PARAMETER dwProcessId     AS LONG.
  DEFINE RETURN PARAMETER hProcess        AS LONG.
END PROCEDURE.

PROCEDURE GetExitCodeProcess EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER hProcess    AS LONG.
  DEFINE OUTPUT PARAMETER ExitCode    AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE WaitForInputIdle EXTERNAL {&USER} :
  DEFINE INPUT  PARAMETER hProcess        AS LONG.
  DEFINE INPUT  PARAMETER dwMilliseconds  AS LONG.
  DEFINE RETURN PARAMETER ReturnValue     AS LONG.
END PROCEDURE.

PROCEDURE TerminateProcess EXTERNAL {&KERNEL} :
  DEFINE INPUT  PARAMETER hProcess    AS LONG.
  DEFINE INPUT  PARAMETER uExitCode   AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

PROCEDURE MAPILogon EXTERNAL {&MAPI} :
DEFINE INPUT PARAMETER ulUIParam AS LONG.
DEFINE INPUT PARAMETER lpszProfileName AS CHAR.
DEFINE INPUT PARAMETER lpszPassword AS CHAR.
DEFINE INPUT PARAMETER flFlags AS LONG.
DEFINE INPUT PARAMETER ulReserved AS LONG.
DEFINE OUTPUT PARAMETER lpSession AS LONG.
DEFINE RETURN PARAMETER wretcode AS {&INT}.
END.

PROCEDURE MAPILogoff EXTERNAL {&MAPI} :
DEFINE INPUT PARAMETER lhSession AS LONG.
DEFINE INPUT PARAMETER ulUIParam AS LONG.
DEFINE INPUT PARAMETER flFlags AS LONG.
DEFINE INPUT PARAMETER ulReserved AS LONG.
DEFINE RETURN PARAMETER wretcode AS {&INT}.
END.

{center.i}

PROCEDURE SetAppstartingCursor :
   DEFINE VARIABLE hcur AS INTEGER NO-UNDO.                                       
   DEFINE VARIABLE retval AS INTEGER NO-UNDO.  
   RUN LoadCursorA (INPUT 0, INPUT {&IDC_APPSTARTING}, OUTPUT hcur).    
   RUN SetSystemCursor (INPUT hcur, INPUT {&IDC_ARROW}, OUTPUT retval). 
   RUN DestroyCursor (INPUT hcur, OUTPUT retval). 
END PROCEDURE.

PROCEDURE SetDefaultCursors :
   DEFINE VARIABLE retval AS INTEGER NO-UNDO.  
   RUN SystemParametersInfoA (INPUT 87, INPUT 0, INPUT 0, INPUT 0, OUTPUT retval).   
END PROCEDURE.
  
PROCEDURE RunningWindows2000_UI:
  /* returns TRUE if you are running Windows 2000 */
 
  DEF OUTPUT PARAMETER Win2000       AS LOGICAL NO-UNDO.
  DEF VAR lpVersionInfo AS MEMPTR.
  DEF VAR dwPlatformID  AS INTEGER NO-UNDO.
  DEF VAR MajorVersion  AS INTEGER NO-UNDO.
  DEF VAR ReturnValue   AS INTEGER NO-UNDO.
 
  SET-SIZE(lpVersionInfo)   = 148.
  PUT-LONG(lpVersionInfo,1) = 148.
  RUN GetVersionExA ( GET-POINTER-VALUE(lpVersionInfo), 
                              OUTPUT ReturnValue).
 
  dwPlatformID = GET-LONG(lpVersionInfo,17).
  MajorVersion = GET-BYTE(lpVersionInfo, 5).
 
  Win2000 = (dwPlatformId=2 AND MajorVersion=5).
 
  SET-SIZE(lpVersionInfo) = 0.
  
END PROCEDURE.

PROCEDURE RunningWindows_UI:
   /*
  OS      dwPlatformID dwMajorVersion dwMinorVersion 
  Win3.1            0             ?     ? 
  Win95             1             4     0 
  Win98             1             4    10 
  WinME             1             4    90 
  NT3.51            2             3    51 
  NT 4.0            2             4     0 
  Win2000           2             5     0 
                                        */

  DEF OUTPUT PARAMETER dwPlatformID  AS INTEGER NO-UNDO.
  DEF OUTPUT PARAMETER MajorVersion  AS INTEGER NO-UNDO.
  DEF OUTPUT PARAMETER ReturnValue   AS INTEGER NO-UNDO.
  DEF VAR lpVersionInfo AS MEMPTR.
  SET-SIZE(lpVersionInfo)   = 148.
  PUT-LONG(lpVersionInfo,1) = 148.
  RUN GetVersionExA ( GET-POINTER-VALUE(lpVersionInfo), 
                              OUTPUT ReturnValue).
 
  dwPlatformID = GET-LONG(lpVersionInfo,17).
  MajorVersion = GET-BYTE(lpVersionInfo, 5).
 
  
  SET-SIZE(lpVersionInfo) = 0.
  
END PROCEDURE.

PROCEDURE DisableWindowClose :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER winhandle AS HANDLE NO-UNDO.
  define var hSysMenu   as  int no-undo.
  define var hParent    as  int no-undo.
  define var hInstance  as  int no-undo.
  define var iRetCode   as  int no-undo.
  define var iCnt       as  int no-undo.
  /*
  run GetParent IN Guru.Konstanter:hpApi (hWnd, output hParent).
  
  run GetParent (input {&window-name}:hWnd,
                output hParent).
   */
  RUN GetParent(INPUT winhandle:HWND, OUTPUT hParent).
/*   hParent = GetParent(winhandle:HWND). */
  /* Get handle to our the window's system menu (Restore, Maximize, Move, close etc.) */
  run GetSystemMenu(input  hParent, 
                    input  0,
                    output hSysMenu).

  if hSysMenu <> 0 then
  do:
    /* Get System menu's menu count */
    run GetMenuItemCount(input hSysMenu,
                         output iCnt).
         
    if iCnt <> 0 then
    do:
      /* Menu count is based on 0 (0, 1, 2, 3...) */

      /* remove the "close option" */
      run RemoveMenu(input hSysMenu, 
                     input iCnt - 1, 
                     input {&MF_BYPOSITION} + {&MF_REMOVE},
                     output iRetCode).

      /* remove the seperator */
      run RemoveMenu(input hSysMenu, 
                     input iCnt - 2, 
                     input {&MF_BYPOSITION} + {&MF_REMOVE},
                     output iRetCode).
            
      /* Force caption bar's refresh which will disable the window close ("X") button */
      run DrawMenuBar(input  hParent,
                      output iRetCode).
      /*{&window-name}:title = "Try to close me!".*/
    end. /* if iCnt <> 0... */
  end. /* if hSysMenu <> 0... */  
END PROCEDURE.

/*
To determine the offset of each structure member starting from the first one, you have to know the sizes of the data types. There are a few things to watch out for.
 
The DEVMODE structure (http://msdn.microsoft.com/en-us/library/windows/desktop/dd183565%28v=vs.85%29.aspx) starts like this:

typedef struct _devicemode {
  TCHAR dmDeviceName[CCHDEVICENAME];
  WORD  dmSpecVersion;
  WORD  dmDriverVersion;
  WORD  dmSize;
  WORD  dmDriverExtra;
  DWORD dmFields;

 
The first member is always at offset 1 (MEMPTR offsets start at 1 instead of 0) so dmDeviceName is at offset 1. Now we have to figure out how big dmDeviceName is so we can find the offset for dmSpecVersion. TCHAR is a character data type. Its size is either 1 if the string is non Unicode or 2 if the string is Unicode. Before 10.2B06 we used the non Unicode version of the DEVMODE structure so dmDeviceName was an array of 1-byte characters. Starting in 10.2B06 we use the Unicode version of DEVMODE so dmDeviceName is an array of 2-byte characters. CCHDEVICENAME is the length of the array. It is defined as 32. So, before 10.2B06 the size of dmDeviceName was 32 (32 * 1); starting in 10.2B06 the size is 64 (32 * 2).
 
Since dmDeviceName was at offset 1 we add its size (32 or 64) to calculate the offset for dmSpecVersion (33 or 65).
 
A WORD and a short are 2 bytes long, a DWORD is 4 bytes long, so we get the following offsets:
 
                                                           offsets before 10.2B06 | offsets in 10.2B06 and later

  TCHAR dmDeviceName[CCHDEVICENAME];//1
  WORD  dmSpecVersion;              //33 or 65
  WORD  dmDriverVersion;            //35 or 67
  WORD  dmSize;                     //37 or 69
  WORD  dmDriverExtra;              //39 or 71
  DWORD dmFields;                   //41 or 73
  short dmOrientation;              //45 or 77
  short dmPaperSize;                //47 or 79
  short dmPaperLength;              //49 or 81
  etc. 

 
Here are some of the things that can make counting offsets more complicated:
 
Pointers and handles have different sizes depending on whether the executable is 32-bit or 64-bit. Pointers and handles are 4 bytes long in 32-bit executables; they are 8 bytes long in 64-bit executables. Handles are the types that begin with �H�, like �HANDLE�, �HGLOBAL�, �HDC�, and �HWND�. Pointer types usually begin with the letters �LP�, like �LPCTSTR�. You have to be careful, however � �LPARAM� starts with �LP� but it isn�t a pointer.
 
The DEVMODE structure contains unions. Unions are two of structure members that occupy the same space. The length of a union is the length of the largest member of the union.
 
Some structure members need to be �aligned�. This means that empty space is inserted in the structure before these members so that they end up in memory at addresses that are evenly divisible by a certain number (usually 4-byte boundaries for 32-bit executables and 8-byte boundaries for 64-bit executables). Pointers and handles are two of the member types which must be aligned.

Below you can find complete example how to determine PRINTDL structure (http://msdn.microsoft.com/en-us/library/windows/desktop/ms646843(v=vs.85).aspx) offsets for 32-bit and 64-bit executable.
 

The 32-bit PRINTDLG structure:
     typedef struct tagPD {    
//size of the data type in the bytes        // offset                              
//4    DWORD           lStructSize;         // 1 
//4    HWND            hwndOwner;           // 5
//4    HGLOBAL         hDevMode;            // 9
//4    HGLOBAL         hDevNames;           // 13
//4    HDC             hDC;                 // 17
//4    DWORD           Flags;               // 21
//2    WORD            nFromPage;           // 25
//2    WORD            nToPage;             // 27
//2    WORD            nMinPage;            // 29
//2    WORD            nMaxPage;            // 31
//2    WORD            nCopies;             // 33
//4    HINSTANCE       hInstance;           // 35 
//4    LPARAM          lCustData;           // 39
//4    LPPRINTHOOKPROC lpfnPrintHook;       // 43
//4    LPSETUPHOOKPROC lpfnSetupHook;       // 47
//4    LPCTSTR         lpPrintTemplateName; // 51
//4    LPCTSTR         lpSetupTemplateName; // 55
//4    HGLOBAL         hPrintTemplate;      // 59
//4    HGLOBAL         hSetupTemplate;      // 63
     } PRINTDLG, *LPPRINTDLG;
The size of 32-bit PRINTDLG structure data is 66 bytes.

The 64-bit PRINTDLG structure:

     typedef struct tagPD {

//size of the data type in the bytes        // offset 

//4    DWORD           lStructSize;         // 1

//4    DWORD           padding1;            // 5

//8    HWND            hwndOwner;           // 9

//8    HGLOBAL         hDevMode;            // 17

//8    HGLOBAL         hDevNames;           // 25

//8    HDC             hDC;                 // 33

//4    DWORD           Flags;               // 41

//2    WORD            nFromPage;           // 45

//2    WORD            nToPage;             // 47

//2    WORD            nMinPage;            // 49

//2    WORD            nMaxPage;            // 51

//2    WORD            nCopies;             // 53

//2    WORD            padding2;            // 55

//8    HINSTANCE       hInstance;           // 57

//8    LPARAM          lCustData;           // 65 

//8    LPPRINTHOOKPROC lpfnPrintHook;       // 73 

//8    LPSETUPHOOKPROC lpfnSetupHook;       // 81

//8    LPCTSTR         lpPrintTemplateName; // 89 

//8    LPCTSTR         lpSetupTemplateName; // 97 

//8    HGLOBAL         hPrintTemplate;      // 105 

//8    HGLOBAL         hSetupTemplate;      // 113 

     } PRINTDLG, *LPPRINTDLG;
The size of 64-bit PRINTDLG structure data is 120 bytes.

*/
