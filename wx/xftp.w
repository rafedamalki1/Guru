
&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------


  File:   winftp.p

  Description: Uses the wininet.dll to access an ftp site.  Detail 
               documentation on funtions can be found at:
               http://msdn.microsoft.com/workshop/networking/wininet/overview/overview.asp
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Todd G. Nist

  Created: 3/31/99

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* handle to internet session */
  define var hInternetSession   as  int  no-undo.
/* handle to the ftp session inside the internet connection */
  define var hFTPSession        as  int  no-undo.
/* current directory which we are processing */
  define var cCurrentDir        as  char no-undo.

&SCOPE  MAX_PATH 260

&SCOPE FILE_ATTRIBUTE_NORMAL  128

/* Internet constants */

&SCOPE INTERNET_OPEN_TYPE_PRECONFIG    0
/* indicates to use config information from registry */
&SCOPE INTERNET_FLAG_EXISITING_CONNECT 536870912
/* used for ftp connections */
&SCOPE INTERNET_FLAG_PASSIVE           134217728

/* Flags for FTP transfer mode */
&SCOPE  FTP_TRANSFER_TYPE_ASCII  1   /* 0x00000001 */
&SCOPE  FTP_TRANSFER_TYPE_BINARY 2   /* 0x00000002 */


&SCOPE INTERNET_DEFAULT_FTP_PORT     21
&SCOPE INTERNET_DEFAULT_GOPHER_PORT  70
&SCOPE INTERNET_DEFAULT_HTTP_PORT    80
&SCOPE INTERNET_DEFAULT_HTTPS_PORT  443
&SCOPE INTERNET_DEFAULT_SOCKS_PORT 1080

/* Type of service to access */
&SCOPE INTERNET_SERVICE_FTP    1
&SCOPE INTERNET_SERVICE_GOPHER 2
&SCOPE INTERNET_SERVICE_HTTP   3

PROCEDURE InternetConnectA EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  hInternetSession  as  long.
  define input parameter  lpszServerName    as  char.
  define input parameter  nServerPort       as  long.
  define input parameter  lpszUserName      as  char.
  define input parameter  lpszPassword      as  char.
  define input parameter  dwService         as  long.
  define input parameter  dwFlags           as  long.
  define input parameter  dwContext         as  long.
  define return parameter hInternetConnect  as  long.
END.

PROCEDURE InternetGetLastResponseInfoA EXTERNAL "wininet.dll" PERSISTENT:
  define output parameter lpdwError          as  long.
  define output parameter lpszBuffer         as  char.
  define input-output  parameter lpdwBufferLength   as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE InternetOpenUrlA EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  hInternetSession  as  long.
  define input parameter  lpszUrl           as  char.
  define input parameter  lpszHeaders       as  char.
  define input parameter  dwHeadersLength   as  long.
  define input parameter  dwFlags           as  long.
  define input parameter  dwContext         as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE InternetOpenA EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  sAgent            as  char.
  define input parameter  lAccessType       as  long.
  define input parameter  sProxyName        as  char.
  define input parameter  sProxyBypass      as  char.
  define input parameter  lFlags            as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE InternetReadFile EXTERNAL "wininet.dll" PERSISTENT:
  define input  parameter  hFile            as  long.
  define output parameter  sBuffer          as  char.
  define input  parameter  lNumBytesToRead  as  long.
  define output parameter  lNumOfBytesRead  as  long.
  define return parameter  iResultCode      as  long.
END.

PROCEDURE InternetCloseHandle EXTERNAL "wininet.dll" PERSISTENT:
  define input parameter  hInet             as  long.
  define return parameter iResultCode       as  long.
END.

PROCEDURE FtpFindFirstFileA EXTERNAL "wininet.dll" PERSISTENT :
    define input parameter  hFtpSession as  long.
    define input parameter  lpFileName as char.
    define input parameter  lpFindFileData as memptr.
    define input parameter  dwFlags        as long.
    define input parameter  dwContext      as long.
    define return parameter hSearch as long.
END PROCEDURE.    


PROCEDURE InternetFindNextFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hSearch as long.
    define input parameter  lpFindFileData as memptr.
    define return parameter found as long.
END PROCEDURE.


PROCEDURE FtpGetCurrentDirectoryA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession as long.
    define input parameter  lpszCurrentDirectory as long.
    define input-output parameter lpdwCurrentDirectory as long.
    define return parameter iRetCode as long.
END PROCEDURE.

PROCEDURE FtpSetCurrentDirectoryA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession as long.
    define input parameter  lpszDirectory as long.
    define return parameter iRetCode as long.
END PROCEDURE.

PROCEDURE FtpOpenFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession  as long.
    define input parameter  lpszFileName as long.
    define input parameter  dwAccess     as long.
    define input parameter  dwFlags      as long.
    define input parameter  dwContext    as long.
    define return parameter iRetCode as long.
END PROCEDURE.

PROCEDURE FtpPutFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession       as long.
    define input parameter  lpszLocalFile     as long.
    define input parameter  lpszNewRemoteFile as long.
    define input parameter  dwFlags           as long.
    define input parameter  dwContext         as long.
    define return parameter iRetCode          as long.
END PROCEDURE.

PROCEDURE FtpGetFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession          as long.
    define input parameter  lpszRemoteFile       as long.
    define input parameter  lpszNewFile          as long.
    define input parameter  fFailIfExists        as long.
    define input parameter  dwFlagsAndAttributes as long.
    define input parameter  dwFlags              as long.
    define input parameter  dwContext            as long.
    define return parameter iRetCode             as long.
END PROCEDURE.

PROCEDURE FtpDeleteFileA EXTERNAL "wininet.dll" PERSISTENT:
    define input parameter  hFtpSession          as long.
    define input parameter  lpszRemoteFile       as long.
    define return parameter iRetCode             as long.
END PROCEDURE.

PROCEDURE GetLastError external "kernel32.dll" :
  define return parameter dwMessageID as long. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS cUrl slDirs slFiles btnConnect 
&Scoped-Define DISPLAYED-OBJECTS cUrl slDirs slFiles 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CloseInternetConnection C-Win 
FUNCTION CloseInternetConnection RETURNS LOGICAL
  ( input phInternetSession as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ConnectWinInet C-Win 
FUNCTION ConnectWinInet RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FTPConnect C-Win 
FUNCTION FTPConnect RETURNS LOGICAL
  ( input pcURL as char) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpDeleteFile C-Win 
FUNCTION FtpDeleteFile RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpGetFile C-Win 
FUNCTION FtpGetFile RETURNS CHARACTER
  ( input pcFilename as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FTPListDir C-Win 
FUNCTION FTPListDir RETURNS INTEGER
  (INPUT cSearchDir      as CHAR,
   INPUT cSearchFileSpec as CHAR,
   INPUT hFTPSession     as INT,
   INPUT cProgCallBack   as CHAR,
   INPUT hCallProc       as HANDLE) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FtpPutFile C-Win 
FUNCTION FtpPutFile RETURNS CHARACTER
  ( input pcLocalFile as char,
    input pcRemoteFile as char )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InternetGetLastResponseInfo C-Win 
FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetButtons C-Win 
FUNCTION SetButtons RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnConnect 
     LABEL "Connect to FTP" 
     SIZE 22.2 BY 1.14.

DEFINE BUTTON btnDelete 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnGet 
     LABEL "Get" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnPut 
     LABEL "Put" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE cUrl AS CHARACTER FORMAT "X(256)":U INITIAL "195.67.71.53/erikolsson/btutskick/sokuppdrag/s-1025" 
     LABEL "URL" 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE slDirs AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 8 NO-UNDO.

DEFINE VARIABLE slFiles AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 36 BY 8 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     cUrl AT ROW 1.95 COL 7.4 COLON-ALIGNED
     slDirs AT ROW 4.24 COL 4 NO-LABEL
     slFiles AT ROW 4.24 COL 42.4 NO-LABEL
     btnPut AT ROW 13.19 COL 4
     btnGet AT ROW 13.19 COL 19.6
     btnDelete AT ROW 13.19 COL 35.2
     btnConnect AT ROW 13.19 COL 56.2
     "Directories:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.57 COL 4
     "Files:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.57 COL 42.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 13.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "FTP Demo"
         HEIGHT             = 13.91
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON btnDelete IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnGet IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnPut IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* FTP Demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* FTP Demo */
DO:
  CloseInternetConnection(hInternetSession).
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnConnect
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnConnect C-Win
ON CHOOSE OF btnConnect IN FRAME DEFAULT-FRAME /* Connect to FTP */
DO:
  
  Session:Set-Wait-State('General':U).
  if cURL:screen-value <> '':U THEN
  do:
    if not ConnectWinInet() then
      message substitute('Unable to establish a connection to &1.',
                         cURL:screen-value).
    else
    do:
      /*-----------------------------------------------------------------------
        Start and FTP Sesion.
      ------------------------------------------------------------------------*/
      if FTPConnect(cURL:Screen-Value) then
      do:
        /*----------------------------------------------------------------------- 
         If hFTPSession is a valid handle, then read the contents of the FTP
         site.
        ------------------------------------------------------------------------*/
        FTPListDir(INPUT '.',
                   INPUT '*.*',
                   INPUT hFTPSession,
                   INPUT 'CreateFileList',
                   INPUT THIS-PROCEDURE).
                 
        SetButtons().
      end.
    end.
  end.
  else
    message 'Please enter a URL...' view-as alert-box.

  Session:Set-Wait-State('':U).
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
  message substitute('Are you sure you want to delete file &1?',
                     trim(entry(1,slFiles:Screen-Value, '(':U) ) )
                     view-as alert-box Question buttons YES-NO
                     update lAnswer as Logical.
  if lAnswer then
    FtpDeleteFile().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnGet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnGet C-Win
ON CHOOSE OF btnGet IN FRAME DEFAULT-FRAME /* Get */
DO:
  define variable cNewFilename  as  char format "x(32)" no-undo.
  
  /* no file selected for down loading */
  if slFiles:screen-value = '':u or
     slFiles:screen-value = ? then
    return no-apply.
  
  message 'Destination File Spec: ' update cNewFileName.

  FtpGetFile(cNewFileName).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPut C-Win
ON CHOOSE OF btnPut IN FRAME DEFAULT-FRAME /* Put */
DO:
  define var cLocalFile  as char format "x(60)".
  define var cRemoteFile as char format "x(60)".

  message 'Local Filename: ' update cLocalFile.
  message 'Remote Filename: ' update cRemoteFile.
  
  FtpPutFile(input cLocalFile,
             input cRemoteFile).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME slDirs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL slDirs C-Win
ON MOUSE-SELECT-DBLCLICK OF slDirs IN FRAME DEFAULT-FRAME
DO:
  define variable cDir                  as  char     no-undo.
  define variable iRetCode              as  integer  no-undo.
  define variable lpCurrentDirectory    as  memptr   no-undo.
  define variable lpDirectory           as  memptr   no-undo.
  define variable dwCurDir              as  int      no-undo.
  define variable cCurDir               as  char     no-undo.
  DEBUGGER:SET-BREAK().
  assign set-size(lpCurrentDirectory) = {&MAX_PATH}
         set-size(lpDirectory)        = {&MAX_PATH}
         dwCurDir                     = {&MAX_PATH}.

  run FtpGetCurrentDirectoryA(input hFTPSession,
                              input get-pointer-value(lpCurrentDirectory),
                              input-output dwCurDir,
                              output iRetCode).

  cCurDir = get-string(lpCurrentDirectory,1).

  if self:screen-value = '..':u then
    assign
    cCurDir = substr(cCurDir,1,length(cCurDir) - ((length(cCurDir) + 1) - r-index(cCurDir, '/')) )
    cCurDir = if cCurDir = '' then '/' else cCurDir
    put-string(lpDirectory,1) = cCurDir.
  else if self:screen-value = '.' then
    put-string(lpDirectory,1) = cCurDir.
  else
    put-string(lpDirectory,1) =get-string(lpCurrentDirectory,1) + '/' + self:screen-value.
                                            
  run FtpSetCurrentDirectoryA(input hFTPSession,
                              input get-pointer-value(lpDirectory),
                              output iRetCode).
    
  if iRetCode = 0 then
  do:
    /* this only works under v9 and above */
    RUN GetLastError(OUTPUT iRetCode).
    IF iRetCode = 12003 THEN /* INTERNET_EXTENDED_ERROR */
      InternetGetLastResponseInfo().
    ELSE
      message 'FtpSetCurrentDirectory failed:' iRetCode view-as alert-box.  
  end.
  
  else 
  do:
    assign
    cDir = self:screen-value
    self:list-items = ''
    self:screen-value = ''
    slFiles:screen-value = ''
    slFiles:list-items = ''.

    FTPListDir(INPUT '.',
               INPUT '*.*',
               INPUT hFTPSession,
               INPUT 'CreateFileList',
               INPUT THIS-PROCEDURE).
  end.

  set-size(lpDirectory) = 0.
  set-size(lpCurrentDirectory) = 0.
  
  SetButtons().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateFileList C-Win 
PROCEDURE CreateFileList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       iFileSize should be converted to a decimal so that it can
               support very large file sizes.  Currently we are only looking
               at the low value and not taking the high value into
               consideration.       
------------------------------------------------------------------------------*/
define input parameter lpFindData   as  memptr no-undo.
define input parameter pcSearchDir  as  char   no-undo.

define variable iFileSize           as  integer no-undo.
define variable lResult             as  logical no-undo.

do with frame {&frame-name}:
    if get-long(lpFindData, 1) = 16 then
      slDirs:add-last(get-string(lpFindData,45)) .
    else
      assign iFileSize = get-long(lpFindData,33)  /* nFileSizeLow */
             lResult = slFiles:add-last(substitute('&1 (&2 &3)',
                                        get-string(lpFindData,45),
                                        iFileSize,
                                        if iFileSize > 1024 then 'KB':U
                                        else 'Bytes':U)).
  end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY cUrl slDirs slFiles 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE cUrl slDirs slFiles btnConnect 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CloseInternetConnection C-Win 
FUNCTION CloseInternetConnection RETURNS LOGICAL
  ( input phInternetSession as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  Close the handle the InternetSession.  Since all other handles are 
            leafs of this handle, the will also be closed when the root is 
            closed. (i.e. hFTPSession. )
 
    Notes:  
------------------------------------------------------------------------------*/
  define variable iRetCode      as  integer no-undo.

  run InternetCloseHandle(input  phInternetSession,
                          output iRetCode).
     

  RETURN iRetCode > 0.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ConnectWinInet C-Win 
FUNCTION ConnectWinInet RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  connect to specified website and exchange information.
    Notes:  
------------------------------------------------------------------------------*/
 
  /*-------------------------------------------------------------------------- 
    Call to establish an Internet session.  The handle, hInternetSession,
    will be used when connecting to the URL. 
  ---------------------------------------------------------------------------*/
  run InternetOpenA(input  'WebBasedAgent',
                    input  {&INTERNET_OPEN_TYPE_PRECONFIG},
                    input  '',
                    input  '',
                    input  0,
                    output hInternetSession).
  
  RETURN hInternetSession <> 0. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FTPConnect C-Win 
FUNCTION FTPConnect RETURNS LOGICAL
  ( input pcURL as char):
/*------------------------------------------------------------------------------
  Purpose:  connect to the ftp site for a given internet session.
    Notes:  
------------------------------------------------------------------------------*/
  def var iError as int no-undo.
  
  run InternetConnectA(input  hInternetSession,
                       input  pcURL,
                       input  {&INTERNET_DEFAULT_FTP_PORT},
                       input  "gep",
                       input  "vsf17ggr",
                       input  {&INTERNET_SERVICE_FTP},
                       input  0,
                       input  0,
                       output hFTPSession).
    
  /* FOR CERN based Firewall support 
  RUN InternetOpenUrlA(INPUT hInternetSession,
                       INPUT pcURL,
                       INPUT '',
                       INPUT 0,
                       INPUT {&INTERNET_FLAG_PASSIVE},
                       INPUT 0,
                       OUTPUT hFTPSession).
  */

  IF hFTPSession = 0 then
  do:
    run GetLastError(output iError).
    message "InternetConnectA Failed:  " iError view-as alert-box.
    InternetGetLastResponseInfo().
    RETURN FALSE.
  end.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpDeleteFile C-Win 
FUNCTION FtpDeleteFile RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Deletes a file from the FTP Server if you have permissions.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpRemoteFile   as  memptr  no-undo.
  define var iRetCode       as  integer no-undo.
  define var cRemoteFile    as  char    no-undo.
  
  assign
  /* remove the file size from the file name */
  cRemoteFile = trim(entry(1,slFiles:Screen-Value in frame {&frame-name}, '(' ) )
  set-size(lpRemoteFile)     = length(cRemoteFile) + 1
  put-string(lpRemoteFile,1) = cRemoteFile.
  
  Session:Set-Wait-State('General':U).
  MESSAGE "ta bort" cRemoteFile  VIEW-AS ALERT-BOX.
  run FtpDeleteFileA(input hFtpSession,
                     input get-pointer-value(lpRemoteFile),
                     output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  else
    message 'File Deleted...' view-as alert-box.

  assign
  set-size(lpRemoteFile)     = 0.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpGetFile C-Win 

FUNCTION FtpGetFile RETURNS CHARACTER
  ( input pcFilename as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Retrieves a file from the FTP Server and stores it under the 
            specified file name, creating a new local file in the process.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpRemoteFile   as  memptr  no-undo.
  define var lpNewFile      as  memptr  no-undo.
  define var fOverwirte     as  log     no-undo.
  define var iRetCode       as  integer no-undo.
  define var cRemoteFile    as  char    no-undo.
  
  assign
  /* remove the file size from the file name */
  cRemoteFile = trim(entry(1,slFiles:Screen-Value in frame {&frame-name}, '(' ) )
  set-size(lpRemoteFile)     = length(cRemoteFile) + 1
  put-string(lpRemoteFile,1) = cRemoteFile
  set-size(lpNewFile)        = length(pcFilename) + 1
  put-string(lpNewFile,1)    = pcFileName.
  MESSAGE "remot" cRemoteFile  "andra file" pcFileName  VIEW-AS ALERT-BOX.
  Session:Set-Wait-State('General':U).
  
  run FtpGetFileA(input hFtpSession,
                 input get-pointer-value(lpRemoteFile),
                 input get-pointer-value(lpNewFile),
                 input 0, /* 1 - fail if file exists, 0 - overwrite */
                 input {&FILE_ATTRIBUTE_NORMAL},
                 input {&FTP_TRANSFER_TYPE_BINARY},
                 input 0,
                 output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  else
    message 'File Received...' view-as alert-box.

  assign
  set-size(lpRemoteFile)     = 0
  set-size(lpNewFile)        = 0.

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FTPListDir C-Win 
FUNCTION FTPListDir RETURNS INTEGER
  (INPUT cSearchDir      as CHAR,
   INPUT cSearchFileSpec as CHAR,
   INPUT hFTPSession     as INT,
   INPUT cProgCallBack   as CHAR,
   INPUT hCallProc       as HANDLE):
/*------------------------------------------------------------------------

  Function:    DirList

  Description: Returns integer corresponding to the error:
                   0 - if there were no errors.
                   1 - file list buffer is too small
                   2 - file list buffer is too big
                   3 - invalid path given
                   5 - directory on drive is invaild

               
               cSearchDir:
                 Directory to search in, can be relative, use /../../, etc.                 
               
               cSearchFileSpec:
                 comma delimited list of file types, can have trailing 
                 comma e.g: " foo?ar.*  ,  *.p, "
                 
               hFTPSession - handle to an open ftp session.

               cProgCallBack:
                 Program to be called if a file is found passing in
                 to input parameters, the directory being searched, 
                 and the filename found.
                 
               hCallProc
                 Handle to the calling program where call back process
                 is to execute.

  Notes:       FtpFindFirstFile can only occur once within a given FTP
               session.  To issue another one, a call must be made
               InternetCloseHandle.

  History: 
          
------------------------------------------------------------------------*/
  def var lpFindData   as memptr    no-undo.
  def var hSearch      as integer   no-undo.
  def var iFound       as integer   no-undo initial 1.
  def var iFileSpec    as integer   no-undo.
  def var cFileList    as char      no-undo.
  def var iRetCode     as integer   no-undo.

  &SCOPE  FIND_DATA-SIZE 4           /* dwFileAttributes       */~
                       + 8           /* ftCreationTime         */~
                       + 8           /* ftLastAccessTime       */~
                       + 8           /* ftLastWriteTime        */~
                       + 4           /* nFileSizeHigh          */~
                       + 4           /* nFileSizeLow           */~
                       + 4           /* dwReserved0            */~
                       + 4           /* dwReserved1            */~
                       + {&MAX_PATH} /* cFileName[MAX_PATH]    */~
                       + 14          /* cAlternateFileName[14] */
  
  /* allocate the memory for the find_data structure */
  assign
  set-size(lpFindData) = {&FIND_DATA-SIZE}.

  do iFileSpec = 1 to num-entries(cSearchFileSpec):

    iFound = 1.
    run FtpFindFirstFileA (input  hFtpSession,
                           input  cSearchDir + '/' + 
                                  entry(iFileSpec, cSearchFileSpec), 
                           input  lpFindData,
                           input  {&INTERNET_FLAG_EXISITING_CONNECT},
                           input  0,
                           output hSearch).

    if hSearch <> -1 then 
    repeat while iFound <> 0:
     
      run value(cProgCallBack) in hCallProc
            (input lpFindData,
             input  cSearchDir).                 /* current directory */
         
      run InternetFindNextFileA (input  hSearch, 
                                 input  lpFindData,
                                 output iFound).
        
    end. /* repeat while ifound <> 0... */

    /* set error for invalid file specification */
    else 
      iRetCode = 5.
 
  end. /* do iFileSpec = 1 to ... */
    
  set-size(lpFindData) = 0.

  /* close file handle now so we can do a find again */
  run InternetCloseHandle (input hSearch, OUTPUT iRetCode).

  return iRetCode.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FtpPutFile C-Win 
FUNCTION FtpPutFile RETURNS CHARACTER
  ( input pcLocalFile as char,
    input pcRemoteFile as char ) :
/*------------------------------------------------------------------------------
  Purpose:  Sends a file to the FTP Server and stores it under the 
            specified file name, creating a new remote file in the process
            if you have the appropriate permissions.  If not you will be told
            so via InternetGetLastResponse.
    Notes:  
------------------------------------------------------------------------------*/

  define var lpLocalFile        as  memptr  no-undo.
  define var lpNewRemoteFile    as  memptr  no-undo.
  define var fOverwirte         as  log     no-undo.
  define var iRetCode           as  integer no-undo.
  
  assign
  /* remove the file size from the file name */
  set-size(lpNewRemoteFile)     = length(pcRemoteFile) + 1
  put-string(lpNewRemoteFile,1) = pcRemoteFile
  set-size(lpLocalFile)         = length(pcLocalFile) + 1
  put-string(lpLocalFile,1)     = pcLocalFile.
  
  Session:Set-Wait-State('General':U).
  
  run FtpPutFileA(input hFtpSession,
                  input get-pointer-value(lpLocalFile),
                  input get-pointer-value(lpNewRemoteFile),
                  input {&FTP_TRANSFER_TYPE_BINARY},
                  input 0,
                  output iRetCode).
                 
  Session:Set-Wait-State('':U).
  
  if iRetCode = 0 then
    InternetGetLastResponseInfo().
  else
    message 'File Sent...' view-as alert-box.

  assign
  set-size(lpNewRemoteFile)     = 0
  set-size(lpLocalFile)         = 0.
 


  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InternetGetLastResponseInfo C-Win 
FUNCTION InternetGetLastResponseInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  If an error is encountered then display what the last response
            was.
    Notes:  
------------------------------------------------------------------------------*/
  define var cBuffer            as  char no-undo.
  define var iBufferSz          as  int init 4096 no-undo.
  define var iResultCode        as  int no-undo.
  define var iTemp              as  int no-undo.
  
  /* allocate for the buffer */
  assign
  cBuffer = fill(' ', iBufferSz).

  run InternetGetLastResponseInfoA (output iResultCode,
                                    output cBuffer,
                                    input-output iBufferSz,
                                    output iTemp).

  message substitute('Error (&1):  &2',
                     iResultCode,
                     substr(cBuffer,1,iBufferSz)) view-as alert-box.
                       
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetButtons C-Win 
FUNCTION SetButtons RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  do with frame {&frame-name}:
    assign
    btnPut:Sensitive = slDirs:List-Items <> ''
    btnGet:Sensitive = slFiles:List-Items <> ? and slFiles:List-Items <> '':U
    btnDelete:Sensitive = slFiles:List-Items <> ? and slFiles:List-Items <> '':U.
  end.
  
  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


