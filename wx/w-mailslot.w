&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: w-mailslot.w

  Description: This programs demonstrates the use of a mailslot for IPC.
               If the toggle box is checked for server, the default, the
               process will create a mailslot via the API CreateMailslot.
               It will then read from the file whenever the read message
               button is pressed or the tick event is fired for the timer. 
               If the toggle box is de-selected, the process will start up 
               as a client process.  It can then write to the mailslot.
               The server process can then read any messages received
               on the mailslot.  If there is no server process up, i.e.
               the mailslot has not been created, the clent process will fail
               and terminate the client.
               
               The application establishes a mailslot on its own machine with 
               a unique name in the form \\.\mailslot\path\mailslotname.  This
               is a virtual path, not physical, and is used for grouping.  The
               client process should open the mailslot with the CreateFile
               function in a file share read or write mode.
               If the mailslot is on the same machine then the mailslot name
               would confrom to the above.  It it is on a remote machine it
               would take the fashing of \\ComputerName\mailslot\.... or
               \\DomainName\mailslot\....  Multiple clients can open the same 
               mailslot.
               
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: 
  
  Author:  Todd G. Nist
           tnist@protech.com
           Protech Systems Inc.
          
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


&GLOB READ_INTERVAL          1000

&GLOB MAILSLOT_WAIT_FOREVER  1
&GLOB CREATE_ALWAYS          2
&GLOB OPEN_EXISTING          3
&GLOB GENERIC_READ          -2147483648
&GLOB GENERIC_WRITE          1073741824
&GLOB GENERIC_EXECUTE        536870912
&GLOB GENERIC_ALL            268435456
&GLOB INVALID_HANDLE        -1
&GLOB FILE_SHARE_READ        1
&GLOB FILE_SHARE_WRITE       2
&GLOB FILE_ATTRIBUTE_NORMAL  128
&GLOB PAGE_READWRITE         4  /* 2 */
&GLOB A                      A

PROCEDURE CreateFile{&A} EXTERNAL "kernel32" :
  define input parameter  lpFilename            as char.
  define input parameter  dwDesiredAccess       as long.
  define input parameter  dwShareMode           as long.
  define input parameter  lpSecurityAttrib      as long.
  define input parameter  dwCreationDisposition as long.
  define input parameter  dwFlagsAndAttributes  as long.
  define input parameter  hTemplateFile         as long.
  define return parameter hObject               as long.
END.

PROCEDURE CreateMailslot{&A} EXTERNAL "kernel32" :
  define input parameter lpName                 as char.
  define input parameter nMaxMessageSize        as long. /* 400 is max accros domains */
  define input parameter lReadTimeout           as long.
  define input parameter lpSecurityAttributes   as long.
  define return parameter hMailslot             as long.
END.

PROCEDURE CloseHandle EXTERNAL "kernel32" :
  define input parameter hObject as long.    /* handle to a process */
  define return parameter hStatus as long.   /* 0 if it failed */
END PROCEDURE.

PROCEDURE GetComputerName{&A} EXTERNAL "kernel32":
  define output       parameter lpBuffer  AS MEMPTR.
  define input-output parameter nSize     AS LONG.
  define return       parameter intResult AS SHORT. /* actually LONG */
END PROCEDURE.

PROCEDURE ReadFile EXTERNAL "kernel32":
 define input  parameter  hFile                as long.
 define output parameter  lpBuffer             as char.
 define input  parameter  nNumberOfBytesToRead as long.
 define output parameter  lpNumberOfBytesRead  as long.
 define input  parameter  lpOverlapped         as long.
 define return parameter  iResultCode          as long.
END.

PROCEDURE WriteFile EXTERNAL "kernel32" :
  define input  parameter hFile                  as long.
  define input  parameter lpBuffer               as char.
  define input  parameter nNumberOfBytesToWrite  as long.
  define output parameter lpNumberOfBytesWritten as long.
  define input  parameter lpOverlapped           as long.
  define return parameter lpResult               as long.
END.

/* Local Variable Definitions ---                                       */

define var iResultCode      as  int     no-undo.
define var hMailslot        as  int     no-undo.
define var cComputerName    as  char    no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbServer cMailslotName cMsg BtnWrite 
&Scoped-Define DISPLAYED-OBJECTS tbServer cMailslotName cMsg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetComputerName W-Win 
FUNCTION GetComputerName RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ReadMailSlot W-Win 
FUNCTION ReadMailSlot RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartProcess W-Win 
FUNCTION StartProcess RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD WriteMailSlot W-Win 
FUNCTION WriteMailSlot RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Timer AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chTimer AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnWrite 
     LABEL "Write Message" 
     SIZE 21 BY 1.14.

DEFINE VARIABLE cMsg AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 72 BY 4 NO-UNDO.

DEFINE VARIABLE cMailslotName AS CHARACTER FORMAT "X(256)":U INITIAL "~\~\.~\mailslot~\demo~\slot1" 
     LABEL "Mailslot" 
     VIEW-AS FILL-IN 
     SIZE 48.6 BY 1 NO-UNDO.

DEFINE VARIABLE tbServer AS LOGICAL INITIAL yes 
     LABEL "Server" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tbServer AT ROW 1.19 COL 2.8
     cMailslotName AT ROW 1.19 COL 23 COLON-ALIGNED
     cMsg AT ROW 2.33 COL 1.6 NO-LABEL
     BtnWrite AT ROW 6.91 COL 52.6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.8 BY 7.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Mailslot Demo"
         HEIGHT             = 7.1
         WIDTH              = 73
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Timer ASSIGN
       FRAME        = FRAME F-Main:HANDLE
       ROW          = 6.52
       COLUMN       = 1.6
       HEIGHT       = 1.33
       WIDTH        = 5.6
       HIDDEN       = yes
       SENSITIVE    = yes.

PROCEDURE adm-create-controls:
      Timer:NAME = "Timer":U .
/* Timer OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      Timer:MOVE-AFTER(cMsg:HANDLE IN FRAME F-Main).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Mailslot Demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Mailslot Demo */
DO:
  run CloseHandle(hMailslot,
                  output iResultCode).

  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnWrite
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnWrite W-Win
ON CHOOSE OF BtnWrite IN FRAME F-Main /* Write Message */
DO:
  WriteMailSlot().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cMsg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cMsg W-Win
ON ENTRY OF cMsg IN FRAME F-Main
DO:
  if hMailSlot = 0 then
  do:
    hMailslot = StartProcess().
    if hMailslot = {&INVALID_HANDLE} THEN
    do:
      message 'error, terminating...'.
      apply 'close' to this-procedure.
    end.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Timer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Timer W-Win
PROCEDURE Timer.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if tbServer:Checked in frame {&frame-name} then
  do:
    cMsg:insert-string( ReadMailSlot()).
    chTimer:Interval    =   {&READ_INTERVAL}.
  end.
  else
    chTimer:Enabled = false.    /* turn off timer since we are not reading */
                                /* from the mailslot if we are the client  */
                                /* in this example.                        */
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load W-Win _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-mailslot.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chTimer = Timer:COM-HANDLE
    UIB_S = chTimer:LoadControls( OCXFile, "Timer":U)
  .
  RUN DISPATCH IN THIS-PROCEDURE("initialize-controls":U) NO-ERROR.
END.
ELSE MESSAGE "w-mailslot.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win _DEFAULT-ENABLE
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
  DISPLAY tbServer cMailslotName cMsg 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE tbServer cMailslotName cMsg BtnWrite 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls W-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign
    chTimer             =   chTimer:CONTROLS:ITEM(1)
    chTimer:Interval    =   {&READ_INTERVAL}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  cComputerName = GetComputerName().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetComputerName W-Win 
FUNCTION GetComputerName RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

/*------------------------------------------------------------------------

  Function:    GetComputerName

  Description: Returns a character string of the computer name.

  History: 
          
------------------------------------------------------------------------*/
  &SCOPE MAX_COMPUTERNAME_LENGTH 16
  /* well, actually 15 but the buffers must be initialized
     to at least ({&MAX_COMPUTERNAME_LENGTH} + 1) = 16 */

  def var iResult       as integer  no-undo.
  def var iBufferSize   as integer  no-undo init {&MAX_COMPUTERNAME_LENGTH}.
  def var lpToString    as memptr   no-undo.
  def var cComputerName as char     no-undo.
  
  set-size(lpToString) = {&MAX_COMPUTERNAME_LENGTH}.

  run GetComputerName{&A} (output       lpToString,
                           input-output iBufferSize,
                           output       iResult).

  if iResult = 1 then
    assign 
    cComputerName = GET-STRING(lpToString,1).

  set-size(lpToString) = 0.

  return ( cComputerName ).
  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ReadMailSlot W-Win 
FUNCTION ReadMailSlot RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  def var cTempStr    as char no-undo.
  def var iBytesRead  as int  no-undo.
  def var iResultCode as int  no-undo.
 
  /* allocate some space */
  cTempStr = fill(' ', 512).

  run ReadFile (input  hMailslot,
                output cTempStr,
                input  512,
                output iBytesRead,
                input  0,
                output iResultCode).
                

  RETURN trim(cTempStr) + (if trim(cTempStr) = '' then '' else chr(10)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartProcess W-Win 
FUNCTION StartProcess RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define var hMailslot as   int no-undo.

  do with frame {&frame-name}:
  
    /* server create mailslot */
    if tbServer:checked then
    do:
      run CreateMailSlot{&A} (input  cMailslotName:Screen-value, /* mailslot name in the form \\.\mailslot\[path]\mailslotname */
                              input  0, /* Maximum message length, 400 max for broadcast across a network domain to multiple mail slots */
                              input  0, /* Read timeout */
                              input  0, /* security attributes */
                              output hMailSlot). /* handle to mailslot or INVALID_HANDLE_VALUE on error */
                              
      assign
      {&window-name}:title = {&window-name}:title + ' - Server'
      btnWrite:sensitive = false.
    end.
    
    /* client side */
    else
    do:
      run CreateFile{&A}( input cMailslotName:Screen-value,
                          {&GENERIC_WRITE},
                          {&FILE_SHARE_READ},
                          0,
                          {&OPEN_EXISTING},
                          {&FILE_ATTRIBUTE_NORMAL},
                          0,   
                          output hMailslot).
      assign
      {&window-name}:title = {&window-name}:title + ' - Client'.
    end.
                         
    if hMailslot = {&INVALID_HANDLE} then
      message 'Error opening mailslot.' view-as alert-box.

  end.

  RETURN hMailslot.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION WriteMailSlot W-Win 
FUNCTION WriteMailSlot RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define var iBytesWritten  as  int     no-undo.
  define var iResultCode    as  int     no-undo.

  /* Write to the mailslot */
  cMsg = "\\":U + cComputerName + " - ":U + cMsg:screen-value in frame {&frame-name}.
  
  run WriteFile(input  hMailslot,
                input  cMsg,
                input  length(cMsg) + 1,
                output iBytesWritten,
                input  0,
                output iResultCode).

  if iResultCode = 0 then
  do:
    message "Error on WriteFile.  Terminating client." view-as alert-box.
    apply "window-close" to {&window-name}.
  end.
  else
    cMsg:Screen-value = "".
  

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



