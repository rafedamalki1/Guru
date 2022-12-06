&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
    
/* Internet Mail List and the listbox.                                  */
DEFINE VARIABLE internetMail AS COM-HANDLE.
DEFINE VARIABLE mailList AS COM-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 BUTTON-SETCOLOR BUTTON-SETFONT ~
FILL-IN-HOSTNAME FILL-IN-MAILLIST FILL-IN-SENDER BUTTON-FILLMAIL EDITOR-MSG ~
BUTTON-OK BUTTON-CANCEL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-HOSTNAME FILL-IN-MAILLIST ~
FILL-IN-SENDER EDITOR-MSG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CFCommonDlg AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFCommonDlg AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CFMail AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFMail AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CFSendList AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFSendList AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-CANCEL 
     LABEL "Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-FILLMAIL 
     LABEL "Fill Mail List" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-OK AUTO-GO 
     LABEL "Send Mail" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SETCOLOR 
     LABEL "Set Color" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SETFONT 
     LABEL "Set Font" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE EDITOR-MSG AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 45 BY 3.24 NO-UNDO.

DEFINE VARIABLE FILL-IN-HOSTNAME AS CHARACTER FORMAT "X(256)":U 
     LABEL "Host Name" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAILLIST AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail List" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SENDER AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sender" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 4.05.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SETCOLOR AT ROW 2.33 COL 11
     BUTTON-SETFONT AT ROW 3.67 COL 11
     FILL-IN-HOSTNAME AT ROW 2.1 COL 43 COLON-ALIGNED
     FILL-IN-MAILLIST AT ROW 3.14 COL 43 COLON-ALIGNED
     FILL-IN-SENDER AT ROW 4.24 COL 43 COLON-ALIGNED
     BUTTON-FILLMAIL AT ROW 6.38 COL 66
     EDITOR-MSG AT ROW 9.86 COL 20 NO-LABEL
     BUTTON-OK AT ROW 10.14 COL 66
     BUTTON-CANCEL AT ROW 12.05 COL 66
     RECT-1 AT ROW 1.52 COL 8
     RECT-2 AT ROW 1.52 COL 31
     "Customize View" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.29 COL 9
     "Specify Mail Information" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 1.29 COL 32
     "Send Message To:" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 6.14 COL 2
     "Message:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 9.86 COL 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.14 ROW 1
         SIZE 81.57 BY 13.27
         CANCEL-BUTTON BUTTON-CANCEL.


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
         TITLE              = "Common Dialog/Crescent Internet ToolPak Sample"
         HEIGHT             = 12.38
         WIDTH              = 82.8
         MAX-HEIGHT         = 27.71
         MAX-WIDTH          = 146.2
         VIRTUAL-HEIGHT     = 27.71
         VIRTUAL-WIDTH      = 146.2
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CFSendList ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 6.14
       COLUMN       = 20
       HEIGHT       = 3.52
       WIDTH        = 45
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CFCommonDlg ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1.29
       COLUMN       = 2
       HEIGHT       = 1.52
       WIDTH        = 6.4
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CFMail ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 2.91
       COLUMN       = 2
       HEIGHT       = 1.43
       WIDTH        = 6.4
       HIDDEN       = yes
       SENSITIVE    = yes.
      CFSendList:NAME = "CFSendList":U .
/* CFSendList OCXINFO:CREATE-CONTROL from: {02ADEC20-91D2-101B-874B-0020AF109266} type: CSList */
      CFCommonDlg:NAME = "CFCommonDlg":U .
/* CFCommonDlg OCXINFO:CREATE-CONTROL from: {F9043C85-F6F2-101A-A3C9-08002B2F49FB} type: CommonDialog */
      CFMail:NAME = "CFMail":U .
/* CFMail OCXINFO:CREATE-CONTROL from: {25F737F3-0D72-11CF-856B-0080C7973784} type: CISMTP */
      CFSendList:MOVE-AFTER(FILL-IN-SENDER:HANDLE IN FRAME DEFAULT-FRAME).
      CFCommonDlg:MOVE-AFTER(BUTTON-CANCEL:HANDLE IN FRAME DEFAULT-FRAME).
      CFMail:MOVE-AFTER(CFCommonDlg).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Common Dialog/Crescent Internet ToolPak Sample */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Common Dialog/Crescent Internet ToolPak Sample */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-CANCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-CANCEL C-Win
ON CHOOSE OF BUTTON-CANCEL IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-FILLMAIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-FILLMAIL C-Win
ON CHOOSE OF BUTTON-FILLMAIL IN FRAME DEFAULT-FRAME /* Fill Mail List */
DO:
    /* Fill the mail list */
    SESSION:SET-WAIT-STATE("GENERAL").
    internetMail:HostName = FILL-IN-HOSTNAME:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    internetMail:MailPort = 25.
    internetMail:Sender = FILL-IN-SENDER:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    internetMail:MailList = FILL-IN-MAILLIST:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    internetMail:ExpandMailList().
    SESSION:SET-WAIT-STATE("").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-OK C-Win
ON CHOOSE OF BUTTON-OK IN FRAME DEFAULT-FRAME /* Send Mail */
DO:
    /* Send the mail */
    DEFINE VARIABLE nResult AS INTEGER INIT 0.
    DEFINE VARIABLE indx1 AS INTEGER.
    DEFINE VARIABLE indx2 AS INTEGER.
    DEFINE VARIABLE strTo AS CHAR.
    
    SESSION:SET-WAIT-STATE("GENERAL").
    internetMail:HostName = FILL-IN-HOSTNAME:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    internetMail:MailPort = 25.
    internetMail:Sender = FILL-IN-SENDER:SCREEN-VALUE IN FRAME DEFAULT-FRAME.
    
    strTo = mailList:SelText.
    /* Extract the chars between the brackets: "abc<this is recipient>xyz" */
    indx1 = INDEX(strTo, "<") + 1.
    indx2 = INDEX(strTo, ">").
    internetMail:Recipient = SUBSTRING(strTo, indx1, indx2 - indx1).
    
    internetMail:MessageSubject = "Mail Sent Using Crescent Internet ToolPak".
    internetMail:MessageBody = EDITOR-MSG:SCREEN-VALUE.
    nResult = internetMail:SendMail.
 
    IF nResult = 0 THEN
        MESSAGE "Mail Sent to " mailList:SelText VIEW-AS ALERT-BOX.
    ELSE
        MESSAGE "Failed to Send Mail to " mailList:SelText " Error " nResult VIEW-AS ALERT-BOX.

    SESSION:SET-WAIT-STATE("").
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SETCOLOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SETCOLOR C-Win
ON CHOOSE OF BUTTON-SETCOLOR IN FRAME DEFAULT-FRAME /* Set Color */
DO:
    /* Display the color dialog */
    DEFINE VARIABLE comDlg AS COM-HANDLE.
    DEFINE VARIABLE rgbval AS INTEGER.
    comDlg = chCFCommonDlg:Controls:Item(1).
    comDlg:Flags = 1.
    comDlg:ShowColor().
    rgbval = comDlg:Color.
   
    /* Use the color selection to change the background color of the message box (OCX) */
    chCFSendList:Controls:Item(1):BackColor = rgbVal.
      
    /* Use the color selection to change the background color of the message to box (Progress Widget) */
    /* First we need to create/find a color entry for the color */
    DEFINE VARIABLE colorNum AS INTEGER.
    DO colorNum = 1 TO COLOR-TABLE:NUM-ENTRIES:
        IF rgbVal = COLOR-TABLE:GET-RGB-VALUE(colorNum) THEN
           LEAVE.
    END.
   /* If we didnÊt find one, create a new color table entry */
    IF colorNum > COLOR-TABLE:NUM-ENTRIES THEN DO:
        COLOR-TABLE:NUM-ENTRIES = colorNum.
        ColorNum = colorNum - 1. 
        COLOR-TABLE:SET-DYNAMIC(colorNum, TRUE).
        COLOR-TABLE:SET-RGB-VALUE(colorNum, rgbval).
    END.

    /* Finally Set the widget color to this color table entry */
    FILL-IN-HOSTNAME:BGCOLOR = colorNum.
    FILL-IN-SENDER:BGCOLOR = colorNum.
    FILL-IN-MAILLIST:BGCOLOR = colorNum.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SETFONT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SETFONT C-Win
ON CHOOSE OF BUTTON-SETFONT IN FRAME DEFAULT-FRAME /* Set Font */
DO:
    /* Display the font dialog */
    DEFINE VARIABLE comDlg AS COM-HANDLE.    
    DEFINE VARIABLE msgFont AS COM-HANDLE.
    
    comDlg = chCFCommonDlg:Controls:Item(1).
    comDlg:Flags = 1.
    comDlg:ShowFont().
  
    /* Use the font selection to change the font of the send list box (OCX) */
    msgFont = chCFSendList:Controls:Item(1):Font.    
    msgFont:Name = comDlg:FontName.
    msgFont:Bold = comDlg:FontBold.
    msgFont:Italic = comDlg:FontItalic.
    msgFont:Size = comDlg:FontSize.      

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win _CONTROL-LOAD
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

OCXFile = SEARCH( "sendmail.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chCFCommonDlg = CFCommonDlg:COM-HANDLE
    UIB_S = chCFCommonDlg:LoadControls( OCXFile, "CFCommonDlg":U)
    chCFMail = CFMail:COM-HANDLE
    UIB_S = chCFMail:LoadControls( OCXFile, "CFMail":U)
    chCFSendList = CFSendList:COM-HANDLE
    UIB_S = chCFSendList:LoadControls( OCXFile, "CFSendList":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, sendmail.wrx, could not be found." skip
             "The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY FILL-IN-HOSTNAME FILL-IN-MAILLIST FILL-IN-SENDER EDITOR-MSG 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 BUTTON-SETCOLOR BUTTON-SETFONT FILL-IN-HOSTNAME 
         FILL-IN-MAILLIST FILL-IN-SENDER BUTTON-FILLMAIL EDITOR-MSG BUTTON-OK 
         BUTTON-CANCEL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /* Set the variables for the Internet Mail List and the listbox to the
       respective controls. */
    internetMail = chCFMail:Controls:Item(1).
    mailList = chCFSendList:Controls:Item(1).

    /* Hook up the Internet Mail List and the listbox */
    internetMail:ListBoxName = mailList.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


