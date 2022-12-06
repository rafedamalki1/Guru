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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-1 vMessage 
&Scoped-Define DISPLAYED-OBJECTS vMessage 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE ListView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chListView AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Mail AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chMail AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE Messages AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chMessages AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "&Read" 
     SIZE 15 BY 1.24.

DEFINE BUTTON NewMailButton 
     LABEL "&New" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE vMessage AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 79.2 BY 6.33 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-1 AT ROW 1 COL 81
     NewMailButton AT ROW 2.29 COL 81.2
     vMessage AT ROW 11.19 COL 1 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.2 BY 16.57.


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
         TITLE              = "Mail Demo"
         HEIGHT             = 16.57
         WIDTH              = 95.4
         MAX-HEIGHT         = 24.67
         MAX-WIDTH          = 132.8
         VIRTUAL-HEIGHT     = 24.67
         VIRTUAL-WIDTH      = 132.8
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
                                                                        */
/* SETTINGS FOR BUTTON NewMailButton IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Messages ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 1
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME ListView ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 1
       HEIGHT       = 10.19
       WIDTH        = 79.4
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME Mail ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1
       COLUMN       = 8.8
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.
      Messages:NAME = "Messages":U .
/* Messages OCXINFO:CREATE-CONTROL from: {20C62CAB-15DA-101B-B9A8-444553540000} type: MAPIMessages */
      ListView:NAME = "ListView":U .
/* ListView OCXINFO:CREATE-CONTROL from: {58DA8D8A-9D6A-101B-AFC0-4210102A8DA7} type: ListView */
      Mail:NAME = "Mail":U .
/* Mail OCXINFO:CREATE-CONTROL from: {20C62CA0-15DA-101B-B9A8-444553540000} type: MAPISession */
      Messages:MOVE-BEFORE(BUTTON-1:HANDLE IN FRAME DEFAULT-FRAME).
      ListView:MOVE-AFTER(Messages).
      Mail:MOVE-AFTER(ListView).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Mail Demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Mail Demo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Read */
DO:
 DEF VAR i AS INT NO-UNDO.
 
 DEF VAR vNewItem AS COM-HANDLE NO-UNDO.
 
  ASSIGN chMail:NewSession = NO
         chMail:UserName = "Julian Lyndon-Smith"
         chMail:DownloadMail = NO
         chMail:Password = "m107kwc".
         
  chMail:SignOn().
  
  ASSIGN chMessages:SessionID = chMail:SessionID.
  
  chMessages:Fetch().
  
  DO i = 0 TO chMessages:MsgCount - 1:
   ASSIGN chMessages:MsgIndex = i.
   
   ASSIGN vNewItem = chListView:ListItems:Add(,"M:" + STRING(i),chMessages:MsgDateReceived)
          vNewItem:SubItems(1) = chMessages:MsgSubject
          vNewItem:SubItems(2) = chMessages:MsgOrigDisplayName.
 END.  
 
 ASSIGN NewMailButton:SENSITIVE = YES.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListView C-Win
PROCEDURE ListView.ListView.ItemClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/

 DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.
 
 ASSIGN chMessages:MsgIndex = INT(ENTRY(2,p-Item:Key,":":U))
        vMessage:SCREEN-VALUE IN FRAME {&FRAME-NAME} = chMessages:MsgNoteText.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NewMailButton
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NewMailButton C-Win
ON CHOOSE OF NewMailButton IN FRAME DEFAULT-FRAME /* New */
DO:
  RUN SendMail.w (chMessages).
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

OCXFile = SEARCH( "mail.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chListView = ListView:COM-HANDLE
    UIB_S = chListView:LoadControls( OCXFile, "ListView":U)
    chMail = Mail:COM-HANDLE
    UIB_S = chMail:LoadControls( OCXFile, "Mail":U)
    chMessages = Messages:COM-HANDLE
    UIB_S = chMessages:LoadControls( OCXFile, "Messages":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, mail.wrx, could not be found." skip
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
  DISPLAY vMessage 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-1 vMessage 
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
 ASSIGN chMail     = chMail:CONTROLS:ITEM(1)
        chMessages = chMessages:CONTROLS:ITEM(1)
        chListView = chListView:CONTROLS:ITEM(1).
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


