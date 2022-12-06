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
&Scoped-Define ENABLED-OBJECTS Btn_Done 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CFImagelist AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFImagelist AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CFListView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFListView AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CFTabStrip AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFTabStrip AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CFTreeView AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFTreeView AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Exit" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Done AT ROW 11.81 COL 33
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16
         DEFAULT-BUTTON Btn_Done.


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
         TITLE              = "Sports database"
         HEIGHT             = 13.48
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CFImagelist ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1.24
       COLUMN       = 71
       HEIGHT       = 1.81
       WIDTH        = 7.6
       HIDDEN       = yes
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CFTabStrip ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 1.48
       COLUMN       = 2
       HEIGHT       = 12.62
       WIDTH        = 78
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CFTreeView ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 3.62
       COLUMN       = 7
       HEIGHT       = 10
       WIDTH        = 69
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CFListView ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 3.62
       COLUMN       = 7
       HEIGHT       = 7.62
       WIDTH        = 69
       HIDDEN       = no
       SENSITIVE    = yes.
      CFImagelist:NAME = "CFImagelist":U .
/* CFImagelist OCXINFO:CREATE-CONTROL from: {58DA8D8F-9D6A-101B-AFC0-4210102A8DA7} type: ImageList */
      CFTabStrip:NAME = "CFTabStrip":U .
/* CFTabStrip OCXINFO:CREATE-CONTROL from: {9ED94440-E5E8-101B-B9B5-444553540000} type: TabStrip */
      CFTreeView:NAME = "CFTreeView":U .
/* CFTreeView OCXINFO:CREATE-CONTROL from: {0713E8A2-850A-101B-AFC0-4210102A8DA7} type: TreeView */
      CFListView:NAME = "CFListView":U .
/* CFListView OCXINFO:CREATE-CONTROL from: {58DA8D8A-9D6A-101B-AFC0-4210102A8DA7} type: ListView */
      CFImagelist:MOVE-BEFORE(Btn_Done:HANDLE IN FRAME DEFAULT-FRAME).
      CFTabStrip:MOVE-AFTER(CFImagelist).
      CFTreeView:MOVE-AFTER(CFTabStrip).
      CFListView:MOVE-AFTER(CFTreeView).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Sports database */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Sports database */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Exit */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CFListView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFListView C-Win
PROCEDURE CFListView.ListView.ItemClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Item
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Item AS COM-HANDLE NO-UNDO.
DEFINE VAR tblName AS CHARACTER.

tblName = p-Item:Text.
RUN dTable.w(tblName).
RELEASE OBJECT p-Item.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CFTabStrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFTabStrip C-Win
PROCEDURE CFTabStrip.TabStrip.MouseDown .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Button
    Shift
    x
    y
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Button AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-Shift  AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-x      AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-y      AS INTEGER NO-UNDO.

IF p-Button <> 2 THEN DO:
    IF (p-x) < 105 THEN DO:
        /* Show ListView, hide TreeView */
        CFTreeView:Visible = NO.
        CFListView:Visible = YES.
    END.
    IF (p-x) > 108 THEN DO:
        /* Show TreeView, hide ListView  */
        CFTreeView:Visible = YES.
        CFListView:Visible = NO.
    END.
 END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
       
/* Hide TreeView control's frame */     
CFTreeView:Visible = no.

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

OCXFile = SEARCH( "dbTest.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chCFImagelist = CFImagelist:COM-HANDLE
    UIB_S = chCFImagelist:LoadControls( OCXFile, "CFImagelist":U)
    chCFListView = CFListView:COM-HANDLE
    UIB_S = chCFListView:LoadControls( OCXFile, "CFListView":U)
    chCFTabStrip = CFTabStrip:COM-HANDLE
    UIB_S = chCFTabStrip:LoadControls( OCXFile, "CFTabStrip":U)
    chCFTreeView = CFTreeView:COM-HANDLE
    UIB_S = chCFTreeView:LoadControls( OCXFile, "CFTreeView":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, dbTest.wrx, could not be found." skip
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
  ENABLE Btn_Done 
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
DEFINE VARIABLE ix1 AS INTEGER INIT 0.
DEFINE VARIABLE ix2 AS INTEGER INIT 0.
DEFINE VARIABLE ix3 AS INTEGER INIT 0.

CFTabStrip:MOVE-TO-BOTTOM().

/* *** Initialize ListView Control **** */

chCFListView:ListView:View = 0.

/* The image list icons have been set through the Custom dialog off
   of the Property sheet */
chCFListView:ListView:Icons = chCFImagelist:ImageList.

FOR EACH _file WHERE not _file._hidden:
    chCFListView:ListView:ListItems:Add(,,_file-name,1,).
END.
    
/* *** Initialize TreeView Control **** */
    
chCFTreeView:TreeView:LineStyle = 1. /* RootLines */
chCFTreeView:TreeView:Style = 7. /* Lines, plus/minus, image, and text */
chCFTreeView:TreeView:ImageList = chCFImagelist:ImageList.
chCFTreeView:TreeView:Nodes:Add(, ,"r" , "Sports database", 2,).
chCFTreeView:TreeView:Nodes:Add("r",4 ,"t-s" , "Tables", 5,).
chCFTreeView:TreeView:Nodes:Add("r",4 ,"s-s" , "Sequences", 8,).   

FOR EACH _file WHERE not _file._hidden:
    chCFTreeView:TreeView:Nodes:Add("t-s", 4, "t" + STRING(ix1),
                                    _file-name + " table", 1,).
    chCFTreeView:TreeView:Nodes:Add("t" + STRING(ix1), 4, "f-s" + STRING(ix1),
                                    "Fields", 3,).
    chCFTreeView:TreeView:Nodes:Add("t" + STRING(ix1), 4, "i-s" + STRING(ix1),
                                    "Indexes", 4,).
                                                                  
    FOR EACH _field OF _file:
        chCFTreeView:TreeView:Nodes:Add("f-s" + STRING(ix1), 4, 
                                        "f" + STRING(ix2), _field-name, 6,).
        ix2 = ix2 + 1. 
    END.
    
    FOR EACH _index OF _file:
        chCFTreeView:TreeView:Nodes:Add("i-s" + STRING(ix1), 4, 
                                        "i" + STRING(ix3), _index-name, 7,).
        ix3 = ix3 + 1. 
    END.

    ix1 = ix1 + 1. 
END. 

ix1 = 0.
FOR EACH _sequence:
    chCFTreeView:TreeView:Nodes:Add("s-s", 4, "s" + STRING(ix1), _seq-name, 9,).
    ix1 = ix1 + 1.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


