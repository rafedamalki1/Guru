&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.      */
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
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Definitions for FRAME frame1                                         */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-3 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-3 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BUTTON-2 
     LABEL "Button 2" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BUTTON-3 
     LABEL "Button 3" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 35 BY 16.13 NO-UNDO.


/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52 BY 16 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.63 BY 18.58.

DEFINE FRAME frame1
     BROWSE-1 AT ROW 1 COL 1
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.75 ROW 2.25
         SIZE 52.5 BY 17
         TITLE "frame1".

DEFINE FRAME frame2
     EDITOR-1 AT ROW 1 COL 1.5 NO-LABEL
     BUTTON-1 AT ROW 2 COL 37
     BUTTON-2 AT ROW 3.5 COL 37
     BUTTON-3 AT ROW 5 COL 37
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.75 ROW 2.25
         SIZE 52.5 BY 17
         TITLE "frame2".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 18.58
         WIDTH              = 90.63
         MAX-HEIGHT         = 18.58
         MAX-WIDTH          = 90.63
         VIRTUAL-HEIGHT     = 18.58
         VIRTUAL-WIDTH      = 90.63
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
/* REPARENT FRAME */
ASSIGN FRAME frame1:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME frame2:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME frame1
                                                                        */
/* BROWSE-TAB BROWSE-1 1 frame1 */
/* SETTINGS FOR FRAME frame2
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 50
       HEIGHT          = 4.17
       WIDTH           = 12.5
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-3 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.25
       COLUMN          = 1.5
       HEIGHT          = 18.25
       WIDTH           = 53
       HIDDEN          = no
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.17
       COLUMN          = 55.5
       HEIGHT          = 17.25
       WIDTH           = 35
       HIDDEN          = no
       SENSITIVE       = yes.
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {2C247F23-8591-11D1-B16A-00C0F0283628} type: ImageList */
      CtrlFrame-3:NAME = "CtrlFrame-3":U .
/* CtrlFrame-3 OCXINFO:CREATE-CONTROL from: {1EFB6596-857C-11D1-B16A-00C0F0283628} type: TabStrip */
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {C74190B6-8589-11D1-B16A-00C0F0283628} type: TreeView */
      CtrlFrame-2:MOVE-BEFORE(FRAME frame1:HANDLE).
      CtrlFrame-3:MOVE-AFTER(CtrlFrame-2).
      CtrlFrame:MOVE-AFTER(CtrlFrame-3).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-3 C-Win OCX.Click
PROCEDURE CtrlFrame-3.TabStrip.Click .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
   IF chCtrlFrame-3:TabStrip:Tabs(1):SELECTED = TRUE THEN DO:
      ASSIGN 
      FRAME frame1:HIDDEN = FALSE.
      FRAME frame2:HIDDEN = TRUE.
      FRAME frame1:MOVE-TO-TOP ( ).
   END.
   ELSE IF chCtrlFrame-3:TabStrip:Tabs(2):SELECTED = TRUE THEN DO:
      ASSIGN 
      FRAME frame1:HIDDEN = TRUE.
      FRAME frame2:HIDDEN = FALSE. 
      FRAME frame2:MOVE-TO-TOP ( ).
   END.
   ELSE DO:
      ASSIGN 
      FRAME frame1:HIDDEN = TRUE.
      FRAME frame2:HIDDEN = TRUE.     
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
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
  RUN loadlist_UI.
  RUN loadtab_UI.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
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

OCXFile = SEARCH( "xtreeview.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
    chCtrlFrame-3 = CtrlFrame-3:COM-HANDLE
    UIB_S = chCtrlFrame-3:LoadControls( OCXFile, "CtrlFrame-3":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "xtreeview.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW FRAME frame1 IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frame1}
  DISPLAY EDITOR-1 
      WITH FRAME frame2 IN WINDOW C-Win.
  ENABLE EDITOR-1 BUTTON-1 BUTTON-2 BUTTON-3 
      WITH FRAME frame2 IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frame2}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadlist_UI C-Win 
PROCEDURE loadlist_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE vkey AS CHARACTER NO-UNDO.
  DEFINE VARIABLE chNode AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chNextNode AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE chNextNode2 AS COM-HANDLE NO-UNDO.
/*Ladda bilder runtime - funkar ej*/
/*   DEFINE VARIABLE oPic AS COM-HANDLE NO-UNDO.                                 */
/*   assign                                                                      */
/*   chCtrlFrame-2:ImageHeight = 24                                              */
/*   chCtrlFrame-2:ImageWidth = 24.                                              */
/*   chCtrlFrame-2:ListImages:ADD(1,'Pic':U + STRING(1),"wx/Bilder/folder.gif"). */
/*   oPic = LOAD-PICTURE(SEARCH(ENTRY(1,"wx/Bilder/folder.gif"))).               */
/*   chCtrlFrame-2:ListImages:add(1,'Pic':U + string(1),oPic).                   */
/*   RELEASE OBJECT oPic NO-ERROR.                                               */


  chCtrlFrame:TreeView:IMAGELIST = chCtrlFrame-2:ImageList.
/*1 - Relative     Index or Key of existing Node
  2 - Relationship Relative Placement # of new Node in Relation To node defined by (1)
    # = 1 (Last) Node is placed after all other nodes of same level
        2 (Next) Node is placed after node defined by (1)
        3 (Prev) Node is placed before node defined by (1)
        4 (Child) [Default] Node becomes a child of node defined by (1)
  3 - Unique String Key (Unique in entire tree view)
  4 - Text String for node [this is the only required parameter]
  5 - Image Index of image in imagelist for node
  6 - SelectedImage Index of image in imagelist for selected Node
   Add three child nodes to root I have created */

  assign
  chNode = chCtrlFrame:TreeView:Nodes:Add(,2,vkey,"Meny",1,1).
/*   chNode:Text = "1st Node". */

  assign
  chNextNode = chCtrlFrame:TreeView:Nodes:Add(chNode,4,vkey,"Dir1",1,1)
  chNextNode2 = chCtrlFrame:TreeView:Nodes:Add(chNextNode,4,vkey,"Flik1",1,1)
  chNextNode2 = chCtrlFrame:TreeView:Nodes:Add(chNextNode,4,vkey,"Flik2",1,1)
  chNextNode2 = chCtrlFrame:TreeView:Nodes:Add(chNextNode,4,vkey,"Flik3",1,1)
  chNextNode2 = chCtrlFrame:TreeView:Nodes:Add(chNextNode,4,vkey,"Flik4",1,1)
  chNextNode = chCtrlFrame:TreeView:Nodes:Add(chNode,4,vkey,"Dir2",1,1).
  chNextNode = chCtrlFrame:TreeView:Nodes:Add(chNode,4,vkey,"Dir3",1,1).
/*   chNode:Expanded = TRUE. */

/*assign chCtrlFrame:TabStrip:ImageList = chCtrlFrame-2:ImageList.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadtab_UI C-Win 
PROCEDURE loadtab_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
/*   assign chCtrlFrame-3:TabStrip:IMAGELIST = chCtrlFrame-2:ImageList. */
/*   chCtrlFrame-3:TabStrip:Tabs:Item(2):Selected = True.               */
  
   chCtrlFrame-3:TabStrip:Tabs:CLEAR().
   DO i = 1 TO 2:
      CASE i:
         WHEN 1 THEN DO:
            chCtrlFrame-3:TabStrip:Tabs:ADD(i,"frame1","frame1").
            vTabFrameHndl[i] = FRAME frame1:HANDLE.
         END.
         WHEN 2 THEN DO:
            chCtrlFrame-3:TabStrip:Tabs:ADD(i,"frame2","frame2").
            vTabFrameHndl[i] = FRAME frame2:HANDLE.
         END.
      END CASE.
   END.
   
   /*Test*/
   chCtrlFrame-3:TabStrip:TABS:Add(3).
   chCtrlFrame-3:TabStrip:TABS:Add(4).
   
   ASSIGN chCtrlFrame-3:TabStrip:tabs(3):caption = "Details":U
   chCtrlFrame-3:TabStrip:tabs(4):caption = "Units":U .
   RUN CtrlFrame-3.TabStrip.Click.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

