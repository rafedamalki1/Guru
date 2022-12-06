
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

PROCEDURE LockWindowUpdate EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER hWndLock AS LONG.
  DEFINE RETURN PARAMETER IsLocked AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOGGLE-lock BUTTON-move TOGGLE-hide ~
FILL-IN-elapsed 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-lock RADIO-SET-before TOGGLE-hide ~
FILL-IN-elapsed 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-move 
     LABEL "Move widgets" 
     SIZE 19 BY 1.14 TOOLTIP "Moves the widgets around and shows how much time it takes".

DEFINE VARIABLE FILL-IN-elapsed AS CHARACTER FORMAT "X(256)":U 
     LABEL "milliseconds" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE RADIO-SET-before AS LOGICAL 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Hide before LockWindowUpdate", Yes,
"Hide during LockWindowUpdate", No
     SIZE 36 BY 1.67 TOOLTIP "watch effect on the desktop. no SetWindowPos allowed during LockWindowUpdate" NO-UNDO.

DEFINE VARIABLE TOGGLE-hide AS LOGICAL INITIAL no 
     LABEL "hide frame during move" 
     VIEW-AS TOGGLE-BOX
     SIZE 27 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-lock AS LOGICAL INITIAL no 
     LABEL "use LockWindowUpdate" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 10" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-11 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 11" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 2" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 4" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 5" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 6" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 7" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 8" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE COMBO-BOX-9 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Combo 9" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 10" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 11" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 12" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 13" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-14 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 14" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-15 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 15" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 16" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-17 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 17" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-18 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 18" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-19 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 19" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 20" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 3" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 4" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 6" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 7" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 8" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 9" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-10 AS LOGICAL INITIAL no 
     LABEL "Toggle 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 14.6 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "Toggle 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "Toggle 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL no 
     LABEL "Toggle 4" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL no 
     LABEL "Toggle 5" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-6 AS LOGICAL INITIAL no 
     LABEL "Toggle 6" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-7 AS LOGICAL INITIAL no 
     LABEL "Toggle 7" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-8 AS LOGICAL INITIAL no 
     LABEL "Toggle 8" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-9 AS LOGICAL INITIAL no 
     LABEL "Toggle 9" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOGGLE-lock AT ROW 1.24 COL 25
     RADIO-SET-before AT ROW 1.24 COL 61 NO-LABEL
     BUTTON-move AT ROW 1.52 COL 4
     TOGGLE-hide AT ROW 2.19 COL 25
     FILL-IN-elapsed AT ROW 19.33 COL 17 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.8 BY 19.19.

DEFINE FRAME FRAME-widgets
     COMBO-BOX-2 AT ROW 1.95 COL 25 COLON-ALIGNED
     FILL-IN-11 AT ROW 1.95 COL 63 COLON-ALIGNED
     FILL-IN-3 AT ROW 2.43 COL 40 COLON-ALIGNED
     FILL-IN-1 AT ROW 2.67 COL 16 COLON-ALIGNED
     TOGGLE-7 AT ROW 3.14 COL 61
     TOGGLE-4 AT ROW 3.38 COL 34
     FILL-IN-2 AT ROW 3.62 COL 8 COLON-ALIGNED
     COMBO-BOX-10 AT ROW 3.62 COL 71 COLON-ALIGNED
     COMBO-BOX-11 AT ROW 4.1 COL 53 COLON-ALIGNED
     FILL-IN-4 AT ROW 4.33 COL 35 COLON-ALIGNED
     COMBO-BOX-7 AT ROW 4.57 COL 19 COLON-ALIGNED
     TOGGLE-1 AT ROW 4.57 COL 68
     TOGGLE-5 AT ROW 5.52 COL 10
     COMBO-BOX-1 AT ROW 5.52 COL 42 COLON-ALIGNED
     FILL-IN-19 AT ROW 5.52 COL 74 COLON-ALIGNED
     FILL-IN-6 AT ROW 6.48 COL 36 COLON-ALIGNED
     FILL-IN-5 AT ROW 6.71 COL 10 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 6.95 COL 56 COLON-ALIGNED
     COMBO-BOX-6 AT ROW 7.19 COL 69 COLON-ALIGNED
     COMBO-BOX-8 AT ROW 7.67 COL 34 COLON-ALIGNED
     TOGGLE-10 AT ROW 7.91 COL 7
     FILL-IN-18 AT ROW 8.14 COL 71 COLON-ALIGNED
     FILL-IN-8 AT ROW 8.86 COL 30 COLON-ALIGNED
     FILL-IN-7 AT ROW 9.1 COL 8 COLON-ALIGNED
     FILL-IN-10 AT ROW 9.1 COL 56 COLON-ALIGNED
     COMBO-BOX-5 AT ROW 9.81 COL 69 COLON-ALIGNED
     TOGGLE-9 AT ROW 10.29 COL 36
     TOGGLE-2 AT ROW 10.76 COL 51
     TOGGLE-6 AT ROW 11 COL 14
     COMBO-BOX-3 AT ROW 11.48 COL 7 COLON-ALIGNED NO-LABEL
     TOGGLE-3 AT ROW 11.48 COL 33
     FILL-IN-17 AT ROW 11.48 COL 71 COLON-ALIGNED
     COMBO-BOX-9 AT ROW 11.71 COL 51 COLON-ALIGNED
     FILL-IN-12 AT ROW 12.43 COL 16 COLON-ALIGNED
     FILL-IN-16 AT ROW 12.67 COL 67 COLON-ALIGNED
     FILL-IN-13 AT ROW 12.91 COL 41 COLON-ALIGNED
     FILL-IN-14 AT ROW 13.62 COL 12 COLON-ALIGNED
     COMBO-BOX-4 AT ROW 14.1 COL 53 COLON-ALIGNED
     FILL-IN-20 AT ROW 14.1 COL 70 COLON-ALIGNED
     FILL-IN-15 AT ROW 14.57 COL 35 COLON-ALIGNED
     TOGGLE-8 AT ROW 14.81 COL 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 3.14
         SIZE 94 BY 15.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
	Last change:  JD    1 Nov 98    3:26 am
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "LockWindowUpdate test/demo"
         HEIGHT             = 19.19
         WIDTH              = 98.8
         MAX-HEIGHT         = 19.19
         MAX-WIDTH          = 98.8
         VIRTUAL-HEIGHT     = 19.19
         VIRTUAL-WIDTH      = 98.8
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-widgets:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-widgets:MOVE-AFTER-TAB-ITEM (TOGGLE-hide:HANDLE IN FRAME DEFAULT-FRAME)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR RADIO-SET RADIO-SET-before IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FRAME-widgets
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* LockWindowUpdate test/demo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* LockWindowUpdate test/demo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-move
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-move C-Win
ON CHOOSE OF BUTTON-move IN FRAME DEFAULT-FRAME /* Move widgets */
DO:

  DEF VAR IsLocked  AS INTEGER NO-UNDO INITIAL 0.
  DEF VAR starttime AS INTEGER NO-UNDO.
    
  ASSIGN toggle-lock.
  ASSIGN toggle-hide.
  ASSIGN radio-set-before.

  starttime = ETIME(NO).
  
  IF toggle-hide AND radio-set-before then
     FRAME FRAME-widgets:VISIBLE = NO.

  IF toggle-lock then 
     RUN LockWindowUpdate(FRAME FRAME-widgets:HWND, OUTPUT IsLocked).

  IF toggle-hide AND (NOT radio-set-before) then
     FRAME FRAME-widgets:VISIBLE = NO.
     
  run MoveWidgets.

  IF toggle-hide AND (NOT radio-set-before) then
     FRAME FRAME-widgets:VISIBLE = YES.

  IF IsLocked NE 0 THEN 
     RUN LockWindowUpdate( 0, OUTPUT IsLocked).

  IF toggle-hide AND radio-set-before then
     FRAME FRAME-widgets:VISIBLE = YES.

  fill-in-elapsed:SCREEN-VALUE = STRING(ETIME(NO) - starttime, ">,>>>,>>9").
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-hide
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-hide C-Win
ON VALUE-CHANGED OF TOGGLE-hide IN FRAME DEFAULT-FRAME /* hide frame during move */
DO:
  ASSIGN {&SELF-NAME}.
  RADIO-SET-before:SENSITIVE = {&SELF-NAME}.
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
  DISPLAY TOGGLE-lock RADIO-SET-before TOGGLE-hide FILL-IN-elapsed 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TOGGLE-lock BUTTON-move TOGGLE-hide FILL-IN-elapsed 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY COMBO-BOX-2 FILL-IN-11 FILL-IN-3 FILL-IN-1 TOGGLE-7 TOGGLE-4 FILL-IN-2 
          COMBO-BOX-10 COMBO-BOX-11 FILL-IN-4 COMBO-BOX-7 TOGGLE-1 TOGGLE-5 
          COMBO-BOX-1 FILL-IN-19 FILL-IN-6 FILL-IN-5 FILL-IN-9 COMBO-BOX-6 
          COMBO-BOX-8 TOGGLE-10 FILL-IN-18 FILL-IN-8 FILL-IN-7 FILL-IN-10 
          COMBO-BOX-5 TOGGLE-9 TOGGLE-2 TOGGLE-6 COMBO-BOX-3 TOGGLE-3 FILL-IN-17 
          COMBO-BOX-9 FILL-IN-12 FILL-IN-16 FILL-IN-13 FILL-IN-14 COMBO-BOX-4 
          FILL-IN-20 FILL-IN-15 TOGGLE-8 
      WITH FRAME FRAME-widgets IN WINDOW C-Win.
  ENABLE COMBO-BOX-2 FILL-IN-11 FILL-IN-3 FILL-IN-1 TOGGLE-7 TOGGLE-4 FILL-IN-2 
         COMBO-BOX-10 COMBO-BOX-11 FILL-IN-4 COMBO-BOX-7 TOGGLE-1 TOGGLE-5 
         COMBO-BOX-1 FILL-IN-19 FILL-IN-6 FILL-IN-5 FILL-IN-9 COMBO-BOX-6 
         COMBO-BOX-8 TOGGLE-10 FILL-IN-18 FILL-IN-8 FILL-IN-7 FILL-IN-10 
         COMBO-BOX-5 TOGGLE-9 TOGGLE-2 TOGGLE-6 COMBO-BOX-3 TOGGLE-3 FILL-IN-17 
         COMBO-BOX-9 FILL-IN-12 FILL-IN-16 FILL-IN-13 FILL-IN-14 COMBO-BOX-4 
         FILL-IN-20 FILL-IN-15 TOGGLE-8 
      WITH FRAME FRAME-widgets IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-widgets}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveWidgets C-Win 
PROCEDURE MoveWidgets :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR hh AS HANDLE NO-UNDO.
  hh = FRAME FRAME-widgets:FIRST-CHILD.
  hh = hh:FIRST-CHILD.
  
  DO WHILE VALID-HANDLE(hh) :
     ASSIGN hh:X = hh:X + RANDOM(-2, 2)
            hh:Y = hh:Y + RANDOM(-2, 2)
            hh:WIDTH-PIXELS = hh:WIDTH-PIXELS + RANDOM(-2, 2)
            hh:HEIGHT-PIXELS = hh:HEIGHT-PIXELS + RANDOM(-2, 2)
            NO-ERROR.
            
     hh = hh:NEXT-SIBLING.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




