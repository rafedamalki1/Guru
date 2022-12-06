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
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Definitions for FRAME FRAME-A                                        */

/* Definitions for FRAME FRAME-B                                        */

/* Definitions for FRAME FRAME-C                                        */

/* Definitions for FRAME FRAME-E                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-8 BUTTON-9 BUTTON-4 BUTTON-5 BUTTON-6 ~
RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-4  NO-FOCUS FLAT-BUTTON
     LABEL "Stolpar" 
     SIZE 8.5 BY .92
     FONT 0.

DEFINE BUTTON BUTTON-5  NO-FOCUS FLAT-BUTTON
     LABEL "Transformatorer" 
     SIZE 15 BY .92
     BGCOLOR 9 FGCOLOR 9 FONT 0.

DEFINE BUTTON BUTTON-6  NO-FOCUS FLAT-BUTTON
     LABEL "Apparater" 
     SIZE 9.88 BY .92
     FONT 0.

DEFINE BUTTON BUTTON-8  NO-FOCUS FLAT-BUTTON
     LABEL "Övriga" 
     SIZE 9.88 BY .92
     BGCOLOR 3 FONT 0.

DEFINE BUTTON BUTTON-9  NO-FOCUS FLAT-BUTTON
     LABEL "Bilder" 
     SIZE 9.88 BY .92
     FONT 0.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 86.5 BY 17.

DEFINE BUTTON BUTTON-1 
     LABEL "Ny Stolpe" 
     SIZE 11 BY .92.

DEFINE BUTTON BUTTON-2 
     LABEL "Ändra" 
     SIZE 7.5 BY .92.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transformator" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-7 
     LABEL "Ny apparat" 
     SIZE 11 BY .92.

DEFINE BUTTON BUTTON-11  NO-FOCUS FLAT-BUTTON
     LABEL "Välj" 
     SIZE 5.5 BY .92
     FONT 0.

DEFINE BUTTON BUTTON-12  NO-FOCUS FLAT-BUTTON
     LABEL "Använd" 
     SIZE 7.5 BY .92
     FONT 0.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 57.5 BY 8.5 NO-UNDO.


/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 35.5 BY 11.75 EXPANDABLE.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 62 BY 9 EXPANDABLE.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 37 BY 11.25 EXPANDABLE.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 33 BY 7.5 EXPANDABLE.

DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 82.5 BY 12.25 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-8 AT ROW 1 COL 38.38
     BUTTON-9 AT ROW 1 COL 48.25
     BUTTON-4 AT ROW 1 COL 1
     BUTTON-5 AT ROW 1 COL 9.5
     BUTTON-6 AT ROW 1 COL 24.63
     RECT-2 AT ROW 2 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.63 BY 18.04.

DEFINE FRAME FRAME-B
     FILL-IN-1 AT ROW 1.5 COL 15 COLON-ALIGNED
     BROWSE-2 AT ROW 3 COL 2
     BUTTON-2 AT ROW 12.25 COL 31.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 85.5 BY 16.67.

DEFINE FRAME FRAME-E
     BROWSE-7 AT ROW 2.75 COL 1.5
     "Bilder" VIEW-AS TEXT
          SIZE 10.5 BY 1.25 AT ROW 1.33 COL 33.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 85.5 BY 16.67.

DEFINE FRAME FRAME-D
     BUTTON-11 AT ROW 1 COL 1.13
     EDITOR-1 AT ROW 4.5 COL 13 NO-LABEL
     BUTTON-12 AT ROW 1 COL 6.63
     "Övriga" VIEW-AS TEXT
          SIZE 8 BY 1.25 AT ROW 3 COL 37.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 85.5 BY 16.67.

DEFINE FRAME FRAME-C
     BROWSE-3 AT ROW 1.25 COL 2
     BROWSE-4 AT ROW 1.25 COL 46
     BUTTON-7 AT ROW 13 COL 15
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 85.5 BY 16.67.

DEFINE FRAME FRAME-A
     BROWSE-1 AT ROW 1.25 COL 2
     BUTTON-1 AT ROW 13.25 COL 14.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 85.5 BY 16.67.


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
         HEIGHT             = 18.17
         WIDTH              = 87
         MAX-HEIGHT         = 19.67
         MAX-WIDTH          = 114.5
         VIRTUAL-HEIGHT     = 19.67
         VIRTUAL-WIDTH      = 114.5
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
ASSIGN FRAME FRAME-A:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-B:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-C:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-D:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-E:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BROWSE-1 1 FRAME-A */
ASSIGN 
       FRAME FRAME-A:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BROWSE-2 FILL-IN-1 FRAME-B */
ASSIGN 
       FRAME FRAME-B:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* BROWSE-TAB BROWSE-3 1 FRAME-C */
/* BROWSE-TAB BROWSE-4 BROWSE-3 FRAME-C */
ASSIGN 
       FRAME FRAME-C:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-D
                                                                        */
ASSIGN 
       FRAME FRAME-D:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-E
                                                                        */
/* BROWSE-TAB BROWSE-7 1 FRAME-E */
ASSIGN 
       FRAME FRAME-E:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



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


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* Stolpar */
DO:
   RUN goma_UI (INPUT 1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* Transformatorer */
DO:
   RUN goma_UI (INPUT 2).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-6 C-Win
ON CHOOSE OF BUTTON-6 IN FRAME DEFAULT-FRAME /* Apparater */
DO:
   RUN goma_UI (INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 C-Win
ON CHOOSE OF BUTTON-8 IN FRAME DEFAULT-FRAME /* Övriga */
DO:
   RUN goma_UI (INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 C-Win
ON CHOOSE OF BUTTON-9 IN FRAME DEFAULT-FRAME /* Bilder */
DO:
   RUN goma_UI (INPUT 5).
END.

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
/*   BUTTON-4:LOAD-MOUSE-POINTER("GLOVE":U). */
/*   BUTTON-5:LOAD-MOUSE-POINTER("GLOVE":U). */
/*   BUTTON-6:LOAD-MOUSE-POINTER("GLOVE":U). */
  RUN enable_UI.
  FRAME FRAME-A:HIDDEN = FALSE.
  FRAME FRAME-B:HIDDEN = TRUE.
  FRAME FRAME-C:HIDDEN = TRUE.
  RUN goma_UI (INPUT 1).
  ASSIGN BUTTON-4:LABEL = "<Stolpar>".
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  ENABLE BUTTON-8 BUTTON-9 BUTTON-4 BUTTON-5 BUTTON-6 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BUTTON-1 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  VIEW FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY FILL-IN-1 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  ENABLE FILL-IN-1 BUTTON-2 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  ENABLE BUTTON-7 
      WITH FRAME FRAME-C IN WINDOW C-Win.
  VIEW FRAME FRAME-C IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
  DISPLAY EDITOR-1 
      WITH FRAME FRAME-D IN WINDOW C-Win.
  ENABLE BUTTON-11 EDITOR-1 BUTTON-12 
      WITH FRAME FRAME-D IN WINDOW C-Win.
  VIEW FRAME FRAME-D IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-D}
  VIEW FRAME FRAME-E IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-E}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
   &Scoped-define FRAME-NAME DEFAULT-FRAME
   IF val = 1 THEN DO:
     FRAME FRAME-A:HIDDEN = FALSE.
     FRAME FRAME-B:HIDDEN = TRUE.    
     FRAME FRAME-C:HIDDEN = TRUE.
     FRAME FRAME-D:HIDDEN = TRUE.    
     FRAME FRAME-E:HIDDEN = TRUE.
     ASSIGN 
     BUTTON-4:LABEL IN FRAME {&FRAME-NAME} = "<Stolpar>"
     BUTTON-5:LABEL  = "Transformatorer"
     BUTTON-6:LABEL = "Apparater"
     BUTTON-8:LABEL  = "Övriga"
     BUTTON-9:LABEL  = "Bilder".
     {&WINDOW-NAME}:TITLE = "Ändring av Stolpar".
   END.
   ELSE IF val = 2 THEN DO:
      FRAME FRAME-A:HIDDEN = TRUE.
      FRAME FRAME-B:HIDDEN = FALSE.     
      FRAME FRAME-C:HIDDEN = TRUE.
      FRAME FRAME-D:HIDDEN = TRUE.    
      FRAME FRAME-E:HIDDEN = TRUE.
      ASSIGN 
      BUTTON-4:LABEL = "Stolpar"
      BUTTON-5:LABEL = "<Transformatorer>"
      BUTTON-6:LABEL = "Apparater"
      BUTTON-8:LABEL  = "Övriga"
      BUTTON-9:LABEL  = "Bilder".
      {&WINDOW-NAME}:TITLE = "Ändring av Transformatorer".
   END.
   ELSE IF val = 3 THEN DO:
     FRAME FRAME-A:HIDDEN = TRUE.
     FRAME FRAME-B:HIDDEN = TRUE.
     FRAME FRAME-C:HIDDEN = FALSE.
     FRAME FRAME-D:HIDDEN = TRUE. 
     FRAME FRAME-E:HIDDEN = TRUE. 
     ASSIGN 
     BUTTON-4:LABEL = "Stolpar"
     BUTTON-5:LABEL = "Transformatorer"
     BUTTON-6:LABEL = "Apparater"
     BUTTON-8:LABEL  = "Övriga"
     BUTTON-9:LABEL  = "Bilder".
     {&WINDOW-NAME}:TITLE = "Ändring av Apparater".
   END.
   ELSE IF val = 4 THEN DO:
     FRAME FRAME-A:HIDDEN = TRUE.
     FRAME FRAME-B:HIDDEN = TRUE.    
     FRAME FRAME-C:HIDDEN = TRUE.
     FRAME FRAME-D:HIDDEN = FALSE. 
     FRAME FRAME-E:HIDDEN = TRUE. 
     ASSIGN 
     BUTTON-4:LABEL = "Stolpar"
     BUTTON-5:LABEL = "Transformatorer"
     BUTTON-6:LABEL = "Apparater"
     BUTTON-8:LABEL  = "Övriga"
     BUTTON-9:LABEL  = "Bilder".
     {&WINDOW-NAME}:TITLE = "Ändring av Övriga".
   END.
   ELSE IF val = 5 THEN DO:
     FRAME FRAME-A:HIDDEN = TRUE.
     FRAME FRAME-B:HIDDEN = TRUE.    
     FRAME FRAME-C:HIDDEN = TRUE.
     FRAME FRAME-D:HIDDEN = TRUE. 
     FRAME FRAME-E:HIDDEN = FALSE. 
     ASSIGN 
     BUTTON-4:LABEL = "Stolpar"
     BUTTON-5:LABEL = "Transformatorer"
     BUTTON-6:LABEL = "Apparater"
     BUTTON-8:LABEL  = "Övriga"
     BUTTON-9:LABEL  = "Bilder".
     {&WINDOW-NAME}:TITLE = "Ändring av Bilder".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

