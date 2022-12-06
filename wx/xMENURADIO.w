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
{windows.i}

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE GWL_HINSTANCE -6  
&GLOBAL-DEFINE GWL_STYLE -16
&GLOBAL-DEFINE BS_PUSHLIKE 4096
&GLOBAL-DEFINE BS_ICON 64
&GLOBAL-DEFINE BS_BITMAP 128
&GLOBAL-DEFINE BM_SETIMAGE 247
&GLOBAL-DEFINE IMAGE_ICON 1
&GLOBAL-DEFINE IMAGE_BITMAP 0
&GLOBAL-DEFINE BM_SETSTYLE 244
 
 
PROCEDURE ExtractIconA EXTERNAL "shell32.dll" :
  DEFINE INPUT  PARAMETER hInst AS LONG.
  DEFINE INPUT  PARAMETER lpszExeFileName AS CHAR.
  DEFINE INPUT  PARAMETER nIconIndex AS LONG.
  DEFINE RETURN PARAMETER hIcon  AS LONG.
END PROCEDURE.
 
PROCEDURE DestroyIcon EXTERNAL "user32.dll" :
  DEFINE INPUT PARAMETER hIcon AS LONG.
END PROCEDURE.
 
PROCEDURE GetWindowLongA EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER phwnd       AS LONG.
  DEFINE INPUT  PARAMETER cindex      AS LONG.
  DEFINE RETURN PARAMETER currentlong AS LONG.
END PROCEDURE.
 
PROCEDURE SetWindowLongA EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER phwnd   AS LONG.
  DEFINE INPUT  PARAMETER cindex  AS LONG.
  DEFINE INPUT  PARAMETER newlong AS LONG.
  DEFINE RETURN PARAMETER oldlong AS LONG.
END PROCEDURE.
 
PROCEDURE InvalidateRect EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER HWND        AS LONG.
  DEFINE INPUT  PARAMETER lpRect      AS LONG.
  DEFINE INPUT  PARAMETER bErase      AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.
 
PROCEDURE SendMessageA EXTERNAL "user32.dll" :
  DEFINE INPUT  PARAMETER HWND        AS LONG.
  DEFINE INPUT  PARAMETER umsg        AS LONG.
  DEFINE INPUT  PARAMETER wparam      AS LONG.
  DEFINE INPUT  PARAMETER lparam      AS LONG.
  DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOGGLE-1 TOGGLE-2 TOGGLE-3 RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-1 TOGGLE-2 TOGGLE-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 84 BY 2.13.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 84 BY 17.92.

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY 1.5 NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "Toggle 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY 1.5 NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "Toggle 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY 1.5 NO-UNDO.

DEFINE BUTTON BUTTON-1 
     LABEL "Första" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BUTTON-2 
     LABEL "Andra" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-7 
     LABEL "Tredje" 
     SIZE 15 BY 1.13.


/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 35.5 BY 8 EXPANDABLE.

DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 62 BY 9 EXPANDABLE.

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 37 BY 7.5 EXPANDABLE.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 33 BY 7.5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOGGLE-1 AT ROW 1.5 COL 26.5
     TOGGLE-2 AT ROW 1.5 COL 44.75
     TOGGLE-3 AT ROW 1.5 COL 63
     RECT-1 AT ROW 1.13 COL 2.5
     RECT-2 AT ROW 3.25 COL 2.5
     "Välj funktion" VIEW-AS TEXT
          SIZE 15.5 BY 1 AT ROW 1.33 COL 3.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.25 BY 20.46.

DEFINE FRAME FRAME-C
     BUTTON-7 AT ROW 5.25 COL 8.5
     BROWSE-3 AT ROW 7 COL 2.5
     BROWSE-4 AT ROW 7 COL 44.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.25 ROW 4.38
         SIZE 80.25 BY 15.88.

DEFINE FRAME FRAME-B
     FILL-IN-1 AT ROW 3.58 COL 8.5 COLON-ALIGNED
     BUTTON-2 AT ROW 5 COL 10
     BROWSE-2 AT ROW 7 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.25 ROW 4.75
         SIZE 80.25 BY 15.75.

DEFINE FRAME FRAME-A
     BUTTON-1 AT ROW 5 COL 9
     BROWSE-1 AT ROW 6.75 COL 2.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.25 ROW 4.75
         SIZE 80.25 BY 15.63.


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
         HEIGHT             = 20.58
         WIDTH              = 86.63
         MAX-HEIGHT         = 21.08
         MAX-WIDTH          = 115.88
         VIRTUAL-HEIGHT     = 21.08
         VIRTUAL-WIDTH      = 115.88
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
       FRAME FRAME-C:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BROWSE-1 BUTTON-1 FRAME-A */
ASSIGN 
       FRAME FRAME-A:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BROWSE-2 BUTTON-2 FRAME-B */
ASSIGN 
       FRAME FRAME-B:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* BROWSE-TAB BROWSE-3 BUTTON-7 FRAME-C */
/* BROWSE-TAB BROWSE-4 BROWSE-3 FRAME-C */
ASSIGN 
       FRAME FRAME-C:HIDDEN           = TRUE.

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


&Scoped-define SELF-NAME TOGGLE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-1 C-Win
ON VALUE-CHANGED OF TOGGLE-1 IN FRAME DEFAULT-FRAME /* Toggle 1 */
DO:
   TOGGLE-1 = INPUT TOGGLE-1.
   RUN goma_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-2 C-Win
ON VALUE-CHANGED OF TOGGLE-2 IN FRAME DEFAULT-FRAME /* Toggle 2 */
DO:
  TOGGLE-2 = INPUT TOGGLE-2.
  RUN goma_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-3 C-Win
ON VALUE-CHANGED OF TOGGLE-3 IN FRAME DEFAULT-FRAME /* Toggle 3 */
DO:
  TOGGLE-3 = INPUT TOGGLE-3.
  RUN goma_UI.
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
  RUN ToggleToButton (toggle-1:HWND).
  RUN ToggleToButton (toggle-2:HWND).
  RUN ToggleToButton (toggle-3:HWND).
  RUN SetIcon (toggle-1:HWND, 'G:\delad\pro9\guru\Ctid\BILDER\prev.bmp').
  RUN SetIcon (toggle-2:HWND, 'G:\delad\pro9\guru\Ctid\BILDER\prev.bmp').
  RUN SetIcon (toggle-3:HWND, 'G:\delad\pro9\guru\Ctid\BILDER\prev.bmp').
  FRAME FRAME-A:HIDDEN = FALSE.
  FRAME FRAME-B:HIDDEN = TRUE.
  FRAME FRAME-C:HIDDEN = TRUE.
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
  DISPLAY TOGGLE-1 TOGGLE-2 TOGGLE-3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE TOGGLE-1 TOGGLE-2 TOGGLE-3 RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BUTTON-7 
      WITH FRAME FRAME-C IN WINDOW C-Win.
  VIEW FRAME FRAME-C IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
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
   &Scoped-define FRAME-NAME DEFAULT-FRAME
   IF TOGGLE-1 = TRUE THEN DO:
     FRAME FRAME-B:HIDDEN = TRUE.
     FRAME FRAME-A:HIDDEN = FALSE.
     FRAME FRAME-C:HIDDEN = TRUE.
   END.
   ELSE IF TOGGLE-2 = TRUE THEN DO:
      FRAME FRAME-B:HIDDEN = FALSE.
      FRAME FRAME-A:HIDDEN = TRUE.
      FRAME FRAME-C:HIDDEN = TRUE.
   END.
   ELSE IF TOGGLE-3 = TRUE THEN DO:
     FRAME FRAME-B:HIDDEN = TRUE.
     FRAME FRAME-A:HIDDEN = TRUE.
     FRAME FRAME-C:HIDDEN = FALSE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetIcon C-Win 
PROCEDURE SetIcon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER HWND         AS INTEGER.
  DEFINE INPUT PARAMETER IconFilename AS CHAR.
 
  DEF VAR hInstance   AS INTEGER NO-UNDO.
  DEF VAR OldIcon     AS INTEGER NO-UNDO.
  DEF VAR hIcon       AS INTEGER NO-UNDO.
 
  RUN GetWindowLongA(HWND,
                     {&GWL_HINSTANCE},
                     OUTPUT hInstance).
  RUN ExtractIconA (hInstance, IconFilename, 0, OUTPUT hIcon).
 
  RUN SendMessageA( HWND, 
                    {&BM_SETIMAGE}, 
                    {&IMAGE_ICON}, 
                    hIcon, 
                    OUTPUT OldIcon).
 
/* free resources when the window closes, or earlier:
     run DestroyIcon (hIcon). */
 
   IF OldIcon NE 0 THEN 
      RUN DestroyIcon (OldIcon).
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ToggleToButton C-Win 
PROCEDURE ToggleToButton :
/*------------------------------------------------------------------------------
   purpose: convert a toggle-box widget to a button.
   note   : don't call this more than once for each toggle-box widget    
------------------------------------------------------------------------------*/             
  DEFINE INPUT PARAMETER HWND AS INTEGER.
 
  DEF VAR styles      AS INTEGER NO-UNDO.
  DEF VAR returnvalue AS INTEGER NO-UNDO.
 
  /* find the current style and add some extra flags to it */
  RUN GetWindowLongA(HWND, {&GWL_STYLE}, OUTPUT styles).
  styles = styles + {&BS_ICON} + {&BS_PUSHLIKE}.
 
  /* according to MSDN you should apply the new style 
     using SendMessage(hwnd,BM_SETSTYLE,....) but it does not work for me */
  RUN SetWindowLongA(HWND, {&GWL_STYLE}, styles, OUTPUT styles).
 
  /* force a repaint: */
  RUN InvalidateRect(HWND,0,1,OUTPUT returnvalue).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

