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
DEFINE VARIABLE fillslh AS HANDLE NO-UNDO.
DEFINE VARIABLE fillh AS HANDLE NO-UNDO.
DEFINE VARIABLE fillslh2 AS HANDLE NO-UNDO.
DEFINE VARIABLE fillh2 AS HANDLE NO-UNDO.

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

/* Definitions for FRAME FRAME-F                                        */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_NYKOPP  NO-FOCUS FLAT-BUTTON
     LABEL "Ny koppling" 
     SIZE 12 BY 1
     FONT 0.

DEFINE BUTTON BUTTON-15 
     LABEL "Söktips" 
     SIZE 7.75 BY 1
     FONT 0.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.5 BY 1.21.

DEFINE BUTTON BUTTON-2  NO-FOCUS FLAT-BUTTON
     LABEL "Ändra" 
     SIZE 7.5 BY 1
     FONT 0.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Transformator" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Text" 
     VIEW-AS FILL-IN 
     SIZE 27.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 1.21.

DEFINE BUTTON BTN_BER  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 1
     BGCOLOR 3 FONT 0.

DEFINE BUTTON BTN_KOPP  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 1
     FONT 0.

DEFINE BUTTON BTN_LINA  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 1
     FONT 0.

DEFINE BUTTON BTN_TRAFO  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 1
     BGCOLOR 9 FGCOLOR 9 FONT 0.

DEFINE BUTTON BTN_UPP  NO-FOCUS
     LABEL "" 
     SIZE 14 BY 1
     FONT 0.

DEFINE BUTTON BTN_BACK  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 3.5 BY 1.

DEFINE BUTTON BTN_OVER  NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 3.5 BY 1.

DEFINE BUTTON BUTTON-14  NO-FOCUS FLAT-BUTTON
     LABEL "Hämta" 
     SIZE 12 BY 1
     FONT 0.

DEFINE BUTTON BUTTON-7  NO-FOCUS FLAT-BUTTON
     LABEL "Ny apparat" 
     SIZE 11 BY 1
     FONT 0.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 33 BY 1.21.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 1.21.

DEFINE BUTTON BUTTON-11  NO-FOCUS FLAT-BUTTON
     LABEL "Välj" 
     SIZE 5.5 BY 1
     FONT 0.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 61 BY 11.25 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 1.21.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 33.5 BY 1 NO-UNDO.


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

DEFINE BROWSE BROWSE-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-7 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 60.5 BY 12.25 EXPANDABLE.

DEFINE BROWSE BROWSE-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-9 C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 61.63 BY 12.75 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.88 BY 19.63.

DEFINE FRAME FRAME-BODY
     BTN_BER AT ROW 1.17 COL 43.5
     BTN_KOPP AT ROW 1.17 COL 1.5
     BTN_LINA AT ROW 1.17 COL 29.5
     BTN_TRAFO AT ROW 1.17 COL 15.5
     BTN_UPP AT ROW 1.17 COL 70.5 RIGHT-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         THREE-D 
         AT COL 1.25 ROW 2.79
         SIZE 92.25 BY 17.75
         BGCOLOR 7 .

DEFINE FRAME FRAME-G
     FILL-IN-2 AT ROW 4.25 COL 19.5 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.

DEFINE FRAME FRAME-F
     BROWSE-9 AT ROW 2.5 COL 2.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.

DEFINE FRAME FRAME-E
     BROWSE-7 AT ROW 3 COL 6
     "Bilder" VIEW-AS TEXT
          SIZE 8.5 BY 1.25 AT ROW 1.5 COL 34.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.

DEFINE FRAME FRAME-D
     BUTTON-11 AT ROW 13.63 COL 2.88
     EDITOR-1 AT ROW 2.25 COL 2.5 NO-LABEL
     "Övriga" VIEW-AS TEXT
          SIZE 8 BY 1.25 AT ROW 1 COL 27.5
     RECT-8 AT ROW 13.54 COL 2.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.

DEFINE FRAME FRAME-C
     BUTTON-14 AT ROW 2.21 COL 14.5
     BROWSE-3 AT ROW 3.33 COL 2
     BUTTON-7 AT ROW 14.71 COL 46.38
     BTN_BACK AT ROW 9.5 COL 40.5
     BTN_OVER AT ROW 8 COL 40.5
     RECT-7 AT ROW 14.58 COL 46
     RECT-9 AT ROW 2.13 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.

DEFINE FRAME FRAME-B
     BUTTON-2 AT ROW 14.63 COL 3.38
     FILL-IN-3 AT ROW 2.75 COL 25.75 COLON-ALIGNED
     FILL-IN-1 AT ROW 4 COL 25.75 COLON-ALIGNED
     BROWSE-2 AT ROW 5.5 COL 3
     RECT-6 AT ROW 14.5 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.

DEFINE FRAME FRAME-A
     BTN_NYKOPP AT ROW 14.08 COL 2.38
     BROWSE-1 AT ROW 2.25 COL 2
     BUTTON-15 AT ROW 14.08 COL 29
     RECT-5 AT ROW 14 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.17
         SIZE 91 BY 16.25.


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
         HEIGHT             = 19.71
         WIDTH              = 93.13
         MAX-HEIGHT         = 23.58
         MAX-WIDTH          = 114.5
         VIRTUAL-HEIGHT     = 23.58
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
ASSIGN FRAME FRAME-A:FRAME = FRAME FRAME-BODY:HANDLE
       FRAME FRAME-B:FRAME = FRAME FRAME-BODY:HANDLE
       FRAME FRAME-BODY:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-C:FRAME = FRAME FRAME-BODY:HANDLE
       FRAME FRAME-D:FRAME = FRAME FRAME-BODY:HANDLE
       FRAME FRAME-E:FRAME = FRAME FRAME-BODY:HANDLE
       FRAME FRAME-F:FRAME = FRAME FRAME-BODY:HANDLE
       FRAME FRAME-G:FRAME = FRAME FRAME-BODY:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BROWSE-1 RECT-5 FRAME-A */
ASSIGN 
       FRAME FRAME-A:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* BROWSE-TAB BROWSE-2 FILL-IN-1 FRAME-B */
ASSIGN 
       FRAME FRAME-B:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-BODY
   UNDERLINE                                                            */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-E:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-G:HANDLE)
       XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-E:HANDLE)
       XXTABVALXX = FRAME FRAME-D:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-B:HANDLE)
       XXTABVALXX = FRAME FRAME-A:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-D:HANDLE)
       XXTABVALXX = FRAME FRAME-F:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-A:HANDLE)
       XXTABVALXX = FRAME FRAME-C:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-F:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR BUTTON BTN_UPP IN FRAME FRAME-BODY
   ALIGN-R                                                              */
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* BROWSE-TAB BROWSE-3 RECT-9 FRAME-C */
ASSIGN 
       FRAME FRAME-C:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-D
                                                                        */
ASSIGN 
       FRAME FRAME-D:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-E
                                                                        */
/* BROWSE-TAB BROWSE-7 TEXT-2 FRAME-E */
ASSIGN 
       FRAME FRAME-E:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-F
                                                                        */
/* BROWSE-TAB BROWSE-9 1 FRAME-F */
ASSIGN 
       FRAME FRAME-F:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-G
                                                                        */
ASSIGN 
       FRAME FRAME-G:HIDDEN           = TRUE.

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


&Scoped-define FRAME-NAME FRAME-BODY
&Scoped-define SELF-NAME BTN_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BER C-Win
ON CHOOSE OF BTN_BER IN FRAME FRAME-BODY
DO:
   BTN_BER:LOAD-IMAGE ("bilder\flik.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN goma_UI (INPUT 4).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPP C-Win
ON CHOOSE OF BTN_KOPP IN FRAME FRAME-BODY
DO:
   BTN_KOPP:LOAD-IMAGE ("bilder\flik.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN goma_UI (INPUT 1).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LINA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LINA C-Win
ON CHOOSE OF BTN_LINA IN FRAME FRAME-BODY
DO:
   BTN_LINA:LOAD-IMAGE ("bilder\flik.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN goma_UI (INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TRAFO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TRAFO C-Win
ON CHOOSE OF BTN_TRAFO IN FRAME FRAME-BODY
DO:
   BTN_TRAFO:LOAD-IMAGE ("bilder\flik.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN goma_UI (INPUT 2).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP C-Win
ON CHOOSE OF BTN_UPP IN FRAME FRAME-BODY
DO:
   BTN_UPP:LOAD-IMAGE ("bilder\flik.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN goma_UI (INPUT 5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 C-Win
ON CHOOSE OF BUTTON-15 IN FRAME FRAME-A /* Söktips */
DO:
  RUN xsoktips.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
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
   &Scoped-define FRAME-NAME FRAME-BODY
   BTN_KOPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
   BTN_KOPP:LOAD-IMAGE-DOWN ("bilder\flik.gif") IN FRAME {&FRAME-NAME}.
/*    BTN_KOPP:LOAD-IMAGE-DOWN ("BILDER\btn_tidsedel_over.gif") IN FRAME {&FRAME-NAME}. */
   BTN_TRAFO:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
   BTN_TRAFO:LOAD-IMAGE-DOWN ("bilder\flik.gif") IN FRAME {&FRAME-NAME}.
   BTN_LINA:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
   BTN_LINA:LOAD-IMAGE-DOWN ("bilder\flik.gif") IN FRAME {&FRAME-NAME}.
   BTN_BER:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
   BTN_BER:LOAD-IMAGE-DOWN ("bilder\flik.gif") IN FRAME {&FRAME-NAME}.
   BTN_UPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
   BTN_UPP:LOAD-IMAGE-DOWN ("bilder\flik.gif") IN FRAME {&FRAME-NAME}.
   
   
   RUN enable_UI.
   DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
  fillh = FILL-IN-1:HANDLE.
  fillslh = FILL-IN-1:SIDE-LABEL-HANDLE.
/*   fillslh:X = fillh:COLUMN. */
/*   cLabel = fillslh:SCREEN-VALUE.       */
/*   fillslh:SCREEN-VALUE = TRIM(cLabel). */

  fillh2 = FILL-IN-3:HANDLE.
  fillslh2 = FILL-IN-3:SIDE-LABEL-HANDLE.
/*   fillslh2:X = fillh2:COLUMN. */
/*   cLabel = fillslh2:SCREEN-VALUE.       */
/*   fillslh2:SCREEN-VALUE = TRIM(cLabel). */
  APPLY "CHOOSE" TO BTN_KOPP IN FRAME {&FRAME-NAME}.
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BTN_NYKOPP RECT-5 BUTTON-15 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  VIEW FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY FILL-IN-3 FILL-IN-1 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  ENABLE BUTTON-2 RECT-6 FILL-IN-3 FILL-IN-1 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  ENABLE BUTTON-14 RECT-7 RECT-9 BUTTON-7 BTN_BACK BTN_OVER 
      WITH FRAME FRAME-C IN WINDOW C-Win.
  VIEW FRAME FRAME-C IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
  DISPLAY EDITOR-1 
      WITH FRAME FRAME-D IN WINDOW C-Win.
  ENABLE BUTTON-11 RECT-8 EDITOR-1 
      WITH FRAME FRAME-D IN WINDOW C-Win.
  VIEW FRAME FRAME-D IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-D}
  VIEW FRAME FRAME-E IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-E}
  VIEW FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
  DISPLAY FILL-IN-2 
      WITH FRAME FRAME-G IN WINDOW C-Win.
  ENABLE FILL-IN-2 
      WITH FRAME FRAME-G IN WINDOW C-Win.
  VIEW FRAME FRAME-G IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-G}
  ENABLE BTN_BER BTN_KOPP BTN_LINA BTN_TRAFO BTN_UPP 
      WITH FRAME FRAME-BODY IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BODY}
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
   &Scoped-define FRAME-NAME FRAME-BODY
/*   BUTTON-4:LOAD-MOUSE-POINTER("GLOVE":U). */
/*   BUTTON-5:LOAD-MOUSE-POINTER("GLOVE":U). */
/*   BUTTON-6:LOAD-MOUSE-POINTER("GLOVE":U). */ 
   &Scoped-define FRAME-NAME FRAME-BODY
   IF val = 1 THEN DO:
      FRAME FRAME-A:HIDDEN = FALSE.
      FRAME FRAME-B:HIDDEN = TRUE.    
      FRAME FRAME-C:HIDDEN = TRUE.
      FRAME FRAME-D:HIDDEN = TRUE.    
      FRAME FRAME-E:HIDDEN = TRUE.
      FRAME FRAME-F:HIDDEN = TRUE.
      FRAME FRAME-G:HIDDEN = TRUE.
      BTN_TRAFO:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_LINA:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_BER:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_UPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.

      {&WINDOW-NAME}:TITLE = "Kopplingar".
   END.
   ELSE IF val = 2 THEN DO:
      FRAME FRAME-A:HIDDEN = TRUE.
      FRAME FRAME-B:HIDDEN = FALSE.     
      FRAME FRAME-C:HIDDEN = TRUE.
      FRAME FRAME-D:HIDDEN = TRUE.    
      FRAME FRAME-E:HIDDEN = TRUE.
      FRAME FRAME-F:HIDDEN = TRUE.
      FRAME FRAME-G:HIDDEN = TRUE.
      BTN_KOPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_LINA:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_BER:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_UPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      {&WINDOW-NAME}:TITLE = "Ändring av Transformatorer".
   END.
   ELSE IF val = 3 THEN DO:
      FRAME FRAME-A:HIDDEN = TRUE.
      FRAME FRAME-B:HIDDEN = TRUE.
      FRAME FRAME-C:HIDDEN = FALSE.
      FRAME FRAME-D:HIDDEN = TRUE. 
      FRAME FRAME-E:HIDDEN = TRUE. 
      FRAME FRAME-F:HIDDEN = TRUE.
      FRAME FRAME-G:HIDDEN = TRUE.
      BTN_KOPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_TRAFO:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_BER:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_UPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
     {&WINDOW-NAME}:TITLE = "Ändring av Apparater".
   END.
   ELSE IF val = 4 THEN DO:
      FRAME FRAME-A:HIDDEN = TRUE.
      FRAME FRAME-B:HIDDEN = TRUE.    
      FRAME FRAME-C:HIDDEN = TRUE.
      FRAME FRAME-D:HIDDEN = FALSE. 
      FRAME FRAME-E:HIDDEN = TRUE.
      FRAME FRAME-F:HIDDEN = TRUE.
      FRAME FRAME-G:HIDDEN = TRUE.
      BTN_KOPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_TRAFO:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_LINA:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_UPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      {&WINDOW-NAME}:TITLE = "Ändring av Övriga".
   END.
   ELSE IF val = 5 THEN DO:
      FRAME FRAME-A:HIDDEN = TRUE.
      FRAME FRAME-B:HIDDEN = TRUE.    
      FRAME FRAME-C:HIDDEN = TRUE.
      FRAME FRAME-D:HIDDEN = TRUE. 
      FRAME FRAME-E:HIDDEN = FALSE. 
      FRAME FRAME-F:HIDDEN = TRUE.
      FRAME FRAME-G:HIDDEN = TRUE.
      BTN_KOPP:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_TRAFO:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      BTN_LINA:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      BTN_BER:LOAD-IMAGE ("bilder\flik2.gif") IN FRAME {&FRAME-NAME}.
      {&WINDOW-NAME}:TITLE = "Ändring av Bilder".
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

