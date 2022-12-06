&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RADIO-SET-3 RADIO-SET-1 RADIO-SET-4 ~
RADIO-SET-2 RADIO-SET-7 RADIO-SET-5 RADIO-SET-8 RADIO-SET-6 RADIO-SET-11 ~
RADIO-SET-9 RADIO-SET-12 RADIO-SET-10 Buttonfg buttonna 
&Scoped-Define DISPLAYED-OBJECTS RADIO-SET-3 RADIO-SET-1 RADIO-SET-4 ~
RADIO-SET-2 RADIO-SET-7 RADIO-SET-5 RADIO-SET-8 RADIO-SET-6 RADIO-SET-11 ~
RADIO-SET-9 RADIO-SET-12 RADIO-SET-10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Buttonfg 
     LABEL "<< Föregående" 
     SIZE 30 BY 2.13
     FONT 43.

DEFINE BUTTON buttonna 
     LABEL "Nästa >>" 
     SIZE 30 BY 2.13
     FONT 11.

DEFINE VARIABLE RADIO-SET-1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-10 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-11 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-12 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-3 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-4 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE RADIO-SET-5 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21 NO-UNDO.

DEFINE VARIABLE RADIO-SET-6 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21 NO-UNDO.

DEFINE VARIABLE RADIO-SET-7 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21 NO-UNDO.

DEFINE VARIABLE RADIO-SET-8 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21 NO-UNDO.

DEFINE VARIABLE RADIO-SET-9 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1,
"", 2,
"", 3,
"", 4,
"", 5
     SIZE 22 BY 1.21
     BGCOLOR 17  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 215 BY 11.92
     BGCOLOR 7 FGCOLOR 7 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RADIO-SET-3 AT ROW 9.75 COL 173 NO-LABEL WIDGET-ID 68
     RADIO-SET-1 AT ROW 9.75 COL 173 NO-LABEL WIDGET-ID 46
     RADIO-SET-4 AT ROW 9.79 COL 200 NO-LABEL WIDGET-ID 74
     RADIO-SET-2 AT ROW 9.79 COL 200 NO-LABEL WIDGET-ID 52
     RADIO-SET-7 AT ROW 12.13 COL 173 NO-LABEL WIDGET-ID 98
     RADIO-SET-5 AT ROW 12.13 COL 173 NO-LABEL WIDGET-ID 86
     RADIO-SET-8 AT ROW 12.21 COL 200 NO-LABEL WIDGET-ID 104
     RADIO-SET-6 AT ROW 12.21 COL 200 NO-LABEL WIDGET-ID 92
     RADIO-SET-11 AT ROW 14.5 COL 173 NO-LABEL WIDGET-ID 128
     RADIO-SET-9 AT ROW 14.5 COL 173 NO-LABEL WIDGET-ID 116
     RADIO-SET-12 AT ROW 14.58 COL 200 NO-LABEL WIDGET-ID 134
     RADIO-SET-10 AT ROW 14.58 COL 200 NO-LABEL WIDGET-ID 122
     Buttonfg AT ROW 18.13 COL 168 WIDGET-ID 190
     buttonna AT ROW 18.13 COL 199 WIDGET-ID 186
     "Kunna se mina konstruktioner placerade i en karta" VIEW-AS TEXT
          SIZE 109 BY 1.67 AT ROW 11.42 COL 32.75 WIDGET-ID 112
          FONT 8
     "vara för dig som användare?" VIEW-AS TEXT
          SIZE 105 BY 1.42 AT ROW 2.92 COL 23 WIDGET-ID 184
          FONT 10
     "Hur bedömer du följande potentiella framtida GURU-funktioner" VIEW-AS TEXT
          SIZE 174 BY 1.92 AT ROW 1.5 COL 23 WIDGET-ID 182
          FONT 10
     "TEST" VIEW-AS TEXT
          SIZE 213 BY 2.63 AT ROW 13.79 COL 16 WIDGET-ID 60
          BGCOLOR 17 FGCOLOR 17 
     "Kunna se mina konstruktioner placerade i en karta" VIEW-AS TEXT
          SIZE 109 BY 1.67 AT ROW 8.79 COL 33 WIDGET-ID 62
          BGCOLOR 17 FONT 8
     "3" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 14.04 COL 18 WIDGET-ID 180
          BGCOLOR 17 FONT 8
     "2" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 11.42 COL 18 WIDGET-ID 178
          FONT 8
     "TEST" VIEW-AS TEXT
          SIZE 213 BY 2.63 AT ROW 8.58 COL 16 WIDGET-ID 80
          BGCOLOR 19 FGCOLOR 19 
     "Kunna se mina konstruktioner placerade i en karta" VIEW-AS TEXT
          SIZE 109 BY 1.67 AT ROW 14.04 COL 33 WIDGET-ID 82
          BGCOLOR 17 FONT 8
     "Mina konstruktioner i GURU visas i en karta" VIEW-AS TEXT
          SIZE 79 BY .92 AT ROW 10.25 COL 33 WIDGET-ID 84
          BGCOLOR 17 FONT 9
     "1" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 8.79 COL 18 WIDGET-ID 176
          BGCOLOR 17 FONT 8
     "#" VIEW-AS TEXT
          SIZE 13 BY 1.21 AT ROW 5 COL 17 WIDGET-ID 174
          BGCOLOR 19 FGCOLOR 17 FONT 10
     "Funktion" VIEW-AS TEXT
          SIZE 25 BY 1.21 AT ROW 5 COL 32 WIDGET-ID 172
          BGCOLOR 19 FGCOLOR 17 FONT 10
     "effekt" VIEW-AS TEXT
          SIZE 9.63 BY .92 AT ROW 6.13 COL 200 WIDGET-ID 160
          BGCOLOR 19 FGCOLOR 17 FONT 11
     "TEST" VIEW-AS TEXT
          SIZE 213 BY 2.63 AT ROW 11.21 COL 16 WIDGET-ID 110
          BGCOLOR 18 FGCOLOR 16 
     "Mycket" VIEW-AS TEXT
          SIZE 9.63 BY .79 AT ROW 5.38 COL 186 WIDGET-ID 158
          BGCOLOR 19 FGCOLOR 17 FONT 11
     "Mina konstruktioner i GURU visas i en karta" VIEW-AS TEXT
          SIZE 79 BY .92 AT ROW 12.88 COL 32.75 WIDGET-ID 114
          FONT 9
     "TEST" VIEW-AS TEXT
          SIZE 213 BY 3.79 AT ROW 5 COL 16 WIDGET-ID 168
          BGCOLOR 22 FGCOLOR 19 
     "Hög" VIEW-AS TEXT
          SIZE 9.63 BY .79 AT ROW 5.42 COL 212 WIDGET-ID 166
          BGCOLOR 19 FGCOLOR 17 FONT 11
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 258.8 BY 31 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "Låg" VIEW-AS TEXT
          SIZE 9.63 BY .79 AT ROW 5.42 COL 200 WIDGET-ID 164
          BGCOLOR 19 FGCOLOR 17 FONT 11
     "effekt" VIEW-AS TEXT
          SIZE 9.63 BY .92 AT ROW 6.13 COL 212 WIDGET-ID 162
          BGCOLOR 19 FGCOLOR 17 FONT 11
     "Mina konstruktioner i GURU visas i en karta" VIEW-AS TEXT
          SIZE 79 BY .92 AT ROW 15.5 COL 33 WIDGET-ID 140
          BGCOLOR 17 FONT 9
     "1   2   3   4   5" VIEW-AS TEXT
          SIZE 21.63 BY 1.42 AT ROW 7.04 COL 173.38 WIDGET-ID 142
          BGCOLOR 19 FGCOLOR 17 FONT 10
     "1   2   3   4   5" VIEW-AS TEXT
          SIZE 21.63 BY 1.42 AT ROW 7.08 COL 200 WIDGET-ID 144
          BGCOLOR 19 FGCOLOR 17 FONT 10
     "Inte så" VIEW-AS TEXT
          SIZE 9.63 BY .79 AT ROW 5.38 COL 173.38 WIDGET-ID 152
          BGCOLOR 19 FGCOLOR 17 FONT 11
     "viktigt" VIEW-AS TEXT
          SIZE 9.63 BY .79 AT ROW 6.13 COL 173.38 WIDGET-ID 154
          BGCOLOR 19 FGCOLOR 17 FONT 11
     "viktigt" VIEW-AS TEXT
          SIZE 9 BY .88 AT ROW 6.13 COL 186 WIDGET-ID 156
          BGCOLOR 19 FGCOLOR 17 FONT 11
     RECT-2 AT ROW 4.75 COL 15 WIDGET-ID 170
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 258.8 BY 31 WIDGET-ID 100.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 31
         WIDTH              = 240
         MAX-HEIGHT         = 48.42
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.42
         VIRTUAL-WIDTH      = 384
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
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
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
  DISPLAY RADIO-SET-3 RADIO-SET-1 RADIO-SET-4 RADIO-SET-2 RADIO-SET-7 
          RADIO-SET-5 RADIO-SET-8 RADIO-SET-6 RADIO-SET-11 RADIO-SET-9 
          RADIO-SET-12 RADIO-SET-10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 RADIO-SET-3 RADIO-SET-1 RADIO-SET-4 RADIO-SET-2 RADIO-SET-7 
         RADIO-SET-5 RADIO-SET-8 RADIO-SET-6 RADIO-SET-11 RADIO-SET-9 
         RADIO-SET-12 RADIO-SET-10 Buttonfg buttonna 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

