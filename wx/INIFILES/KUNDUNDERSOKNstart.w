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
&Scoped-Define ENABLED-OBJECTS RECT-2 TOGGLE-1 TOGGLE-4 TOGGLE-2 TOGGLE-5 ~
TOGGLE-3 TOGGLE-6 Buttonfg buttonna 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-1 TOGGLE-4 TOGGLE-2 TOGGLE-5 ~
TOGGLE-3 TOGGLE-6 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Buttonfg 
     LABEL "Nej tack" 
     SIZE 30 BY 2.14
     FONT 11.

DEFINE BUTTON buttonna 
     LABEL "N�sta >>" 
     SIZE 30 BY 2.14
     FONT 11.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 112 BY 20
     BGCOLOR 7 FGCOLOR 7 .

DEFINE VARIABLE TOGGLE-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE TOGGLE-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE TOGGLE-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81
     BGCOLOR 17  NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOGGLE-1 AT ROW 13.86 COL 90 WIDGET-ID 224
     TOGGLE-4 AT ROW 16.24 COL 90 WIDGET-ID 230
     TOGGLE-2 AT ROW 19.1 COL 90 WIDGET-ID 226
     TOGGLE-5 AT ROW 21.48 COL 90 WIDGET-ID 232
     TOGGLE-3 AT ROW 24.33 COL 90 WIDGET-ID 228
     TOGGLE-6 AT ROW 26.71 COL 90 WIDGET-ID 234
     Buttonfg AT ROW 29.57 COL 66 WIDGET-ID 190
     buttonna AT ROW 29.57 COL 97 WIDGET-ID 186
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 2.62 AT ROW 20.76 COL 17 WIDGET-ID 196
          BGCOLOR 16 FGCOLOR 16 
     "Annat, tex administrat�r av Guru" VIEW-AS TEXT
          SIZE 54.2 BY 1.67 AT ROW 26.24 COL 32.8 WIDGET-ID 212
          FONT 8
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 2.62 AT ROW 23.33 COL 17 WIDGET-ID 210
          BGCOLOR 17 FGCOLOR 17 
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 2.62 AT ROW 25.95 COL 17 WIDGET-ID 208
          BGCOLOR 16 FGCOLOR 16 
     "Markv�rderare" VIEW-AS TEXT
          SIZE 55 BY 1.67 AT ROW 21 COL 33 WIDGET-ID 204
          FONT 8
     "2" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 15.76 COL 19 WIDGET-ID 202
          FONT 8
     "2" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 15.76 COL 19 WIDGET-ID 200
          FONT 8
     "4" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 21 COL 19 WIDGET-ID 198
          FONT 8
     "6" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 26.24 COL 19 WIDGET-ID 222
          FONT 8
     "Ink�pare" VIEW-AS TEXT
          SIZE 51 BY 1.67 AT ROW 23.62 COL 33 WIDGET-ID 218
          BGCOLOR 17 FONT 8
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 2.62 AT ROW 15.52 COL 17 WIDGET-ID 110
          BGCOLOR 16 FGCOLOR 16 
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 3.81 AT ROW 9.1 COL 17 WIDGET-ID 168
          BGCOLOR 19 FGCOLOR 19 
     "Beredare" VIEW-AS TEXT
          SIZE 55.2 BY 1.67 AT ROW 15.76 COL 33.8 WIDGET-ID 112
          FONT 8
     "Min anv�ndarroll i Guru �r:" VIEW-AS TEXT
          SIZE 51 BY 1.19 AT ROW 9.1 COL 33 WIDGET-ID 172
          BGCOLOR 19 FGCOLOR 17 FONT 10
     "#" VIEW-AS TEXT
          SIZE 13 BY 1.19 AT ROW 9.1 COL 18 WIDGET-ID 174
          BGCOLOR 19 FGCOLOR 17 FONT 10
     "1" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 13.14 COL 19 WIDGET-ID 176
          BGCOLOR 17 FONT 8
     "Mont�r" VIEW-AS TEXT
          SIZE 55 BY 1.67 AT ROW 18.38 COL 34 WIDGET-ID 82
          BGCOLOR 17 FONT 8
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 2.62 AT ROW 12.91 COL 17 WIDGET-ID 80
          BGCOLOR 17 FGCOLOR 17 
     "2" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 15.76 COL 19 WIDGET-ID 178
          FONT 8
     "3" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 18.38 COL 19 WIDGET-ID 180
          BGCOLOR 17 FONT 8
     "Projekt�r" VIEW-AS TEXT
          SIZE 55 BY 1.67 AT ROW 13.14 COL 34 WIDGET-ID 62
          BGCOLOR 17 FONT 8
     "TEST" VIEW-AS TEXT
          SIZE 110 BY 2.62 AT ROW 18.14 COL 17 WIDGET-ID 60
          BGCOLOR 17 FGCOLOR 17 
     "KUNDUNDERS�KNING - ditt svar �r v�rdefullt!" VIEW-AS TEXT
          SIZE 174 BY 1.91 AT ROW 1.48 COL 23 WIDGET-ID 182
          FONT 10
     "Vill du svara p� n�gra fr�gor ang�ende" VIEW-AS TEXT
          SIZE 105 BY 1.43 AT ROW 4.81 COL 24 WIDGET-ID 184
          FONT 10
     "5" VIEW-AS TEXT
          SIZE 12 BY 1.67 AT ROW 23.62 COL 19 WIDGET-ID 214
          BGCOLOR 17 FONT 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 258.8 BY 32.24 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     "Guru och karta, markv�rdering, mobil mm?" VIEW-AS TEXT
          SIZE 105 BY 1.43 AT ROW 6.05 COL 24 WIDGET-ID 192
          FONT 10
     "(man kan ange flera)" VIEW-AS TEXT
          SIZE 46 BY .95 AT ROW 10.29 COL 33 WIDGET-ID 194
          BGCOLOR 19 FGCOLOR 17 FONT 8
     RECT-2 AT ROW 8.86 COL 16 WIDGET-ID 170
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 258.8 BY 32.24 WIDGET-ID 100.


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
         HEIGHT             = 32.24
         WIDTH              = 258.8
         MAX-HEIGHT         = 48.43
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.43
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
  DISPLAY TOGGLE-1 TOGGLE-4 TOGGLE-2 TOGGLE-5 TOGGLE-3 TOGGLE-6 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-2 TOGGLE-1 TOGGLE-4 TOGGLE-2 TOGGLE-5 TOGGLE-3 TOGGLE-6 Buttonfg 
         buttonna 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

