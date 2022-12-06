&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          RT               PROGRESS
*/
&Scoped-define WINDOW-NAME    WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 09/05/97 -  2:30 pm

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



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-A
&Scoped-define BROWSE-NAME BROWSE-1

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 AONRTAB.ANVANDARE ~
AONRTAB.AONR AONRTAB.DELNR AONRTAB.ORT 
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH AONRTAB NO-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 AONRTAB
&Scoped-define TABLES-IN-QUERY-BROWSE-1 AONRTAB 

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define FIELDS-IN-QUERY-FRAME-A 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-A 
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-1}

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 15.38 BY 1.23
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Ny" 
     SIZE 7.63 BY 1.32.

DEFINE BUTTON BUTTON-10 
     LABEL "Årsuppdela" 
     SIZE 15.38 BY 1.23.

DEFINE BUTTON BUTTON-11 
     LABEL "Kalkyl" 
     SIZE 15.38 BY 1.23.

DEFINE BUTTON BUTTON-12 
     LABEL "Rapporter" 
     SIZE 15.38 BY 1.23.

DEFINE BUTTON BUTTON-13 
     LABEL "Ao-nummer" 
     SIZE 15.38 BY 1.23.

DEFINE BUTTON BUTTON-2 
     LABEL "Ändra" 
     SIZE 7.63 BY 1.32.

DEFINE BUTTON BUTTON-4 
     LABEL "Koppla" 
     SIZE 7.63 BY 1.32.

DEFINE BUTTON BUTTON-5 
     LABEL "Ta bort" 
     SIZE 7.63 BY 1.32.

DEFINE BUTTON BUTTON-6 
     LABEL "Visa" 
     SIZE 7.63 BY 1.32.

DEFINE BUTTON BUTTON-7 
     LABEL "Avsluta banknr" 
     SIZE 15.38 BY 1.23.

DEFINE BUTTON BUTTON-8 
     LABEL "Visa avslutade" 
     SIZE 15.38 BY 1.23.

DEFINE BUTTON BUTTON-9 
     LABEL "Resurser" 
     SIZE 15.38 BY 1.23.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Banknr" 
     VIEW-AS FILL-IN 
     SIZE 29.38 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 29.38 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE RADIO-SET-1 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig", "1",
"Alla", "2"
     SIZE 18.88 BY 1.18 NO-UNDO.

DEFINE VARIABLE RADIO-SET-2 AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillf.", "1",
"Fasta", "2"
     SIZE 18.63 BY 1.18 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 61 BY 15.14
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 61 BY 2.36
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 61 BY 4.95
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 20 BY 22.45
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY BROWSE-1 FOR AONRTAB SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1 QUERY BROWSE-1 NO-LOCK DISPLAY 
      AONRTAB.ANVANDARE
      AONRTAB.AONR
      AONRTAB.DELNR
      AONRTAB.ORT
    WITH SIZE 58 BY 12.55
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RADIO-SET-1 AT ROW 1.5 COL 8.25 NO-LABEL
     RADIO-SET-2 AT ROW 1.5 COL 41.5 NO-LABEL
     BROWSE-1 AT ROW 2.91 COL 2.38
     BUTTON-10 AT ROW 3 COL 64.13
     BUTTON-11 AT ROW 5 COL 64.13
     BUTTON-12 AT ROW 7 COL 64.13
     BUTTON-9 AT ROW 9 COL 64.13
     BUTTON-7 AT ROW 11 COL 64.13
     BUTTON-8 AT ROW 13 COL 64.13
     BUTTON-13 AT ROW 15 COL 64.13
     BUTTON-1 AT ROW 16.64 COL 7.38
     BUTTON-2 AT ROW 16.64 COL 17.38
     BUTTON-5 AT ROW 16.64 COL 27.38
     BUTTON-6 AT ROW 16.64 COL 37.38
     BUTTON-4 AT ROW 16.64 COL 47.38
     FILL-IN-1 AT ROW 20.27 COL 12.38 COLON-ALIGNED
     Btn_Cancel AT ROW 21.5 COL 64.13
     FILL-IN-2 AT ROW 21.73 COL 12.38 COLON-ALIGNED
     RECT-1 AT ROW 1 COL 1
     RECT-4 AT ROW 1 COL 62
     "Visa:" VIEW-AS TEXT
          SIZE 5.63 BY .77 AT ROW 1.64 COL 2.38
     "Bnr:" VIEW-AS TEXT
          SIZE 4.63 BY .77 AT ROW 1.64 COL 36.25
     RECT-2 AT ROW 16.14 COL 1
     RECT-3 AT ROW 18.5 COL 1
     "Sök" VIEW-AS TEXT
          SIZE 3.75 BY .77 AT ROW 19.09 COL 14.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.13 BY 22.5
         CANCEL-BUTTON Btn_Cancel.

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "GURU - Bank"
         COLUMN             = 12.38
         ROW                = 2.14
         HEIGHT             = 22.55
         WIDTH              = 81.13
         MAX-HEIGHT         = 23.59
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 23.59
         VIRTUAL-WIDTH      = 82
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "RT.AONRTAB"
     _Options          = "NO-LOCK"
     _OrdList          = ""
     _FldNameList[1]   = RT.AONRTAB.ANVANDARE
     _FldNameList[2]   = RT.AONRTAB.AONR
     _FldNameList[3]   = RT.AONRTAB.DELNR
     _FldNameList[4]   = RT.AONRTAB.ORT
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1 _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  DISPLAY RADIO-SET-1 RADIO-SET-2 FILL-IN-1 FILL-IN-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-1 RECT-4 RADIO-SET-1 RADIO-SET-2 BROWSE-1 BUTTON-10 BUTTON-11 
         BUTTON-12 BUTTON-9 BUTTON-7 BUTTON-8 BUTTON-13 RECT-2 BUTTON-1 
         BUTTON-2 BUTTON-5 BUTTON-6 BUTTON-4 RECT-3 FILL-IN-1 Btn_Cancel 
         FILL-IN-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
