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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


/*{EGENBNS.I}*/
DEFINE VARIABLE brwh AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc2 AS HANDLE NO-UNDO.
DEFINE VARIABLE varsum AS INTEGER NO-UNDO.
DEFINE VARIABLE brwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE exbrwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE exfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE newfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE indexvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ttname AS CHARACTER NO-UNDO.
DEFINE VARIABLE tabname AS CHARACTER NO-UNDO.
DEFINE VARIABLE okvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.

globforetag = FORETAG.FORETAG.
{FORESTYR.I}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-TAB FILL-IN-WHERE BTN_STARTA ~
BTN_FLYTTA FILL-IN-startc FILL-IN-TILL BTN_QUIT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TAB FILL-IN-WHERE FILL-IN-startc ~
FILL-IN-TILL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_FLYTTA 
     LABEL "Flytta" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_QUIT 
     LABEL "Quit" 
     SIZE 9 BY .79.

DEFINE BUTTON BTN_STARTA 
     LABEL "Öppna" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE FILL-IN-startc AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Flytta kolumn från" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tabell" 
     VIEW-AS FILL-IN 
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILL AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-WHERE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Where sats" 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-TAB AT ROW 1.5 COL 8.88 COLON-ALIGNED
     FILL-IN-WHERE AT ROW 1.5 COL 39.5 COLON-ALIGNED
     BTN_STARTA AT ROW 1.5 COL 105.5
     BTN_FLYTTA AT ROW 3.42 COL 60.5
     FILL-IN-startc AT ROW 3.54 COL 22 COLON-ALIGNED
     FILL-IN-TILL AT ROW 3.54 COL 40 COLON-ALIGNED
     BTN_QUIT AT ROW 25.25 COL 111.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122.5 BY 25.5.


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
         HEIGHT             = 25.5
         WIDTH              = 122.5
         MAX-HEIGHT         = 25.5
         MAX-WIDTH          = 122.5
         VIRTUAL-HEIGHT     = 25.5
         VIRTUAL-WIDTH      = 122.5
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
                                                                        */
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


&Scoped-define SELF-NAME BTN_FLYTTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FLYTTA C-Win
ON CHOOSE OF BTN_FLYTTA IN FRAME DEFAULT-FRAME /* Flytta */
DO:
   FILL-IN-startc = INPUT FILL-IN-startc.
   FILL-IN-TILL = INPUT FILL-IN-TILL.
   RUN movecol_UI IN brwproc (INPUT FILL-IN-startc,INPUT FILL-IN-TILL).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_QUIT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_QUIT C-Win
ON CHOOSE OF BTN_QUIT IN FRAME DEFAULT-FRAME /* Quit */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_STARTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_STARTA C-Win
ON CHOOSE OF BTN_STARTA IN FRAME DEFAULT-FRAME /* Öppna */
DO:
   FILL-IN-TAB = INPUT FILL-IN-TAB.
   FILL-IN-WHERE = INPUT FILL-IN-WHERE.
   ASSIGN            
   tabname = FILL-IN-TAB.          
   kommandoquery = "FOR EACH " + tabname + " WHERE " + FILL-IN-WHERE.
   RUN createbrw_UI IN brwproc (INPUT tabname,INPUT kommandoquery).
   RUN settriggerproc_UI IN brwproc (INPUT "offend_UI").
   RUN DYNBRW.P PERSISTENT SET brwproc2 (INPUT brwh).
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
DO:
   RUN releaseh_UI IN brwproc.
   IF VALID-HANDLE(brwproc) THEN DELETE PROCEDURE brwproc NO-ERROR.
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   CREATE BROWSE brwh.
   RUN DYNTTSKARP.P PERSISTENT SET brwproc (INPUT brwh,INPUT FRAME {&FRAME-NAME}:HANDLE). 
   
  

   RUN enable_UI.
   ENABLE ALL WITH FRAME {&FRAME-NAME}. 
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
  DISPLAY FILL-IN-TAB FILL-IN-WHERE FILL-IN-startc FILL-IN-TILL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-TAB FILL-IN-WHERE BTN_STARTA BTN_FLYTTA FILL-IN-startc 
         FILL-IN-TILL BTN_QUIT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE offend_UI C-Win 
PROCEDURE offend_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN ttfillnext_UI IN brwproc.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

