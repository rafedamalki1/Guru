&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r10 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 -  1:43 pm

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

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-21

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ERSATTNING

/* Definitions for BROWSE BROWSE-21                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-21 ERSATTNING.LONTILLAGG ~
ERSATTNING.KOD ERSATTNING.LONKODTEXT ERSATTNING.ENHET ERSATTNING.MOMS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-21 
&Scoped-define QUERY-STRING-BROWSE-21 FOR EACH ERSATTNING NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-21 OPEN QUERY BROWSE-21 FOR EACH ERSATTNING NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-21 ERSATTNING
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-21 ERSATTNING


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-21}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-21 BTN_NY BTN_AND BTN_BORT BTN_AVSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "?ndra":L 
     SIZE 10 BY 1.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-21 FOR 
      ERSATTNING SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-21 WINDOW-1 _STRUCTURED
  QUERY BROWSE-21 NO-LOCK DISPLAY
      ERSATTNING.LONTILLAGG FORMAT "x(4)":U
      ERSATTNING.KOD FORMAT "x(2)":U
      ERSATTNING.LONKODTEXT FORMAT "x(20)":U
      ERSATTNING.ENHET FORMAT "x(2)":U
      ERSATTNING.MOMS FORMAT "JA/NEJ":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 76 BY 8.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-21 AT ROW 3 COL 3.5
     BTN_NY AT ROW 14 COL 6.5
     BTN_AND AT ROW 14 COL 21.5
     BTN_BORT AT ROW 14 COL 36
     BTN_AVSL AT ROW 15.25 COL 70
     "Ersattning" VIEW-AS TEXT
          SIZE 18 BY 1.5 AT ROW 12 COL 6.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 81.88 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ersattning"
         COLUMN             = 16
         ROW                = 11.25
         HEIGHT             = 16.42
         WIDTH              = 82.38
         MAX-HEIGHT         = 20.17
         MAX-WIDTH          = 83.13
         VIRTUAL-HEIGHT     = 20.17
         VIRTUAL-WIDTH      = 83.13
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BROWSE-21 TEXT-1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-21
/* Query rebuild information for BROWSE BROWSE-21
     _TblList          = "rt9.ERSATTNING"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = rt9.ERSATTNING.LONTILLAGG
     _FldNameList[2]   = rt9.ERSATTNING.KOD
     _FldNameList[3]   = rt9.ERSATTNING.LONKODTEXT
     _FldNameList[4]   = rt9.ERSATTNING.ENHET
     _FldNameList[5]   = rt9.ERSATTNING.MOMS
     _Query            is OPENED
*/  /* BROWSE BROWSE-21 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-21
&Scoped-define SELF-NAME BROWSE-21
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-21 WINDOW-1
ON VALUE-CHANGED OF BROWSE-21 IN FRAME FRAME-A
DO:
  brec = RECID(ERSATTNING).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* ?ndra */
DO:
  RUN andra.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WINDOW-1
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-A /* AVSLUTA */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Bort */
DO:
  RUN bort.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
  RUN ny.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(ERSATTNING).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN ERSANY.W (INPUT RECID(ERSATTNING)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH ERSATTNING NO-LOCK.
    FIND FIRST ERSATTNING WHERE RECID(ERSATTNING) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE ERSATTNING THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort WINDOW-1 
PROCEDURE bort :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA ERSATTNING?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE ERSATTNING.LONTILLAGG
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND ERSATTNING WHERE RECID(ERSATTNING) = brec NO-LOCK NO-ERROR.
    FIND NEXT ERSATTNING NO-LOCK NO-ERROR.
    IF AVAILABLE ERSATTNING THEN borec = RECID(ERSATTNING).
    ELSE DO:
      FIND ERSATTNING WHERE RECID(ERSATTNING) = brec NO-LOCK NO-ERROR.
      FIND PREV ERSATTNING NO-LOCK NO-ERROR.
      IF AVAILABLE ERSATTNING THEN borec = RECID(ERSATTNING).
    END.
    FIND ERSATTNING WHERE RECID(ERSATTNING) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE ERSATTNING THEN DELETE ERSATTNING.
    OPEN QUERY {&BROWSE-NAME} FOR EACH ERSATTNING NO-LOCK. 
    brec = borec.
    FIND FIRST ERSATTNING WHERE RECID(ERSATTNING) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE ERSATTNING THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
    END.  
    {musarrow.i}   
  END. 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  ENABLE BROWSE-21 BTN_NY BTN_AND BTN_BORT BTN_AVSL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny WINDOW-1 
PROCEDURE ny :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN ERSANY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH ERSATTNING NO-LOCK.
    FIND FIRST ERSATTNING WHERE RECID(ERSATTNING) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE ERSATTNING THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

