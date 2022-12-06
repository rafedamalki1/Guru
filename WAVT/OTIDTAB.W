&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
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
DEFINE NEW SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES OVERTIDTAB

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 OVERTIDTAB.DAGNR ~
OVERTIDTAB.OVERTIDTILL OVERTIDTAB.KOD OVERTIDTAB.START1 OVERTIDTAB.STOPP1 ~
OVERTIDTAB.START2 OVERTIDTAB.STOPP2 OVERTIDTAB.OVERTIDUTTAG ~
OVERTIDTAB.ARBSTART OVERTIDTAB.ARBSLUT OVERTIDTAB.FORKL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH OVERTIDTAB NO-LOCK ~
    BY OVERTIDTAB.KOD ~
       BY OVERTIDTAB.DAGNR ~
        BY OVERTIDTAB.OVERTIDTILL ~
         BY OVERTIDTAB.OVERTIDUTTAG
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH OVERTIDTAB NO-LOCK ~
    BY OVERTIDTAB.KOD ~
       BY OVERTIDTAB.DAGNR ~
        BY OVERTIDTAB.OVERTIDTILL ~
         BY OVERTIDTAB.OVERTIDUTTAG.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 OVERTIDTAB
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 OVERTIDTAB


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 BTN_NY BTN_AND BTN_BORT BTN_AVSL ~
FILL-IN_KOD 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_KOD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
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

DEFINE VARIABLE FILL-IN_KOD AS CHARACTER FORMAT "X(2)" INITIAL ? 
     LABEL "KOD" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      OVERTIDTAB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 WINDOW-1 _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      OVERTIDTAB.DAGNR FORMAT "9":U
      OVERTIDTAB.OVERTIDTILL FORMAT "x(8)":U
      OVERTIDTAB.KOD FORMAT "x(2)":U
      OVERTIDTAB.START1 FORMAT "-99999":U
      OVERTIDTAB.STOPP1 FORMAT "-99999":U
      OVERTIDTAB.START2 FORMAT "-99999":U
      OVERTIDTAB.STOPP2 FORMAT "-99999":U
      OVERTIDTAB.OVERTIDUTTAG FORMAT "x(1)":U
      OVERTIDTAB.ARBSTART FORMAT "JA/NEJ":U
      OVERTIDTAB.ARBSLUT FORMAT "yes/no":U
      OVERTIDTAB.FORKL FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 94.5 BY 16.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-1 AT ROW 2.5 COL 3
     BTN_NY AT ROW 20.5 COL 5.63
     BTN_AND AT ROW 20.5 COL 20.63
     BTN_BORT AT ROW 20.5 COL 35.13
     BTN_AVSL AT ROW 22.75 COL 89
     FILL-IN_KOD AT ROW 23.5 COL 18 COLON-ALIGNED
     "Overtidtab" VIEW-AS TEXT
          SIZE 18 BY .96 AT ROW 19.25 COL 5.13
     "Sök på:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 23 COL 2.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.88 BY 23.54.


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
         TITLE              = "Overtidtab"
         COLUMN             = 3.25
         ROW                = 8.33
         HEIGHT             = 23.67
         WIDTH              = 99.75
         MAX-HEIGHT         = 23.67
         MAX-WIDTH          = 99.75
         VIRTUAL-HEIGHT     = 23.67
         VIRTUAL-WIDTH      = 99.75
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-1 TEXT-2 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "RT9.OVERTIDTAB"
     _Options          = "NO-LOCK"
     _OrdList          = "RT9.OVERTIDTAB.KOD|yes,RT9.OVERTIDTAB.DAGNR|yes,RT9.OVERTIDTAB.OVERTIDTILL|yes,RT9.OVERTIDTAB.OVERTIDUTTAG|yes"
     _FldNameList[1]   = RT9.OVERTIDTAB.DAGNR
     _FldNameList[2]   = RT9.OVERTIDTAB.OVERTIDTILL
     _FldNameList[3]   = RT9.OVERTIDTAB.KOD
     _FldNameList[4]   = RT9.OVERTIDTAB.START1
     _FldNameList[5]   = RT9.OVERTIDTAB.STOPP1
     _FldNameList[6]   = RT9.OVERTIDTAB.START2
     _FldNameList[7]   = RT9.OVERTIDTAB.STOPP2
     _FldNameList[8]   = RT9.OVERTIDTAB.OVERTIDUTTAG
     _FldNameList[9]   = RT9.OVERTIDTAB.ARBSTART
     _FldNameList[10]   = RT9.OVERTIDTAB.ARBSLUT
     _FldNameList[11]   = RT9.OVERTIDTAB.FORKL
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 WINDOW-1
ON VALUE-CHANGED OF BROWSE-1 IN FRAME FRAME-A
DO:
  brec = RECID(OVERTIDTAB).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
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


&Scoped-define SELF-NAME FILL-IN_KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_KOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_KOD IN FRAME FRAME-A /* KOD */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   posok = "".
   FILL-IN_KOD = INPUT FILL-IN_KOD.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(OVERTIDTAB).
   IF FILL-IN_KOD = "" THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   posok = FILL-IN_KOD.

   FIND OVERTIDTAB WHERE RECID(OVERTIDTAB) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT OVERTIDTAB WHERE  OVERTIDTAB.KOD = posok       
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE OVERTIDTAB THEN DO:
      MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE OVERTIDTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(OVERTIDTAB).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
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
  brec = RECID(OVERTIDTAB).
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
  RUN OTIDNY.W (INPUT RECID(OVERTIDTAB)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERTIDTAB USE-INDEX OVER NO-LOCK.
    FIND FIRST OVERTIDTAB WHERE RECID(OVERTIDTAB) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE OVERTIDTAB THEN DO:
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
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA OVERTIDTAB?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE OVERTIDTAB.OVERTIDTILL
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND OVERTIDTAB WHERE RECID(OVERTIDTAB) = brec NO-LOCK NO-ERROR.
    FIND NEXT OVERTIDTAB NO-LOCK NO-ERROR.
    IF AVAILABLE OVERTIDTAB THEN borec = RECID(OVERTIDTAB).
    ELSE DO:
      FIND OVERTIDTAB WHERE RECID(OVERTIDTAB) = brec NO-LOCK NO-ERROR.
      FIND PREV OVERTIDTAB NO-LOCK NO-ERROR.
      IF AVAILABLE OVERTIDTAB THEN borec = RECID(OVERTIDTAB).
    END.
    FIND OVERTIDTAB WHERE RECID(OVERTIDTAB) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE OVERTIDTAB THEN DELETE OVERTIDTAB.
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERTIDTAB USE-INDEX OVER NO-LOCK. 
    brec = borec.
    FIND FIRST OVERTIDTAB WHERE RECID(OVERTIDTAB) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE OVERTIDTAB THEN DO:
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
  DISPLAY FILL-IN_KOD 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BROWSE-1 BTN_NY BTN_AND BTN_BORT BTN_AVSL FILL-IN_KOD 
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
  RUN OTIDNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERTIDTAB USE-INDEX OVER NO-LOCK.
    FIND FIRST OVERTIDTAB WHERE RECID(OVERTIDTAB) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE OVERTIDTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

