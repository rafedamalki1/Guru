&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_OVER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES OVERKOD

/* Definitions for BROWSE BRW_OVER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_OVER OVERKOD.OVERTIDTILL OVERKOD.KOD ~
OVERKOD.VILART OVERKOD.LONKODTEXT OVERKOD.OVERTIDUTTAG OVERKOD.ERSATTNING ~
OVERKOD.ENKEL OVERKOD.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OVER OVERKOD.OVERTIDTILL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OVER OVERKOD
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OVER OVERKOD
&Scoped-define QUERY-STRING-BRW_OVER FOR EACH OVERKOD NO-LOCK ~
    BY OVERKOD.KOD ~
       BY OVERKOD.VILART INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OVER OPEN QUERY BRW_OVER FOR EACH OVERKOD NO-LOCK ~
    BY OVERKOD.KOD ~
       BY OVERKOD.VILART INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OVER OVERKOD
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OVER OVERKOD


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_OVER}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_OVER BTN_NY BTN_AND BTN_BORT BTN_AVSL 

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
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.17.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OVER FOR 
      OVERKOD SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OVER WINDOW-1 _STRUCTURED
  QUERY BRW_OVER NO-LOCK DISPLAY
      OVERKOD.OVERTIDTILL COLUMN-LABEL "Löneart" FORMAT "x(4)":U
      OVERKOD.KOD COLUMN-LABEL "Kod" FORMAT "x(2)":U
      OVERKOD.VILART COLUMN-LABEL "Visad! löneart" FORMAT "X(8)":U
      OVERKOD.LONKODTEXT COLUMN-LABEL "Text" FORMAT "x(30)":U
      OVERKOD.OVERTIDUTTAG COLUMN-LABEL "Uttag" FORMAT "x(1)":U
      OVERKOD.ERSATTNING COLUMN-LABEL "Ersättning kr" FORMAT "->>,>>9.99":U
      OVERKOD.ENKEL COLUMN-LABEL "Enke/!Kval" FORMAT "x(4)":U
      OVERKOD.ENHET COLUMN-LABEL "Enhet" FORMAT "x(2)":U
  ENABLE
      OVERKOD.OVERTIDTILL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 96.63 BY 21.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_OVER AT ROW 2.58 COL 2.38
     BTN_NY AT ROW 26.75 COL 7
     BTN_AND AT ROW 26.75 COL 22
     BTN_BORT AT ROW 26.75 COL 36.5
     BTN_AVSL AT ROW 27.25 COL 88.5
     "Overkod" VIEW-AS TEXT
          SIZE 18 BY 1.5 AT ROW 24.75 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.5 BY 27.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Overkod"
         COLUMN             = 8.38
         ROW                = 8.25
         HEIGHT             = 28
         WIDTH              = 100
         MAX-HEIGHT         = 28
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 28
         VIRTUAL-WIDTH      = 100
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
/* BROWSE-TAB BRW_OVER TEXT-1 FRAME-A */
ASSIGN 
       BRW_OVER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_OVER:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OVER
/* Query rebuild information for BROWSE BRW_OVER
     _TblList          = "rt9.OVERKOD"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "rt9.OVERKOD.KOD|yes,rt9.OVERKOD.VILART|yes"
     _FldNameList[1]   > rt9.OVERKOD.OVERTIDTILL
"OVERTIDTILL" "Löneart" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > rt9.OVERKOD.KOD
"KOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > rt9.OVERKOD.VILART
"VILART" "Visad! löneart" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > rt9.OVERKOD.LONKODTEXT
"LONKODTEXT" "Text" "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > rt9.OVERKOD.OVERTIDUTTAG
"OVERTIDUTTAG" "Uttag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > rt9.OVERKOD.ERSATTNING
"ERSATTNING" "Ersättning kr" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > rt9.OVERKOD.ENKEL
"ENKEL" "Enke/!Kval" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > rt9.OVERKOD.ENHET
"ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_OVER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_OVER
&Scoped-define SELF-NAME BRW_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OVER WINDOW-1
ON VALUE-CHANGED OF BRW_OVER IN FRAME FRAME-A
DO:
  brec = RECID(OVERKOD).
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
   {ALLSTARTDYN.I}
  RUN enable_UI.
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(OVERKOD).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   
   /*RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_OVER:HANDLE IN FRAME {&FRAME-NAME}).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN OKODNY.W (INPUT RECID(OVERKOD)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERKOD USE-INDEX VILART NO-LOCK.
    /*RUN openbdynspec_UI IN brwproc[1].            */
    FIND FIRST OVERKOD WHERE RECID(OVERKOD) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE OVERKOD THEN DO:
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
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA OVERKOD?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE OVERKOD.OVERTIDTILL
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND OVERKOD WHERE RECID(OVERKOD) = brec NO-LOCK NO-ERROR.
    FIND NEXT OVERKOD NO-LOCK NO-ERROR.
    IF AVAILABLE OVERKOD THEN borec = RECID(OVERKOD).
    ELSE DO:
      FIND OVERKOD WHERE RECID(OVERKOD) = brec NO-LOCK NO-ERROR.
      FIND PREV OVERKOD NO-LOCK NO-ERROR.
      IF AVAILABLE OVERKOD THEN borec = RECID(OVERKOD).
    END.
    FIND OVERKOD WHERE RECID(OVERKOD) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE OVERKOD THEN DELETE OVERKOD.
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERKOD USE-INDEX VILART NO-LOCK. 
    /*RUN openbdynspec_UI IN brwproc[1].            */
    brec = borec.
    FIND FIRST OVERKOD WHERE RECID(OVERKOD) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE OVERKOD THEN DO:
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
  ENABLE BRW_OVER BTN_NY BTN_AND BTN_BORT BTN_AVSL 
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
  RUN OKODNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH OVERKOD USE-INDEX VILART NO-LOCK.
    /*RUN openbdynspec_UI IN brwproc[1].            */
    FIND FIRST OVERKOD WHERE RECID(OVERKOD) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE OVERKOD THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

