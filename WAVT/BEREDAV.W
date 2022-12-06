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
&Scoped-define NEW
{GLOBVAR2DEL1.I}

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE BUFFER bavtab FOR BEREDSKAPAV.

{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-6

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BEREDSKAPAV

/* Definitions for BROWSE BROWSE-6                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-6 BEREDSKAPAV.BEREDSKAPSAVTAL ~
BEREDSKAPAV.DATUM BEREDSKAPAV.BEREDSKAP BEREDSKAPAV.BERSTART1 ~
BEREDSKAPAV.BERSTOPP1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-6 
&Scoped-define QUERY-STRING-BROWSE-6 FOR EACH BEREDSKAPAV NO-LOCK ~
    BY BEREDSKAPAV.DATUM ~
       BY BEREDSKAPAV.BEREDSKAPSAVTAL ~
        BY BEREDSKAPAV.BEREDSKAP ~
         BY BEREDSKAPAV.BERSTART1 ~
          BY BEREDSKAPAV.BERSTOPP1
&Scoped-define OPEN-QUERY-BROWSE-6 OPEN QUERY BROWSE-6 FOR EACH BEREDSKAPAV NO-LOCK ~
    BY BEREDSKAPAV.DATUM ~
       BY BEREDSKAPAV.BEREDSKAPSAVTAL ~
        BY BEREDSKAPAV.BEREDSKAP ~
         BY BEREDSKAPAV.BERSTART1 ~
          BY BEREDSKAPAV.BERSTOPP1.
&Scoped-define TABLES-IN-QUERY-BROWSE-6 BEREDSKAPAV
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-6 BEREDSKAPAV


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-6}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-6 BTN_NY BTN_AND BTN_BORT ~
FILL-IN_DAT2 FILL-IN_DAT3 FILL-IN_DATUM FILL-IN-KOD FILL-IN_DAT4 ~
FILL-IN_DAT5 BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_DAT2 FILL-IN_DAT3 FILL-IN_DATUM ~
FILL-IN-KOD FILL-IN_DAT4 FILL-IN_DAT5 

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
     SIZE 11 BY 1.17.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "x(2)" 
     LABEL "KOD" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.

DEFINE VARIABLE FILL-IN_DAT2 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

DEFINE VARIABLE FILL-IN_DAT3 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

DEFINE VARIABLE FILL-IN_DAT4 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

DEFINE VARIABLE FILL-IN_DAT5 AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "DATUM" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-6 FOR 
      BEREDSKAPAV SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-6 WINDOW-1 _STRUCTURED
  QUERY BROWSE-6 NO-LOCK DISPLAY
      BEREDSKAPAV.BEREDSKAPSAVTAL FORMAT "XX":U
      BEREDSKAPAV.DATUM FORMAT "99/99/99":U
      BEREDSKAPAV.BEREDSKAP FORMAT "x(4)":U
      BEREDSKAPAV.BERSTART1 FORMAT "99.99":U
      BEREDSKAPAV.BERSTOPP1 FORMAT "99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 72 BY 14.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-6 AT ROW 1.33 COL 5
     BTN_NY AT ROW 17.83 COL 5.88
     BTN_AND AT ROW 17.83 COL 20.88
     BTN_BORT AT ROW 17.83 COL 35.38
     FILL-IN_DAT2 AT ROW 20.75 COL 31.13 COLON-ALIGNED
     FILL-IN_DAT3 AT ROW 20.75 COL 49.25 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 20.79 COL 9.5 COLON-ALIGNED
     FILL-IN-KOD AT ROW 23.54 COL 29.25 COLON-ALIGNED
     FILL-IN_DAT4 AT ROW 23.54 COL 41.38 COLON-ALIGNED
     FILL-IN_DAT5 AT ROW 23.54 COL 58.13 COLON-ALIGNED
     BTN_AVSL AT ROW 25.25 COL 69
     "Sök kopiera:" VIEW-AS TEXT
          SIZE 13.25 BY .88 AT ROW 22.25 COL 26
     "Sök byt:" VIEW-AS TEXT
          SIZE 9.38 BY .67 AT ROW 19.79 COL 26.38
     "Sök på:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 19.79 COL 3.13
     "Beredskapav" VIEW-AS TEXT
          SIZE 18 BY .83 AT ROW 16.58 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 26.63.


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
         TITLE              = "Beredskapav"
         COLUMN             = 37.25
         ROW                = 10.5
         HEIGHT             = 26.96
         WIDTH              = 80.5
         MAX-HEIGHT         = 26.96
         MAX-WIDTH          = 83.13
         VIRTUAL-HEIGHT     = 26.96
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-6 TEXT-2 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-6
/* Query rebuild information for BROWSE BROWSE-6
     _TblList          = "rt9.BEREDSKAPAV"
     _Options          = "NO-LOCK"
     _OrdList          = "rt9.BEREDSKAPAV.DATUM|yes,rt9.BEREDSKAPAV.BEREDSKAPSAVTAL|yes,rt9.BEREDSKAPAV.BEREDSKAP|yes,rt9.BEREDSKAPAV.BERSTART1|yes,rt9.BEREDSKAPAV.BERSTOPP1|yes"
     _FldNameList[1]   = rt9.BEREDSKAPAV.BEREDSKAPSAVTAL
     _FldNameList[2]   = rt9.BEREDSKAPAV.DATUM
     _FldNameList[3]   = rt9.BEREDSKAPAV.BEREDSKAP
     _FldNameList[4]   = rt9.BEREDSKAPAV.BERSTART1
     _FldNameList[5]   = rt9.BEREDSKAPAV.BERSTOPP1
     _Query            is OPENED
*/  /* BROWSE BROWSE-6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-6
&Scoped-define SELF-NAME BROWSE-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-6 WINDOW-1
ON VALUE-CHANGED OF BROWSE-6 IN FRAME FRAME-A
DO:
  brec = RECID(BEREDSKAPAV).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
  brec = RECID(BEREDSKAPAV).
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
  brec = RECID(BEREDSKAPAV).
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


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON LEAVE OF FILL-IN-KOD IN FRAME FRAME-A /* KOD */
DO:
  FILL-IN-KOD = INPUT FILL-IN-KOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DAT3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DAT3 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DAT3 IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   
   FILL-IN_DAT2 = INPUT FILL-IN_DAT2.
   FILL-IN_DAT3 = INPUT FILL-IN_DAT3.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(BEREDSKAPAV).    
   posok = FILL-IN_DAT2.
   FOR EACH BEREDSKAPAV WHERE BEREDSKAPAV.DATUM = FILL-IN_DAT2 USE-INDEX BERAV EXCLUSIVE-LOCK:
      ASSIGN BEREDSKAPAV.DATUM = FILL-IN_DAT3.
   END.   
   OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPAV NO-LOCK INDEXED-REPOSITION.
   FIND FIRST BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR. 
   IF AVAILABLE BEREDSKAPAV THEN DO:
     REPOSITION {&BROWSE-NAME} TO RECID brec.
   END.
   /*FIND BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT BEREDSKAPAV WHERE  BEREDSKAPAV.DATUM = posok       
   USE-INDEX BERAV NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BEREDSKAPAV THEN DO:
      MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE BEREDSKAPAV THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(BEREDSKAPAV).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DAT5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DAT5 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DAT5 IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   FILL-IN_DAT4 = INPUT FILL-IN_DAT4.
   FILL-IN_DAT5 = INPUT FILL-IN_DAT5.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(BEREDSKAPAV).
     
   posok = FILL-IN_DAT4.
   IF FILL-IN-KOD NE "" THEN DO:
      FOR EACH BEREDSKAPAV WHERE BEREDSKAPAV.DATUM = FILL-IN_DAT4 
      AND BEREDSKAPAV.BEREDSKAPSAVTAL =  FILL-IN-KOD EXCLUSIVE-LOCK:
         CREATE bavtab.
         ASSIGN   
         bavtab.BEREDSKAPSAVTAL  = BEREDSKAPAV.BEREDSKAPSAVTAL
         bavtab.BEREDSKAP  = BEREDSKAPAV.BEREDSKAP
         bavtab.BERANTAL  = BEREDSKAPAV.BERANTAL
         bavtab.BERSTART1  = BEREDSKAPAV.BERSTART1
         bavtab.BERSTOPP1  = BEREDSKAPAV.BERSTOPP1
         bavtab.DATUM  = FILL-IN_DAT5.    
      END.            
   END.
   ELSE DO:
      FOR EACH BEREDSKAPAV WHERE BEREDSKAPAV.DATUM = FILL-IN_DAT4 
      EXCLUSIVE-LOCK:
         CREATE bavtab.
         ASSIGN   
         bavtab.BEREDSKAPSAVTAL  = BEREDSKAPAV.BEREDSKAPSAVTAL
         bavtab.BEREDSKAP  = BEREDSKAPAV.BEREDSKAP
         bavtab.BERANTAL  = BEREDSKAPAV.BERANTAL
         bavtab.BERSTART1  = BEREDSKAPAV.BERSTART1
         bavtab.BERSTOPP1  = BEREDSKAPAV.BERSTOPP1
         bavtab.DATUM  = FILL-IN_DAT5.    
      END.       
   END.
          
   OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPAV NO-LOCK INDEXED-REPOSITION.
   FIND FIRST BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR. 
   IF AVAILABLE BEREDSKAPAV THEN DO:
     REPOSITION {&BROWSE-NAME} TO RECID brec.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DATUM IN FRAME FRAME-A /* DATUM */
DO:
   DEFINE VARIABLE posok AS DATE NO-UNDO.   
   
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(BEREDSKAPAV).
/*   IF FILL-IN_DATUM = " THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.*/      
   posok = FILL-IN_DATUM.

   FIND BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT BEREDSKAPAV WHERE  BEREDSKAPAV.DATUM = posok       
   USE-INDEX BERAV NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BEREDSKAPAV THEN DO:
      MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE BEREDSKAPAV THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(BEREDSKAPAV).
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
  brec = RECID(BEREDSKAPAV).
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
  RUN BERAVNY.W (INPUT RECID(BEREDSKAPAV)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPAV NO-LOCK INDEXED-REPOSITION.
    FIND FIRST BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE BEREDSKAPAV THEN DO:
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
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA BEREDSKAPAV?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE BEREDSKAPAV.BEREDSKAPSAVTAL
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR.
    FIND NEXT BEREDSKAPAV NO-LOCK NO-ERROR.
    IF AVAILABLE BEREDSKAPAV THEN borec = RECID(BEREDSKAPAV).
    ELSE DO:
      FIND BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR.
      FIND PREV BEREDSKAPAV NO-LOCK NO-ERROR.
      IF AVAILABLE BEREDSKAPAV THEN borec = RECID(BEREDSKAPAV).
    END.
    FIND BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE BEREDSKAPAV THEN DELETE BEREDSKAPAV.
    OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPAV NO-LOCK INDEXED-REPOSITION. 
    brec = borec.
    FIND FIRST BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE BEREDSKAPAV THEN DO:
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
  DISPLAY FILL-IN_DAT2 FILL-IN_DAT3 FILL-IN_DATUM FILL-IN-KOD FILL-IN_DAT4 
          FILL-IN_DAT5 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BROWSE-6 BTN_NY BTN_AND BTN_BORT FILL-IN_DAT2 FILL-IN_DAT3 
         FILL-IN_DATUM FILL-IN-KOD FILL-IN_DAT4 FILL-IN_DAT5 BTN_AVSL 
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
  RUN BERAVNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPAV NO-LOCK INDEXED-REPOSITION.
    FIND FIRST BEREDSKAPAV WHERE RECID(BEREDSKAPAV) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE BEREDSKAPAV THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

