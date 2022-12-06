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
&Scoped-define BROWSE-NAME BROWSE-8

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES UTRYCKNING

/* Definitions for BROWSE BROWSE-8                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-8 UTRYCKNING.KOD ~
UTRYCKNING.UTRYCKNBER UTRYCKNING.UTRYCKNEJBER UTRYCKNING.KLOCKAN ~
UTRYCKNING.BER2 UTRYCKNING.EJBER2 UTRYCKNING.AVBE UTRYCKNING.ENDMALTID ~
UTRYCKNING.EXAKTM UTRYCKNING.EXTID UTRYCKNING.EXTRAARBPASS UTRYCKNING.HALV ~
UTRYCKNING.INTERNAT UTRYCKNING.UTRYCK1 UTRYCKNING.LAGOVER UTRYCKNING.LUFT ~
UTRYCKNING.NODF UTRYCKNING.OB UTRYCKNING.OVLAPP UTRYCKNING.PARA8 ~
UTRYCKNING.RHALV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-8 
&Scoped-define QUERY-STRING-BROWSE-8 FOR EACH UTRYCKNING NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-8 OPEN QUERY BROWSE-8 FOR EACH UTRYCKNING NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-8 UTRYCKNING
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-8 UTRYCKNING


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-8}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-8 BTN_NY BTN_AND BTN_BORT BTN_AVSL 

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
DEFINE QUERY BROWSE-8 FOR 
      UTRYCKNING SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-8 WINDOW-1 _STRUCTURED
  QUERY BROWSE-8 NO-LOCK DISPLAY
      UTRYCKNING.KOD FORMAT "x(2)":U
      UTRYCKNING.UTRYCKNBER FORMAT "99999":U
      UTRYCKNING.UTRYCKNEJBER FORMAT "99999":U
      UTRYCKNING.KLOCKAN FORMAT "yes/no":U
      UTRYCKNING.BER2 FORMAT "99999":U
      UTRYCKNING.EJBER2 FORMAT "99999":U
      UTRYCKNING.AVBE FORMAT "JA/NEJ":U
      UTRYCKNING.ENDMALTID FORMAT "JA/NEJ":U
      UTRYCKNING.EXAKTM FORMAT "JA/NEJ":U
      UTRYCKNING.EXTID FORMAT "yes/no":U
      UTRYCKNING.EXTRAARBPASS FORMAT "9999":U
      UTRYCKNING.HALV FORMAT "yes/no":U
      UTRYCKNING.INTERNAT FORMAT "JA/NEJ":U
      UTRYCKNING.UTRYCK1 FORMAT "ja/nej":U
      UTRYCKNING.LAGOVER FORMAT "yes/no":U
      UTRYCKNING.LUFT FORMAT "yes/no":U
      UTRYCKNING.NODF FORMAT "JA/NEJ":U
      UTRYCKNING.OB FORMAT "yes/no":U
      UTRYCKNING.OVLAPP FORMAT "JA/NEJ":U
      UTRYCKNING.PARA8 FORMAT "JA/NEJ":U
      UTRYCKNING.RHALV FORMAT "yes/no":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 98.5 BY 14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-8 AT ROW 2.5 COL 2
     BTN_NY AT ROW 18.75 COL 6.5
     BTN_AND AT ROW 18.75 COL 21.5
     BTN_BORT AT ROW 18.75 COL 36
     BTN_AVSL AT ROW 19.75 COL 89
     "Utryckning" VIEW-AS TEXT
          SIZE 18 BY 1.5 AT ROW 16.75 COL 6.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 99.63 BY 19.79.


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
         TITLE              = "Utryckning"
         COLUMN             = 18.88
         ROW                = 7.63
         HEIGHT             = 20.38
         WIDTH              = 100
         MAX-HEIGHT         = 20.38
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 20.38
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
                                                                        */
/* BROWSE-TAB BROWSE-8 TEXT-1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-8
/* Query rebuild information for BROWSE BROWSE-8
     _TblList          = "RT9.UTRYCKNING"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = RT9.UTRYCKNING.KOD
     _FldNameList[2]   = RT9.UTRYCKNING.UTRYCKNBER
     _FldNameList[3]   = RT9.UTRYCKNING.UTRYCKNEJBER
     _FldNameList[4]   = RT9.UTRYCKNING.KLOCKAN
     _FldNameList[5]   = RT9.UTRYCKNING.BER2
     _FldNameList[6]   = RT9.UTRYCKNING.EJBER2
     _FldNameList[7]   = RT9.UTRYCKNING.AVBE
     _FldNameList[8]   = RT9.UTRYCKNING.ENDMALTID
     _FldNameList[9]   = RT9.UTRYCKNING.EXAKTM
     _FldNameList[10]   = RT9.UTRYCKNING.EXTID
     _FldNameList[11]   = RT9.UTRYCKNING.EXTRAARBPASS
     _FldNameList[12]   = RT9.UTRYCKNING.HALV
     _FldNameList[13]   = RT9.UTRYCKNING.INTERNAT
     _FldNameList[14]   = RT9.UTRYCKNING.UTRYCK1
     _FldNameList[15]   = RT9.UTRYCKNING.LAGOVER
     _FldNameList[16]   = RT9.UTRYCKNING.LUFT
     _FldNameList[17]   = RT9.UTRYCKNING.NODF
     _FldNameList[18]   = RT9.UTRYCKNING.OB
     _FldNameList[19]   = RT9.UTRYCKNING.OVLAPP
     _FldNameList[20]   = RT9.UTRYCKNING.PARA8
     _FldNameList[21]   = RT9.UTRYCKNING.RHALV
     _Query            is OPENED
*/  /* BROWSE BROWSE-8 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-8
&Scoped-define SELF-NAME BROWSE-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-8 WINDOW-1
ON VALUE-CHANGED OF BROWSE-8 IN FRAME FRAME-A
DO:
  brec = RECID(UTRYCKNING).
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
  RUN enable_UI.
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(UTRYCKNING).
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
  RUN UTRYCKNY.W (INPUT RECID(UTRYCKNING)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH UTRYCKNING NO-LOCK.
    FIND FIRST UTRYCKNING WHERE RECID(UTRYCKNING) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE UTRYCKNING THEN DO:
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
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA UTRYCKNING?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE UTRYCKNING.KOD
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND UTRYCKNING WHERE RECID(UTRYCKNING) = brec NO-LOCK NO-ERROR.
    FIND NEXT UTRYCKNING NO-LOCK NO-ERROR.
    IF AVAILABLE UTRYCKNING THEN borec = RECID(UTRYCKNING).
    ELSE DO:
      FIND UTRYCKNING WHERE RECID(UTRYCKNING) = brec NO-LOCK NO-ERROR.
      FIND PREV UTRYCKNING NO-LOCK NO-ERROR.
      IF AVAILABLE UTRYCKNING THEN borec = RECID(UTRYCKNING).
    END.
    FIND UTRYCKNING WHERE RECID(UTRYCKNING) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE UTRYCKNING THEN DELETE UTRYCKNING.
    OPEN QUERY {&BROWSE-NAME} FOR EACH UTRYCKNING NO-LOCK. 
    brec = borec.
    FIND FIRST UTRYCKNING WHERE RECID(UTRYCKNING) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE UTRYCKNING THEN DO:
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
  ENABLE BROWSE-8 BTN_NY BTN_AND BTN_BORT BTN_AVSL 
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
  RUN UTRYCKNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH UTRYCKNING NO-LOCK.
    FIND FIRST UTRYCKNING WHERE RECID(UTRYCKNING) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE UTRYCKNING THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

