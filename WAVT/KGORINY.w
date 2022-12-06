&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER BESTRECO AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES KALKKATEGORI

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 KALKKATEGORI.TYP KALKKATEGORI.RADNR ~
KALKKATEGORI.NAMN KALKKATEGORI.VINAMN KALKKATEGORI.PRIS KALKKATEGORI.OPRIS ~
KALKKATEGORI.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 KALKKATEGORI.RADNR ~
KALKKATEGORI.NAMN KALKKATEGORI.VINAMN KALKKATEGORI.PRIS KALKKATEGORI.OPRIS ~
KALKKATEGORI.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 KALKKATEGORI
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 KALKKATEGORI
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH KALKKATEGORI NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH KALKKATEGORI NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 KALKKATEGORI
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 KALKKATEGORI


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS KALKKATEGORI.RADNR KALKKATEGORI.NAMN ~
KALKKATEGORI.VINAMN KALKKATEGORI.PRIS KALKKATEGORI.OPRIS ~
KALKKATEGORI.OMRADE 
&Scoped-define ENABLED-TABLES KALKKATEGORI
&Scoped-define FIRST-ENABLED-TABLE KALKKATEGORI
&Scoped-Define ENABLED-OBJECTS CMB_TYP BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS KALKKATEGORI.TYP KALKKATEGORI.RADNR ~
KALKKATEGORI.NAMN KALKKATEGORI.VINAMN KALKKATEGORI.PRIS KALKKATEGORI.OPRIS ~
KALKKATEGORI.OMRADE 
&Scoped-define DISPLAYED-TABLES KALKKATEGORI
&Scoped-define FIRST-DISPLAYED-TABLE KALKKATEGORI
&Scoped-Define DISPLAYED-OBJECTS CMB_TYP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "AVBRYT":L 
     SIZE 11 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 10.5 BY 1.

DEFINE VARIABLE CMB_TYP AS CHARACTER FORMAT "X(256)":U 
     LABEL "TYPER" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "?" 
     DROP-DOWN-LIST
     SIZE 11.88 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      KALKKATEGORI SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     KALKKATEGORI.TYP AT ROW 2.46 COL 10.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     CMB_TYP AT ROW 2.46 COL 31 COLON-ALIGNED
     KALKKATEGORI.RADNR AT ROW 3.46 COL 10.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     KALKKATEGORI.NAMN AT ROW 4.46 COL 10.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     KALKKATEGORI.VINAMN AT ROW 5.67 COL 10.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     KALKKATEGORI.PRIS AT ROW 7.25 COL 9.88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     KALKKATEGORI.OPRIS AT ROW 8.33 COL 9.88 COLON-ALIGNED
          LABEL "Ö-PRIS"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     KALKKATEGORI.OMRADE AT ROW 9.5 COL 9.88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     BTN_OK AT ROW 13.04 COL 10.75
     BTN_AVBRYT AT ROW 13.04 COL 25.25
     SPACE(12.99) SKIP(1.01)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "KALKKATEGORI":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN KALKKATEGORI.OPRIS IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN KALKKATEGORI.TYP IN FRAME DIALOG-3
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt9.KALKKATEGORI"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* AVBRYT */
DO:
  {muswait.i}
  musz = TRUE.
  IF bestreco = ? THEN DO:
    FIND FIRST KALKKATEGORI WHERE RECID(KALKKATEGORI) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE KALKKATEGORI THEN DELETE KALKKATEGORI.
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* OK */
DO:
  {muswait.i}
  DO TRANSACTION:
    ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
    APPLY "GO" TO FRAME {&FRAME-NAME}.    
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TYP DIALOG-3
ON VALUE-CHANGED OF CMB_TYP IN FRAME DIALOG-3 /* TYPER */
DO:
   CMB_TYP = INPUT CMB_TYP.
   KALKKATEGORI.TYP = CMB_TYP.
   DISPLAY KALKKATEGORI.TYP WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   FOR EACH KALKTYP NO-LOCK:
      status-ok = CMB_TYP:ADD-LAST(KALKTYP.TYP).
   END.
   /*status-ok = CMB_TYP:DELETE("")*/
   

   IF BESTRECO = ? THEN DO TRANSACTION:
      CREATE KALKKATEGORI.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
      brec = RECID(KALKKATEGORI).
      CMB_TYP:SCREEN-VALUE = "PERS".
      CMB_TYP = INPUT CMB_TYP.
      KALKKATEGORI.TYP = CMB_TYP. 
   END.
   ELSE DO TRANSACTION:
      FIND KALKKATEGORI WHERE RECID(KALKKATEGORI) = BESTRECO EXCLUSIVE-LOCK.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + KALKKATEGORI.NAMN.
      brec = RECID(KALKKATEGORI).
      CMB_TYP:SCREEN-VALUE = KALKKATEGORI.TYP.
      CMB_TYP = INPUT CMB_TYP.
   END.    
   
   RUN enable_UI.
   {musarrow.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY CMB_TYP 
      WITH FRAME DIALOG-3.
  IF AVAILABLE KALKKATEGORI THEN 
    DISPLAY KALKKATEGORI.TYP KALKKATEGORI.RADNR KALKKATEGORI.NAMN 
          KALKKATEGORI.VINAMN KALKKATEGORI.PRIS KALKKATEGORI.OPRIS 
          KALKKATEGORI.OMRADE 
      WITH FRAME DIALOG-3.
  ENABLE CMB_TYP KALKKATEGORI.RADNR KALKKATEGORI.NAMN KALKKATEGORI.VINAMN 
         KALKKATEGORI.PRIS KALKKATEGORI.OPRIS KALKKATEGORI.OMRADE BTN_OK 
         BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

