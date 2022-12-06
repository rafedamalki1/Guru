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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TRAKTATAB

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 TRAKTATAB.TRAKTKOD ~
TRAKTATAB.TRAAVTAL TRAKTATAB.VILART TRAKTATAB.FORKL TRAKTATAB.ERSATTNING ~
TRAKTATAB.VALBAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 TRAKTATAB.TRAKTKOD ~
TRAKTATAB.TRAAVTAL TRAKTATAB.VILART TRAKTATAB.FORKL TRAKTATAB.ERSATTNING ~
TRAKTATAB.VALBAR 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 TRAKTATAB
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 TRAKTATAB
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH TRAKTATAB SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH TRAKTATAB SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 TRAKTATAB
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 TRAKTATAB


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS TRAKTATAB.TRAKTKOD TRAKTATAB.TRAAVTAL ~
TRAKTATAB.VILART TRAKTATAB.FORKL TRAKTATAB.ERSATTNING TRAKTATAB.VALBAR 
&Scoped-define ENABLED-TABLES TRAKTATAB
&Scoped-define FIRST-ENABLED-TABLE TRAKTATAB
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS TRAKTATAB.TRAKTKOD TRAKTATAB.TRAAVTAL ~
TRAKTATAB.VILART TRAKTATAB.FORKL TRAKTATAB.ERSATTNING TRAKTATAB.VALBAR 
&Scoped-define DISPLAYED-TABLES TRAKTATAB
&Scoped-define FIRST-DISPLAYED-TABLE TRAKTATAB


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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      TRAKTATAB SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     TRAKTATAB.TRAKTKOD AT ROW 1.25 COL 19 COLON-ALIGNED
          LABEL "Traktkod"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     TRAKTATAB.TRAAVTAL AT ROW 2.5 COL 19 COLON-ALIGNED
          LABEL "Traavtal"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     TRAKTATAB.VILART AT ROW 3.75 COL 19 COLON-ALIGNED
          LABEL "Visad löneart"
          VIEW-AS FILL-IN 
          SIZE 5.63 BY .92
     TRAKTATAB.FORKL AT ROW 4.96 COL 19 COLON-ALIGNED
          LABEL "Text" FORMAT "X(45)"
          VIEW-AS FILL-IN 
          SIZE 35.5 BY 1
     TRAKTATAB.ERSATTNING AT ROW 6.25 COL 19 COLON-ALIGNED
          LABEL "Ersättning kr"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     TRAKTATAB.VALBAR AT ROW 7.5 COL 19 COLON-ALIGNED
          LABEL "Valbar"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     BTN_OK AT ROW 12.5 COL 11
     BTN_AVBRYT AT ROW 12.5 COL 25.5
     SPACE(22.87) SKIP(1.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Traktatab":L.


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

/* SETTINGS FOR FILL-IN TRAKTATAB.ERSATTNING IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN TRAKTATAB.FORKL IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN TRAKTATAB.TRAAVTAL IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN TRAKTATAB.TRAKTKOD IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN TRAKTATAB.VALBAR IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN TRAKTATAB.VILART IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt9.TRAKTATAB"
     _Options          = "SHARE-LOCK"
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
    FIND FIRST TRAKTATAB WHERE RECID(TRAKTATAB) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE TRAKTATAB THEN DELETE TRAKTATAB.
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
   IF BESTRECO = ? THEN DO TRANSACTION:
     CREATE TRAKTATAB.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(TRAKTATAB).
   END.
   ELSE DO TRANSACTION:
     FIND TRAKTATAB WHERE RECID(TRAKTATAB) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + TRAKTATAB.TRAKTKOD.
     brec = RECID(TRAKTATAB).
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
  IF AVAILABLE TRAKTATAB THEN 
    DISPLAY TRAKTATAB.TRAKTKOD TRAKTATAB.TRAAVTAL TRAKTATAB.VILART TRAKTATAB.FORKL 
          TRAKTATAB.ERSATTNING TRAKTATAB.VALBAR 
      WITH FRAME DIALOG-3.
  ENABLE TRAKTATAB.TRAKTKOD TRAKTATAB.TRAAVTAL TRAKTATAB.VILART TRAKTATAB.FORKL 
         TRAKTATAB.ERSATTNING TRAKTATAB.VALBAR BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

