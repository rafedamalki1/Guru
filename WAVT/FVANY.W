&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r10 GUI
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

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES FVARO

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH FVARO SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH FVARO SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 FVARO
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 FVARO


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_FRKOD FILL-IN_AONR FILL-IN_DELNR ~
FILL-IN_DEL FILL-IN_PROC BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_FRKOD FILL-IN_AONR FILL-IN_DELNR ~
FILL-IN_DEL FILL-IN_PROC 

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

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "AONR" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_DEL AS LOGICAL FORMAT "JA/NEJ" INITIAL NO 
     LABEL "DEL" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT "999" INITIAL 0 
     LABEL "DELNR" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1.

DEFINE VARIABLE FILL-IN_FRKOD AS CHARACTER FORMAT "x(8)" 
     LABEL "FRKOD" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_PROC AS LOGICAL FORMAT "TIM/PRO" INITIAL NO 
     LABEL "PROC" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      FVARO SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN_FRKOD AT ROW 2.75 COL 16 COLON-ALIGNED
     FILL-IN_AONR AT ROW 4.38 COL 16 COLON-ALIGNED
     FILL-IN_DELNR AT ROW 6 COL 16 COLON-ALIGNED
     FILL-IN_DEL AT ROW 7.63 COL 16 COLON-ALIGNED
     FILL-IN_PROC AT ROW 9.25 COL 16 COLON-ALIGNED
     BTN_OK AT ROW 11.5 COL 10.5
     BTN_AVBRYT AT ROW 11.5 COL 25
     SPACE(14.99) SKIP(1.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "FVARO":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT9.FVARO"
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
    FIND FIRST FVARO WHERE RECID(FVARO) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE FVARO THEN DELETE FVARO.
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
    ASSIGN
    FVARO.AONR = INPUT FILL-IN_AONR
    FVARO.DELNR =  INPUT FILL-IN_DELNR
    FVARO.DEL = INPUT FILL-IN_DEL
    FVARO.FRKOD = INPUT FILL-IN_FRKOD
    FVARO.PROC =  INPUT FILL-IN_PROC .
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
     CREATE FVARO.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(FVARO).
   END.
   ELSE DO TRANSACTION:
     FIND FVARO WHERE RECID(FVARO) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "?NDRA-" + FVARO.FRKOD.
     ASSIGN
     FILL-IN_AONR = FVARO.AONR
     FILL-IN_DELNR = FVARO.DELNR 
     FILL-IN_DEL = FVARO.DEL  
     FILL-IN_FRKOD = FVARO.FRKOD  
     FILL-IN_PROC = FVARO.PROC.
     brec = RECID(FVARO).
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
  DISPLAY FILL-IN_FRKOD FILL-IN_AONR FILL-IN_DELNR FILL-IN_DEL FILL-IN_PROC 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN_FRKOD FILL-IN_AONR FILL-IN_DELNR FILL-IN_DEL FILL-IN_PROC 
         BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

