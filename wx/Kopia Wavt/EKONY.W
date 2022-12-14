&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt8              PROGRESS
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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EKONOMIKONTO

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 EKONOMIKONTO.KONTO EKONOMIKONTO.MED ~
EKONOMIKONTO.TYP EKONOMIKONTO.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 EKONOMIKONTO.KONTO ~
EKONOMIKONTO.MED EKONOMIKONTO.TYP EKONOMIKONTO.BENAMNING 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 EKONOMIKONTO
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 EKONOMIKONTO

&Scoped-define FIELD-PAIRS-IN-QUERY-DIALOG-3~
 ~{&FP1}KONTO ~{&FP2}KONTO ~{&FP3}~
 ~{&FP1}MED ~{&FP2}MED ~{&FP3}~
 ~{&FP1}TYP ~{&FP2}TYP ~{&FP3}~
 ~{&FP1}BENAMNING ~{&FP2}BENAMNING ~{&FP3}
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH EKONOMIKONTO NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 EKONOMIKONTO
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 EKONOMIKONTO


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EKONOMIKONTO.KONTO EKONOMIKONTO.MED ~
EKONOMIKONTO.TYP EKONOMIKONTO.BENAMNING 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}KONTO ~{&FP2}KONTO ~{&FP3}~
 ~{&FP1}MED ~{&FP2}MED ~{&FP3}~
 ~{&FP1}TYP ~{&FP2}TYP ~{&FP3}~
 ~{&FP1}BENAMNING ~{&FP2}BENAMNING ~{&FP3}
&Scoped-define ENABLED-TABLES EKONOMIKONTO
&Scoped-define FIRST-ENABLED-TABLE EKONOMIKONTO
&Scoped-Define ENABLED-OBJECTS RECT-11 RECT-12 BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS EKONOMIKONTO.KONTO EKONOMIKONTO.MED ~
EKONOMIKONTO.TYP EKONOMIKONTO.BENAMNING 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "AVBRYT":L 
     SIZE 11 BY 1.18.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 10.5 BY 1.18.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44 BY 6.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44 BY 3
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      EKONOMIKONTO SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     EKONOMIKONTO.KONTO AT ROW 2.95 COL 13.75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 8 
     EKONOMIKONTO.MED AT ROW 3.95 COL 13.75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     EKONOMIKONTO.TYP AT ROW 4.95 COL 13.75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
          BGCOLOR 8 
     EKONOMIKONTO.BENAMNING AT ROW 5.95 COL 13.75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 8 
     BTN_OK AT ROW 10 COL 11
     BTN_AVBRYT AT ROW 10 COL 25.5
     RECT-11 AT ROW 2 COL 2
     RECT-12 AT ROW 9 COL 2
     SPACE(6.12) SKIP(0.63)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "EKONOMIKONTO":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
                                                                        */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT8.EKONOMIKONTO"
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
    FIND EKONOMIKONTO WHERE RECID(EKONOMIKONTO) = brec EXCLUSIVE-LOCK.    
    IF AVAILABLE EKONOMIKONTO THEN DELETE EKONOMIKONTO.
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
     CREATE EKONOMIKONTO.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(EKONOMIKONTO).
   END.
   ELSE DO TRANSACTION:
     FIND EKONOMIKONTO WHERE RECID(EKONOMIKONTO) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "?NDRA-" + EKONOMIKONTO.KONTO.
     brec = RECID(EKONOMIKONTO).
   END.    
  RUN enable_UI.
  {musarrow.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3 _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3 _DEFAULT-ENABLE
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
  IF AVAILABLE EKONOMIKONTO THEN 
    DISPLAY EKONOMIKONTO.KONTO EKONOMIKONTO.MED EKONOMIKONTO.TYP 
          EKONOMIKONTO.BENAMNING 
      WITH FRAME DIALOG-3.
  ENABLE RECT-11 EKONOMIKONTO.KONTO EKONOMIKONTO.MED EKONOMIKONTO.TYP 
         EKONOMIKONTO.BENAMNING RECT-12 BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


