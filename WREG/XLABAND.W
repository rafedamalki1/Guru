&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          limo             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 11/20/97 -  1:14 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE xlabelrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES XLABEL

/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define QUERY-STRING-DIALOG-1 FOR EACH XLABEL SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-1 OPEN QUERY DIALOG-1 FOR EACH XLABEL SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-1 XLABEL
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-1 XLABEL


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_MENYVART SEL_FOR FILL-IN_MENY ~
FILL-IN_ORDNING FILL-IN_KOPPLING FILL-IN_STORKOPPLING BTN_REG BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_MENYVART SEL_FOR FILL-IN_MENY ~
FILL-IN_ORDNING FILL-IN_KOPPLING FILL-IN_STORKOPPLING 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 12 BY 2.

DEFINE BUTTON BTN_REG 
     LABEL "Registrera":L 
     SIZE 12 BY 2.

DEFINE VARIABLE FILL-IN_KOPPLING AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "KOPPLING" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_MENY AS CHARACTER FORMAT "x(25)" 
     LABEL "MENY" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_MENYVART AS CHARACTER FORMAT "x(8)" 
     LABEL "MENYVART" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_ORDNING AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "ORDNING" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_STORKOPPLING AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "STORKOPPLING" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE SEL_FOR AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 27.25 BY 9.25
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-1 FOR 
      XLABEL SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN_MENYVART AT ROW 1.5 COL 13.63 COLON-ALIGNED
     SEL_FOR AT ROW 2.25 COL 48.38 NO-LABEL
     FILL-IN_MENY AT ROW 3.54 COL 13.63 COLON-ALIGNED
     FILL-IN_ORDNING AT ROW 5.58 COL 13.63 COLON-ALIGNED
     FILL-IN_KOPPLING AT ROW 7.63 COL 13.63 COLON-ALIGNED
     FILL-IN_STORKOPPLING AT ROW 9.63 COL 13.63 COLON-ALIGNED
     BTN_REG AT ROW 12.63 COL 21
     BTN_AVS AT ROW 12.67 COL 44.88
     "FÖRETAG" VIEW-AS TEXT
          SIZE 24.38 BY .88 AT ROW 1.25 COL 49
     SPACE(7.49) SKIP(15.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "ÄNDRING AV SEKRETESS".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _TblList          = "LIMO.XLABEL"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:  
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Registrera */
DO:
   DO TRANSACTION:
      FIND XLABEL WHERE RECID(XLABEL) = xlabelrec EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN
      XLABEL.KOPPLING = INPUT FILL-IN_KOPPLING
      XLABEL.MENY = INPUT FILL-IN_MENY
      XLABEL.MENYVART = INPUT FILL-IN_MENYVART 
      XLABEL.ORDNING = INPUT FILL-IN_ORDNING
      XLABEL.STORKOPPLING = INPUT FILL-IN_STORKOPPLING.
   END.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Registrera */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   FIND XLABEL WHERE RECID(XLABEL) = xlabelrec NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN_KOPPLING = XLABEL.KOPPLING
   FILL-IN_MENY = XLABEL.MENY
   FILL-IN_MENYVART = XLABEL.MENYVART 
   FILL-IN_ORDNING = XLABEL.ORDNING
   FILL-IN_STORKOPPLING = XLABEL.STORKOPPLING.
   antal_raknare = 1.
   REPEAT:
      IF XLABEL.FORETAG[antal_raknare] NE "" THEN DO:
         status-ok =SEL_FOR:ADD-LAST(XLABEL.FORETAG[antal_raknare])IN FRAME {&FRAME-NAME}.    
      END.
      antal_raknare = antal_raknare + 1. 
      IF antal_raknare > 50 THEN LEAVE.
   END.     
   RUN enable_UI.
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_MENYVART SEL_FOR FILL-IN_MENY FILL-IN_ORDNING FILL-IN_KOPPLING 
          FILL-IN_STORKOPPLING 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_MENYVART SEL_FOR FILL-IN_MENY FILL-IN_ORDNING FILL-IN_KOPPLING 
         FILL-IN_STORKOPPLING BTN_REG BTN_AVS 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

