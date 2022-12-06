&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

  Created: 95/10/22 -  2:57 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
 DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.

/* Local Variable Definitions ---                                       */
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.
 DEFINE SHARED VARIABLE reprowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
&Scoped-define NEW 
&Scoped-define SHARED SHARED

{MTRLTEMP.I}

/* emea
DEFINE SHARED TEMP-TABLE mtrltemp NO-UNDO LIKE customer .
*/
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-14 RECT-15 FILL-IN-BESTP FILL-IN-BESTKV ~
FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ARSOM btn_ok BTN_AVB FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-ENHET 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BESTP FILL-IN-BESTKV ~
FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ARSOM FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENHET 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-ARSOM AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Omsättning/år" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTKV AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Best-antal" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTP AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Best-punkt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(6)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.38 BY 11.08
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 50.38 BY 2.75
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-BESTP AT ROW 6.17 COL 15.88 COLON-ALIGNED
     FILL-IN-BESTKV AT ROW 7.67 COL 15.88 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 9.17 COL 15.88 COLON-ALIGNED
     FILL-IN-FACKID2 AT ROW 9.17 COL 32.38 COLON-ALIGNED
     FILL-IN-ARSOM AT ROW 10.67 COL 15.88 COLON-ALIGNED
     btn_ok AT ROW 13.04 COL 9.75
     BTN_AVB AT ROW 13.04 COL 29.75
     FILL-IN-ENR AT ROW 1.67 COL 15.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.17 COL 15.88 COLON-ALIGNED
     FILL-IN-ENHET AT ROW 4.67 COL 15.88 COLON-ALIGNED
     RECT-14 AT ROW 1.17 COL 1.5
     RECT-15 AT ROW 12.25 COL 1.5
     SPACE(0.50) SKIP(0.17)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Depå information":L
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Depå information */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Depå information */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO: 
               APPLY "GO" TO FRAME {&FRAME-NAME}. 
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 DIALOG-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME DIALOG-1 /* Fack-id (aa) */
DO: 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 DIALOG-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME DIALOG-1 /* + (999999) */
DO: 
   
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
   
      FIND mtrltemp WHERE ROWID(mtrltemp) = mtrl_rowid NO-ERROR.
      ASSIGN
      
          /*FILL-IN-BEN = mtrltemp.name*/
      FILL-IN-BEN = mtrltemp.enr.
        
  RUN enable_UI.       
   
  FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
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
  DISPLAY FILL-IN-BESTP FILL-IN-BESTKV FILL-IN-FACKID1 FILL-IN-FACKID2 
          FILL-IN-ARSOM FILL-IN-ENR FILL-IN-BEN FILL-IN-ENHET 
      WITH FRAME DIALOG-1.
  ENABLE RECT-14 RECT-15 FILL-IN-BESTP FILL-IN-BESTKV FILL-IN-FACKID1 
         FILL-IN-FACKID2 FILL-IN-ARSOM btn_ok BTN_AVB FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-ENHET 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

