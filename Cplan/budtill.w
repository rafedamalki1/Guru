&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}


&Scoped-define SHARED SHARED
{OMRTEMPW.I}
{SOKDEF.I}
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE valomr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kontrec AS RECID NO-UNDO.

DEFINE SHARED TEMP-TABLE kontkod   
   FIELD KONTO AS CHARACTER 
   FIELD KONTONR AS CHARACTER 
   FIELD BENAMNING AS CHARACTER 
   INDEX KNR IS PRIMARY KONTONR ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-MONEY FILL-IN-TIMMAR BTN_OK BTN_AVB ~
FILL-IN-KONTONR FILL-IN-BEN 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-MONEY FILL-IN-TIMMAR ~
FILL-IN-KONTONR FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 30 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-KONTONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Kontonr" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-MONEY AS INTEGER FORMAT "->>>>>>>9":U INITIAL 0 
     LABEL "Pengar" 
     VIEW-AS FILL-IN 
     SIZE 15.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIMMAR AS INTEGER FORMAT "->>>>>>>9":U INITIAL 0 
     LABEL "Timmar" 
     VIEW-AS FILL-IN 
     SIZE 15.38 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-MONEY AT ROW 5 COL 12 COLON-ALIGNED
     FILL-IN-TIMMAR AT ROW 6.5 COL 12 COLON-ALIGNED
     BTN_OK AT ROW 8.29 COL 15
     BTN_AVB AT ROW 8.29 COL 30
     FILL-IN-KONTONR AT ROW 2 COL 12 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.5 COL 12 COLON-ALIGNED
     SPACE(1.49) SKIP(5.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Budget"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Budget */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 53   
   soktemp.SOKCHAR[1] = valomr
   soktemp.SOKCHAR[2] = kontkod.KONTO
   soktemp.SOKCHAR[3] = kontkod.KONTONR
   soktemp.SOKCHAR[4] = kontkod.BENAMNING
   soktemp.SOKINT[1] = franar
   soktemp.SOKINT[2] = INPUT FILL-IN-MONEY 
   soktemp.SOKINT[3] = INPUT FILL-IN-TIMMAR.
   {SOKANROP.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   /*Ladda områden*/
   {OMRHMT.I}
   FIND FIRST kontkod WHERE RECID(kontkod) = kontrec NO-LOCK NO-ERROR.   
   ASSIGN
   FILL-IN-BEN = kontkod.BENAMNING
   FILL-IN-KONTONR = kontkod.KONTONR.
   /*Hämta budget*/
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 52   
   soktemp.SOKCHAR[1] = valomr
   soktemp.SOKCHAR[2] = kontkod.KONTO
   soktemp.SOKCHAR[3] = kontkod.KONTONR
   soktemp.SOKINT[1] = franar.
   {SOKANROP.I}  
   ASSIGN
   FILL-IN-MONEY = soktemp.SOKINT[2]  /*BUDGET.PENGAR*/
   FILL-IN-TIMMAR = soktemp.SOKINT[3]. /*BUDGET.TIMMAR*/
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = valomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   ASSIGN FRAME {&FRAME-NAME}:TITLE = "Budget för " + LC(Guru.Konstanter:gomrk) + ":" + omrtemp.NAMN +
   " Årtal:" + STRING(franar,"9999").
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-MONEY FILL-IN-TIMMAR FILL-IN-KONTONR FILL-IN-BEN 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-MONEY FILL-IN-TIMMAR BTN_OK BTN_AVB FILL-IN-KONTONR 
         FILL-IN-BEN 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

