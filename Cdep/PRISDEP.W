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
 {ALLDEF.I}
 {GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparsaldo AS DECIMAL NO-UNDO. 

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}

DEFINE SHARED TEMP-TABLE pspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD ANVANDARE AS CHARACTER
   FIELD NYPRIS AS DECIMAL
   FIELD TIDPUNKT AS DECIMAL
   FIELD DATUM AS DATE. 



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-ENHET FILL-IN-PRIS btn_ok BTN_AVB ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-FACKID1 FILL-IN-ANTAL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENHET FILL-IN-PRIS FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-FACKID1 FILL-IN-ANTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Saldo" 
      VIEW-AS TEXT 
     SIZE 8 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 18 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(8)":U 
     LABEL "Fack-id" 
      VIEW-AS TEXT 
     SIZE 18 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-ENHET AT ROW 4.92 COL 10.38 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 6.54 COL 10.38 COLON-ALIGNED
     btn_ok AT ROW 11 COL 16.38
     BTN_AVB AT ROW 11 COL 31.38
     FILL-IN-ENR AT ROW 1.67 COL 10.38 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.29 COL 10.38 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 8.17 COL 10.38 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 9.83 COL 10.38 COLON-ALIGNED
     SPACE(25.61) SKIP(1.61)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändring av saldo":L
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
ON END-ERROR OF FRAME DIALOG-1 /* Ändring av saldo */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändring av saldo */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO:   
   ASSIGN 
   FILL-IN-PRIS = INPUT FILL-IN-PRIS
   FILL-IN-ENHET = INPUT FILL-IN-ENHET.  
   IF FILL-IN-PRIS NE sparsaldo THEN DO:
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-ERROR.       
      CREATE pspec_mtrl.
      ASSIGN 
      pspec_mtrl.ANVANDARE = Guru.Konstanter:globanv
      pspec_mtrl.NYPRIS = FILL-IN-PRIS
      pspec_mtrl.TIDPUNKT = TIME
      pspec_mtrl.DATUM = TODAY.                 
      BUFFER-COPY spec_mtrl TO pspec_mtrl.
      ASSIGN
      spec_mtrl.ENHET = FILL-IN-ENHET
      spec_mtrl.NPRIS = FILL-IN-PRIS
      spec_mtrl.BPRIS = FILL-IN-PRIS.
   END.
   ELSE DO:
      ASSIGN
      spec_mtrl.ENHET = FILL-IN-ENHET
      sparsaldo = sparsaldo.
   END.                       
   APPLY "GO" TO FRAME {&FRAME-NAME}.      
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
   {DIA_M_START.I}
   FIND spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-ERROR.   
   ASSIGN
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk 
   FILL-IN-BEN = spec_mtrl.BENAMNING
   FILL-IN-ENR = spec_mtrl.ENR
   FILL-IN-ENHET = spec_mtrl.ENHET  
   FILL-IN-PRIS = spec_mtrl.NPRIS
   FILL-IN-ANTAL = spec_mtrl.SALDO
   FILL-IN-FACKID1 = spec_mtrl.FACKID
   sparsaldo = spec_mtrl.NPRIS.    
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
  DISPLAY FILL-IN-ENHET FILL-IN-PRIS FILL-IN-ENR FILL-IN-BEN FILL-IN-FACKID1 
          FILL-IN-ANTAL 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-ENHET FILL-IN-PRIS btn_ok BTN_AVB FILL-IN-ENR FILL-IN-BEN 
         FILL-IN-FACKID1 FILL-IN-ANTAL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

