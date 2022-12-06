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
 DEFINE OUTPUT PARAMETER bkomm AS CHARACTER.
/* Local Variable Definitions ---                                       */
 {ALLDEF.I}
 {GLOBVAR2DEL1.I}
 {HOPPSEK2W.I}
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparsaldo AS INTEGER NO-UNDO. 

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-KOMM btn_ok BTN_AVB FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-FRAGA 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KOMM FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-FRAGA 

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

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 37.75 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enr" 
      VIEW-AS TEXT 
     SIZE 18 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRAGA AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 76.5 BY .63
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOMM AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 56.75 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-KOMM AT ROW 5.92 COL 10.25 COLON-ALIGNED
     btn_ok AT ROW 7.96 COL 49.63
     BTN_AVB AT ROW 7.96 COL 64.63
     FILL-IN-ENR AT ROW 1.67 COL 10.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.29 COL 10.25 COLON-ALIGNED
     FILL-IN-FRAGA AT ROW 4.63 COL 1 COLON-ALIGNED NO-LABEL
     SPACE(0.00) SKIP(4.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kommentar borttag":L
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
ON END-ERROR OF FRAME DIALOG-1 /* Kommentar borttag */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kommentar borttag */
DO:
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* Ok */
DO: 
   
   ASSIGN       
   FILL-IN-KOMM = INPUT FILL-IN-KOMM.                         
   bkomm = FILL-IN-KOMM.
   IF FILL-IN-KOMM = "" THEN DO:
      MESSAGE "Kommentar till borttag är obligatoriskt" VIEW-AS ALERT-BOX TITLE "Meddelande".
      UNDO,RETRY.
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
   FILL-IN-ENR = spec_mtrl.ENR.
   FILL-IN-FRAGA = "Vill du ta bort detta " + LC(Guru.Konstanter:genk) + " från depån? Saldot är större än 0 och en minus korrigering kommer att skapas.".
         /*sparsaldo = spec_mtrl.SALDO.    */
   
         
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
  DISPLAY FILL-IN-KOMM FILL-IN-ENR FILL-IN-BEN FILL-IN-FRAGA 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-KOMM btn_ok BTN_AVB FILL-IN-ENR FILL-IN-BEN FILL-IN-FRAGA 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

