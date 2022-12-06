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

  Created: 95/05/18 -  9:13 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER kalkspec AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER mtrl_rowid AS ROWID.
                        
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED
{HDTEMP.I}
{SMTRL.I}
{KALKTEMP2.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-ANTAL btn_ok BTN_UP BTN_MIN ~
FILL-IN-ENR FILL-IN-LOP FILL-IN-BEN FILL-IN-ENH BTN_AVB ED_KOM 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANTAL FILL-IN-ENR FILL-IN-LOP ~
FILL-IN-BEN FILL-IN-ENH ED_KOM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_ok 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE ED_KOM AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 206
     SIZE 78 BY 2.5
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS DECIMAL FORMAT ">>>9.999":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 33 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENH AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetskod" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOP AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Löpnr" 
      VIEW-AS TEXT 
     SIZE 16 BY .75 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-ANTAL AT ROW 8.17 COL 11.75 COLON-ALIGNED
     btn_ok AT ROW 16.83 COL 49.5
     BTN_UP AT ROW 7.75 COL 22.25
     BTN_MIN AT ROW 8.88 COL 22.25
     FILL-IN-ENR AT ROW 1.75 COL 11.88 COLON-ALIGNED
     FILL-IN-LOP AT ROW 3.33 COL 11.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 4.75 COL 11.88 COLON-ALIGNED
     FILL-IN-ENH AT ROW 6.33 COL 11.88 COLON-ALIGNED
     BTN_AVB AT ROW 16.83 COL 65.13
     ED_KOM AT ROW 12 COL 2 NO-LABEL
     "Anmärkning:" VIEW-AS TEXT
          SIZE 14 BY .63 AT ROW 11.25 COL 2.5
     SPACE(64.24) SKIP(6.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Uppdatera Antal":L
         DEFAULT-BUTTON btn_ok.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN DIALOG-1
ON CHOOSE OF BTN_MIN IN FRAME DIALOG-1 /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ok DIALOG-1
ON CHOOSE OF btn_ok IN FRAME DIALOG-1 /* OK */
DO: 
   {muswait.i} 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.   
   IF kalkspec = FALSE THEN DO:
      ASSIGN 
      SUBSTRING(hdkalktemp.BENAMNING,50,1000) = INPUT ED_KOM.
      hdkalktemp.ANTAL = FILL-IN-ANTAL.   
   END.
   ELSE DO:
      ASSIGN 
      SUBSTRING(kalk_temp.BENAMNING,50,1000) = INPUT ED_KOM.
      kalk_temp.ANTAL = FILL-IN-ANTAL.
   END.   
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP DIALOG-1
ON CHOOSE OF BTN_UP IN FRAME DIALOG-1 /* + */
DO: 
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ED_KOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ED_KOM DIALOG-1
ON LEAVE OF ED_KOM IN FRAME DIALOG-1
DO:
  ED_KOM = INPUT ED_KOM.
  
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
   FILL-IN-LOP:FORMAT =  Guru.Konstanter:varforetypchar[6].
   IF kalkspec = FALSE THEN DO:      
      FIND hdkalktemp WHERE ROWID(hdkalktemp) = mtrl_rowid NO-ERROR.
      ASSIGN 
      FILL-IN-BEN =  SUBSTRING(hdkalktemp.BENAMNING,1,40)
      FILL-IN-ENR = hdkalktemp.ARBKOD
      FILL-IN-LOP = hdkalktemp.LOPNR
      FILL-IN-ANTAL = hdkalktemp.ANTAL
      FILL-IN-ENH = hdkalktemp.ENHET.
       ED_KOM = SUBSTRING(hdkalktemp.BENAMNING,50).
   END.
   ELSE DO:
      FIND kalk_temp WHERE ROWID(kalk_temp) = mtrl_rowid NO-ERROR.
      ASSIGN 
      FILL-IN-BEN = SUBSTRING(kalk_temp.BENAMNING,1,40) 
      FILL-IN-ENR = kalk_temp.ARBKOD
      FILL-IN-LOP = kalk_temp.LOPNR
      FILL-IN-ANTAL = kalk_temp.ANTAL
      FILL-IN-ENH = kalk_temp.ENHET.
      ED_KOM = SUBSTRING(kalk_temp.BENAMNING,50).
   END.   
   RUN enable_UI.       
   {FRMSIZED.I}
   
   {musarrow.i} 
   {DIA_M_SLUT.I}

    APPLY "ENTRY" TO FILL-IN-ANTAL.
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
  DISPLAY FILL-IN-ANTAL FILL-IN-ENR FILL-IN-LOP FILL-IN-BEN FILL-IN-ENH ED_KOM 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-ANTAL btn_ok BTN_UP BTN_MIN FILL-IN-ENR FILL-IN-LOP 
         FILL-IN-BEN FILL-IN-ENH BTN_AVB ED_KOM 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

