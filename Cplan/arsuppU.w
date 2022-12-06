&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{PLANNRTEMP.I}
{AVTPLANTEMP.I}
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.
DEFINE VARIABLE t%var AS DECIMAL NO-UNDO.
DEFINE VARIABLE radval AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE arsupptemp
   FIELD A% AS DECIMAL
   FIELD AKR AS INTEGER 
   FIELD ARTAL AS INTEGER
   FIELD MASK% AS DECIMAL
   FIELD MASKKR AS INTEGER
   FIELD MTRL% AS DECIMAL
   FIELD MTRLKR AS INTEGER
   FIELD O% AS DECIMAL
   FIELD OKR AS INTEGER
   FIELD PLANNR AS CHARACTER
   FIELD T% AS DECIMAL
   FIELD TKR AS INTEGER
   INDEX PLAN IS PRIMARY PLANNR ARTAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL FILL-IN-MTRL% FILL-IN-MTRLKR ~
FILL-IN-A% FILL-IN-AKR FILL-IN-MASK% FILL-IN-MASKKR FILL-IN-O% FILL-IN-OKR ~
FILL-IN-T% BTN_OK BTN_AVB FILL-IN-ARTAL 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL FILL-IN-MTRL% FILL-IN-MTRLKR ~
FILL-IN-A% FILL-IN-AKR FILL-IN-MASK% FILL-IN-MASKKR FILL-IN-O% FILL-IN-OKR ~
FILL-IN-T% FILL-IN-ARTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-A% AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Arbete (%)" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.04 NO-UNDO.

DEFINE VARIABLE FILL-IN-AKR AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Arbete (kr)" 
     VIEW-AS FILL-IN 
     SIZE 12.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Ange värden för" 
      VIEW-AS TEXT 
     SIZE 6.75 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASK% AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Maskin (%)" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.04 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASKKR AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Maskin (kr)" 
     VIEW-AS FILL-IN 
     SIZE 12.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MTRL% AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Materiel (%)" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.04 NO-UNDO.

DEFINE VARIABLE FILL-IN-MTRLKR AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Materiel (kr)" 
     VIEW-AS FILL-IN 
     SIZE 12.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-O% AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Övrigt (%)" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.04 NO-UNDO.

DEFINE VARIABLE FILL-IN-OKR AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Övrigt (kr)" 
     VIEW-AS FILL-IN 
     SIZE 12.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-T% AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Totalt (%)" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.04 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Delar", 1,
"Totalt", 2
     SIZE 35.13 BY .63 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_VAL AT ROW 2.92 COL 16.38 NO-LABEL
     FILL-IN-MTRL% AT ROW 4.75 COL 14.63 COLON-ALIGNED
     FILL-IN-MTRLKR AT ROW 4.75 COL 37.63 COLON-ALIGNED
     FILL-IN-A% AT ROW 6.75 COL 14.63 COLON-ALIGNED
     FILL-IN-AKR AT ROW 6.75 COL 37.63 COLON-ALIGNED
     FILL-IN-MASK% AT ROW 8.75 COL 14.63 COLON-ALIGNED
     FILL-IN-MASKKR AT ROW 8.75 COL 37.63 COLON-ALIGNED
     FILL-IN-O% AT ROW 10.75 COL 14.63 COLON-ALIGNED
     FILL-IN-OKR AT ROW 10.75 COL 37.63 COLON-ALIGNED
     FILL-IN-T% AT ROW 12.75 COL 14.63 COLON-ALIGNED
     BTN_OK AT ROW 14.33 COL 23.38
     BTN_AVB AT ROW 14.33 COL 38.38
     FILL-IN-ARTAL AT ROW 1.63 COL 31.25 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 52.75 BY 14.67
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Årsuppdelning"
         HEIGHT             = 14.58
         WIDTH              = 53
         MAX-HEIGHT         = 18.08
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 18.08
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       FILL-IN-A%:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AKR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-MASK%:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-MASKKR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-MTRL%:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-MTRLKR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-O%:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-OKR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-T%:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Årsuppdelning */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
     musz = TRUE.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Årsuppdelning */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = TRUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   ASSIGN
   FILL-IN-MTRL% = INPUT FILL-IN-MTRL%
   FILL-IN-MTRLKR = INPUT FILL-IN-MTRLKR
   FILL-IN-A% = INPUT FILL-IN-A%
   FILL-IN-AKR = INPUT FILL-IN-AKR
   FILL-IN-MASK% = INPUT FILL-IN-MASK%
   FILL-IN-MASKKR = INPUT FILL-IN-MASKKR
   FILL-IN-O% = INPUT FILL-IN-O%
   FILL-IN-OKR = INPUT FILL-IN-OKR
   FILL-IN-T% = INPUT FILL-IN-T%.
   IF finns = FALSE THEN CREATE arsupptemp.
   ASSIGN
   arsupptemp.PLANNR = plannrvar
   arsupptemp.ARTAL = artalvar
   arsupptemp.MTRL%   = INPUT FILL-IN-MTRL%
   arsupptemp.MTRLKR = INPUT FILL-IN-MTRLKR
   arsupptemp.A%      = INPUT FILL-IN-A%
   arsupptemp.AKR    = INPUT FILL-IN-AKR
   arsupptemp.MASK%   = INPUT FILL-IN-MASK%
   arsupptemp.MASKKR = INPUT FILL-IN-MASKKR
   arsupptemp.O%      = INPUT FILL-IN-O%
   arsupptemp.OKR    = INPUT FILL-IN-OKR
   arsupptemp.T%      = INPUT FILL-IN-T%.
   IF RAD_VAL = 1 THEN DO:      
      IF FILL-IN-MTRL% > 0 AND FILL-IN-MTRLKR > 0 THEN DO:
         MESSAGE "Obs! Antingen % eller kr. Ej både och."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-MTRL% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      IF FILL-IN-A% > 0 AND FILL-IN-AKR > 0 THEN DO:
         MESSAGE "Obs! Antingen % eller kr. Ej både och."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-A% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      IF FILL-IN-MASK% > 0 AND FILL-IN-MASKKR > 0 THEN DO:
         MESSAGE "Obs! Antingen % eller kr. Ej både och."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-MASK% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      IF FILL-IN-O% > 100 AND FILL-IN-OKR > 0 THEN DO:
         MESSAGE "Obs! Antingen % eller kr. Ej både och."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-O% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      
      IF FILL-IN-MTRL% > 100 THEN DO:
         MESSAGE "Obs! Procentsatsen får ej vara större än 100."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-MTRL% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      IF FILL-IN-A% > 100 THEN DO:
         MESSAGE "Obs! Procentsatsen får ej vara större än 100."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-A% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      IF FILL-IN-MASK% > 100 THEN DO:
         MESSAGE "Obs! Procentsatsen får ej vara större än 100."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-MASK% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.
      IF FILL-IN-O% > 100 THEN DO:
         MESSAGE "Obs! Procentsatsen får ej vara större än 100."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-O% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END. 
      RUN appat_UI (INPUT 1). 
   END.
   ELSE DO:      
      IF FILL-IN-T% = 0 THEN DO:
         MESSAGE "Obs! % Kan ej vara 0."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-T% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.  
      IF FILL-IN-T% > 100 THEN DO:
         MESSAGE "Obs! Procentsatsen får ej vara större än 100."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-T% IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.  
      ASSIGN t%var = FILL-IN-T%.

      RUN appat_UI (INPUT 2). 
   END.
   FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND
   valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF AVAILABLE valplantemp THEN DO:
      FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar AND
      plannrtemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF AVAILABLE plannrtemp THEN DO:
         ASSIGN
         plannrtemp.UPP = TRUE    
         plannrtemp.UPPNR = TRUE. 
      END.
      ASSIGN
      valplantemp.UPP = TRUE   
      valplantemp.UPPNR = TRUE.
   END.          
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:      
      ASSIGN
      FILL-IN-T%:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      FILL-IN-MTRL%:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-MTRLKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-A%:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-AKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-MASK%:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-MASKKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-O%:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-OKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:      
      ASSIGN
      FILL-IN-T%:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      FILL-IN-MTRL%:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-MTRLKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-A%:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-AKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-MASK%:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-MASKKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-O%:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-OKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}
/*    FIND FIRST PLANNRTAB WHERE RECID(PLANNRTAB) = aonrrec NO-LOCK NO-ERROR. */
   FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
   valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   FILL-IN-T% = INPUT FILL-IN-T%.
   t%var = FILL-IN-T%.   
   RUN appat_UI (INPUT 3).

   FIND FIRST arsupptemp WHERE arsupptemp.PLANNR = plannrvar AND
   arsupptemp.ARTAL = artalvar USE-INDEX PLAN NO-LOCK NO-ERROR.
   IF AVAILABLE arsupptemp THEN DO:
      IF arsupptemp.T% > 0 OR arsupptemp.TKR > 0 THEN DO:
         ASSIGN
         RAD_VAL = 2
         FILL-IN-T% = arsupptemp.T%.
      END.
      ELSE DO:
         ASSIGN
         RAD_VAL = 1
         FILL-IN-MTRL% = arsupptemp.MTRL%
         FILL-IN-MTRLKR = arsupptemp.MTRLKR
         FILL-IN-A% = arsupptemp.A%
         FILL-IN-AKR = arsupptemp.AKR
         FILL-IN-MASK% = arsupptemp.MASK%
         FILL-IN-MASKKR = arsupptemp.MASKKR
         FILL-IN-O% = arsupptemp.O%
         FILL-IN-OKR = arsupptemp.OKR.
      END.
   END.
   
   ASSIGN 
   RAD_VAL = radval     
   FILL-IN-ARTAL = artalvar. 
   RUN enable_UI.   
   {FRMSIZE.I}  
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appat_UI C-Win 
PROCEDURE appat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN ARSUPPAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vart,INPUT plannrvar,OUTPUT radval,INPUT-OUTPUT artalvar,
      INPUT-OUTPUT finns,INPUT-OUTPUT t%var,
      INPUT-OUTPUT TABLE valplantemp,INPUT-OUTPUT TABLE arsupptemp).
   END.
   ELSE DO:
      RUN ARSUPPAPP.P
      (INPUT vart,INPUT plannrvar,OUTPUT radval,INPUT-OUTPUT artalvar,
      INPUT-OUTPUT finns,INPUT-OUTPUT t%var,
      INPUT-OUTPUT TABLE valplantemp,INPUT-OUTPUT TABLE arsupptemp).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL FILL-IN-MTRL% FILL-IN-MTRLKR FILL-IN-A% FILL-IN-AKR 
          FILL-IN-MASK% FILL-IN-MASKKR FILL-IN-O% FILL-IN-OKR FILL-IN-T% 
          FILL-IN-ARTAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_VAL FILL-IN-MTRL% FILL-IN-MTRLKR FILL-IN-A% FILL-IN-AKR 
         FILL-IN-MASK% FILL-IN-MASKKR FILL-IN-O% FILL-IN-OKR FILL-IN-T% BTN_OK 
         BTN_AVB FILL-IN-ARTAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

