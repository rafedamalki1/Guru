&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
*/
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_OMR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES OMRADETAB

/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR OMRADETAB.OMRADE OMRADETAB.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH OMRADETAB NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR OMRADETAB
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR OMRADETAB


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_OMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_OMR FILL-IN-OMR FILL-IN-GOMR BTN_KOR ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-OMR FILL-IN-GOMR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY  NO-CONVERT-3D-COLORS
     LABEL "Avsluta" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BTN_KOR 
     LABEL "K�r" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FILL-IN-GOMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Gammalt omr�de" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nytt omr�de" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OMR FOR 
      OMRADETAB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      OMRADETAB.OMRADE FORMAT "x(6)":U WIDTH 6.5
      OMRADETAB.NAMN FORMAT "x(16)":U WIDTH 47.63
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 33.5 BY 7.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_OMR AT ROW 3.18 COL 3.5
     FILL-IN-OMR AT ROW 3.18 COL 51.5 COLON-ALIGNED
     FILL-IN-GOMR AT ROW 11.36 COL 23 COLON-ALIGNED
     BTN_KOR AT ROW 13.27 COL 15
     BTN_AVB AT ROW 13.27 COL 45.5
     "Markera omr�de och ange den nya omr�desbeteckningen" VIEW-AS TEXT
          SIZE 55 BY .68 AT ROW 1.55 COL 9.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 15.91
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
         TITLE              = "Byte av omr�desbeteckningar"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_OMR 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "RT9.OMRADETAB"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > RT9.OMRADETAB.OMRADE
"OMRADETAB.OMRADE" ? ? "character" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" ""
     _FldNameList[2]   > RT9.OMRADETAB.NAMN
"OMRADETAB.NAMN" ? ? "character" ? ? ? ? ? ? no ? no no "47.63" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Byte av omr�desbeteckningar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Byte av omr�desbeteckningar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OMR
&Scoped-define SELF-NAME BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OMR C-Win
ON VALUE-CHANGED OF BRW_OMR IN FRAME DEFAULT-FRAME
DO:
   status-ok = BRW_OMR:SELECT-FOCUSED-ROW() NO-ERROR.
   FILL-IN-GOMR = OMRADETAB.OMRADE.
   DISPLAY FILL-IN-GOMR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOR C-Win
ON CHOOSE OF BTN_KOR IN FRAME DEFAULT-FRAME /* K�r */
DO:
   ASSIGN
   FILL-IN-GOMR = INPUT FILL-IN-GOMR
   FILL-IN-OMR = INPUT FILL-IN-OMR.
   IF FILL-IN-GOMR = "" THEN DO:
      MESSAGE "V�lj eller skriv det gamla omr�det"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   MESSAGE "Vill Ni byta omr�de:" + FILL-IN-GOMR + " till:" + FILL-IN-OMR + 
   "?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.
   IF svar THEN DO:   
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN XBYTOMR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT FILL-IN-GOMR, INPUT FILL-IN-OMR).
      END.
      ELSE DO:
         RUN XBYTOMR.P
         (INPUT FILL-IN-GOMR, INPUT FILL-IN-OMR).
      END.
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
  RUN enable_UI.
  {musarrow.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN-OMR FILL-IN-GOMR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_OMR FILL-IN-OMR FILL-IN-GOMR BTN_KOR BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

