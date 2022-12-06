&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{TELENAMN.I}
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE smsproch AS HANDLE NO-UNDO. /* SMSAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE intele AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE infil AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE telvar AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TEL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tele_namn

/* Definitions for BROWSE BRW_TEL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TEL tele_namn.NAMN tele_namn.TELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TEL 
&Scoped-define QUERY-STRING-BRW_TEL FOR EACH tele_namn NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TEL OPEN QUERY BRW_TEL FOR EACH tele_namn NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TEL tele_namn
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TEL tele_namn


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 BRW_TEL FBTN_VISA FBTN_SKRIV BTN_NY ~
BTN_AND BTN_BORT FBTN_OK FILL-IN-TEL FILL-IN-NAMN BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TEL FILL-IN-NAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEL AS CHARACTER FORMAT "X(12)":U 
     LABEL "Telenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TEL FOR 
      tele_namn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TEL C-Win _STRUCTURED
  QUERY BRW_TEL NO-LOCK DISPLAY
      tele_namn.NAMN FORMAT "X(40)":U
      tele_namn.TELNR FORMAT "X(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 61 BY 9.25
         TITLE "Upplagda telefoner".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_TEL AT ROW 1.67 COL 1.5
     FBTN_VISA AT ROW 3.67 COL 64.63
     FBTN_SKRIV AT ROW 4.75 COL 64.63
     BTN_NY AT ROW 11.75 COL 16.63
     BTN_AND AT ROW 11.75 COL 29.38
     BTN_BORT AT ROW 11.75 COL 42.13
     FBTN_OK AT ROW 13 COL 64.63
     FILL-IN-TEL AT ROW 14.04 COL 16.88 COLON-ALIGNED
     FILL-IN-NAMN AT ROW 14.04 COL 39 COLON-ALIGNED
     BTN_AVB AT ROW 14.08 COL 64.63
     "Sök på:" VIEW-AS TEXT
          SIZE 8 BY .83 AT ROW 14.21 COL 2.38
     RECT-3 AT ROW 13.88 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.38 BY 14.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tele_namn
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration inkommande telefoner"
         HEIGHT             = 14.46
         WIDTH              = 78.25
         MAX-HEIGHT         = 19.83
         MAX-WIDTH          = 96.75
         VIRTUAL-HEIGHT     = 19.83
         VIRTUAL-WIDTH      = 96.75
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
/* BROWSE-TAB BRW_TEL RECT-3 DEFAULT-FRAME */
ASSIGN 
       BRW_TEL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TEL
/* Query rebuild information for BROWSE BRW_TEL
     _TblList          = "Temp-Tables.tele_namn"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tele_namn.NAMN
     _FldNameList[2]   = Temp-Tables.tele_namn.TELNR
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TEL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administration inkommande telefoner */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Administration inkommande telefoner */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   {muswait.i}
   andra = TRUE.
   BRW_TEL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE tele_namn THEN DO:
      telvar = tele_namn.TELNR.
      
      RUN NYSMS.W (INPUT-OUTPUT telvar).
      
      FIND FIRST tele_namn WHERE tele_namn.TELNR = telvar NO-LOCK NO-ERROR.
      IF AVAILABLE tele_namn THEN DO:
         RUN openbdynspec_UI IN brwproc[1].
         RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(tele_namn)).
      END.
      andra = FALSE.      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      RUN btnoksmstele_UI IN smsproch 
         (INPUT infil, INPUT sparfil, INPUT intele, INPUT TABLE tele_namn).
      APPLY "CLOSE":U TO THIS-PROCEDURE.  
   END.
   ELSE IF NOT svar THEN DO:       
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.                    
   ELSE DO:
      musz = musz.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   BRW_TEL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   IF AVAILABLE tele_namn THEN DO:
      MESSAGE "Vill du ta bort " + tele_namn.NAMN + "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO: 
         DELETE tele_namn.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].                          
      END.        
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   {muswait.i}   
   andra = FALSE.
   
   telvar = "".
   RUN NYSMS.W (INPUT-OUTPUT telvar).
   
   IF musz = FALSE THEN DO:
      FIND FIRST tele_namn WHERE tele_namn.TELNR = telvar NO-LOCK NO-ERROR.
      IF AVAILABLE tele_namn THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tele_namn)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   musz = FALSE.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK C-Win
ON CHOOSE OF FBTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   RUN btnoksmstele_UI IN smsproch 
      (INPUT infil, INPUT sparfil, INPUT intele, INPUT TABLE tele_namn).
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON CHOOSE OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   {muswait.i}   
   skrivut = TRUE.
   RUN SKRIVVAL.W (INPUT FALSE).   
   IF musz = TRUE THEN DO:
       musz = FALSE.
       skrivut = FALSE.  
   END.                 
   IF skrivut = FALSE THEN DO:
      skrivut = skrivut.
   END.   
   ELSE DO:       
      {AVBGOM.I}
      IF musz = FALSE THEN RUN VSMSTEL.W.
      ELSE musz = FALSE. 
      {AVBFRAM.I}
   END.                       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   skrivut = FALSE.
   musz = FALSE.   
   {AVBGOM.I}
   IF musz = FALSE THEN RUN VSMSTEL.W.
   ELSE musz = FALSE.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TEL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:  
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   IF intele = TRUE THEN DO:
      {&WINDOW-NAME}:TITLE = "Administration inkommande telefoner".
      IF OPSYS = "WIN32" OR OPSYS = "MS-DOS" THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" THEN DO:                      
            ASSIGN 
            infil = {GRANSMS.I} + "settings\legalin.txt"         
            sparfil = {GRANSMS.I} + "settings\spar_leg.txt".
         END.  
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            ASSIGN 
            infil = {GRANSMS.I} + "settings\legalin.txt"        
            sparfil = {GRANSMS.I} + "settings\spar_leg.txt".   
         END.          
      END.
      RUN legaltxt_UI IN smsproch (INPUT infil, OUTPUT TABLE tele_namn).
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Administration utgående telefoner".
      IF OPSYS = "WIN32" OR OPSYS = "MS-DOS" THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" THEN DO:            
            ASSIGN 
            infil = {GRANSMS.I} + "settings\address.txt"         
            sparfil = {GRANSMS.I} + "settings\spar_add.txt".
         END.  
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            ASSIGN 
            infil = {GRANSMS.I} + "settings\address.txt"        
            sparfil = {GRANSMS.I} + "settings\spar_add.txt".
         END.          
      END.    
      RUN adresstxt_UI IN smsproch (INPUT infil, OUTPUT TABLE tele_namn).
   END.   
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN openbdynspec_UI IN brwproc[1].
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_TEL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-NAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "NAMN").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-TEL:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TELNR").
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
  DISPLAY FILL-IN-TEL FILL-IN-NAMN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 BRW_TEL FBTN_VISA FBTN_SKRIV BTN_NY BTN_AND BTN_BORT FBTN_OK 
         FILL-IN-TEL FILL-IN-NAMN BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

