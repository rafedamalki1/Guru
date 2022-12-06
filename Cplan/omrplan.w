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
DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
   {JURPERST.I}
{OMRTEMPW.I}
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{AVTPLANTEMP.I}

&Scoped-define NEW NEW 
DEFINE INPUT PARAMETER bortomr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE tillar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE tillrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE omr AS LOGICAL NO-UNDO.  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_valda2 AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare2 AS INTEGER NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO. 
DEFINE VARIABLE checkvar AS LOGICAL NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE omrkonto    
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMREC AS RECID  
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING.

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
&Scoped-define INTERNAL-TABLES omrtemp

/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp.OMRADE omrtemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_ARTAL BRW_OMR FBTN_VISA FBTN_SKRIV ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_ARTAL FILL-IN-VALJ 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27.13 BY 1.38
     FONT 17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OMR FOR 
      omrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 41.13 BY 11.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_ARTAL AT ROW 1.54 COL 1.88
     FILL-IN-VALJ AT ROW 2.96 COL 1.5 NO-LABEL
     BRW_OMR AT ROW 4.42 COL 1.5
     FBTN_VISA AT ROW 6.5 COL 44
     FBTN_SKRIV AT ROW 7.58 COL 44
     BTN_AVB AT ROW 15.75 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 58.75 BY 16.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Välj område"
         HEIGHT             = 16.08
         WIDTH              = 58.75
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 100
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
/* BROWSE-TAB BRW_OMR FILL-IN-VALJ DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp.OMRADE
"omrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp.NAMN
"omrtemp.NAMN" "Benämning" "X(30)" "character" ? ? ? ? ? ? no "ANGE ORAGNISTIONENSBENÄMNING" no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Välj område */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Välj område */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                            
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON CHOOSE OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:        
   {muswait.i} 
   RUN check_UI.
   IF checkvar = FALSE THEN DO:
      EMPTY TEMP-TABLE omrkonto NO-ERROR.       
      RUN omr_UI.       
      ASSIGN 
      skrivut = TRUE.
      RUN SKRIVVAL.W (INPUT TRUE).   
      IF musz = TRUE THEN DO:
          musz = FALSE.
          skrivut = FALSE.  
      END.                 
      IF skrivut = FALSE THEN DO:
         skrivut = skrivut.
      END.   
      ELSE DO:  
         {AVBGOM.I}
         RUN OMRPLUTF.W.
         {AVBFRAM.I}   
      END.
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
   RUN check_UI.   
   IF checkvar = FALSE THEN DO:
      EMPTY TEMP-TABLE omrkonto NO-ERROR.       
      RUN omr_UI.        
      ASSIGN 
      skrivut = FALSE.
      {AVBGOM.I}
      RUN OMRPLUTF.W.
      {AVBFRAM.I}   
   END.   
   {musarrow.i}        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OMR
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
   {ALLSTARTDYN.I} 
   /*Ladda områden*/
   {OMRHMT.I}
   FOR EACH omrtemp:
      FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE judavdtemp THEN DO:
         DELETE omrtemp.
      END.      
   END.
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0").    
   /*LADDAR ÅR I CMB_ARTAL*/  
   uppar = franar.
   slutar = YEAR(TODAY) + 4.
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   omrtemp.OMRADE:LABEL IN BROWSE BRW_OMR = Guru.Konstanter:gomrk. 
   C-Win:TITLE = "Välj " + LC(Guru.Konstanter:gomrk). 
   FILL-IN-VALJ = "Välj " + LC(Guru.Konstanter:gomrk). 
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(franar,"9999")
   CMB_ARTAL = INPUT CMB_ARTAL.
   RUN enable_UI.   
   {FRMSIZE.I}               
   ENABLE BRW_OMR WITH FRAME {&FRAME-NAME}.  
   OPEN QUERY BRW_OMR FOR EACH omrtemp USE-INDEX OMR NO-LOCK .     
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}]
      (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.  
   END.
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check_UI C-Win 
PROCEDURE check_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   checkvar = FALSE.
   IF BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO: 
      checkvar = TRUE.
      MESSAGE "Inget " + LC(Guru.Konstanter:gomrk) + " är markerat." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO BRW_OMR IN FRAME {&FRAME-NAME}.
      RETURN. 
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
  DISPLAY CMB_ARTAL FILL-IN-VALJ 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_ARTAL BRW_OMR FBTN_VISA FBTN_SKRIV BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omr_UI C-Win 
PROCEDURE omr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   antal_valda2 = BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.   
   antal_raknare2 = 1.
   DO WHILE antal_raknare2 LE antal_valda2 :
      status-ok = BRW_OMR:FETCH-SELECTED-ROW(antal_raknare2).
      CREATE omrkonto.
      ASSIGN 
      omrkonto.OMREC = RECID(omrtemp)
      omrkonto.OMRADE = omrtemp.OMRADE
      omrkonto.NAMN = omrtemp.NAMN        
      antal_raknare2 = antal_raknare2 + 1.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

