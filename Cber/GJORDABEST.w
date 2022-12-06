&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



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

{INKTEMP.I}
{AVDTEMP.I}
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{HOPPSEK2W.I}
{BESTKUNDALLT.I}
{PRIOTEMP.I}
&Scoped-define NEW 
{FAKTTYPDEF.I}
{BRWSOK.I}
{OMRTEMPW.I}
{ANSPROJBER.I}
{SOKDEF.I}
{ARBATE.I}
{AVTPLANTEMP.I}
{JURPERST.I}
&Scoped-define NEW 
&Scoped-define SHARED 
{LEVERANT.I}
{EXECLIN.I}
{BESTVISTT.I} 
&Scoped-define SHARED SHARED
{DIRDEF.I}   
&Scoped-define NEW
{ANVTEMPS.I}
DEFINE SHARED TEMP-TABLE berkalanvandartemp NO-UNDO LIKE anvandartemp.
{TIDUTTT.I}

DEFINE TEMP-TABLE delNRaonr NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   INDEX AONR IS PRIMARY AONR DELNR.
DEFINE SHARED TEMP-TABLE aoval NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD AONRREC AS RECID
   INDEX AONR IS PRIMARY AONR DELNR. 

DEFINE TEMP-TABLE batemp NO-UNDO   
   FIELD ANVANDARE AS CHARACTER 
   FIELD ANVNAMN AS CHARACTER.
DEFINE VARIABLE hlevkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE hlevnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE anv AS CHARACTER NO-UNDO.
DEFINE VARIABLE hamtapp AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE valar AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE valmanad AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE valdelnrlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE delnummer AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.
DEFINE VARIABLE tillbakaaonr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tillbakadelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE valdarec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE kuurvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE projvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bortvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE val1 AS LOGICAL.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE VARIABLE bestrak AS INTEGER NO-UNDO.
DEFINE VARIABLE spdep AS INTEGER NO-UNDO.
DEFINE VARIABLE spbestnr AS INTEGER NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE VARIABLE labelvar1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelvar3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelvar4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE priok AS INTEGER NO-UNDO.
DEFINE VARIABLE aartk AS INTEGER NO-UNDO.
DEFINE VARIABLE ftyp AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR evaldaao.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VAONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES evaldaao

/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR evaldaao.AONR evaldaao.DELNR ~
evaldaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR 
&Scoped-define QUERY-STRING-BRW_VAONR FOR EACH evaldaao NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH evaldaao NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR evaldaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR evaldaao


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_VAONR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VAONR FBTN_VISA TOG_LEV CMB_LEV FBTN_EX ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_LEV CMB_LEV 

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

DEFINE BUTTON FBTN_EX 
     LABEL "Visa i excel" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Leverantör till Excel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_LEV AS LOGICAL INITIAL no 
     LABEL "Alla leverantörer till Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.13 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VAONR FOR 
      evaldaao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR C-Win _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      evaldaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      evaldaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      evaldaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 59.5 BY 25.25
         FONT 4
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_VAONR AT ROW 2.5 COL 4.5
     FBTN_VISA AT ROW 2.83 COL 91
     TOG_LEV AT ROW 6.04 COL 80.88 WIDGET-ID 4
     CMB_LEV AT ROW 7.04 COL 57.5 WIDGET-ID 2
     FBTN_EX AT ROW 8.63 COL 91
     BTN_AVB AT ROW 28.21 COL 91
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: evaldaao T "?" NO-UNDO temp-db evaldaao
      TABLE: uvaldaao T "?" NO-UNDO temp-db uvaldaao
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.92
         WIDTH              = 105.75
         MAX-HEIGHT         = 28.92
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.92
         VIRTUAL-WIDTH      = 125
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VAONR 1 DEFAULT-FRAME */
ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_VAONR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEV IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.evaldaao"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.evaldaao.AONR
"evaldaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.evaldaao.DELNR
"evaldaao.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.evaldaao.ORT
"evaldaao.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_VAONR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
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
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV C-Win
ON VALUE-CHANGED OF CMB_LEV IN FRAME DEFAULT-FRAME /* Leverantör till Excel */
DO:                           
   CMB_LEV = INPUT CMB_LEV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX C-Win
ON CHOOSE OF FBTN_EX IN FRAME DEFAULT-FRAME /* Visa i excel */
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   RUN visa2_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_LEV C-Win
ON VALUE-CHANGED OF TOG_LEV IN FRAME DEFAULT-FRAME /* Alla leverantörer till Excel */
DO:
   {muswait.i}
   TOG_LEV = INPUT TOG_LEV. 
   IF TOG_LEV = TRUE THEN DO:
      CMB_LEV:VISIBLE = FALSE.
   END.
   ELSE IF TOG_LEV = FALSE THEN DO:
      CMB_LEV:VISIBLE = TRUE.
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAONR
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
   IF VALID-HANDLE(kuurvalapph) THEN DELETE PROCEDURE kuurvalapph.
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2 NO-ERROR.
      nyttaoapph2 = ?.
   END.  
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.      
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
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      LEAVE MAIN-BLOCK.       
   END.           
   RUN delnrkoll_UI.              
   ASSIGN
   evaldaao.AONR:LABEL IN BROWSE BRW_VAONR = Guru.Konstanter:gaok.
   &Scoped-define FORMATNAMN evaldaao.AONR
   &Scoped-define BROWSE-NAME BRW_VAONR
   {AOFORMAT1.I}
   RUN enable_UI.
   RUN ladda_UI.    
   ASSIGN C-Win:TITLE = "Gjorda Beställningar".   
   GET FIRST BRW_VAONR NO-LOCK.    
   {musarrow.i}   
   {WIN_M_SLUT.I}   
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   /* Fylla leverantörslistan och positionera till huvudlev*/
   RUN hamtalev_UI IN hamtapp (OUTPUT TABLE templeverant, OUTPUT hlevkod).
   FOR EACH templeverant NO-LOCK:
      CMB_LEV:ADD-LAST(templeverant.LEVNAMN).
   END.
   FIND FIRST templeverant WHERE templeverant.LEVKOD = hlevkod NO-LOCK NO-ERROR.
   CMB_LEV:SCREEN-VALUE = templeverant.LEVNAMN.
   CMB_LEV = INPUT CMB_LEV.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      FBTN_VISA:VISIBLE = TRUE.
      FBTN_EX:VISIBLE = TRUE.
      TOG_LEV:VISIBLE = TRUE. 
      CMB_LEV:VISIBLE = TRUE.      
   END.
   ELSE IF Guru.Konstanter:globforetag = "gran" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "LULE"  THEN DO:
      FBTN_VISA:VISIBLE = TRUE.
      FBTN_EX:VISIBLE = FALSE.
      TOG_LEV:VISIBLE = FALSE.
      CMB_LEV:VISIBLE = FALSE.
   END.
   ELSE IF Guru.Konstanter:globforetag = "fors" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
      FBTN_VISA:VISIBLE = TRUE.   
      FBTN_EX:VISIBLE = TRUE.
      TOG_LEV:VISIBLE = TRUE. 
   END.
   ELSE IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
      FBTN_VISA:VISIBLE = TRUE.
      FBTN_EX:VISIBLE = TRUE.
      TOG_LEV:VISIBLE = TRUE.
      CMB_LEV:VISIBLE = TRUE.
   END.
   ELSE  DO:
      FBTN_VISA:VISIBLE = TRUE.   
      FBTN_EX:VISIBLE = TRUE.
      TOG_LEV:VISIBLE = TRUE. 
   END.
   
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaao_UI C-Win 
PROCEDURE allaao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_VAONR:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN KUURVALAPP.P PERSISTENT SET kuurvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN INKDIVHAMTAPP.P PERSISTENT SET hamtapp ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KUURVALAPP.P PERSISTENT SET kuurvalapph.
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
      RUN INKDIVHAMTAPP.P PERSISTENT SET hamtapp.
      RUN NETTOMARK.P PERSISTENT SET nettoh. 
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokomp_UI C-Win 
PROCEDURE aokomp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnextra_UI C-Win 
PROCEDURE btnextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnupp_UI C-Win 
PROCEDURE btnupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delnrkoll_UI C-Win 
PROCEDURE delnrkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   BRW_VAONR:SELECT-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.      
   RUN refreshbrw_UI IN brwproc[2].
   BRW_VAONR:DESELECT-ROWS() NO-ERROR.   
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
  DISPLAY TOG_LEV CMB_LEV 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_VAONR FBTN_VISA TOG_LEV CMB_LEV FBTN_EX BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI C-Win 
PROCEDURE excel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE pfarg AS INTEGER NO-UNDO.
   DEFINE VARIABLE efarg AS INTEGER NO-UNDO.
   DEFINE VARIABLE efarg2 AS INTEGER NO-UNDO.   
   pfarg = Guru.Konstanter:varforetypval[28].
   RUN profexcelfarg_UI (INPUT pfarg, OUTPUT efarg).
   RUN profexcelfarg_UI (INPUT Guru.Konstanter:varforetypval[38], OUTPUT efarg2).   
   bladvar = 0.
   iRad = 1.
   ASSIGN
   breddantal = 9
   bredd[1] = 10
   bredd[2] = 50
   bredd[3] = 10
   bredd[4] = 6
   bredd[5] = 8
   bredd[6] = 10
   bredd[7] = 10
   bredd[8] = 10
   bredd[9] = 10
   slutbredd =  bredd[9].
   IF allac[1] = "" THEN RUN allac_UI.   
   allachar[1] = TRUE.
   RUN colbredd_UI.  
   RUN startexcel_UI.
   RUN namnbladexcel_UI (INPUT "Uppföljning beställt materiel").   
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   RUN kolumnexcel_UI.   
   RUN center_UI (INPUT 4, INPUT "C").
   RUN center_UI (INPUT 3, INPUT "E").
   RUN center_UI (INPUT 4, INPUT "F").   
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END. 
      IF SUBSTRING(tidut.UT,1) = "==" THEN DO:
         LEAVE.
      END.      
   END.
   raknare = 1.
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF AVAILABLE tidut THEN DO:
      REPEAT:
         IF tidut.UT NE "" THEN DO:                         /*                       höjd    färg  */
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 12,INPUT FALSE,INPUT 14,INPUT 14,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         END.
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.     
         IF SUBSTRING(tidut.UT,1) = "==" THEN DO:
            LEAVE.
         END.         
      END.
   END.
   raknare = 1.
   FIND NEXT tidut NO-LOCK NO-ERROR.
   REPEAT:
      IF tidut.UT NE "" THEN DO:                                                    /**/
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
      END.
      
      IF SUBSTRING(tidut.UT,230) = "@" THEN DO:         
         RUN bgcellc_UI (INPUT "F", INPUT "F", INPUT efarg).
      END.
      ELSE IF SUBSTRING(tidut.UT,230) = "#" THEN DO:         
         RUN bgcellc_UI (INPUT "F", INPUT "F", INPUT efarg2).
      END.
      ELSE RUN bgcellc_UI (INPUT "F", INPUT "F", INPUT 0).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
   RUN slutexcel_UI. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extraval_UI C-Win 
PROCEDURE extraval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtfavoriter_UI C-Win 
PROCEDURE hmtfavoriter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddaurval_UI C-Win 
PROCEDURE laddaurval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI C-Win 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapaoval_UI C-Win 
PROCEDURE skapaoval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparaaofavoriter_UI C-Win 
PROCEDURE sparaaofavoriter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SuperExcel_UI C-Win 
PROCEDURE SuperExcel_UI :
EMPTY TEMP-TABLE tempbermtrlz.   
   FOR EACH tempbermtrl NO-LOCK:
      CREATE tempbermtrlz.
      BUFFER-COPY tempbermtrl TO tempbermtrlz.            
   END.   
   FOR EACH tempbermtrlz NO-LOCK:
      tempbermtrlz.BENAMNING = TRIM(tempbermtrlz.BENAMNING,CHR(9)). 
      tempbermtrlz.BENAMNING = TRIM(tempbermtrlz.BENAMNING,CHR(10)). 
      tempbermtrlz.BENAMNING = TRIM(tempbermtrlz.BENAMNING,CHR(11)). 
      tempbermtrlz.BENAMNING = TRIM(tempbermtrlz.BENAMNING,CHR(12)). 
      tempbermtrlz.BENAMNING = TRIM(tempbermtrlz.BENAMNING,CHR(13)).
      tempbermtrlz.POSTNR = REPLACE(tempbermtrlz.POSTNR," ","") NO-ERROR. 
   END.
      
   RUN BESTVIS.W (INPUT 1, INPUT TABLE tempbermtrlz, INPUT TABLE tempbeststat).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidutar_UI C-Win 
PROCEDURE tidutar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER sumpris AS INTEGER NO-UNDO.   
   DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE bredd AS INTEGER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE nrcol AS INTEGER EXTENT 50 NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE str AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levtitel AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dagens AS DATE NO-UNDO.
   DEFINE VARIABLE tid AS CHARACTER NO-UNDO.
   DEFINE VARIABLE stad AS DATE NO-UNDO.
   DEFINE VARIABLE stod AS DATE NO-UNDO.
   DEFINE VARIABLE utfardare AS CHARACTER NO-UNDO.
   EMPTY TEMP-TABLE tidut.
   ASSIGN
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6.   
   ASSIGN
   breddantal = 6   /*antal kolumner*/
   bredd[1] = 10
   bredd[2] = 50
   bredd[3] = 10
   bredd[4] = 6
   bredd[5] = 8
   bredd[6] = 10.
   ASSIGN
   utnr[1] = 1
   utnr[2] = 2
   utnr[3] = 3
   utnr[4] = 4
   utnr[5] = 5
   utnr[6] = 6.   
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1] + 1.            
      i = i + 1.
   END.   
   ASSIGN
   str = "".  
   i = 1.
   DO WHILE i <= utnr[breddantal] + bredd[breddantal] - 1:
      str = str + "=".     
      i = i + 1.
   END.   
   i = 2.      
   DO WHILE i <= breddantal:             
      SUBSTRING(str,(utnr[i] - 1),1) = ".".      
      i = i + 1.
   END.
   dagens = TODAY.
   tid = STRING(TIME, "HH:MM:SS").
   IF TOG_LEV = TRUE THEN levtitel = "Alla Leverantörer".
   ELSE IF TOG_LEV = FALSE THEN levtitel = CMB_LEV.   
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "Gjorda beställningar".
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = STRING(dagens) + " " + tid.
   CREATE tidut.  
   CREATE tidut.  
   SUBSTRING(tidut.UT,1) = "Beställningar för: " + levtitel.
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "Antal beställningar: " + STRING(bestrak).
   CREATE tidut.
   CREATE tidut.
   /*rubriker*/
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "==".   
   CREATE tidut.
   SUBSTRING(tidut.UT,utnr[nrcol[1]]) = "ENR".
   SUBSTRING(tidut.UT,utnr[nrcol[2]]) = "BENÄMNING".
   SUBSTRING(tidut.UT,utnr[nrcol[3]]) = "ANTAL".
   SUBSTRING(tidut.UT,utnr[nrcol[4]]) = "ENHET".
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "LEV-ID".
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = "Pris".   
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "==".   
   FOR EACH tempbermtrl WHERE tempbermtrl.ANTAL > 0 USE-INDEX ENR  NO-LOCK:
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,utnr[nrcol[1]]) = tempbermtrl.ENR.
      SUBSTRING(tidut.UT,utnr[nrcol[2]]) = SUBSTRING(tempbermtrl.BENAMNING,1,50).
      SUBSTRING(tidut.UT,utnr[nrcol[3]]) = STRING(tempbermtrl.ANTAL).
      SUBSTRING(tidut.UT,utnr[nrcol[4]]) = tempbermtrl.ENHET.
      SUBSTRING(tidut.UT,utnr[nrcol[5]]) = tempbermtrl.LEVKOD.
      SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(tempbermtrl.PRIS).
      IF tempbermtrl.RABATT = TRUE THEN DO:
         SUBSTRING(tidut.UT,230) = "@".
      END.
      IF tempbermtrl.RABATT = ? THEN DO:
         SUBSTRING(tidut.UT,230) = "#".
      END.
   END.

   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,utnr[nrcol[5]]) = "Totalpris:".
   SUBSTRING(tidut.UT,utnr[nrcol[6]]) = STRING(sumpris).
    RUN excel_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttcopy_UI C-Win 
PROCEDURE ttcopy_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ttjmf_UI C-Win 
PROCEDURE ttjmf_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utf_UI C-Win 
PROCEDURE utf_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: omg bbq
------------------------------------------------------------------------------*/
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utvald_UI C-Win 
PROCEDURE utvald_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa2_UI C-Win 
PROCEDURE visa2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE levkodvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumpris AS INTEGER NO-UNDO.
   levkodvar = CMB_LEV.   
   EMPTY TEMP-TABLE tempbermtrl.
   EMPTY TEMP-TABLE tempbermtrl2.
   EMPTY TEMP-TABLE tempbeststat NO-ERROR.    
   {muswait.i}      
   FOR EACH evaldaao NO-LOCK:
       RUN hamtabermtrl_UI IN hamtapp (INPUT TRUE, INPUT "Alla", INPUT evaldaao.AONR, INPUT evaldaao.DELNR, INPUT evaldaao.OMRADE, INPUT 01/01/89, INPUT TODAY, OUTPUT TABLE tempbermtrl APPEND, OUTPUT TABLE tempbeststat APPEND).                   
    END.
    FIND FIRST tempbermtrl WHERE NO-LOCK NO-ERROR.
    RUN aoproj_UI IN hamtapp (INPUT-OUTPUT TABLE tempbermtrl).
   /* Nettomärkning */
   RUN uppnettomark_UI IN nettoh (INPUT-OUTPUT TABLE tempbermtrl).
   FOR EACH tempbermtrl:
      IF tempbermtrl.RABATT = TRUE THEN tempbermtrl.FARG = "Färg2".
      ELSE IF tempbermtrl.RABATT = FALSE  THEN tempbermtrl.FARG = "Nej".
      ELSE IF tempbermtrl.RABATT = ?  THEN tempbermtrl.FARG = "Färg1".
      /* 
      ljusgrön = ?   = färg1
      mörkgrön = Ja  = färg2
      ofärgat = Nej = Nej
      */
   END.
      
   {musarrow.i}  
   
    
   FIND FIRST evaldaao NO-LOCK NO-ERROR.
   IF AVAILABLE evaldaao THEN DO:      
      RUN SuperExcel_UI.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI C-Win 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE levkodvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sumpris AS INTEGER NO-UNDO.
   levkodvar = CMB_LEV.   
   EMPTY TEMP-TABLE tempbermtrl.
   EMPTY TEMP-TABLE tempbermtrl2.   
   {muswait.i}      
   bestrak = 0.
   FOR EACH evaldaao NO-LOCK:
      bestrak =  bestrak + 1.
      EMPTY TEMP-TABLE tempbermtrl2.
      RUN hamtabermtrl_UI IN hamtapp (INPUT TOG_LEV, INPUT levkodvar, INPUT evaldaao.AONR, INPUT evaldaao.DELNR, INPUT evaldaao.OMRADE, INPUT 01/01/89, INPUT TODAY, OUTPUT TABLE tempbermtrl2, OUTPUT TABLE tempbeststat APPEND).                    
      FOR EACH tempbermtrl2 NO-LOCK:
         FIND FIRST tempbermtrl WHERE tempbermtrl.ENR = tempbermtrl2.ENR AND tempbermtrl.LEVKOD = tempbermtrl2.LEVKOD NO-ERROR.    
         /*om enr finns lägg till i antal*/
         IF AVAILABLE tempbermtrl THEN DO:
            tempbermtrl.ANTAL = tempbermtrl.ANTAL + tempbermtrl2.ANTAL.
            tempbermtrl.PRIS = tempbermtrl.PRIS + (tempbermtrl2.PRIS * tempbermtrl2.ANTAL).
         END.
         ELSE DO:
            CREATE tempbermtrl.
            BUFFER-COPY tempbermtrl2 TO tempbermtrl.
            tempbermtrl.PRIS = tempbermtrl.PRIS * tempbermtrl.ANTAL.
         END.                  
      END.                          
   END.   
   FOR EACH tempbermtrl NO-LOCK:
      sumpris = sumpris + tempbermtrl.PRIS.
      tempbermtrl.PRIS = INTEGER(tempbermtrl.PRIS). /*ören bort efteråt, sumpris blir summerad med ören, men respektive pris visas utan ören*/
   END.      
   RUN uppnettomark_UI IN nettoh (INPUT-OUTPUT TABLE tempbermtrl).      
   RUN tidutar_UI (INPUT sumpris).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

