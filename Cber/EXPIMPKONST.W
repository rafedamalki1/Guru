&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEFINE INPUT PARAMETER typval AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I} 
{TTEXPIMPHD.I}
&SCOPED-DEFINE SHARED SHARED
{OMRTEMPW.I}
{BERTEMPTAB.I}
{LISTMTRL.I}
&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{HUVLEVTEMP.I}
{KONSTRMTRL.I}
{BEREDNINGTEMP.I}
{EXTRADATA.I}
{EXTRATAB.I}  
{HOPALLA.I}
DEFINE VARIABLE valnlevmtrlkat AS CHARACTER NO-UNDO.

DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ekoppdataapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE valimpomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valanv AS CHARACTER NO-UNDO.




DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vbytlevapph3 AS HANDLE NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(78)" NO-UNDO.       
DEFINE VARIABLE tmpattrlist AS CHARACTER FORMAT "X(78)" NO-UNDO. 
DEFINE VARIABLE tmpdirlist AS CHARACTER FORMAT "X(78)" NO-UNDO.  
DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE delbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE varbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE varomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar3 AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttnr AS INTEGER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnuppxml AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE tempomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempnum AS INTEGER NO-UNDO.
DEFINE VARIABLE listobj AS CHARACTER NO-UNDO.
DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO. 
DEFINE STREAM dirstrom.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE skrivaover AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE expimpapph AS HANDLE NO-UNDO.
DEFINE VARIABLE skapanyttnummer AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaprochspar AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE avcadaccessh AS HANDLE NO-UNDO.
DEFINE VARIABLE avbryt AS LOGICAL NO-UNDO.
DEFINE VARIABLE korfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbhand AS CHARACTER NO-UNDO.
DEFINE VARIABLE felhand AS LOGICAL NO-UNDO.
DEFINE VARIABLE xmlsparfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE aofinns AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparasing AS LOGICAL NO-UNDO.
DEFINE VARIABLE nodtabhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE lnamnex AS CHARACTER NO-UNDO.
DEFINE VARIABLE antmtrlber AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE kon_grupp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE TEMP-TABLE avcadtemp
   FIELD ID AS INTEGER LABEL "Id"
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD COMMENT AS CHARACTER LABEL "Kommentar"
   FIELD AVVAGNING AS INTEGER LABEL "Avvägning"
   INDEX ID IS PRIMARY ID.
DEFINE TEMP-TABLE tempnamn NO-UNDO
   FIELD NAMNH AS HANDLE
   FIELD TORGTH AS HANDLE
   FIELD NODTAB AS CHARACTER.
DEFINE TEMP-TABLE tempaobersing2 NO-UNDO LIKE tempaobersing.
DEFINE TEMP-TABLE valtemp                  
   FIELD OMRADE AS CHARACTER
   FIELD BERNR AS INTEGER   
   FIELD BENAMNING AS CHARACTER
   FIELD NYTTNR AS INTEGER INITIAL 0
   INDEX OMR OMRADE BERNR ASCENDING.
DEFINE TEMP-TABLE imptemp NO-UNDO
   FIELD INVAR AS CHARACTER
   FIELD INDIR AS CHARACTER.
DEFINE TEMP-TABLE berfil NO-UNDO
   FIELD INDIR AS CHARACTER
   FIELD FILNAMNS AS CHARACTER
   FIELD ROWBER  AS ROWID
   INDEX ROWBER ROWBER.
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_KON

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstgrptemp

/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON konstgrptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH konstgrptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH konstgrptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON konstgrptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON konstgrptemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_KON}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_BARBAR RAD_MTR CMB_LEV TOG_MTRLPRIS ~
SEL_XML BRW_KON BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_MTR CMB_LEV TOG_MTRLPRIS SEL_XML ~
FILL-IN-BER 

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
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_BARBAR 
     LABEL "Bärbar dator" 
     SIZE 7.25 BY 2.33.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 30.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE RAD_MTR AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Beredningsupplägg", 1,
"Materielkatalog", 2
     SIZE 37 BY .71 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bärbardator", 1,
"Handdator", 2,
"AvCAD", 3
     SIZE 53.5 BY .75 NO-UNDO.

DEFINE VARIABLE SEL_XML AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 52.5 BY 16.5 NO-UNDO.

DEFINE VARIABLE TOG_MTRLPRIS AS LOGICAL INITIAL no 
     LABEL "Ta med pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KON FOR 
      konstgrptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON C-Win _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konstgrptemp.BENAMNING FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 43 BY 16.5
         TITLE "Välj konstruktionsgrupp(er)" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MBTN_BARBAR AT ROW 1.08 COL 1.13
     RAD_VAL AT ROW 1.25 COL 59.5 NO-LABEL
     RAD_MTR AT ROW 9.04 COL 16.25 NO-LABEL WIDGET-ID 18
     CMB_LEV AT ROW 9.04 COL 66.5 COLON-ALIGNED WIDGET-ID 16
     TOG_MTRLPRIS AT ROW 9.04 COL 88.88 WIDGET-ID 14
     SEL_XML AT ROW 11 COL 60 NO-LABEL
     BRW_KON AT ROW 11.75 COL 14
     BTN_OK AT ROW 28.04 COL 83.5
     BTN_AVB AT ROW 28.04 COL 98.5
     FILL-IN-BER AT ROW 3.63 COL 2.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.38 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: avcadtemp T "?" NO-UNDO temp-db avcadtemp
      TABLE: beredningtemp T "?" NO-UNDO TEMP-DB beredningtemp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av byggnummer"
         HEIGHT             = 28.42
         WIDTH              = 112.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 112.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 112.38
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
/* BROWSE-TAB BRW_KON SEL_XML DEFAULT-FRAME */
ASSIGN 
       BRW_KON:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BER IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       MBTN_BARBAR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konstgrptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstgrptemp.BENAMNING
"konstgrptemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Val av byggnummer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Val av byggnummer */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = FALSE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   IF VALID-HANDLE(avcadaccessh) THEN RUN close_UI IN avcadaccessh.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   {muswait.i}      
   ASSIGN
   RAD_MTR = INPUT RAD_MTR
   TOG_MTRLPRIS = INPUT TOG_MTRLPRIS
   CMB_LEV = INPUT CMB_LEV.
   /*Export*/   
   IF typval = 1 THEN DO:
      CMB_LEV = INPUT CMB_LEV.
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV
      USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE levtemp THEN valnlevmtrlkat = levtemp.LEVKOD.
      ELSE valnlevmtrlkat = "".
      RUN export_UI.      
   END.
   ELSE IF typval = 2 THEN DO:
      /*Import*/
      CMB_LEV = INPUT CMB_LEV.
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV
      USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE levtemp THEN valnlevmtrlkat = levtemp.LEVKOD.
      ELSE valnlevmtrlkat = "".
      RUN import_UI.
   END.
   {musarrow.i}   
   ASSIGN
   valomrade = valimpomrade.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_BARBAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_BARBAR C-Win
ON CHOOSE OF MBTN_BARBAR IN FRAME DEFAULT-FRAME /* Bärbar dator */
DO:   
   RAD_VAL = 1.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_MTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_MTR C-Win
ON VALUE-CHANGED OF RAD_MTR IN FRAME DEFAULT-FRAME
DO:
   RAD_MTR = INPUT RAD_MTR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:

   mvar = "*.xml".

      ASSIGN
      BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      mvar = "*$.xml".
      /*import*/
      IF typval = 2 THEN DO:  
         IF RAD_MTR = 1 THEN mvar = "*UppBere.xml".
         ELSE IF RAD_MTR = 2 THEN mvar = "*mtrl.xml".
         FILL-IN-BER = "Import från annan dator.".
         
      END. 
      ELSE DO:
         /*Export*/
         FILL-IN-BER = "Export till annan dator.".
         IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            ASSIGN
            CMB_LEV:HIDDEN = FALSE 
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                               
         END.
         ELSE DO:
            TOG_MTRLPRIS = FALSE.
            ASSIGN
            CMB_LEV:HIDDEN = true 
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
      END.
      RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).   
   DISPLAY FILL-IN-BER WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MTRLPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRLPRIS C-Win
ON VALUE-CHANGED OF TOG_MTRLPRIS IN FRAME DEFAULT-FRAME /* Ta med pris */
DO:
   TOG_MTRLPRIS = INPUT TOG_MTRLPRIS.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KON
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
      
   IF VALID-HANDLE(ekoppdataapph) THEN DELETE PROCEDURE ekoppdataapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
   IF VALID-HANDLE(expimpapph) THEN DELETE PROCEDURE expimpapph NO-ERROR.   
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   DEBUGGER:SET-BREAK().
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}      
   RAD_MTR = 1.
   IF Guru.Konstanter:globomr = "" THEN DO:
      FIND FIRST omrtemp NO-LOCK NO-ERROR.
      Guru.Konstanter:globomr = omrtemp.OMRADE.
   END.      
   skrivaover = TRUE.  
   status-ok = CMB_LEV:DELETE("0").
   RUN levhmt_UI IN expimpapph (OUTPUT TABLE levtemp,OUTPUT TABLE huvlevtemp). 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE 
   USE-INDEX LEV NO-LOCK:
      IF levtemp.LEVKOD = "99" THEN DO:
      END.
      ELSE DO:
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN).
      END.      
   END.   
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD = huvlevtemp.LEVKOD NO-LOCK NO-ERROR.
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   END.  
   IF typval = 1 THEN DO:       
      FILL-IN-BER = "Export till annan dator.".
      {&WINDOW-NAME}:TITLE = "Export".     
      EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
      tthandle = TEMP-TABLE beredningtemp:HANDLE.
      IF valomrade = "" OR valomrade = ? THEN DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT "").
      END.
      ELSE DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT " WHERE OMRADE = '" + valomrade + "'").
         FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = valomrade AND 
         beredningtemp.BERNR = valbernr NO-LOCK NO-ERROR.   
      END.      
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Import".
      FILL-IN-BER = "Import från annan dator.".     
   END.
   RUN enable_UI. 
   SEL_XML:HIDDEN = TRUE.
   {FRMSIZE.I}
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN musz = TRUE.  
   IF musz = TRUE THEN DO:
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      musz = FALSE.
      LEAVE MAIN-BLOCK. 
   END.        
   ASSIGN
   BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN selfill_UI.     
   ASSIGN
   MBTN_BARBAR:COLUMN = 1.
   ASSIGN
   Guru.GlobalaVariabler:collefth = MBTN_BARBAR:HANDLE.
   MBTN_BARBAR:LOAD-IMAGE("BILDER\xbtn_barb.gif") NO-ERROR.       
   RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
   IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN.
   ELSE DO:
      ASSIGN
      CMB_LEV:HIDDEN = TRUE
      TOG_MTRLPRIS:HIDDEN = TRUE       
      RAD_MTR:HIDDEN = TRUE.
   END.
   IF typval = 1 THEN DO:
   END.
   {musarrow.i}
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_KON:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING"). 
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPUAPPHD.P PERSISTENT SET expimpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPUAPPHD.P PERSISTENT SET expimpapph.
   END. 
   /*HD HÄR*/
   
   tthandle = TEMP-TABLE konstgrptemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRATABHMT.P PERSISTENT SET ekoppdataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRATABHMT.P PERSISTENT SET ekoppdataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN VBYTLEVAPP3.P PERSISTENT SET vbytlevapph3 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VBYTLEVAPP3.P PERSISTENT SET vbytlevapph3.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvit_UI C-Win 
PROCEDURE btnvit_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER btnh AS HANDLE NO-UNDO.
   DEFINE VARIABLE btnlabel AS CHARACTER NO-UNDO.
   IF btnnovit NE ? THEN DO:
      btnlabel = btnnovit:IMAGE.
      btnlabel = REPLACE(btnlabel,"_vit.gif",".gif"). 
      btnnovit:LOAD-IMAGE (btnlabel) NO-ERROR.
   END.
   btnlabel = btnh:IMAGE.
   btnlabel = REPLACE(btnlabel,".gif","_vit.gif"). 
   btnh:LOAD-IMAGE (btnlabel) NO-ERROR.
   btnnovit = btnh.   
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
  DISPLAY RAD_MTR CMB_LEV TOG_MTRLPRIS SEL_XML FILL-IN-BER 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE MBTN_BARBAR RAD_MTR CMB_LEV TOG_MTRLPRIS SEL_XML BRW_KON BTN_OK 
         BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export_UI C-Win 
PROCEDURE export_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
   filnamn = fildir.
   RUN expxml_UI.                    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expxml_UI C-Win 
PROCEDURE expxml_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_MTR = 2 THEN DO:
      RUN expmtrlkat_UI IN expimpapph (INPUT valnlevmtrlkat,INPUT TOG_MTRLPRIS, OUTPUT TABLE tempmtrl).   
   END.
  
   IF RAD_MTR = 1 THEN DO:           
      RUN selxml_UI (INPUT "Konstruktionsupplägget sammanställs").
      EMPTY TEMP-TABLE kon_grupp NO-ERROR. 
      
      FOR EACH konstgrptemp:
         CREATE kon_grupp.
         BUFFER-COPY konstgrptemp TO kon_grupp.
      END.      
      /*HD HÄR*/
      
      RUN expupplagghd_UI IN expimpapph (
                                     INPUT TOG_MTRLPRIS,
                                     INPUT TABLE kon_grupp,
                                     OUTPUT TABLE tempkonstgrupp,   
                                     OUTPUT TABLE tempbbenamning,    
                                     OUTPUT TABLE tempedigrupp,      
                                     OUTPUT TABLE tempkonstruktion,  
                                     OUTPUT TABLE tempkonstval,      
                                     OUTPUT TABLE tempmtrlber,       
                                     OUTPUT TABLE tempberstolp,      
                                     OUTPUT TABLE tempberskap,                                             
                                     OUTPUT TABLE tempkalkber,
                                     OUTPUT TABLE tempforlagg,
                                     OUTPUT TABLE tempsamforlagg,
                                     OUTPUT TABLE tempytbelagg,
                                     OUTPUT TABLE hdhandtemp,
                                     OUTPUT TABLE hdkkopptemp,
                                     OUTPUT TABLE tempersattemp,
                                     OUTPUT TABLE temputbyteslista,
                                     OUTPUT TABLE temputbyteslnamn).
      
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "KABLAR"
      inextradatatemp.HUVUDCH = ? 
      inextradatatemp.HUVUDINT = ?. 
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FOR EACH extradatatemp:
         IF extradatatemp.SOKLOG[1] = TRUE THEN DO:
            CREATE tempkabel.
            ASSIGN
            tempkabel.KABEL = extradatatemp.HUVUDCH
            tempkabel.KABID = extradatatemp.HUVUDINT 
            tempkabel.DIAMETER = extradatatemp.SOKINT[1]
            tempkabel.BORTTAGEN = extradatatemp.SOKLOG[1].

         END.
         ELSE DO:
            CREATE tempkabel.
            ASSIGN
            tempkabel.KABEL = extradatatemp.HUVUDCH
            tempkabel.KABID = extradatatemp.HUVUDINT 
            tempkabel.DIAMETER = extradatatemp.SOKINT[1]
            tempkabel.BORTTAGEN = extradatatemp.SOKLOG[1].
         END.
      END.            
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   END.
   
   /*export*/
   RUN utinxml_UI (INPUT 1). 
   IF RAD_MTR = 1 THEN MESSAGE "Upplägget är exporterat till " filnamnuppxml VIEW-AS ALERT-BOX.
   IF RAD_MTR = 2 THEN MESSAGE "Matteriellistan är exporterat till " filnamnuppxml VIEW-AS ALERT-BOX.  
   Guru.Konstanter:varforetypchar[50] = "".
      
   MESSAGE "Vill du skicka upplägget som E-post?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svaru AS LOGICAL.
   CASE svaru:
      WHEN TRUE THEN DO:
         IF Guru.Konstanter:varforetypchar[50] = "" THEN Guru.Konstanter:varforetypchar[50] = filnamnuppxml.                    
         ELSE Guru.Konstanter:varforetypchar[50] = Guru.Konstanter:varforetypchar[50] + "," + filnamnuppxml.              
      END.
      WHEN FALSE THEN DO:
         filnamnuppxml = "".
      END.
   END CASE.      
   IF Guru.Konstanter:varforetypchar[50] NE ""  THEN DO:      
      DEFINE VARIABLE infiler AS CHARACTER NO-UNDO.
      IF RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE THEN infiler = filnamnuppxml.
      
      DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
      file-info:file-name = ".".
      orgdir = file-info:full-pathname.
      RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT infiler).
      RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
   END.
   Guru.Konstanter:varforetypchar[50] = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import_UI C-Win 
PROCEDURE import_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE bortsvar AS LOGICAL NO-UNDO.   
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".   
   /*import handdator*/
   EMPTY TEMP-TABLE valtemp NO-ERROR.
   /*import*/      
   RUN utinxml_UI (INPUT 2).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.                   
   MESSAGE "Import klar!" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selfill_UI C-Win 
PROCEDURE selfill_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      IF OS-GETENV("SystemDrive") NE ? THEN webclienttempdir = OS-GETENV("SystemDrive") + "\temp\".
      ELSE webclienttempdir = "c:\temp\".
      OS-CREATE-DIR VALUE(webclienttempdir) . 
      OS-CREATE-DIR VALUE(webclienttempdir + Guru.Konstanter:globanv) .      
      IF OS-ERROR NE 0  THEN DO:
         webclienttempdir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv.
         OS-CREATE-DIR VALUE(webclienttempdir + Guru.Konstanter:globanv).
         webclienttempdir = webclienttempdir + "\".                
      END.
      ELSE webclienttempdir = webclienttempdir + Guru.Konstanter:globanv + "\".              
   END.   
   OS-CREATE-DIR VALUE(fildir) NO-ERROR. 
   mappvar = fildir.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selxml_UI C-Win 
PROCEDURE selxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER txtvar AS CHARACTER NO-UNDO.
   SEL_XML:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   SEL_XML:ADD-LAST(txtvar + " " + STRING(TIME,"HH:MM:SS")).
   DISPLAY SEL_XML WITH FRAME {&FRAME-NAME}.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utinxml_UI C-Win 
PROCEDURE utinxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynxml AS HANDLE NO-UNDO.
   DEFINE VARIABLE sparfilnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE okand AS LOGICAL NO-UNDO.
   okand = TRUE.
   mvarhj = REPLACE(mvar,"*","").
   IF fildir = "" THEN  RUN selfill_UI.   
   filnamnuppxml = fildir + "mtrl.xml".
   {UTXKMLKOLL.I}
   RUN VALUE(vxmlpro) PERSISTENT SET dynxml (INPUT okand).
   EMPTY TEMP-TABLE tempnamn NO-ERROR.    
   IF vad = 1 THEN DO: /* 1 = export (skapa xml fil) */      
      EMPTY TEMP-TABLE tempnamn NO-ERROR.    
      sparfilnamn = filnamnuppxml.      
      IF RAD_MTR = 1 THEN DO:
         RUN uttab_UI (INPUT 2,INPUT TRUE).
      END.   
      ELSE IF RAD_MTR = 2 THEN DO:
         RUN uttab_UI (INPUT 3,INPUT TRUE).
      END.    
      IF RAD_MTR = 1 THEN  filnamnuppxml = fildir + "uppmtrl.xml".
      ELSE DO:
         filnamnuppxml = fildir + cmb_lev + "mtrl.xml".
      END.
      sparfilnamn = filnamnuppxml.
      RUN selxml_UI (INPUT "Materielkatalog och/eller Konstruktionsuppläggets filen skapas").         
      FOR EACH tempnamn:
         IF sparfilnamn NE "" THEN RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE,INPUT "start",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT sparfilnamn).
         ELSE RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE ,INPUT "",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT "").
         sparfilnamn = "".
      END.
      RUN xmlutslut_UI IN dynxml.
      RUN xmlslut_UI IN dynxml.
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
      RUN selxml_UI (INPUT "Kopieringen färdig").                     
   END.
   /*import*/
   IF vad = 2 THEN DO:
      EMPTY TEMP-TABLE tempaobersing NO-ERROR.
      EMPTY TEMP-TABLE tempglobforetag NO-ERROR.
      IF Guru.Konstanter:appcon THEN DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.
      ELSE DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar.      
      END.      
      /*import kataloger*/      
      IF RAD_MTR = 1 THEN  filnamnuppxml = fildir + "uppmtrl.xml".
      ELSE filnamnuppxml = fildir + cmb_lev + "mtrl.xml".
      sparfilnamn = filnamnuppxml.   
      MESSAGE "Vill Ni skriva över Materielkatalog eller Konstruktionsupplägget som finns i denna dator?" SKIP
      "Filen som läses in ska vara : "  filnamnuppxml      
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE mtrkat AS LOGICAL.
      IF mtrkat THEN. 
      ELSE DO:
         RETURN NO-APPLY.           
      END.
      IF RAD_MTR = 1 THEN DO:
         RUN uttab_UI (INPUT 2,INPUT TRUE).
         RUN selxml_UI (INPUT "Det gamla Konstruktionsupplägget tas bort!").
         RUN delupplagg_UI IN expimpapph.
      END.   
      IF RAD_MTR = 2 THEN DO:
         RUN uttab_UI (INPUT 3,INPUT TRUE).
         RUN selxml_UI (INPUT "De gamla katalogerna tas bort!").     
         RUN delmtrl_UI IN expimpapph (INPUT valnlevmtrlkat).
      END.    
    
      RUN selxml_UI (INPUT "Materielkatalog eller Konstruktionsupplägget läses in i Guru").         
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
      RUN xmlslut_UI IN dynxml.
      FOR EACH tempnamn:       
         IF tempnamn.NODTAB = "TEMPKABEL" THEN.
         ELSE IF tempnamn.NODTAB = "tempglobforetag" THEN.
         ELSE IF tempnamn.NODTAB = "tempaobersing" THEN.
         ELSE  IF tempnamn.NODTAB = "MTRL" THEN RUN sparamtrltemp_UI IN laddaprochspar (INPUT valnlevmtrlkat,INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).
         ELSE IF tempnamn.NODTAB = "tFORLAGG" OR tempnamn.NODTAB = "tYTBELAGG" THEN DO:
            nodtabhj = tempnamn.NODTAB.
            tempnamn.NODTAB = "".
            tempnamn.NODTAB = SUBSTRING(nodtabhj,2).
            RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).
         END.   
         ELSE RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).  
      END.            
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      FOR EACH tempkabel:
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "KABLAR"  
         inextradatatemp.HUVUDCH = tempkabel.KABEL
         inextradatatemp.HUVUDINT = tempkabel.KABID
         inextradatatemp.SOKINT[1] = tempkabel.DIAMETER
         inextradatatemp.SOKLOG[1] = tempkabel.BORTTAGEN.
      END.
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      RUN selxml_UI (INPUT "Kopieringen färdig").               
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   END.
   ELSE IF vad = 4 THEN DO:
      /*läser in för att kolla rätt företag + rätt aonr */
      RUN uttab_UI (INPUT 0,INPUT TRUE).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT berfil.INDIR + berfil.FILNAMNS).
      RUN xmlslut_UI IN dynxml.
      FIND FIRST tempaobersing NO-ERROR.
   END.   
   IF VALID-HANDLE(laddaprochspar) THEN DELETE PROCEDURE laddaprochspar NO-ERROR.   
   IF VALID-HANDLE(dynxml) THEN DELETE PROCEDURE dynxml.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uttab_UI C-Win 
PROCEDURE uttab_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER delvar AS LOGICAL NO-UNDO.
  
   IF delvar = TRUE THEN DO: 
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
     /* singel kopp till aotab (fr. extrakopplingar) */
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempaobersing:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempaobersing:HANDLE.
      tempnamn.NODTAB = "tempaobersing".
   END.   
   IF vad = 0 THEN DO:
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempglobforetag:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempglobforetag:HANDLE.
      tempnamn.NODTAB = "tempglobforetag".
   END.
   IF vad = 2 THEN DO:
      /*HD HÄR*/
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempbbenamning:HANDLE.      
      tempnamn.TORGTH = TEMP-TABLE tempbbenamning:HANDLE.
      tempnamn.NODTAB = "BBENAMNING".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempedigrupp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempedigrupp:HANDLE.
      tempnamn.NODTAB = "EDIGRUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkonstgrupp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE tempkonstgrupp:HANDLE.
      tempnamn.NODTAB = "KONSTGRUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkonstruktion:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempkonstruktion:HANDLE.
      tempnamn.NODTAB = "KONSTRUKTION".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkonstval:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempkonstval:HANDLE.
      tempnamn.NODTAB = "KONSTVAL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmtrlber:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempmtrlber:HANDLE.
      tempnamn.NODTAB = "MTRLBER".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberstolp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempberstolp:HANDLE.
      tempnamn.NODTAB = "BERSTOLP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberskap:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempberskap:HANDLE.
      tempnamn.NODTAB = "BERSKAP".
      /*KALKYL-BEREDNING SUNDSVALL*/
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkalkber:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempkalkber:HANDLE.
      tempnamn.NODTAB = "KALKBER".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempforlagg:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempforlagg:HANDLE.
      tempnamn.NODTAB = "tFORLAGG".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempsamforlagg:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempsamforlagg:HANDLE.
      tempnamn.NODTAB = "SAMFORLAGG".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempytbelagg:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempytbelagg:HANDLE.
      tempnamn.NODTAB = "tYTBELAGG".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkabel:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempkabel:HANDLE.
      tempnamn.NODTAB = "TEMPKABEL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdhandtemp:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE hdhandtemp:HANDLE.
      tempnamn.NODTAB = "HDHANDELSE".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkkopptemp:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE hdkkopptemp:HANDLE.
      tempnamn.NODTAB = "HDKKOPP".
      
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER temputbyteslista:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE temputbyteslista:HANDLE.
      tempnamn.NODTAB = "UTBYTESLISTA".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER temputbyteslnamn:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE temputbyteslnamn:HANDLE.
      tempnamn.NODTAB = "UTBYTESLNAMN".
                        
      FIND FIRST tempnamn WHERE tempnamn.NODTAB = "tempglobforetag" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tempnamn THEN DO:
         CREATE tempnamn.        
         tempnamn.NAMNH = BUFFER tempglobforetag:HANDLE.
         tempnamn.TORGTH = TEMP-TABLE tempglobforetag:HANDLE.
         tempnamn.NODTAB = "tempglobforetag".
      END.                  
      /*OBS EXTRADATA FÖR KABLAR*/
   END.      
   IF vad = 3 THEN DO:
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmtrl:HANDLE.       
      tempnamn.TORGTH = TEMP-TABLE tempmtrl:HANDLE.
      tempnamn.NODTAB = "MTRL".
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI C-Win 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER txtvar AS CHARACTER NO-UNDO.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      korfil = SEARCH("wguru.exe").
      korfil = REPLACE(korfil,Guru.Konstanter:guruvar,"..\").      
   END.
   ELSE DO:      
      korfil = SEARCH("pget.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      korfil = SEARCH("pput.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).      
      korfil = SEARCH("wguru.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      korfil = SESSION:TEMP-DIRECTORY + "wguru.exe". 
   END.    
   filnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".   
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
   filnamn = REPLACE(filnamn,"\","\\").
   xmlsparfilnamn = filnamn + xmlsparfilnamn.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      dbhand = webclienttempdir.
      dbhand = REPLACE(webclienttempdir,"\","\\").
   END.
   ELSE DO:
      dbhand = REPLACE(SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv,"\","\\").
   END.   
   korfil = korfil + ' "' + txtvar + '"' + ' "' + xmlsparfilnamn + '"' + ' "' + dbhand + '"'.   
   OS-COMMAND SILENT VALUE(korfil).         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

