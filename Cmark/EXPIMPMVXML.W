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
/*Ändring av Mikael*/
/* DEFINE INPUT PARAMETER valrec AS RECID NO-UNDO. */
DEFINE INPUT PARAMETER typval AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER valmark AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER valomm AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER valbenm AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER valaom AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER valdelm AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{TTEXPIMPMV.I}
{VARDERINGTEMP.I}
 
&SCOPED-DEFINE SHARED SHARED
{MARKTEMP.I}
{OMRTEMPW.I}

&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED
{EXTRADATA.I}
DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE vbenmark AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valimpomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valanv AS CHARACTER NO-UNDO.






DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(78)" NO-UNDO.       
DEFINE VARIABLE tmpattrlist AS CHARACTER FORMAT "X(78)" NO-UNDO. 
DEFINE VARIABLE tmpdirlist AS CHARACTER FORMAT "X(78)" NO-UNDO.  
DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE delbernr AS INTEGER NO-UNDO.
DEFINE VARIABLE varbernr AS INTEGER NO-UNDO.
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
DEFINE VARIABLE tempvardnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempnum AS INTEGER NO-UNDO.
DEFINE VARIABLE listobj AS CHARACTER NO-UNDO.
DEFINE VARIABLE typdator AS INTEGER NO-UNDO.
DEFINE VARIABLE spartypdator AS INTEGER NO-UNDO.
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

DEFINE VARIABLE avbryt AS LOGICAL NO-UNDO.
DEFINE VARIABLE korfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbhand AS CHARACTER NO-UNDO.
DEFINE VARIABLE felhand AS LOGICAL NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tempnamn NO-UNDO
      FIELD NAMNH AS HANDLE
       FIELD TORGTH AS HANDLE
      FIELD NODTAB AS CHARACTER.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VARD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES varderingtemp

/* Definitions for BROWSE BRW_VARD                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VARD varderingtemp.OMRADE ~
varderingtemp.VARDNR varderingtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VARD 
&Scoped-define QUERY-STRING-BRW_VARD FOR EACH varderingtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VARD OPEN QUERY BRW_VARD FOR EACH varderingtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VARD varderingtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VARD varderingtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_BARBAR TOG_VARD SEL-IMP BRW_VARD ~
MBTN_HMT SEL_XML BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-VARDNR FILL-IN-BEN FILL-IN-AONR ~
FILL-IN-DELNR TOG_VARD SEL-IMP SEL_XML FILL-IN-BER FILL-IN-BER-2 

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

DEFINE BUTTON MBTN_HMT  NO-FOCUS
     LABEL "Sök värdering" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Välj värdering för import" 
      VIEW-AS TEXT 
     SIZE 39.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VARDNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Värdering nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bärbardator", 1
     SIZE 53.5 BY .75 NO-UNDO.

DEFINE VARIABLE SEL-IMP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 94 BY 16.5 NO-UNDO.

DEFINE VARIABLE SEL_XML AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 52.5 BY 16.5 NO-UNDO.

DEFINE VARIABLE TOG_VARD AS LOGICAL INITIAL no 
     LABEL "Värdering" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VARD FOR 
      varderingtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VARD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VARD C-Win _STRUCTURED
  QUERY BRW_VARD NO-LOCK DISPLAY
      varderingtemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      varderingtemp.VARDNR COLUMN-LABEL "Värdering" FORMAT "->>>>>>9":U
      varderingtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 75 BY 16.5
         TITLE "Välj värdering för import" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MBTN_BARBAR AT ROW 1.08 COL 1.13
     RAD_VAL AT ROW 1.25 COL 59.5 NO-LABEL
     FILL-IN-VARDNR AT ROW 6.08 COL 16.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 7.25 COL 16.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 8.38 COL 16.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 8.38 COL 38.5 COLON-ALIGNED
     TOG_VARD AT ROW 9.38 COL 28.5
     SEL-IMP AT ROW 11 COL 2.63 NO-LABEL
     BRW_VARD AT ROW 11 COL 12.13
     MBTN_HMT AT ROW 7.29 COL 98.5
     SEL_XML AT ROW 11 COL 60 NO-LABEL
     BTN_OK AT ROW 28.04 COL 83.5
     BTN_AVB AT ROW 28.04 COL 98.5
     FILL-IN-BER AT ROW 3.63 COL 2.5 NO-LABEL
     FILL-IN-BER-2 AT ROW 4.88 COL 2.5 NO-LABEL
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
      TABLE: varderingtemp T "?" NO-UNDO TEMP-DB varderingtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av värdering"
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
                                                                        */
/* BROWSE-TAB BRW_VARD SEL-IMP DEFAULT-FRAME */
ASSIGN 
       BRW_VARD:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BER IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BER-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DELNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-VARDNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       MBTN_BARBAR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_HMT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       SEL-IMP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VARD
/* Query rebuild information for BROWSE BRW_VARD
     _TblList          = "Temp-Tables.varderingtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.varderingtemp.OMRADE
"varderingtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.varderingtemp.VARDNR
"varderingtemp.VARDNR" "Värdering" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.varderingtemp.BENAMNING
"varderingtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VARD */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Val av värdering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Val av värdering */
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
   {muswait.i}      
   ASSIGN
   TOG_VARD = INPUT TOG_VARD   
   SEL-IMP = INPUT SEL-IMP.   
   /*Export*/
   
   IF typval = 1 THEN DO:
      RUN export_UI.      
   END.
   ELSE IF typval = 2 THEN DO:
      /*Import*/
      RUN import_UI.
   END.
   {musarrow.i}
   /*OS-DELETE VALUE(fildir).*/
   ASSIGN
   valomm = valimpomrade.
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


&Scoped-define SELF-NAME MBTN_HMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_HMT C-Win
ON CHOOSE OF MBTN_HMT IN FRAME DEFAULT-FRAME /* Sök värdering */
DO:     
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO FORMAT "X(50)".
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den fil som Ni vill koppla"
   FILTERS        "All Files (*£.xml)"  "*£.xml"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.   
   IF OKvald = TRUE THEN DO:
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      RUN in_UI.  
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
/*    RAD_VAL = INPUT RAD_VAL. */
   spartypdator = typdator.
   typdator = RAD_VAL.
   IF typdator = 1 THEN DO:
      ASSIGN      
      BRW_VARD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      /*import*/
      IF typval = 2 THEN DO:     
         FILL-IN-BER = "Import från annan dator.".
         MBTN_HMT:HIDDEN = FALSE.
         IF TOG_VARD = TRUE THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ELSE SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         TOG_VARD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         
      END.
      ELSE DO:
         /*Export*/
         FILL-IN-BER = "Export till annan dator.".
      END.
      RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
   END.
   
   DISPLAY FILL-IN-BER WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL-IMP C-Win
ON VALUE-CHANGED OF SEL-IMP IN FRAME DEFAULT-FRAME
DO:
   tempnum = SEL-IMP:LOOKUP(SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   tempomr = ENTRY(tempnum, listobj, ";").
   tempvardnr = ENTRY(1, tempomr, ",").
   tempomr = ENTRY(2, tempomr, ",").
   FIND FIRST varderingtemp WHERE varderingtemp.OMRADE = tempomr AND varderingtemp.VARDNR = INTEGER(tempvardnr) NO-LOCK NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VARD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VARD C-Win
ON VALUE-CHANGED OF TOG_VARD IN FRAME DEFAULT-FRAME /* Värdering */
DO:
   TOG_VARD = INPUT TOG_VARD.
   IF (valomm = ? OR valomm = "") AND typval = 1 THEN DO:
      MESSAGE "Ingen värdering vald.".
      ASSIGN TOG_VARD = FALSE. 

      DISPLAY TOG_VARD WITH FRAME {&FRAME-NAME}.
   END.
   IF typval = 2 THEN DO:
      IF typdator = 1 THEN DO:      
         SEL-IMP:HIDDEN = NOT TOG_VARD.
      END.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VARD
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
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.   
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
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   IF Guru.Konstanter:globomr = "" THEN DO:
      FIND FIRST omrtemp NO-LOCK NO-ERROR.
      Guru.Konstanter:globomr = omrtemp.OMRADE.
   END.   
   skrivaover = TRUE.
   IF typval = 1 THEN DO:   
      FILL-IN-BER = "Export till annan dator.".
      {&WINDOW-NAME}:TITLE = "Export".
     /*Ändring av Mikael*/      
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      tthandle = TEMP-TABLE varderingtemp:HANDLE.      
      IF valomm = "" OR valomm = ? THEN DO:         
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT "").
         ASSIGN
         FILL-IN-VARDNR = ?
         FILL-IN-AONR = '?'
         FILL-IN-DELNR = ? 
         FILL-IN-BEN = '?'.  
      END.
      ELSE DO:         
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT " WHERE OMRADE = '" + valomm + "'").
         FIND FIRST varderingtemp WHERE varderingtemp.OMRADE = valomm AND 
         varderingtemp.VARDNR = valmark NO-LOCK NO-ERROR.   
         ASSIGN
         FILL-IN-VARDNR = varderingtemp.VARDNR
         FILL-IN-AONR = varderingtemp.AONR
         FILL-IN-DELNR = varderingtemp.DELNR
         FILL-IN-BEN = varderingtemp.BENAMNING.   
      END.      
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Import".
      FILL-IN-BER = "Import från annan dator.".
      
   END.
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.                                                                     
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   TOG_VARD = TRUE.
   RUN enable_UI.    
   {FRMSIZE.I}
   IF Guru.SharedVariable:singel = FALSE THEN DO:
   /*client-server*/
      IF typval = 2 THEN DO:      
         /*import*/
         ASSIGN
         TOG_VARD = TRUE                  
         TOG_VARD:ROW = 4. 
         DISP  TOG_VARD WITH FRAME {&FRAME-NAME}. 
      END.
      ELSE DO:
         /*export*/
         
      END.
   END.
   ELSE DO:
      /*singel*/
      IF typval = 1 THEN DO:               
      END.
      ELSE DO:
         /*import*/         
         ASSIGN     
         TOG_VARD = TRUE.         
         DISP TOG_VARD WITH FRAME {&FRAME-NAME}. 
      END.
   END.
   IF typval = 1 THEN DO:
      ASSIGN
      FILL-IN-BER-2:HIDDEN = TRUE
      MBTN_HMT:HIDDEN = TRUE.
   END.
   IF typval = 2 THEN DO:
      ASSIGN
      FILL-IN-VARDNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-DELNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ASSIGN
   SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   BRW_VARD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   ASSIGN            
   typdator = 1
   RAD_VAL = 1.
   
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN selfill_UI.    
   RUN in_UI.
   ASSIGN
   MBTN_BARBAR:COLUMN = 1.
   ASSIGN
   Guru.GlobalaVariabler:collefth = MBTN_BARBAR:HANDLE.
   MBTN_BARBAR:LOAD-IMAGE("BILDER\xbtn_barb.gif") NO-ERROR.
   RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
   IF typval = 1 THEN DO:
      SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      BRW_VARD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   SEL_XML:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_VARD:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN setcolindex_UI IN brwproc[1] (INPUT "OMRADE").    
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).

   /*tthandle = TEMP-TABLE varderingtemp:HANDLE.*/
   tthandle = ?.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "", INPUT "").
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPUAPPMV.P PERSISTENT SET expimpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPUAPPMV.P PERSISTENT SET expimpapph.
   END. 
   
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
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
  DISPLAY FILL-IN-VARDNR FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR TOG_VARD SEL-IMP 
          SEL_XML FILL-IN-BER FILL-IN-BER-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE MBTN_BARBAR TOG_VARD SEL-IMP BRW_VARD MBTN_HMT SEL_XML BTN_OK BTN_AVB 
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
   IF TOG_VARD = TRUE THEN DO: 
      filnamn = fildir + STRING(varderingtemp.VARDNR) + "!" + varderingtemp.OMRADE + "!".
     

      RUN skapaexport_UI IN expimpapph (INPUT varderingtemp.VARDNR, INPUT varderingtemp.OMRADE, 
                                        OUTPUT TABLE tempvard,     
                                        OUTPUT TABLE tempfastighet,   
                                        OUTPUT TABLE tempfastvard,   
                                        OUTPUT TABLE tempfastextra,
                                        OUTPUT TABLE tempfastextra2,  
                                        OUTPUT TABLE tempmarkagare,    
                                        OUTPUT TABLE tempmarkfast,  
                                        OUTPUT TABLE tempakerkab,  
                                        OUTPUT TABLE tempakervard,
                                        OUTPUT TABLE tempskogvard,   
                                        OUTPUT TABLE tempvolskog,
                                        OUTPUT TABLE temprevavtal,
                                        OUTPUT TABLE tempnatproj,
                                        OUTPUT TABLE tempnatproj2,
                                        OUTPUT TABLE tempnathsand).  
      FOR EACH tempvard WHERE tempvard.VARDNR <= 0:
         DELETE tempvard.
      END.
      FOR EACH tempfastvard WHERE tempfastvard.VARDNR <= 0:
         DELETE tempfastvard.
      END.
      FOR EACH tempakervard WHERE tempakervard.VARDNR <= 0:
          DELETE tempakervard.
      END.        
      RUN utinxml_UI (INPUT 1). 
      Guru.Konstanter:varforetypchar[50] = "".      
      MESSAGE  "Värderingen är exporterad till "  filnamn  SKIP
         "Vill du skicka markvärderingen som E-post?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
      CASE svar:
         WHEN TRUE THEN DO:
            Guru.Konstanter:varforetypchar[50] = filnamn.               
         END.
         WHEN FALSE THEN DO:
            
         END.
      END CASE.
      IF Guru.Konstanter:varforetypchar[50] NE ""  THEN DO:
         /*RUN EMEDD.W (INPUT FALSE).*/
         DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
         file-info:file-name = ".".
         orgdir = file-info:full-pathname.
         RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT filnamn).
         RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
      END.
      Guru.Konstanter:varforetypchar[50] = "".
   END.
   
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
   DEFINE VARIABLE finnsber AS LOGICAL NO-UNDO.
   DEFINE VARIABLE finnsomr AS LOGICAL NO-UNDO.
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
   IF typdator = 1 THEN DO:        
      IF SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? AND TOG_VARD = TRUE THEN DO: 
         MESSAGE "Du har inte valt någon värdering." VIEW-AS ALERT-BOX
         TITLE "Meddelande".
         musz = TRUE.
         RETURN NO-APPLY.
      END.
      ELSE filnamn = fildir + tempvardnr + "!" + tempomr + "!".
      /*koll om Värderingen finns + FIXAR UPPLÄGGET*/
      RUN selxml_UI (INPUT "Markvärdering kontrolleras").
      EMPTY TEMP-TABLE dubfastighet NO-ERROR.
      ASSIGN
      varbernr = 0
      varomrade = "". 
      IF TOG_VARD = TRUE THEN DO:
         varomrade = "".
         MESSAGE "Ni måste skapa en ny värdering." SKIP
                 "Vill Du det?" VIEW-AS ALERT-BOX                            
         QUESTION BUTTONS YES-NO TITLE "Ny värdering?" UPDATE svar.         
         
         IF svar THEN DO:                           
            vart = "NYA".    
                                                      
            IF Guru.Konstanter:globforetag = "csweo" THEN RUN ANDRAVARD2.W (INPUT-OUTPUT nyttnr, INPUT varderingtemp.OMRADE,INPUT varderingtemp.BENAMNING).
            ELSE RUN ANDRAVARD2.W (INPUT-OUTPUT nyttnr, INPUT Guru.Konstanter:globomr,INPUT varderingtemp.BENAMNING).
            
            vart = "".     
            RUN nyvardomr_UI IN expimpapph (INPUT nyttnr,OUTPUT varomrade).
            
            IF varomrade = "" THEN DO:
               MESSAGE "Import av värdering avbruten! Kontakta Elpool i Umeå AB 090/184540!"
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               musz = TRUE.
               RETURN NO-APPLY.
            END.
            ELSE DO:
               MESSAGE "Ditt nya värderingsnummer är: " + Guru.Konstanter:gomrk + " " + varomrade + " " + STRING(nyttnr)
               VIEW-AS ALERT-BOX TITLE "Meddelande".   
               EMPTY TEMP-TABLE dubfastighet NO-ERROR. 
               ASSIGN
               valimpomrade = varomrade
               valmark = nyttnr.                                    
            END.           
         END.
         ELSE DO:
            MESSAGE "Import av värdering avbruten!"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            musz = TRUE.
            RETURN NO-APPLY.
         END.                                   
      END.
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = INTEGER(tempvardnr) AND  varderingtemp.OMRADE = tempomr 
         NO-ERROR.
      RUN utinxml_UI (INPUT 2).
      EMPTY TEMP-TABLE dubfastighet NO-ERROR. 
      RUN selxml_UI (INPUT "Markvärdering skapas").
      FOR EACH tempvard:            
         DELETE tempvard.         
      END.      
      FOR EACH tempfastextra:
         tempfastextra.VARDNR = nyttnr.      
      END.
      FOR EACH tempfastextra2:
         tempfastextra2.VARDNR = nyttnr.      
      END.
      FOR EACH tempakerkab:
         tempakerkab.VARDNR = nyttnr.      
      END.
      FOR EACH tempakervard:
         tempakervard.VARDNR = nyttnr.      
      END.
      FOR EACH tempskogvard:
         tempskogvard.VARDNR = nyttnr.      
      END.
      FOR EACH tempvolskog:
         tempvolskog.VARDNR = nyttnr.      
      END.
      FOR EACH tempfastvard:
         tempfastvard.VARDNR = nyttnr.      
      END.
      FOR EACH temprevavtal:
         temprevavtal.VARDNR = nyttnr.      
      END.
      
      
      RUN skapaimp_UI IN expimpapph (INPUT TABLE tempvard,     
                         INPUT TABLE tempfastighet,   
                         INPUT TABLE tempfastvard,   
                         INPUT TABLE tempfastextra,
                         INPUT TABLE tempfastextra2,  
                         INPUT TABLE tempmarkagare,    
                         INPUT TABLE tempmarkfast,  
                         INPUT TABLE tempakerkab,  
                         INPUT TABLE tempakervard,
                         INPUT TABLE tempskogvard,   
                         INPUT TABLE tempvolskog,
                         INPUT TABLE temprevavtal,
                         INPUT TABLE tempnatproj,
                         INPUT TABLE tempnatproj2,
                         INPUT TABLE tempnathsand,
                         OUTPUT TABLE dubfastighet).
      FIND FIRST dubfastighet NO-ERROR.
      IF AVAILABLE dubfastighet THEN DO:
         {AMERICANEUROPEAN.I}
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".       
         OUTPUT TO VALUE(felfil).
         PUT "Dessa Fastigheter med markägare fanns redan i databasen. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH dubfastighet WHERE dubfastighet.BETECKNING NE "":           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 dubfastighet.BETECKNING FORMAT "X(30)"  AT 12.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         
         RUN OPENDOC.P (felfil,"","",NO). 
         {EUROPEANAMERICAN.I}        
      END.   
   END.
  
   IF Guru.Konstanter:globforetag = "csweo" THEN DO:
      OS-DELETE VALUE(berfil.INDIR + berfil.FILNAMNS) NO-ERROR.
   END.    
   MESSAGE "Import klar!" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inxml_UI C-Win 
PROCEDURE inxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   
   DEFINE VARIABLE inber AS INTEGER NO-UNDO.
   DEFINE VARIABLE inomrade AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inbenamning AS CHARACTER NO-UNDO.
   DEFINE VARIABLE h1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE h2 AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
   EMPTY TEMP-TABLE berfil NO-ERROR. 
   
   ASSIGN
   listobj = ""
   tempstring  = ""
   tmpfilnamn = "".
   IF fildir = "" THEN RETURN.
   INPUT STREAM dirstrom FROM OS-DIR(fildir) NO-ECHO.
   REPEAT:
      /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
      SET STREAM dirstrom tmpfilnamn tmpdirlist tmpattrlist NO-ERROR.

      IF tmpfilnamn MATCHES "*£.xml" THEN DO:
         
         IF INDEX(tmpfilnamn,"!",1) > 0 THEN DO:
            inber = INTEGER(SUBSTRING(tmpfilnamn,1,INDEX(tmpfilnamn,"!",1) - 1)).            
            IF INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1) > 0 THEN DO: 
               IF listobj = "" THEN listobj = ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
               ELSE listobj = listobj + ";" + ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
               h1 = INDEX(tmpfilnamn,"!",1) + 1.
               h2 = INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1)).
               inomrade = SUBSTRING(tmpfilnamn,h1,h2 - h1).
               inbenamning = SUBSTRING(tmpfilnamn,h2 + 1,INDEX(tmpfilnamn,"£.xml") - (h2 + 1)).
               IF inbenamning NE "" THEN DO:
                  CREATE varderingtemp.
                  ASSIGN
                  varderingtemp.VARDNR = inber
                  varderingtemp.OMRADE = inomrade
                  varderingtemp.BENAMNING = inbenamning.
                  CREATE berfil.
                  ASSIGN
                  berfil.INDIR = fildir
                  berfil.FILNAMNS = tmpfilnamn
                  berfil.ROWBER = ROWID(varderingtemp).
                  tempstring = "Värderingsnr: " + STRING(varderingtemp.VARDNR) + " " + Guru.Konstanter:gomrk + ": " + varderingtemp.OMRADE + 
                  " Benm: " + varderingtemp.BENAMNING.
                  FIND FIRST imptemp WHERE imptemp.INVAR = tempstring NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE imptemp THEN DO:
                     CREATE imptemp.
                     ASSIGN
                     imptemp.INDIR = fildir
                     imptemp.INVAR = tempstring.
                     SEL-IMP:ADD-LAST(tempstring) IN FRAME {&FRAME-NAME}.  
                  END.            
               END.
            END.
         END.
      END.
   END.
   INPUT STREAM dirstrom CLOSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE in_UI C-Win 
PROCEDURE in_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   IF typval = 1 THEN RETURN.
   EMPTY TEMP-TABLE imptemp NO-ERROR. 
   RUN inxml_UI.       
   DISPLAY SEL-IMP WITH FRAME {&FRAME-NAME}. 
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
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
      fildir = "\\SRV00486\APPL\GURU\DOKUMENT\BEREDNING\" + Guru.Konstanter:globanv + "\".
   END.
   
   ELSE DO:
      fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
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
   RUN selfill_UI.
   {UTXKMLKOLL.I}
   RUN VALUE(vxmlpro) PERSISTENT SET dynxml (INPUT FALSE).
   EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   IF vad = 1 THEN DO:
      RUN uttab_UI (INPUT 1).
      fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      filnamn = fildir + STRING(varderingtemp.VARDNR) + "!" + varderingtemp.OMRADE + "!" + varderingtemp.BENAMNING + "£.xml" .
      filnamn = REPLACE(filnamn,",","").
      sparfilnamn = filnamn.
      RUN selxml_UI (INPUT "Markvärderingsfil skapas").
      FOR EACH tempnamn:
         IF sparfilnamn NE "" THEN RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE,INPUT "start",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT sparfilnamn).
         ELSE RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE ,INPUT "", INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT "").
         sparfilnamn = "".
      END.
      RUN xmlutslut_UI IN dynxml.
      RUN xmlslut_UI IN dynxml.
      RUN selxml_UI (INPUT "Kopieringen färdig").               
   END.
   IF vad = 2 THEN DO:
      RUN uttab_UI (INPUT 1).      
      RUN selxml_UI (INPUT "Markvärderingen läses in i Guru").
      FIND FIRST berfil WHERE berfil.ROWBER = ROWID(varderingtemp) NO-LOCK NO-ERROR.
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT berfil.INDIR + berfil.FILNAMNS).
      FIND FIRST varderingtemp NO-LOCK NO-ERROR.
     
         
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   END.
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
   IF vad = 1 THEN DO:
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempvolskog:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempvolskog:HANDLE.
      tempnamn.NODTAB = "tempvolskog".
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempskogvard:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempskogvard:HANDLE.
      tempnamn.NODTAB = "tempskogvard".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempakervard:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempakervard:HANDLE.
      tempnamn.NODTAB = "tempakervard".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempakerkab:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempakerkab:HANDLE.
      tempnamn.NODTAB = "tempakerkab".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmarkfast:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempmarkfast:HANDLE.
      tempnamn.NODTAB = "tempmarkfast".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmarkagare:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempmarkagare:HANDLE.
      tempnamn.NODTAB = "tempmarkagare".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempfastextra:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempfastextra:HANDLE.
      tempnamn.NODTAB = "tempfastextra".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempfastextra2:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempfastextra2:HANDLE.
      tempnamn.NODTAB = "tempfastextra2".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempfastvard:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempfastvard:HANDLE.
      tempnamn.NODTAB = "tempfastvard".           
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempfastighet:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempfastighet:HANDLE.
      tempnamn.NODTAB = "tempfastighet".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempvard:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempvard:HANDLE.
      tempnamn.NODTAB = "tempvard".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER temprevavtal:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE temprevavtal:HANDLE.
      tempnamn.NODTAB = "temprevavtal".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempnatproj:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempnatproj:HANDLE.
      tempnamn.NODTAB = "tempnatproj".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempnatproj2:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempnatproj2:HANDLE.
      tempnamn.NODTAB = "tempnatproj2".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempnathsand:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempnathsand:HANDLE.
      tempnamn.NODTAB = "tempnathsand".
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

