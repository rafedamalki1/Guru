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
{GATILL.I}
/* Local Variable Definitions ---                                       */
{EXTRATAB.I}  
{ALLDEF.I}
{SOKDEF.I}
{FAKTIN.I}
{KALKIN.I}
{BERIN.I}
{MARKVARDIN.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{FAKTTYPDEF.I}
{ANSPROJBER.I}
{AUTOMREGTEMP.I}
{OMRTEMPW.I}
{HOPPSEK2W.I}
{AVTPLANTEMP.I}
{ANVPERS.I}
&Scoped-define SHARED SHARED 
{AVDTEMP.I}
{DIRDEF.I}
{AONRDEF.I}
{BESTKUNDALLT.I}
{JURPERST.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{AVTAONRTEMP.I}
/*{PLANNRTEMP.I} */







DEFINE SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1 NO-UNDO.
DEFINE NEW SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE omrbildvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttsparatao AS LOGICAL NO-UNDO. 
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE tidbgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidagamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidpgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrvalvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoomradenr AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrserierec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE avtrec AS RECID NO-UNDO.
DEFINE VARIABLE sokmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE varfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE Okvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE openqkoll AS LOGICAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE projrapp AS CHARACTER FORMAT "X(101)" NO-UNDO.  
DEFINE VARIABLE omradespar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bestspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE aonrrecsp AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecsp2 AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel2 AS RECID NO-UNDO.
DEFINE VARIABLE stat AS INTEGER NO-UNDO.
DEFINE VARIABLE bortaoapph AS HANDLE NO-UNDO.                     /*BORTAOAPP.P*/

DEFINE NEW SHARED VARIABLE nyttaoapph AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE nyttaoejapph AS HANDLE NO-UNDO.                              /* dokhant.P */
DEFINE VARIABLE editanm AS CHARACTER FORMAT "x(68)" NO-UNDO.
DEFINE VARIABLE flagga AS INTEGER NO-UNDO.
DEFINE VARIABLE aokopprec AS RECID NO-UNDO.
DEFINE VARIABLE aotidrec AS RECID NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bestvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE radfast AS LOGICAL NO-UNDO.
DEFINE VARIABLE autoregvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_BERKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KALKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KOPPKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_TIDLKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE reslog AS LOGICAL NO-UNDO.
DEFINE VARIABLE ordnvar AS INTEGER NO-UNDO.
DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE extraomr AS CHARACTER NO-UNDO EXTENT 2.
DEFINE VARIABLE sparavdat AS DATE NO-UNDO.
DEFINE VARIABLE brwkoll AS INTEGER NO-UNDO EXTENT 10.
DEFINE VARIABLE gfastkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE gtillkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE VARIABLE mainvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE arbbspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbfspar AS DECIMAL NO-UNDO.

DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynfrmh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE tmpquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE openquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE tmpcolhnext AS HANDLE NO-UNDO.
DEFINE VARIABLE fieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE tablefieldh AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE tidsplantemp NO-UNDO 
   FIELD AONR           AS CHARACTER
   FIELD DELNR          AS INTEGER
   FIELD AKTIVITET       AS CHARACTER 
   FIELD TIDLAGE        AS CHARACTER
   FIELD IDTIDLAG        AS CHARACTER
   FIELD TIDSPERIOD    AS CHARACTER
   FIELD START       AS DATE 
   FIELD SLUT        AS DATE 
   FIELD PERSONAL         AS CHARACTER
   FIELD MON            AS CHARACTER
   FIELD TIS            AS CHARACTER
   FIELD ONS            AS CHARACTER
   FIELD TOR            AS CHARACTER
   FIELD FRE            AS CHARACTER
   FIELD LOR            AS CHARACTER
   FIELD SON            AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TIDSPLAN

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidsplantemp

/* Definitions for BROWSE BRW_TIDSPLAN                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDSPLAN tidsplantemp.AONR ~
tidsplantemp.DELNR tidsplantemp.AKTIVITET tidsplantemp.TIDLAGE ~
tidsplantemp.TIDSPERIOD tidsplantemp.START tidsplantemp.SLUT ~
tidsplantemp.PERSONAL tidsplantemp.MON tidsplantemp.TIS tidsplantemp.ONS ~
tidsplantemp.TOR tidsplantemp.FRE tidsplantemp.LOR tidsplantemp.SON 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDSPLAN tidsplantemp.AKTIVITET ~
tidsplantemp.TIDLAGE tidsplantemp.START tidsplantemp.SLUT ~
tidsplantemp.PERSONAL tidsplantemp.MON tidsplantemp.TIS tidsplantemp.ONS ~
tidsplantemp.TOR tidsplantemp.FRE tidsplantemp.LOR tidsplantemp.SON 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TIDSPLAN tidsplantemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TIDSPLAN tidsplantemp
&Scoped-define QUERY-STRING-BRW_TIDSPLAN FOR EACH tidsplantemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TIDSPLAN OPEN QUERY BRW_TIDSPLAN FOR EACH tidsplantemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TIDSPLAN tidsplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDSPLAN tidsplantemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_TIDSPLAN}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 BRW_TIDSPLAN FILL-IN-AKT ~
CMB_VECKOR FILL-IN-TILLF BTN_VECKA 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AKT CMB_VECKOR FILL-IN-TILLF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_VECKA 
     LABEL "Lägg till" 
     SIZE 10.5 BY 1.13.

DEFINE VARIABLE CMB_VECKOR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lägg till veckor" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 9.5 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-AKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tillfälligt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 9  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 93 BY 3.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 30.88 BY 3.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TIDSPLAN FOR 
      tidsplantemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TIDSPLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDSPLAN C-Win _STRUCTURED
  QUERY BRW_TIDSPLAN NO-LOCK DISPLAY
      tidsplantemp.AONR FORMAT "X(6)":U
      tidsplantemp.DELNR FORMAT "999":U
      tidsplantemp.AKTIVITET FORMAT "X(8)":U
      tidsplantemp.TIDLAGE FORMAT "X(8)":U
      tidsplantemp.TIDSPERIOD FORMAT "X(8)":U
      tidsplantemp.START FORMAT "99/99/99":U
      tidsplantemp.SLUT FORMAT "99/99/99":U
      tidsplantemp.PERSONAL FORMAT "X(8)":U WIDTH 12
      tidsplantemp.MON FORMAT "X(2)":U
      tidsplantemp.TIS FORMAT "X(2)":U
      tidsplantemp.ONS FORMAT "X(2)":U
      tidsplantemp.TOR FORMAT "X(2)":U
      tidsplantemp.FRE FORMAT "X(2)":U
      tidsplantemp.LOR FORMAT "X(2)":U
      tidsplantemp.SON FORMAT "X(2)":U
  ENABLE
      tidsplantemp.AKTIVITET
      tidsplantemp.TIDLAGE
      tidsplantemp.START
      tidsplantemp.SLUT
      tidsplantemp.PERSONAL
      tidsplantemp.MON
      tidsplantemp.TIS
      tidsplantemp.ONS
      tidsplantemp.TOR
      tidsplantemp.FRE
      tidsplantemp.LOR
      tidsplantemp.SON
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 124 BY 23.25 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_TIDSPLAN AT ROW 2.5 COL 1.5
     FILL-IN-AKT AT ROW 26.25 COL 34.5 COLON-ALIGNED
     CMB_VECKOR AT ROW 26.25 COL 113 COLON-ALIGNED
     FILL-IN-TILLF AT ROW 27.5 COL 34.5 COLON-ALIGNED
     BTN_VECKA AT ROW 27.5 COL 113.75
     "Projekt" VIEW-AS TEXT
          SIZE 10 BY 1.04 AT ROW 26.08 COL 2.13
     RECT-1 AT ROW 26 COL 1.5
     RECT-2 AT ROW 26 COL 94.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 28.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: anlaggtemp T "?" NO-UNDO temp-db anlaggtemp
      TABLE: ansvaraotemp T "?" NO-UNDO temp-db ansvaraotemp
      TABLE: aonrkonttemp T "?" NO-UNDO temp-db aonrkonttemp
      TABLE: aonrtidperstemp T "?" NO-UNDO temp-db aonrtidperstemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: arbarttemp T "?" NO-UNDO temp-db arbarttemp
      TABLE: avtalaonrtemp T "?" NO-UNDO temp-db avtalaonrtemp
      TABLE: beratemp T "?" NO-UNDO temp-db beratemp
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: bestkundextra T "?" NO-UNDO temp-db bestkundextra
      TABLE: dagboktemp T "?" NO-UNDO temp-db dagboktemp
      TABLE: gatill T "?" NO-UNDO temp-db gatill
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
      TABLE: projtemp T "?" NO-UNDO temp-db projtemp
      TABLE: tidsplantemp T "?" NO-UNDO temp-db tidsplantemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Guru-Projekthuvud"
         HEIGHT             = 28.42
         WIDTH              = 124.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.88
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.88
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
/* BROWSE-TAB BRW_TIDSPLAN RECT-2 DEFAULT-FRAME */
ASSIGN 
       FILL-IN-AKT:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-TILLF:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDSPLAN
/* Query rebuild information for BROWSE BRW_TIDSPLAN
     _TblList          = "Temp-Tables.tidsplantemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tidsplantemp.AONR
     _FldNameList[2]   = Temp-Tables.tidsplantemp.DELNR
     _FldNameList[3]   > Temp-Tables.tidsplantemp.AKTIVITET
"tidsplantemp.AKTIVITET" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tidsplantemp.TIDLAGE
"tidsplantemp.TIDLAGE" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.tidsplantemp.TIDSPERIOD
     _FldNameList[6]   > Temp-Tables.tidsplantemp.START
"tidsplantemp.START" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.tidsplantemp.SLUT
"tidsplantemp.SLUT" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.tidsplantemp.PERSONAL
"tidsplantemp.PERSONAL" ? ? "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.tidsplantemp.MON
"tidsplantemp.MON" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.tidsplantemp.TIS
"tidsplantemp.TIS" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.tidsplantemp.ONS
"tidsplantemp.ONS" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.tidsplantemp.TOR
"tidsplantemp.TOR" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > Temp-Tables.tidsplantemp.FRE
"tidsplantemp.FRE" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > Temp-Tables.tidsplantemp.LOR
"tidsplantemp.LOR" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > Temp-Tables.tidsplantemp.SON
"tidsplantemp.SON" ? "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_TIDSPLAN */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Guru-Projekthuvud */
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
ON WINDOW-CLOSE OF C-Win /* Guru-Projekthuvud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TIDSPLAN
&Scoped-define SELF-NAME BRW_TIDSPLAN
&Scoped-define SELF-NAME tidsplantemp.MON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.MON BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.MON IN BROWSE BRW_TIDSPLAN /* m */
DO:
  ASSIGN tidsplantemp.MON:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tidsplantemp.TIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.TIS BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.TIS IN BROWSE BRW_TIDSPLAN /* ti */
DO:
  ASSIGN tidsplantemp.TIS:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.ONS BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.ONS IN BROWSE BRW_TIDSPLAN /* o */
DO:
  ASSIGN tidsplantemp.ONS:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.TOR BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.TOR IN BROWSE BRW_TIDSPLAN /* to */
DO:
  ASSIGN tidsplantemp.TOR:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.FRE BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.FRE IN BROWSE BRW_TIDSPLAN /* f */
DO:
  ASSIGN tidsplantemp.FRE:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.LOR BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.LOR IN BROWSE BRW_TIDSPLAN /* l */
DO:
  ASSIGN tidsplantemp.LOR:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tidsplantemp.SON BRW_TIDSPLAN _BROWSE-COLUMN C-Win
ON MOUSE-SELECT-CLICK OF tidsplantemp.SON IN BROWSE BRW_TIDSPLAN /* s */
DO:
  ASSIGN tidsplantemp.SON:BGCOLOR IN BROWSE BRW_TIDSPLAN = 12.
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
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DELETE PROCEDURE nyttaoapph NO-ERROR.
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
   FOR EACH valdaao NO-LOCK:
      RUN laddaaotid IN nyttaoapph (INPUT valdaao.AONR,INPUT valdaao.DELNR,OUTPUT TABLE aotidslagtemp).
      FOR EACH aotidslagtemp WHERE aotidslagtemp.AONR = valdaao.AONR AND aotidslagtemp.DELNR = valdaao.DELNR
         AND aotidslagtemp.IDTLAG = "AOUPPLAGT" 
         NO-LOCK:
         CREATE tidsplantemp.
         ASSIGN 
         tidsplantemp.AONR = valdaao.AONR
         tidsplantemp.DELNR = valdaao.DELNR.
         ASSIGN
         tidsplantemp.AKTIVITET = valdaao.ORT           
         tidsplantemp.TIDLAGE =  aotidslagtemp.IDTLAG
         tidsplantemp.TIDSPERIOD =  ""
         tidsplantemp.START     = aotidslagtemp.DAT1
         tidsplantemp.SLUT    = aotidslagtemp.DAT2
         tidsplantemp.PERSONAL   = aotidslagtemp.ANVANDARE1                         
         tidsplantemp.MON = ""
         tidsplantemp.TIS = ""
         tidsplantemp.ONS = ""
         tidsplantemp.TOR = ""    
         tidsplantemp.FRE = ""
         tidsplantemp.LOR = ""
         tidsplantemp.SON = "".
      END.           
   END.
   RUN createbrw_UI.
   RUN enable_UI.
   
   {FRMSIZE.I}  
   {WIN_M_SLUT.I}
   mainvar = FALSE.   
   IF NOT THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.  
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createbrw_UI C-Win 
PROCEDURE createbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rakn AS INTEGER NO-UNDO.
   DEFINE VARIABLE antalveckor AS INTEGER NO-UNDO.
   DEFINE VARIABLE brwh AS HANDLE NO-UNDO.
   DEFINE VARIABLE colbrwh AS HANDLE NO-UNDO.
   ASSIGN
   brwh = BROWSE BRW_TIDSPLAN:HANDLE
   BRW_TIDSPLAN:FONT IN FRAME DEFAULT-FRAME = 4
   rakn = 0
   antalveckor = 4.
   DO WHILE rakn < antalveckor:
      colbrwh = brwh:ADD-LIKE-COLUMN("tidsplantemp.MON").
      ASSIGN
      colbrwh:WIDTH-CHARS = 2
      colbrwh:LABEL = "m" 
      colbrwh:READ-ONLY = FALSE.
      tidsplantemp.MON:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.
      
      colbrwh = brwh:ADD-LIKE-COLUMN("tidsplantemp.TIS").
      ASSIGN
      colbrwh:WIDTH-CHARS = 2
      colbrwh:LABEL = "ti" 
      colbrwh:READ-ONLY = FALSE.
      tidsplantemp.TIS:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.

      
      BRW_TIDSPLAN:ADD-LIKE-COLUMN("tidsplantemp.ONS").
      tidsplantemp.ONS:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.
      BRW_TIDSPLAN:ADD-LIKE-COLUMN("tidsplantemp.TOR").
      tidsplantemp.TOR:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.
      BRW_TIDSPLAN:ADD-LIKE-COLUMN("tidsplantemp.FRE").
      tidsplantemp.FRE:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.
      BRW_TIDSPLAN:ADD-LIKE-COLUMN("tidsplantemp.LOR").
      tidsplantemp.LOR:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.
      BRW_TIDSPLAN:ADD-LIKE-COLUMN("tidsplantemp.SON").
      tidsplantemp.SON:READ-ONLY IN BROWSE  BRW_TIDSPLAN = FALSE.
      
      rakn = rakn + 1.
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
  DISPLAY FILL-IN-AKT CMB_VECKOR FILL-IN-TILLF 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 BRW_TIDSPLAN FILL-IN-AKT CMB_VECKOR FILL-IN-TILLF 
         BTN_VECKA 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

