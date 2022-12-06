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
/*DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.*/
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW NEW
{UPPGHMT.I}
{PERBEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED
{PHMT.I}
{TIDUTTTNEW.I}
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE varec AS RECID NO-UNDO.
DEFINE VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE BUFFER persprisbuff FOR perspristemp.
DEFINE VARIABLE prisq AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES prisbefattningstemp perspristemp tidut

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF prisbefattningstemp.NAMN ~
prisbefattningstemp.PRISA prisbefattningstemp.STARTDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF prisbefattningstemp.NAMN ~
prisbefattningstemp.PRISA prisbefattningstemp.STARTDATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEF prisbefattningstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEF prisbefattningstemp
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH prisbefattningstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH prisbefattningstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEF prisbefattningstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF prisbefattningstemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS perspristemp.PERSONALKOD ~
perspristemp.OMRADE perspristemp.PBEFATTNING perspristemp.VIBEFATTNING ~
perspristemp.PRIS perspristemp.STARTDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS perspristemp.VIBEFATTNING ~
perspristemp.PRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS perspristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS perspristemp
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH perspristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH perspristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS perspristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS perspristemp


/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for BROWSE BRW_VPERS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VPERS perspristemp.PERSONALKOD ~
perspristemp.PERSONNUMMER perspristemp.FORNAMN perspristemp.EFTERNAMN ~
perspristemp.OMRADE perspristemp.PBEFATTNING perspristemp.VIBEFATTNING ~
perspristemp.PRIS perspristemp.OPRIS perspristemp.STARTDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VPERS perspristemp.PRIS ~
perspristemp.OPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VPERS perspristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VPERS perspristemp
&Scoped-define QUERY-STRING-BRW_VPERS FOR EACH perspristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VPERS OPEN QUERY BRW_VPERS FOR EACH perspristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VPERS perspristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VPERS perspristemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL CMB_LIST BRW_VPERS BRW_PERS BRW_BEF ~
FBTN_SPARA BTN_NVE-4 FILL-IN-ARBSTARTDAT BTN_FVE-4 BTN_BORT BTN_LISTA ~
BTN_BORTFLER BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL CMB_LIST FILL-IN-ARBSTARTDAT 

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
     SIZE 6.5 BY 1.13 TOOLTIP "Ändra pris på markerad person eller vald befattning".

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1 TOOLTIP "Borttagning av markerad person eller vald befattning".

DEFINE BUTTON BTN_BORTFLER 
     LABEL "Ta bort befattning" 
     SIZE 12 BY 1 TOOLTIP "Borttagning av vald befattning".

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_LISTA 
     LABEL "Uppdatera prislista" 
     SIZE 12 BY 1 TOOLTIP "Arbetsboken uppdaterar prislistan".

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 5 BY 1.13 TOOLTIP "Ny post för markerd person eller vald befattning".

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SPARA 
     LABEL "Snabbspara" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LIST AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Välj visning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Pågående prislistor","Sista prislistan" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ARBSTARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Arbetsbokens startdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .92 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Priser", 1,
"Visa och kontroll", 2,
"Aktuellt pris ordinarie befattning", 3
     SIZE 68 BY 1.25 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      prisbefattningstemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      perspristemp SCROLLING.

DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.

DEFINE QUERY BRW_VPERS FOR 
      perspristemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF C-Win _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      prisbefattningstemp.NAMN FORMAT "x(256)":U WIDTH 20
      prisbefattningstemp.PRISA COLUMN-LABEL "Pris" FORMAT "->>>>>9":U
      prisbefattningstemp.STARTDATUM COLUMN-LABEL "Startdatum" FORMAT "99/99/99":U
  ENABLE
      prisbefattningstemp.NAMN
      prisbefattningstemp.PRISA
      prisbefattningstemp.STARTDATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 38.5 BY 19.5.

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      perspristemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      perspristemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(8)":U
            WIDTH 6
      perspristemp.PBEFATTNING COLUMN-LABEL "Ord.befattning" FORMAT "X(256)":U
            WIDTH 15.5
      perspristemp.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(256)":U
            WIDTH 14.5
      perspristemp.PRIS COLUMN-LABEL "Pris" FORMAT "->>>>>9":U
      perspristemp.STARTDATUM COLUMN-LABEL "Startdatum" FORMAT "99/99/99":U
            WIDTH 8
  ENABLE
      perspristemp.VIBEFATTNING
      perspristemp.PRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 62 BY 19.5.

DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT C-Win _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(130)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 89.75 BY 25.21.

DEFINE BROWSE BRW_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VPERS C-Win _STRUCTURED
  QUERY BRW_VPERS NO-LOCK DISPLAY
      perspristemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      perspristemp.PERSONNUMMER COLUMN-LABEL "Persnr" FORMAT "999999-9999":U
            WIDTH 14
      perspristemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      perspristemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 15
      perspristemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(8)":U
            WIDTH 6
      perspristemp.PBEFATTNING COLUMN-LABEL "Ord.befattning" FORMAT "X(256)":U
            WIDTH 15.5
      perspristemp.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(256)":U
            WIDTH 14.5
      perspristemp.PRIS COLUMN-LABEL "Pris" FORMAT "->>>>>9":U
      perspristemp.OPRIS COLUMN-LABEL "Öv.pris" FORMAT "->>>>>9":U
      perspristemp.STARTDATUM COLUMN-LABEL "Startdatum" FORMAT "99/99/99":U
            WIDTH 8
  ENABLE
      perspristemp.PRIS
      perspristemp.OPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 103 BY 19.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_VAL AT ROW 1.08 COL 21.63 NO-LABEL
     BRW_UT AT ROW 2.04 COL 1.25
     CMB_LIST AT ROW 3.04 COL 41.5 COLON-ALIGNED WIDGET-ID 2
     BRW_VPERS AT ROW 4.58 COL 1.5
     BRW_PERS AT ROW 4.58 COL 1.5
     BRW_BEF AT ROW 4.58 COL 65.88
     FBTN_SPARA AT ROW 7.75 COL 105.88
     BTN_NVE-4 AT ROW 24.33 COL 91.25
     FILL-IN-ARBSTARTDAT AT ROW 24.63 COL 79.25 COLON-ALIGNED
     BTN_NVE-3 AT ROW 24.67 COL 6
     BTN_FVE-4 AT ROW 25.21 COL 91.25
     BTN_FVE-3 AT ROW 25.54 COL 6
     BTN_BORT AT ROW 26.08 COL 23.88
     BTN_LISTA AT ROW 26.08 COL 71.38
     BTN_BORTFLER AT ROW 26.08 COL 84.88
     FILL-IN-STARTDAT AT ROW 26.67 COL 12 COLON-ALIGNED
     BTN_OK AT ROW 27.79 COL 90.75
     BTN_AVB AT ROW 27.79 COL 105.75
     BTN_NY AT ROW 27.92 COL 3
     BTN_AND AT ROW 28 COL 2.5
     "Arbetsbok" VIEW-AS TEXT
          SIZE 37.5 BY 1 AT ROW 3 COL 61
          FONT 17
     "Gällande prislistor" VIEW-AS TEXT
          SIZE 28.5 BY 1 AT ROW 3 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.63 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: befattningstemp T "?" NO-UNDO temp-db befattningstemp
      TABLE: perspristemp T "?" NO-UNDO temp-db perspristemp
      TABLE: prisbefattningstemp T "?" NO-UNDO temp-db prisbefattningstemp
      TABLE: tidut T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Debiteringspriser"
         HEIGHT             = 28.42
         WIDTH              = 120.63
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 121.25
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 121.25
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
/* BROWSE-TAB BRW_UT RAD_VAL DEFAULT-FRAME */
/* BROWSE-TAB BRW_VPERS CMB_LIST DEFAULT-FRAME */
/* BROWSE-TAB BRW_PERS BRW_VPERS DEFAULT-FRAME */
/* BROWSE-TAB BRW_BEF BRW_PERS DEFAULT-FRAME */
ASSIGN 
       BRW_BEF:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_BEF:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_PERS:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR BROWSE BRW_UT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 4000.

ASSIGN 
       BRW_VPERS:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_VPERS:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_VPERS:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

/* SETTINGS FOR BUTTON BTN_AND IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AND:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_NY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NY:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.prisbefattningstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.prisbefattningstemp.NAMN
"prisbefattningstemp.NAMN" ? "x(256)" "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.prisbefattningstemp.PRISA
"prisbefattningstemp.PRISA" "Pris" "->>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.prisbefattningstemp.STARTDATUM
"prisbefattningstemp.STARTDATUM" "Startdatum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.perspristemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.perspristemp.PERSONALKOD
"perspristemp.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.perspristemp.OMRADE
"perspristemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.perspristemp.PBEFATTNING
"perspristemp.PBEFATTNING" "Ord.befattning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.perspristemp.VIBEFATTNING
"perspristemp.VIBEFATTNING" "Befattning" "X(256)" "character" ? ? ? ? ? ? yes ? no no "14.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.perspristemp.PRIS
"perspristemp.PRIS" "Pris" "->>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.perspristemp.STARTDATUM
"perspristemp.STARTDATUM" "Startdatum" ? "date" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidut.ut
"tidut.ut" ? "X(130)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VPERS
/* Query rebuild information for BROWSE BRW_VPERS
     _TblList          = "Temp-Tables.perspristemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.perspristemp.PERSONALKOD
"perspristemp.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.perspristemp.PERSONNUMMER
"perspristemp.PERSONNUMMER" "Persnr" "999999-9999" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.perspristemp.FORNAMN
"perspristemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.perspristemp.EFTERNAMN
"perspristemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.perspristemp.OMRADE
"perspristemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.perspristemp.PBEFATTNING
"perspristemp.PBEFATTNING" "Ord.befattning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.perspristemp.VIBEFATTNING
"perspristemp.VIBEFATTNING" "Befattning" "X(256)" "character" ? ? ? ? ? ? no ? no no "14.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.perspristemp.PRIS
"perspristemp.PRIS" "Pris" "->>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.perspristemp.OPRIS
"perspristemp.OPRIS" "Öv.pris" "->>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.perspristemp.STARTDATUM
"perspristemp.STARTDATUM" "Startdatum" ? "date" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VPERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Debiteringspriser */
DO:
  /* This event will close the window and terminate the procedure.  */
  status-ok = BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  perspristemp.PRIS = INPUT BROWSE BRW_PERS perspristemp.PRIS.   
  RUN avb_UI.   
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define SELF-NAME BRW_BEF
&Scoped-define SELF-NAME prisbefattningstemp.PRISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prisbefattningstemp.PRISA BRW_BEF _BROWSE-COLUMN C-Win
ON ENTRY OF prisbefattningstemp.PRISA IN BROWSE BRW_BEF /* Pris */
DO:
   IF AVAILABLE prisbefattningstemp THEN DO:
      DISPLAY prisbefattningstemp.PRISA WITH BROWSE BRW_BEF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prisbefattningstemp.PRISA BRW_BEF _BROWSE-COLUMN C-Win
ON LEAVE OF prisbefattningstemp.PRISA IN BROWSE BRW_BEF /* Pris */
DO:
   IF AVAILABLE prisbefattningstemp THEN DO:
      status-ok = BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      prisbefattningstemp.PRISA = INPUT BROWSE BRW_BEF prisbefattningstemp.PRISA.   
      DISPLAY prisbefattningstemp.PRISA WITH BROWSE BRW_BEF.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME prisbefattningstemp.STARTDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prisbefattningstemp.STARTDATUM BRW_BEF _BROWSE-COLUMN C-Win
ON ENTRY OF prisbefattningstemp.STARTDATUM IN BROWSE BRW_BEF /* Startdatum */
DO:
   IF AVAILABLE prisbefattningstemp THEN DO:
      DISPLAY prisbefattningstemp.STARTDATUM WITH BROWSE BRW_BEF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL prisbefattningstemp.STARTDATUM BRW_BEF _BROWSE-COLUMN C-Win
ON LEAVE OF prisbefattningstemp.STARTDATUM IN BROWSE BRW_BEF /* Startdatum */
DO:
   IF AVAILABLE prisbefattningstemp THEN DO:
      status-ok = BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      prisbefattningstemp.STARTDATUM = INPUT BROWSE BRW_BEF prisbefattningstemp.STARTDATUM.   
      DISPLAY prisbefattningstemp.STARTDATUM WITH BROWSE BRW_BEF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&Scoped-define SELF-NAME perspristemp.PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL perspristemp.PRIS BRW_PERS _BROWSE-COLUMN C-Win
ON ENTRY OF perspristemp.PRIS IN BROWSE BRW_PERS /* Pris */
DO:
   IF AVAILABLE perspristemp THEN DO:
      DISPLAY perspristemp.PRIS WITH BROWSE BRW_PERS.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL perspristemp.PRIS BRW_PERS _BROWSE-COLUMN C-Win
ON LEAVE OF perspristemp.PRIS IN BROWSE BRW_PERS /* Pris */
DO:
   IF AVAILABLE perspristemp THEN DO:
      status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      perspristemp.PRIS = INPUT BROWSE BRW_PERS perspristemp.PRIS.   
      DISPLAY perspristemp.PRIS WITH BROWSE BRW_PERS.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VPERS
&Scoped-define SELF-NAME BRW_VPERS
&Scoped-define SELF-NAME perspristemp.PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL perspristemp.PRIS BRW_VPERS _BROWSE-COLUMN C-Win
ON ENTRY OF perspristemp.PRIS IN BROWSE BRW_VPERS /* Pris */
DO:
   IF AVAILABLE perspristemp THEN DO:
      DISPLAY perspristemp.PRIS WITH BROWSE BRW_VPERS.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL perspristemp.PRIS BRW_VPERS _BROWSE-COLUMN C-Win
ON LEAVE OF perspristemp.PRIS IN BROWSE BRW_VPERS /* Pris */
DO:
   IF AVAILABLE perspristemp THEN DO:
      status-ok = BRW_VPERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      perspristemp.PRIS = INPUT BROWSE BRW_VPERS perspristemp.PRIS.   
      DISPLAY perspristemp.PRIS WITH BROWSE BRW_VPERS.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME perspristemp.OPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL perspristemp.OPRIS BRW_VPERS _BROWSE-COLUMN C-Win
ON ENTRY OF perspristemp.OPRIS IN BROWSE BRW_VPERS /* Öv.pris */
DO:
  IF AVAILABLE perspristemp THEN DO:
      DISPLAY perspristemp.OPRIS WITH BROWSE BRW_VPERS.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL perspristemp.OPRIS BRW_VPERS _BROWSE-COLUMN C-Win
ON LEAVE OF perspristemp.OPRIS IN BROWSE BRW_VPERS /* Öv.pris */
DO:
  IF AVAILABLE perspristemp THEN DO:
      status-ok = BRW_VPERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      perspristemp.OPRIS = INPUT BROWSE BRW_VPERS perspristemp.OPRIS.   
      DISPLAY perspristemp.OPRIS WITH BROWSE BRW_VPERS.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   status-ok = BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   FILL-IN-STARTDAT = perspristemp.STARTDATUM.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
   IF NOT AVAILABLE prisbefattningstemp THEN DO:
      MESSAGE "Du måste välja någon befattning till höger!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   MESSAGE "Vill Du ändra poster för samtliga personer med " prisbefattningstemp.NAMN " och startdatum" FILL-IN-STARTDAT " ?"          
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         OPEN QUERY pq FOR EACH perspristemp WHERE 
         perspristemp.STARTDATUM = FILL-IN-STARTDAT AND
         perspristemp.VIBEFATTNING = prisbefattningstemp.NAMN.
         GET FIRST pq.
         DO WHILE AVAILABLE(perspristemp):
            perspristemp.PRIS = prisbefattningstemp.PRISA.             
            GET NEXT pq.
         END.
         RUN openbdynspec_UI IN brwproc[1].
      END.
   END CASE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   RUN avb_UI.   
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON GO OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE perspristemp THEN DO:
      MESSAGE "Du måste välja någon!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   MESSAGE "Vill du ta bort " perspristemp.VIBEFATTNING                                        
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         IF perspristemp.STARTAD = TRUE THEN DO:
            MESSAGE "Du kan inte ta bort något post som är pågående!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         IF perspristemp.VIBEFATTNING = "FRÅNVARO." OR perspristemp.VIBEFATTNING = "RESTID..." THEN DO:
            MESSAGE "Du kan inte ta bort FRÅNVARO. eller RESTID..." VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
            IF perspristemp.VIBEFATTNING = "EJ.KOSTN." THEN DO:
               MESSAGE "Du kan inte ta bort " perspristemp.VIBEFATTNING VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            IF perspristemp.VIBEFATTNING = "FASTPRIS1" THEN DO:
               MESSAGE "Du kan inte ta bort " perspristemp.VIBEFATTNING VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
         END.
         IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
            IF perspristemp.VIBEFATTNING BEGINS "FELAVHJ" THEN DO:
               MESSAGE "Du kan inte ta bort " perspristemp.VIBEFATTNING VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
         END.
         IF perspristemp.PBEFATTNING = perspristemp.VIBEFATTNING THEN DO:
            MESSAGE "Du kan inte ta bort personens huvudpris!" perspristemp.PBEFATTNING VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
         perspristemp.BORT = TRUE.
         persrec = RECID(perspristemp).
         status-ok = BRW_PERS:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
         IF status-ok = TRUE THEN persrec2 = RECID(perspristemp).
         ELSE DO:          
            status-ok = BRW_PERS:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.          
            IF status-ok = TRUE THEN persrec2 = RECID(perspristemp).          
            ELSE persrec2 = persrec.
         END.
         RUN openbdynspec_UI IN brwproc[1].
         IF persrec2 NE persrec THEN DO:
            FIND FIRST perspristemp WHERE RECID(perspristemp) = persrec2 NO-LOCK NO-ERROR.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(perspristemp)).              
            RUN lastselectdyn_UI IN brwproc[1].  
         END.         
      END.
      WHEN FALSE THEN DO:
                   
      END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTFLER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTFLER C-Win
ON CHOOSE OF BTN_BORTFLER IN FRAME DEFAULT-FRAME /* Ta bort befattning */
DO:
   status-ok = BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE prisbefattningstemp THEN DO:
      MESSAGE "Du måste välja någon befattning till höger!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   MESSAGE "Vill du ta bort samtliga poster med " prisbefattningstemp.NAMN " på prislistor som ej är på började ?"    
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         OPEN QUERY pq FOR EACH perspristemp WHERE perspristemp.STARTAD = FALSE AND
         perspristemp.BEFATTNING = prisbefattningstemp.BEFATTNING.         
         GET FIRST pq.
         DO WHILE AVAILABLE(perspristemp):
            
            IF perspristemp.VIBEFATTNING = "FRÅNVARO." OR 
            perspristemp.VIBEFATTNING = "RESTID..." OR 
            perspristemp.PBEFATTNING = perspristemp.VIBEFATTNING THEN DO:
               musz = musz.
            END.
            ELSE DO:
               IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                  IF perspristemp.VIBEFATTNING BEGINS "FELAVHJ" THEN musz = musz.
                  ELSE perspristemp.BORT = TRUE.                
               END.
               ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
                  IF perspristemp.VIBEFATTNING = "EJ.KOSTN." THEN musz = musz.
                  ELSE IF perspristemp.VIBEFATTNING = "FASTPRIS1" THEN musz = musz.
                  ELSE perspristemp.BORT = TRUE.                
               END.
               ELSE perspristemp.BORT = TRUE.
            END.
            GET NEXT pq.
         END.
         RUN openbdynspec_UI IN brwproc[1].
      END.
      WHEN FALSE THEN DO:
      END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 C-Win
ON CHOOSE OF BTN_FVE-3 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1. 
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 C-Win
ON CHOOSE OF BTN_FVE-4 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-ARBSTARTDAT = INPUT FILL-IN-ARBSTARTDAT.   
   FILL-IN-ARBSTARTDAT = FILL-IN-ARBSTARTDAT - 1. 
   DISPLAY FILL-IN-ARBSTARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LISTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LISTA C-Win
ON CHOOSE OF BTN_LISTA IN FRAME DEFAULT-FRAME /* Uppdatera prislista */
DO:

   MESSAGE "Vill skapa en ny/ändra prislista med angivet startdatum ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:        
         APPLY "LEAVE" TO FILL-IN-ARBSTARTDAT.
         MESSAGE "Skall poster skapas för befattningar med timmpris 0 kr ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val12 AS LOGICAL.
         CASE val12:
            WHEN TRUE THEN DO:
               val12 = TRUE.
            END.
            WHEN FALSE THEN DO:
               val12 = FALSE.
            END.
         END CASE.   
         {muswait.i}
         FOR EACH prisbefattningstemp:
            IF prisbefattningstemp.STARTDATUM NE ? THEN DO:
               PRIS:
               FOR EACH valperstemp:                  
                  FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = valperstemp.PERSONALKOD AND
                  perspristemp.VIBEFATTNING = prisbefattningstemp.NAMN AND 
                  perspristemp.STARTDATUM = prisbefattningstemp.STARTDATUM NO-ERROR.                  
                  IF AVAILABLE perspristemp THEN DO:
                     perspristemp.PRIS = prisbefattningstemp.PRISA.
                  END.
                  ELSE DO:
                     IF prisbefattningstemp.NAMN = "FRÅNVARO." OR prisbefattningstemp.NAMN = "RESTID..." THEN DO:              
                        IF val12 = TRUE THEN RUN skapapris_UI (INPUT FALSE).                     
                        ELSE RUN skapapris_UI (INPUT TRUE).                     
                        NEXT PRIS.
                     END.
                     IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                        IF prisbefattningstemp.NAMN BEGINS "FELAVHJ" THEN DO:
                           IF val12 = TRUE THEN RUN skapapris_UI (INPUT FALSE).                     
                           ELSE RUN skapapris_UI (INPUT TRUE).                     
                           NEXT PRIS.
                        END.
                     END.                           
                     IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
                        IF prisbefattningstemp.NAMN = "EJ.KOSTN." THEN DO:
                           IF val12 = TRUE THEN RUN skapapris_UI (INPUT FALSE).                     
                           ELSE RUN skapapris_UI (INPUT TRUE).                     
                           NEXT PRIS.
                        END.
                        IF prisbefattningstemp.NAMN = "FASTPRIS1" THEN DO:
                           IF val12 = TRUE THEN RUN skapapris_UI (INPUT FALSE).                     
                           ELSE RUN skapapris_UI (INPUT TRUE).                     
                           NEXT PRIS.
                        END.
                     END.                           
                     IF val12 = FALSE AND prisbefattningstemp.PRISA = 0 THEN DO:
                        NEXT PRIS.
                     END.
                     ELSE DO:
                        RUN skapapris_UI (INPUT FALSE).                        
                        NEXT PRIS.
                     END.
                  END.                              
               END.              
            END.                       
         END.                 
         REPEAT:
            FIND FIRST perspristemp WHERE perspristemp.PBEFATTNING = "" NO-ERROR.
            IF NOT AVAILABLE perspristemp THEN LEAVE.
            FIND FIRST valperstemp WHERE valperstemp.PERSONALKOD = perspristemp.PERSONALKOD NO-LOCK NO-ERROR.
            FIND FIRST prisbefattningstemp WHERE prisbefattningstemp.BEFATTNING = valperstemp.BEFATTNING NO-LOCK NO-ERROR.
            IF AVAILABLE prisbefattningstemp THEN DO:
               FOR EACH perspristemp WHERE perspristemp.PERSONALKOD = valperstemp.PERSONALKOD AND perspristemp.PBEFATTNING = "":
                   perspristemp.PBEFATTNING = prisbefattningstemp.NAMN.
               END.
            END.
            ELSE DO:
               FOR EACH perspristemp WHERE perspristemp.PERSONALKOD = valperstemp.PERSONALKOD AND perspristemp.PBEFATTNING = "":
                   perspristemp.PBEFATTNING = "Saknas " + valperstemp.BEFATTNING.
               END.
            END.                          
         END.
         
         /*
         OPEN QUERY bpq FOR EACH prisbefattningstemp:        
         EACH perspristemp WHERE perspristemp.STARTAD = TRUE AND
         perspristemp.VIBEFATTNING = prisbefattningstemp.NAMN.
         GET FIRST bpq.
         DO WHILE AVAILABLE(perspristemp):
            IF prisbefattningstemp.STARTDATUM NE ? THEN DO:
               FIND FIRST persprisbuff WHERE 
               persprisbuff.PERSONALKOD = perspristemp.PERSONALKOD AND
               persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN AND 
               persprisbuff.STARTDATUM = prisbefattningstemp.STARTDATUM NO-ERROR.
               IF NOT AVAILABLE persprisbuff THEN DO:                
                  IF prisbefattningstemp.NAMN = "FRÅNVARO." OR prisbefattningstemp.NAMN = "RESTID..." THEN DO:              
                     CREATE persprisbuff.
                     BUFFER-COPY perspristemp TO persprisbuff. 
                     ASSIGN
                     persprisbuff.PRIS = prisbefattningstemp.PRISA
                     persprisbuff.BEFATTNING = prisbefattningstemp.BEFATTNING
                     persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN
                     persprisbuff.STARTDATUM = prisbefattningstemp.STARTDATUM
                     persprisbuff.STARTAD = FALSE.
                  END.
                  ELSE DO:
                     IF val12 = FALSE AND prisbefattningstemp.PRISA = 0 THEN DO:
                        IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
                           IF prisbefattningstemp.NAMN BEGINS "FELAVHJ" THEN DO:
                              CREATE persprisbuff.
                              BUFFER-COPY perspristemp TO persprisbuff. 
                              ASSIGN
                              persprisbuff.PRIS = prisbefattningstemp.PRISA
                              persprisbuff.BEFATTNING = prisbefattningstemp.BEFATTNING
                              persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN
                              persprisbuff.STARTDATUM = prisbefattningstemp.STARTDATUM
                              persprisbuff.STARTAD = FALSE.
                           END.
                        END.
                        musz = musz.
                     END.
                     ELSE DO:
                        CREATE persprisbuff.
                        BUFFER-COPY perspristemp TO persprisbuff. 
                        ASSIGN
                        persprisbuff.PRIS = prisbefattningstemp.PRISA
                        persprisbuff.BEFATTNING = prisbefattningstemp.BEFATTNING
                        persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN
                        persprisbuff.STARTDATUM = prisbefattningstemp.STARTDATUM
                        persprisbuff.STARTAD = FALSE.
                     END.
                  END.
               END.
               ELSE DO:
                  IF val12 = FALSE AND prisbefattningstemp.PRISA = 0 THEN musz = musz.
                  ELSE persprisbuff.PRIS = prisbefattningstemp.PRISA.
               END.
            END.
            
            GET NEXT bpq.
         END.
         */
         FOR EACH prisbefattningstemp:
            prisbefattningstemp.STARTDATUM = ?.
         END.
         RUN openbdynspec_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[2].
      END.
   END CASE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 C-Win
ON CHOOSE OF BTN_NVE-3 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 C-Win
ON CHOOSE OF BTN_NVE-4 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-ARBSTARTDAT = INPUT FILL-IN-ARBSTARTDAT.   
   FILL-IN-ARBSTARTDAT = FILL-IN-ARBSTARTDAT + 1.        
   DISPLAY FILL-IN-ARBSTARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   status-ok = BRW_BEF:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE prisbefattningstemp THEN DO:
      MESSAGE "Du måste välja någon befattning till höger!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   MESSAGE "Vill du lägga upp nya poster för samtliga personer ?"          
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         FIND FIRST persprisbuff WHERE 
         persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN AND 
         persprisbuff.STARTDATUM = FILL-IN-STARTDAT NO-ERROR.
         IF AVAILABLE persprisbuff THEN DO:
            MESSAGE "Redan upplaggd." VIEW-AS ALERT-BOX.
            RETURN NO-APPLY. 
         END.
         OPEN QUERY vpq FOR EACH valperstemp. 
         GET FIRST vpq.
         DO WHILE AVAILABLE(valperstemp):
            FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = valperstemp.PERSONALKOD AND
            perspristemp.STARTAD = TRUE AND 
            perspristemp.VIBEFATTNING = prisbefattningstemp.NAMN NO-LOCK NO-ERROR.
            IF NOT AVAILABLE perspristemp THEN DO:
               FIND FIRST perspristemp WHERE 
               perspristemp.PERSONALKOD = valperstemp.PERSONALKOD AND
               perspristemp.STARTAD = TRUE  
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE perspristemp THEN DO:
                  FIND FIRST perspristemp WHERE 
                  perspristemp.PERSONALKOD = valperstemp.PERSONALKOD                   
                  NO-LOCK NO-ERROR.                   
               END.
            END.
            CREATE persprisbuff.
            BUFFER-COPY perspristemp TO persprisbuff. 
            ASSIGN
            persprisbuff.BEFATTNING = prisbefattningstemp.BEFATTNING
            persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN
            persprisbuff.PRIS = prisbefattningstemp.PRISA
            persprisbuff.STARTDATUM = FILL-IN-STARTDAT
            persprisbuff.STARTAD = FALSE.
            GET NEXT vpq.
         END.
         RUN openbdynspec_UI IN brwproc[1].
      END.
      WHEN FALSE THEN DO:
         FIND FIRST persprisbuff WHERE persprisbuff.PERSONALKOD = perspristemp.PERSONALKOD AND
         persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN AND 
         persprisbuff.STARTDATUM = FILL-IN-STARTDAT NO-ERROR.
         IF AVAILABLE persprisbuff THEN DO:
            MESSAGE "Redan upplaggd." VIEW-AS ALERT-BOX.
            RETURN NO-APPLY. 
         END.
         CREATE persprisbuff.
         BUFFER-COPY perspristemp TO persprisbuff. 
         ASSIGN
         persprisbuff.PRIS = prisbefattningstemp.PRISA
         persprisbuff.BEFATTNING = prisbefattningstemp.BEFATTNING
         persprisbuff.VIBEFATTNING = prisbefattningstemp.NAMN
         persprisbuff.STARTDATUM = FILL-IN-STARTDAT
         persprisbuff.STARTAD = FALSE.
         RUN openbdynspec_UI IN brwproc[1].
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(perspristemp)).              
         RUN lastselectdyn_UI IN brwproc[1].  
      END.
   END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   RUN spara_UI (INPUT 2).
   APPLY "GO" TO BTN_OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON GO OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LIST C-Win
ON VALUE-CHANGED OF CMB_LIST IN FRAME DEFAULT-FRAME /* Välj visning */
DO:
   CMB_LIST = input CMB_LIST.
   APPLY "VALUE-CHANGED" TO RAD_VAL.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SPARA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SPARA C-Win
ON CHOOSE OF FBTN_SPARA IN FRAME DEFAULT-FRAME /* Snabbspara */
DO:
   RUN spara_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ARBSTARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARBSTARTDAT C-Win
ON LEAVE OF FILL-IN-ARBSTARTDAT IN FRAME DEFAULT-FRAME /* Arbetsbokens startdatum */
DO:
   FILL-IN-ARBSTARTDAT = INPUT FILL-IN-ARBSTARTDAT.
   MESSAGE "Vill att alla poster i arbetsboken skall ha detta startdatum ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         GET FIRST BRW_BEF.
         DO WHILE AVAILABLE(prisbefattningstemp):
            prisbefattningstemp.STARTDATUM = FILL-IN-ARBSTARTDAT.
            GET NEXT BRW_BEF.
         END.
         RUN openbdynspec_UI IN brwproc[2].
      END.
   END CASE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARBSTARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-ARBSTARTDAT IN FRAME DEFAULT-FRAME /* Arbetsbokens startdatum */
DO:
   ASSIGN
   FILL-IN-ARBSTARTDAT = INPUT FILL-IN-ARBSTARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-ARBSTARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-ARBSTARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-ARBSTARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Start datum */
DO:
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Start datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   
   RAD_VAL = INPUT RAD_VAL.
   {muswait.i}   
   ASSIGN
   FILL-IN-ARBSTARTDAT:HIDDEN = TRUE 
   BRW_BEF:HIDDEN = TRUE 
   BRW_PERS:HIDDEN = TRUE 
   BTN_BORT:HIDDEN = TRUE 
   BTN_BORTFLER:HIDDEN = TRUE 
   BTN_FVE-4:HIDDEN = TRUE 
   BTN_LISTA:HIDDEN = TRUE
   BTN_NVE-4:HIDDEN = TRUE 
   BTN_OK:HIDDEN = TRUE 
   FBTN_SPARA:HIDDEN = TRUE
   CMB_LIST:HIDDEN = TRUE
   BRW_UT:HIDDEN = TRUE
   BRW_VPERS:HIDDEN = TRUE.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN
      FILL-IN-ARBSTARTDAT:HIDDEN = FALSE 
      BRW_BEF:HIDDEN = FALSE 
      BRW_PERS:HIDDEN = FALSE 
      BTN_BORT:HIDDEN = FALSE 
      BTN_BORTFLER:HIDDEN = FALSE 
      BTN_FVE-4:HIDDEN = FALSE 
      BTN_LISTA:HIDDEN = FALSE
      BTN_NVE-4:HIDDEN = FALSE 
      BTN_OK:HIDDEN = FALSE 
      
      FBTN_SPARA:HIDDEN = FALSE.

   END.
   ELSE IF RAD_VAL = 2 THEN  DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PERPRISKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE valperstemp,OUTPUT TABLE tidut).   
      END.
      ELSE DO:
         RUN PERPRISKOLL.P 
         (INPUT TABLE valperstemp,OUTPUT TABLE tidut).   
      END.
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}. 
      BRW_UT:HIDDEN = FALSE.
      OPEN QUERY BRW_UT FOR EACH tidut.
   END.
   ELSE DO:
      CMB_LIST:HIDDEN = FALSE.    
      BRW_VPERS:HIDDEN = FALSE.
      BTN_OK:HIDDEN = FALSE.  
         FBTN_SPARA:HIDDEN = FALSE.
         
       IF CMB_LIST = "Sista prislistan" THEN DO:
         BTN_OK:HIDDEN = FALSE.  
         FBTN_SPARA:HIDDEN = FALSE.
         FIND FIRST perspristemp  WHERE USE-INDEX STARTDATUM NO-LOCK NO-ERROR.
         prisq = "perspristemp.PBEFATTNING = perspristemp.VIBEFATTNING " + " AND perspristemp.STARTDATUM = " + STRING( perspristemp.STARTDATUM).
         
         RUN setcolsortvar_UI IN brwproc[4] (INPUT prisq).        
      END.
      ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "perspristemp.PBEFATTNING = perspristemp.VIBEFATTNING").
      RUN openbdynspec_UI IN brwproc[4].
      /*OPEN QUERY BRW_VPERS FOR EACH perspristemp WHERE perspristemp.PBEFATTNING = perspristemp.VIBEFATTNING.*/   
   END.   
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
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
   ASSIGN
   RAD_VAL = 1
   FILL-IN-ARBSTARTDAT = TODAY
   FILL-IN-STARTDAT = TODAY.
   CMB_LIST = "Pågående prislistor".
   IF  Guru.Konstanter:globforetag = "cGKAL"  THEN perspristemp.OPRIS:VISIBLE IN BROWSE BRW_VPERS = TRUE.
   ELSE perspristemp.OPRIS:VISIBLE IN BROWSE BRW_VPERS = FALSE.
   RUN enable_UI.
   {FRMSIZE.I}
   RUN spara_UI (INPUT 1).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "perspristemp.BORT = FALSE").
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   {musarrow.i}   
   
   APPLY "VALUE-CHANGED" TO RAD_VAL. 
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
   
   ASSIGN
   perspristemp.VIBEFATTNING:READ-ONLY IN BROWSE BRW_PERS = TRUE    
   prisbefattningstemp.NAMN:READ-ONLY IN BROWSE BRW_BEF = TRUE.
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_BEF:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_VPERS:HANDLE IN FRAME {&FRAME-NAME}). 

   /*RUN setcolindex_UI IN brwproc[4] (INPUT "STARTDATUM").*/     
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN addmenuitem2_UI IN brwproc[2] (INPUT BRW_BEF:HANDLE, INPUT "Sätt startdatum", INPUT "stalman_UI", INPUT THIS-PROCEDURE).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI C-Win 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "Vill spara innan du avslutar ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         RUN spara_UI (INPUT 2).
      END.
   END CASE.   
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
  DISPLAY RAD_VAL CMB_LIST FILL-IN-ARBSTARTDAT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_VAL CMB_LIST BRW_VPERS BRW_PERS BRW_BEF FBTN_SPARA BTN_NVE-4 
         FILL-IN-ARBSTARTDAT BTN_FVE-4 BTN_BORT BTN_LISTA BTN_BORTFLER BTN_OK 
         BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapapris_UI C-Win 
PROCEDURE skapapris_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ejnoll AS LOGICAL NO-UNDO.
   FIND FIRST persprisbuff WHERE persprisbuff.PERSONALKOD = valperstemp.PERSONALKOD NO-LOCK NO-ERROR.
   RELEASE perspristemp NO-ERROR.
   IF Guru.Konstanter:globforetag = "SUND" THEN DO:
      IF valperstemp.BEFATTNING = prisbefattningstemp.BEFATTNING THEN CREATE perspristemp.
      ELSE IF prisbefattningstemp.NAMN = "FRÅNVARO." OR prisbefattningstemp.NAMN = "RESTID..." OR 
      prisbefattningstemp.NAMN = "EJ.KOSTN." OR prisbefattningstemp.NAMN = "FASTPRIS1" THEN CREATE perspristemp.  
   END.
   ELSE CREATE perspristemp.
   IF AVAILABLE perspristemp THEN DO:
      IF AVAILABLE persprisbuff  THEN DO:
         ASSIGN
         perspristemp.EFTERNAMN = persprisbuff.EFTERNAMN
         perspristemp.FORNAMN = persprisbuff.FORNAMN
         perspristemp.PERSONNUMMER =  persprisbuff.PERSONNUMMER.
      END.   
      ASSIGN
      perspristemp.PERSONALKOD = valperstemp.PERSONALKOD
      perspristemp.OMRADE = valperstemp.OMRADE
      perspristemp.PBEFATTNING = ""
      perspristemp.PRIS = prisbefattningstemp.PRISA
      perspristemp.OPRIS = prisbefattningstemp.PRISA
      perspristemp.BEFATTNING = prisbefattningstemp.BEFATTNING
      perspristemp.VIBEFATTNING = prisbefattningstemp.NAMN
      perspristemp.STARTDATUM = prisbefattningstemp.STARTDATUM.                    
      IF perspristemp.PRIS = 0 AND ejnoll = TRUE THEN DO:
         FIND FIRST persprisbuff WHERE persprisbuff.KLAR = FALSE AND 
         persprisbuff.STARTAD = TRUE AND 
         persprisbuff.PERSONALKOD = perspristemp.PERSONALKOD AND
         persprisbuff.BEFATTNING = perspristemp.BEFATTNING AND 
         persprisbuff.STARTDATUM < perspristemp.STARTDATUM
         NO-ERROR.        
         IF AVAILABLE persprisbuff THEN DO:
            perspristemp.PRIS = persprisbuff.PRIS.
         END.     
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI C-Win 
PROCEDURE spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}   
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   IF vadgora = 1 THEN DO:   
      /*HÄMTAR PRISLISTOR*/
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PERBEFPRIS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT TABLE valperstemp,OUTPUT TABLE perspristemp,OUTPUT TABLE prisbefattningstemp).   
      END.
      ELSE DO:
         RUN PERBEFPRIS.P 
         (INPUT vadgora,INPUT TABLE valperstemp,OUTPUT TABLE perspristemp,OUTPUT TABLE prisbefattningstemp).   
      END. 
   END.
   IF vadgora = 2 THEN DO:
      /*SPARA*/
      IF AVAILABLE perspristemp THEN varec = RECID(perspristemp).
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PERBEFAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vadgora,INPUT TABLE perspristemp).
      END.
      ELSE DO:
         RUN PERBEFAPP.P                            
         (INPUT vadgora,INPUT TABLE perspristemp).
      END. 
      FOR EACH perspristemp WHERE perspristemp.BORT = TRUE:
         DELETE perspristemp.
      END.
      FIND perspristemp WHERE RECID(perspristemp) = varec NO-ERROR.
   END.
   {musarrow.i}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stalman_UI C-Win 
PROCEDURE stalman_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   prisbefattningstemp.STARTDATUM = INPUT BROWSE BRW_BEF prisbefattningstemp.STARTDATUM.   
   IF prisbefattningstemp.STARTDATUM = ? THEN prisbefattningstemp.STARTDATUM = TODAY.
   Guru.GlobalaVariabler:regdatum = prisbefattningstemp.STARTDATUM.
   RUN AlmanBtn.w.
   prisbefattningstemp.STARTDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY prisbefattningstemp.STARTDATUM WITH BROWSE BRW_BEF.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

