&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/15/96 -  1:50 pm

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
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER hjenr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vdelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER umeter AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER dtitt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER onummer AS CHARACTER NO-UNDO.


/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE VARIABLE leverant AS CHARACTER NO-UNDO.
/*DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.*/
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE VARIABLE lager_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE lplats AS INTEGER NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE rapptyp AS INTEGER NO-UNDO.
DEFINE VARIABLE depaapph AS HANDLE NO-UNDO.
DEFINE VARIABLE leveransaoapph AS HANDLE NO-UNDO.
DEFINE VARIABLE dnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjstrang  AS CHARACTER NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE kopplapplats AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjtrnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE trumnr AS INTEGER NO-UNDO.
DEFINE VARIABLE trummauttagen AS LOGICAL NO-UNDO.


{DEFSOK.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LAGERTRUM.I}
{AOBESTMTRLDEP.I}

DEFINE BUFFER lagerbuff FOR lagertrum.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_AOLAGP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aotrum lagertrum

/* Definitions for BROWSE BRW_AOLAGP                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_AOLAGP aotrum.TRUMMAPLATS aotrum.AONR ~
aotrum.DELNR aotrum.stmeter aotrum.datumut aotrum.enr aotrum.benamning ~
aotrum.BESTALLARE aotrum.LAGERPLATSI aotrum.LAGERPLATSC aotrum.PRIS ~
aotrum.TRUMMANR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AOLAGP 
&Scoped-define QUERY-STRING-BRW_AOLAGP FOR EACH aotrum NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AOLAGP OPEN QUERY BRW_AOLAGP FOR EACH aotrum NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AOLAGP aotrum
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AOLAGP aotrum


/* Definitions for BROWSE BRW_LAGERPLATS                                */
&Scoped-define FIELDS-IN-QUERY-BRW_LAGERPLATS lagertrum.TRUMMAPLATS ~
lagertrum.enr lagertrum.benamning lagertrum.lagmeter lagertrum.aokopp ~
lagertrum.AONR lagertrum.DELNR lagertrum.datum lagertrum.LAGERPLATSI ~
lagertrum.LAGERPLATSC lagertrum.pris lagertrum.stmeter lagertrum.utmeter ~
lagertrum.ANVANDARE lagertrum.TRUMMANR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LAGERPLATS lagertrum.TRUMMAPLATS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LAGERPLATS lagertrum
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LAGERPLATS lagertrum
&Scoped-define QUERY-STRING-BRW_LAGERPLATS FOR EACH lagertrum NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LAGERPLATS OPEN QUERY BRW_LAGERPLATS FOR EACH lagertrum NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LAGERPLATS lagertrum
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LAGERPLATS lagertrum


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-6 RECT_SOK1 RECT_SOK2 IMAGE-16 ~
TOG_VUTT TOG_EJNOLL RAD_VALKOP RAD_AOTVAL BRW_LAGERPLATS BRW_AOLAGP ~
btn_over btn_back BTN_NY BTN_BORT FBTN_VISA FILL-IN-ATRUMMAPLATS ~
FILL-IN-TRUMMAPLATS FILL-IN-AENR FILL-IN-ENR FILL-IN-AAONR FILL-IN-AONR ~
FBTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-UTMETER TOG_VUTT TOG_EJNOLL ~
RAD_VALKOP RAD_AOTVAL FILL-IN-ATRUMMAPLATS FILL-IN-TRUMMAPLATS FILL-IN-AENR ~
FILL-IN-ENR FILL-IN-AAONR FILL-IN-AONR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AAONR AS CHARACTER FORMAT "X(40)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-AENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(40)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ATRUMMANR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ATRUMMAPLATS AS CHARACTER FORMAT "X(11)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRUMMANR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRUMMAPLATS AS CHARACTER FORMAT "X(11)":U 
     LABEL "Trumma namn" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTMETER AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 87 BY 1.25
     FONT 17 NO-UNDO.

DEFINE IMAGE IMAGE-16
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_AOTVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bokade trummor på valt aonr", 1,
"Alla bokade trummor", 2
     SIZE 51.13 BY .75 NO-UNDO.

DEFINE VARIABLE RAD_VALKOP AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ej bokade trummor", 1,
"Alla trummor", 2
     SIZE 53.5 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT_SOK1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 3.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT_SOK2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.88 BY 3.25
     BGCOLOR 8 .

DEFINE VARIABLE TOG_EJNOLL AS LOGICAL INITIAL no 
     LABEL "Visa ej tomma trummor" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.63 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_VUTT AS LOGICAL INITIAL no 
     LABEL "Visa uttag" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AOLAGP FOR 
      aotrum SCROLLING.

DEFINE QUERY BRW_LAGERPLATS FOR 
      lagertrum SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AOLAGP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AOLAGP WINDOW-1 _STRUCTURED
  QUERY BRW_AOLAGP NO-LOCK DISPLAY
      aotrum.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "X(6)":U
      aotrum.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      aotrum.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      aotrum.stmeter COLUMN-LABEL "Lager" FORMAT ">>>>9":U
      aotrum.datumut COLUMN-LABEL "Bokad" FORMAT "99/99/99":U
      aotrum.enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      aotrum.benamning COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 14
      aotrum.BESTALLARE COLUMN-LABEL "Bokad!av" FORMAT "x(256)":U
            WIDTH 10
      aotrum.LAGERPLATSI COLUMN-LABEL "P-plats" FORMAT "->>>>>>9":U
      aotrum.LAGERPLATSC COLUMN-LABEL "P-plats!namn" FORMAT "x(256)":U
            WIDTH 10
      aotrum.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9.99":U
      aotrum.TRUMMANR COLUMN-LABEL "Id" FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 58.88 BY 16.5
         TITLE "Uttagna trummor på aonr".

DEFINE BROWSE BRW_LAGERPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LAGERPLATS WINDOW-1 _STRUCTURED
  QUERY BRW_LAGERPLATS NO-LOCK DISPLAY
      lagertrum.TRUMMAPLATS COLUMN-LABEL "Trumma!namn" FORMAT "X(6)":U
      lagertrum.enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      lagertrum.benamning COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 20
      lagertrum.lagmeter COLUMN-LABEL "Lager" FORMAT ">>>>9":U
      lagertrum.aokopp COLUMN-LABEL "pnr" FORMAT "Ja/Nej":U
      lagertrum.AONR COLUMN-LABEL "aonr" FORMAT "X(6)":U
      lagertrum.DELNR COLUMN-LABEL "del!nr" FORMAT "999":U
      lagertrum.datum FORMAT "99/99/99":U
      lagertrum.LAGERPLATSI COLUMN-LABEL "P-plats" FORMAT "->>>>>>9":U
      lagertrum.LAGERPLATSC COLUMN-LABEL "P-plats!namn" FORMAT "x(256)":U
            WIDTH 10
      lagertrum.pris COLUMN-LABEL "Pris" FORMAT ">>>>>9.99":U
      lagertrum.stmeter COLUMN-LABEL "Start!lager" FORMAT ">>>>9":U
      lagertrum.utmeter COLUMN-LABEL "Utgått" FORMAT "->>>>9":U
      lagertrum.ANVANDARE COLUMN-LABEL "Upplagd av/!Uttag av" FORMAT "x(256)":U
            WIDTH 10
      lagertrum.TRUMMANR COLUMN-LABEL "Id" FORMAT ">>>>>9":U
  ENABLE
      lagertrum.TRUMMAPLATS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 60 BY 16.5
         TITLE "Trummor".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-UTMETER AT ROW 1 COL 13.5 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     TOG_VUTT AT ROW 1.63 COL 2.13
     TOG_EJNOLL AT ROW 1.63 COL 18.88 WIDGET-ID 8
     RAD_VALKOP AT ROW 2.46 COL 2.13 NO-LABEL
     RAD_AOTVAL AT ROW 2.46 COL 66.13 NO-LABEL
     BRW_LAGERPLATS AT ROW 3.42 COL 1.5
     BRW_AOLAGP AT ROW 3.42 COL 66.13
     btn_over AT ROW 5.5 COL 61.75
     btn_back AT ROW 9 COL 61.75
     BTN_NY AT ROW 20.17 COL 11
     BTN_BORT AT ROW 20.17 COL 24.75
     FBTN_VISA AT ROW 20.17 COL 66
     FILL-IN-ATRUMMAPLATS AT ROW 21.63 COL 86.5 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-ATRUMMANR AT ROW 21.63 COL 86.63 COLON-ALIGNED
     FILL-IN-TRUMMANR AT ROW 21.67 COL 22.88 COLON-ALIGNED
     FILL-IN-TRUMMAPLATS AT ROW 21.67 COL 22.88 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-AENR AT ROW 22.63 COL 86.63 COLON-ALIGNED
     FILL-IN-ENR AT ROW 22.67 COL 22.88 COLON-ALIGNED
     FILL-IN-AAONR AT ROW 23.63 COL 86.63 COLON-ALIGNED
     FILL-IN-AONR AT ROW 23.67 COL 22.88 COLON-ALIGNED
     FBTN_AVB AT ROW 23.75 COL 111.38
     IMAGE-6 AT ROW 21.75 COL 2
     RECT_SOK1 AT ROW 21.5 COL 1.5
     RECT_SOK2 AT ROW 21.5 COL 66.13
     IMAGE-16 AT ROW 21.75 COL 66.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 24.13.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: aotrum T "?" NO-UNDO temp-db aotrum
      TABLE: lagertrum T "?" NO-UNDO temp-db lagertrum
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Trum meny - Bokade trummor på aonr"
         HEIGHT             = 24.17
         WIDTH              = 125.25
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 125.25
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 125.25
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_LAGERPLATS RAD_AOTVAL FRAME-A */
/* BROWSE-TAB BRW_AOLAGP BRW_LAGERPLATS FRAME-A */
ASSIGN 
       BRW_AOLAGP:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_AOLAGP:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_AOLAGP:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_LAGERPLATS:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_LAGERPLATS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ATRUMMANR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-ATRUMMANR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TRUMMANR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TRUMMANR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UTMETER IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       RECT_SOK1:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       RECT_SOK2:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AOLAGP
/* Query rebuild information for BROWSE BRW_AOLAGP
     _TblList          = "Temp-Tables.aotrum"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aotrum.TRUMMAPLATS
"aotrum.TRUMMAPLATS" "Trumma!namn" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aotrum.AONR
"aotrum.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aotrum.DELNR
"aotrum.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aotrum.stmeter
"aotrum.stmeter" "Lager" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aotrum.datumut
"aotrum.datumut" "Bokad" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.aotrum.enr
"aotrum.enr" "Enr" "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.aotrum.benamning
"aotrum.benamning" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.aotrum.BESTALLARE
"aotrum.BESTALLARE" "Bokad!av" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.aotrum.LAGERPLATSI
"aotrum.LAGERPLATSI" "P-plats" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.aotrum.LAGERPLATSC
"aotrum.LAGERPLATSC" "P-plats!namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.aotrum.PRIS
"aotrum.PRIS" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.aotrum.TRUMMANR
"aotrum.TRUMMANR" "Id" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AOLAGP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LAGERPLATS
/* Query rebuild information for BROWSE BRW_LAGERPLATS
     _TblList          = "Temp-Tables.lagertrum"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.lagertrum.TRUMMAPLATS
"TRUMMAPLATS" "Trumma!namn" "X(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.lagertrum.enr
"enr" "Enr" "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.lagertrum.benamning
"benamning" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.lagertrum.lagmeter
"lagmeter" "Lager" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.lagertrum.aokopp
"aokopp" "pnr" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.lagertrum.AONR
"AONR" "aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.lagertrum.DELNR
"DELNR" "del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = Temp-Tables.lagertrum.datum
     _FldNameList[9]   > Temp-Tables.lagertrum.LAGERPLATSI
"LAGERPLATSI" "P-plats" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.lagertrum.LAGERPLATSC
"LAGERPLATSC" "P-plats!namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.lagertrum.pris
"pris" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.lagertrum.stmeter
"stmeter" "Start!lager" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.lagertrum.utmeter
"utmeter" "Utgått" "->>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.lagertrum.ANVANDARE
"ANVANDARE" "Upplagd av/!Uttag av" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.lagertrum.TRUMMANR
"TRUMMANR" "Id" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LAGERPLATS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_AOLAGP
&Scoped-define SELF-NAME BRW_AOLAGP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AOLAGP WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_AOLAGP IN FRAME FRAME-A /* Uttagna trummor på aonr */
DO:
   RUN infoES_UI (INPUT 1).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LAGERPLATS
&Scoped-define SELF-NAME BRW_LAGERPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LAGERPLATS WINDOW-1
ON ENTRY OF BRW_LAGERPLATS IN FRAME FRAME-A /* Trummor */
DO:
   
   /*
   APPLY "VALUE-CHANGED" TO BRW_VLEV.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LAGERPLATS WINDOW-1
ON HOME OF BRW_LAGERPLATS IN FRAME FRAME-A /* Trummor */
DO:
   status-ok = BRW_LAGERPLATS:SELECT-FOCUSED-ROW() NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LAGERPLATS WINDOW-1
ON LEAVE OF BRW_LAGERPLATS IN FRAME FRAME-A /* Trummor */
DO:
  /*IF  THEN*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LAGERPLATS WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_LAGERPLATS IN FRAME FRAME-A /* Trummor */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LAGERPLATS WINDOW-1
ON ROW-LEAVE OF BRW_LAGERPLATS IN FRAME FRAME-A /* Trummor */
DO:
  IF AVAILABLE lagertrum THEN DO:
     RUN visa_UI.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LAGERPLATS WINDOW-1
ON VALUE-CHANGED OF BRW_LAGERPLATS IN FRAME FRAME-A /* Trummor */
DO:   
  status-ok = BRW_LAGERPLATS:SELECT-FOCUSED-ROW() NO-ERROR.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lagertrum.TRUMMAPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lagertrum.TRUMMAPLATS BRW_LAGERPLATS _BROWSE-COLUMN WINDOW-1
ON LEAVE OF lagertrum.TRUMMAPLATS IN BROWSE BRW_LAGERPLATS /* Trumma!namn */
DO:
  IF AVAILABLE lagertrum THEN DO:
      trumnr = lagertrum.TRUMMANR.                
      IF lagertrum.TRUMMAPLATS NE INPUT BROWSE BRW_LAGERPLATS lagertrum.TRUMMAPLATS THEN DO:
         hjtrnamn = INPUT BROWSE BRW_LAGERPLATS lagertrum.TRUMMAPLATS.
         IF hjtrnamn = "" OR hjtrnamn = "E" THEN DO:
            MESSAGE "Trumma namn måste fyllas i " VIEW-AS ALERT-BOX. 
         END.
         ELSE DO:   
            FIND FIRST lagerbuff WHERE lagerbuff.TRUMMAPLATS = INPUT BROWSE BRW_LAGERPLATS lagertrum.TRUMMAPLATS AND
            lagerbuff.TRUMMANR NE lagertrum.TRUMMANR AND lagerbuff.LAGMETER > 0 NO-LOCK NO-ERROR.
            IF AVAILABLE lagerbuff THEN DO:
               MESSAGE "Trumma namn " + INPUT BROWSE BRW_LAGERPLATS lagertrum.TRUMMAPLATS + " finns redan upplagt på " + STRING(lagerbuff.TRUMMANR)  
               VIEW-AS ALERT-BOX.               
            END.
            ELSE DO:
               /*BYTNAMN ÄVEN PÅ UTTAG*/
               FOR EACH lagertrum  WHERE lagertrum.TRUMMANR = trumnr:
                  lagertrum.TRUMMAPLATS = INPUT BROWSE BRW_LAGERPLATS lagertrum.TRUMMAPLATS.
                  RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).                     
               END.
               FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = trumnr AND lagertrum.TRUMMA = "S" NO-ERROR.
               IF AVAILABLE lagertrum  THEN DO:
                  RUN visa_UI.
               END.                     
            END.
            
         END.          
      END.
   END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   

   antal_valda = BRW_AOLAGP:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_AOLAGP:FETCH-SELECTED-ROW(antal_raknare). 
      spec_rowid = ROWID(aotrum).               
      EMPTY TEMP-TABLE eaotrum NO-ERROR. 
      CREATE eaotrum.
      BUFFER-COPY aotrum TO eaotrum.    
      RUN UTTRUM.W (INPUT TABLE eaotrum).
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO:      
         FIND FIRST eaotrum NO-ERROR.
         IF AVAILABLE eaotrum THEN DO:                                       
            FIND FIRST aotrum WHERE ROWID(aotrum) = spec_rowid NO-ERROR.
            IF AVAILABLE aotrum THEN DELETE aotrum.
            RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
         END.                                    
         
      END.
      antal_raknare = antal_raknare + 1.   
   END.
   IF RAD_AOTVAL = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "aotrum.aonr =  '" + vaonr + "' AND aotrum.DELNR = " + STRING(vdelnr) ).
   ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "").
   RUN openbdynspec_UI IN brwproc[4].
   RUN resetcolindex_UI IN brwproc[4]. 
   RUN title_UI IN brwproc[4].  
   RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang ).    
   RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").                    
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:                   
   ASSIGN
   antal_valda = BRW_LAGERPLATS:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      ASSIGN                                   
      status-ok = BRW_LAGERPLATS:FETCH-SELECTED-ROW(antal_raknare)
      lager_rowid = ROWID(lagertrum).                           
      FIND FIRST aotrum WHERE aotrum.TRUMMANR = lagertrum.TRUMMANR NO-LOCK NO-ERROR.
      IF AVAILABLE aotrum THEN DO:         
         MESSAGE "Trumma namn " + lagertrum.TRUMMAPLATS + " Id: " + STRING(lagertrum.TRUMMANR) +  " är bokad på " + Guru.Konstanter:gaok + " " + aotrum.AONR + " " + STRING(aotrum.DELNR) + ". Borttag ej möjligt!"   VIEW-AS ALERT-BOX.
         RETURN.
      END.
      IF lagertrum.lagmeter > 0  THEN DO:
         MESSAGE "Det går inte att ta bort en trumma där lagervärdet är större än noll. På Trumma namn " + lagertrum.TRUMMAPLATS + " Id: " + STRING(lagertrum.TRUMMANR) +  " finns det " + STRING(lagertrum.lagmeter) + " meter"  VIEW-AS ALERT-BOX.
         RETURN.
      END.
      
      FIND FIRST lagerbuff WHERE lagerbuff.TRUMMANR = lagertrum.TRUMMANR AND lagerbuff.TRUMMA BEGINS "U" AND lagerbuff.DATUM > DATE(MONTH(TODAY), DAY(TODAY),YEAR(TODAY ) - 1 ) NO-LOCK NO-ERROR.
      IF AVAILABLE lagerbuff THEN DO:
         MESSAGE "Det går inte att ta bort en trumma där något uttag är gjort under det senaste året. På Trumma namn " + lagertrum.TRUMMAPLATS + " Id: " + STRING(lagertrum.TRUMMANR) + " finns uttag  " + STRING(lagerbuff.DATUM)  VIEW-AS ALERT-BOX.
         RETURN.

      END.
      
      MESSAGE "Vill du ta bort Trumma namn : "  + lagertrum.TRUMMAPLATS + " Id:" + STRING(lagertrum.TRUMMANR) + " med alla uttag ?"   VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
      IF svar THEN DO TRANSACTION:
         FIND FIRST lagertrum WHERE ROWID(lagertrum) = lager_rowid NO-LOCK.
         FOR EACH lagerbuff WHERE lagerbuff.TRUMMANR = lagertrum.TRUMMANR NO-LOCK:
            DELETE lagerbuff.
         END.
         IF AVAILABLE lagertrum THEN DO:
            DELETE lagertrum.            
         END.
         RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
      END.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].
      antal_raknare = antal_raknare + 1.   
   END. 
   IF RAD_VALKOP = 1 THEN DO:    
      RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang ).
      /*IF TOG_VUTT = TRUE THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "lagertrum.aokopp =  FALSE" ).      
      ELSE RUN setcolsortvar_UI IN brwproc[1] (INPUT "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'" ).*/               
   END.        
    RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").           
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].  
   RUN title_UI IN brwproc[1].          
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:                                        
   {muswait.i}   
   EMPTY TEMP-TABLE elagertrum NO-ERROR. 
   RUN TRUMREG.W (INPUT-OUTPUT TABLE elagertrum,INPUT 0,OUTPUT lplats).      
   FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = lplats NO-LOCK NO-ERROR.
   
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(lagertrum)).
    RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").    
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   EMPTY TEMP-TABLE elagertrum NO-ERROR.
   musz = FALSE.
   
   {musarrow.i}                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A
DO:   
    
   antal_valda = BRW_LAGERPLATS:NUM-SELECTED-ROWS.   
   IF antal_valda > 1 THEN DO:           
      MESSAGE "Det går bara att ta ut en trumma i taget!"  VIEW-AS ALERT-BOX.
      RETURN.      
   END.      
   IF AVAILABLE lagertrum THEN DO:
      IF lagertrum.LAGMETER = 0 THEN DO:
                
         MESSAGE "Trumma namn " + lagertrum.TRUMMAPLATS + " Id: " + STRING(lagertrum.TRUMMANR) + " är är slut och går ej att ta ut på " + Guru.Konstanter:gaok  VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:      
      EMPTY TEMP-TABLE lplagertrum NO-ERROR.
      CREATE lplagertrum.
      BUFFER-COPY lagertrum TO lplagertrum.
      ASSIGN 
      lplagertrum.AONR = vaonr
      lplagertrum.DELNR = vdelnr.
      /*ej tvingande koppla pplats. 
      BESTDEPAO ska inte skapas om uttaget görs från depå - oavsett från vilken depå
      BESTDEPAO SKA bara skapas om uttaget görs från Inköp  20200219*/
      kopplapplats = FALSE.
      IF varifran = 3 THEN DO:
         kopplapplats = FALSE.  /*trumma från inköp ska inte läggas på pplats 20190610*/
         
         /*men BESTDEPAO måste sparas i alla fall Lena 20190617 */         
         RUN sparbdepaotrumma_UI IN leveransaoapph (INPUT vaonr, INPUT vdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT lplagertrum.TRUMMANR,INPUT Guru.Konstanter:globanv, INPUT TABLE lplagertrum, INPUT onummer).
         trummauttagen = TRUE.
      END.
      
         
      /*kopplapplats = FALSE.  
      IF Guru.GlobalaVariabler:GuruVdepnr = 7 OR Guru.GlobalaVariabler:GuruVdepnr = 1 OR Guru.GlobalaVariabler:GuruVdepnr = 8 THEN.
      ELSE DO:  
         /*men BESTDEPAO måste sparas i alla fall Lena 20190617 */         
         RUN sparbdepaotrumma_UI IN leveransaoapph (INPUT vaonr, INPUT vdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT lplagertrum.TRUMMANR,INPUT Guru.Konstanter:globanv, INPUT TABLE lplagertrum, INPUT onummer).
      END.   
      trummauttagen = TRUE.*/
      
      /*/*Felavhjälpning inte tvingande att koppla till lager lena 20190515*/      
      kopplapplats = TRUE.
      IF Guru.GlobalaVariabler:GuruVdepnr = 7 OR Guru.GlobalaVariabler:GuruVdepnr = 1 OR Guru.GlobalaVariabler:GuruVdepnr = 8 THEN DO:
         kopplapplats = FALSE.        
      END.      
      /*gurka 20190911
       om ej tvingande att lägga på pplats någon gång
      ELSE DO:
         /*ej lagerplats trumma tvingande i något fall Lena 20190906*/
         kopplapplats = FALSE.  /*trumma från inköp ska inte läggas på pplats 20190610*/
         /*men BESTDEPAO måste sparas i alla fall Lena 20190617 */
         RUN sparbdepaotrumma_UI IN leveransaoapph (INPUT vaonr, INPUT vdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT lplagertrum.TRUMMANR,INPUT Guru.Konstanter:globanv, INPUT TABLE lplagertrum, INPUT onummer).
      END.*/
      IF varifran = 3 THEN DO:
         kopplapplats = FALSE.  /*trumma från inköp ska inte läggas på pplats 20190610*/
         
         /*men BESTDEPAO måste sparas i alla fall Lena 20190617 */         
         RUN sparbdepaotrumma_UI IN leveransaoapph (INPUT vaonr, INPUT vdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT lplagertrum.TRUMMANR,INPUT Guru.Konstanter:globanv, INPUT TABLE lplagertrum, INPUT onummer).
         trummauttagen = TRUE.
      END.   
      */
      /* ska aldrig vara true 20200219
      IF kopplapplats = TRUE THEN DO:
          
         IF VALID-HANDLE(leveransaoapph)  THEN RUN sparbdepaotrumma_UI IN leveransaoapph (INPUT vaonr, INPUT vdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT lplagertrum.TRUMMANR,INPUT Guru.Konstanter:globanv, INPUT TABLE lplagertrum, INPUT onummer).
         /* test pplats*/
         {AVBGOM.I}                 
         RUN LEVERANSVAO.W (INPUT 3, INPUT 0, INPUT-OUTPUT TABLE lplagertrum , INPUT-OUTPUT TABLE eaobest_mtrl).
         {AVBFRAM.I}
         FIND FIRST lplagertrum WHERE NO-LOCK NO-ERROR.
         IF lplagertrum.LAGERPLATSI = 0 THEN DO:
            MESSAGE "Trumma " + lplagertrum.TRUMMAPLATS + " måste kopplas till en p-plats innan den kan tas ut"  VIEW-AS ALERT-BOX.
            IF VALID-HANDLE(leveransaoapph)  THEN RUN bortbdepaotrumma_UI IN leveransaoapph (INPUT vaonr, INPUT vdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT lplagertrum.TRUMMANR,INPUT Guru.Konstanter:globanv, INPUT TABLE lplagertrum).
            RETURN.
         END.   
         ELSE DO:
            FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = lplagertrum.TRUMMANR NO-LOCK NO-ERROR.
            IF AVAILABLE lagertrum THEN DO:
               ASSIGN 
               lagertrum.LAGERPLATSI = lplagertrum.LAGERPLATSI
               lagertrum.LAGERPLATSC = lplagertrum.LAGERPLATSC.
            END.   
         END.   
      END.*/
      /*GÖR CHECK OM trumman ligger på en lagerplats*/
   END.
   antal_raknare = 1.   
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_LAGERPLATS:FETCH-SELECTED-ROW(antal_raknare).
         
         RUN over_UI.
      END.             
      IF RAD_AOTVAL = 1 THEN DO : 
         RUN setcolsortvar_UI IN brwproc[4] (INPUT "aotrum.aonr =  '" + vaonr + "' AND aotrum.DELNR = " + STRING(vdelnr) ).         
      END.
      ELSE DO:
         RUN setcolsortvar_UI IN brwproc[4] (INPUT "").         
      END.
      RUN openbdynspec_UI IN brwproc[4].
      RUN resetcolindex_UI IN brwproc[4].
      RUN title_UI IN brwproc[4].
      FIND FIRST aotrum WHERE ROWID(aotrum) = spec_rowid NO-LOCK.
      IF AVAILABLE aotrum THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(aotrum)).
         RUN lastselectdyn_UI IN brwproc[4].        
      END.       
           
      RUN selnextprevrow_UI IN brwproc[1].
      RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang).
      /*RUN setcolsortvar_UI IN brwproc[1] (INPUT "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'" ).*/
       RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").    
      RUN openbdynspec_UI IN brwproc[1].
      RUN resetcolindex_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
      RUN title_UI IN brwproc[1].
 
      status-ok = BRW_AOLAGP:SELECT-FOCUSED-ROW().     
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AVB WINDOW-1
ON CHOOSE OF FBTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   
   IF varifran = 3 THEN DO:
      FIND FIRST aotrum WHERE aotrum.aonr =  vaonr AND aotrum.DELNR =  vdelnr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE aotrum THEN DO:
         FIND FIRST lagertrum WHERE lagertrum.ENR = hjenr NO-LOCK NO-ERROR.
         IF AVAILABLE lagertrum  THEN DO:
            MESSAGE "Du måste välja en trumma genom att pila över den till höger" 
            VIEW-AS ALERT-BOX.
            RETURN.
         END.
         ELSE DO:
            MESSAGE "Alla trummor med detta enr är redan bokade på projekt. Detta måste åtgärdas manuellt"
            VIEW-AS ALERT-BOX.   
         END.   
         IF trummauttagen = FALSE THEN DO:
            /*om ingen trumma är uttagen med BTN_OVER*/
            MESSAGE "Du måste välja en trumma genom att pila över den till höger" 
            VIEW-AS ALERT-BOX.
            RETURN.
         END.   
      END.
      
   END.   
 
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:   
   
   {AVBGOM.I}
   RUN RAPPTYPT.W (OUTPUT rapptyp).   
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      IF rapptyp = 1 THEN DO:           
         status-ok = BRW_LAGERPLATS:SELECT-FOCUSED-ROW() NO-ERROR.  
         CREATE elagertrum.
         BUFFER-COPY lagertrum TO elagertrum.
      END.
      
      RUN UTRERAPPT.W (INPUT rapptyp,INPUT TABLE elagertrum, INPUT TABLE lagertrum, INPUT TABLE aotrum).                                     
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
   END.
   {AVBFRAM.I}

     {musarrow.i}               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AAONR WINDOW-1
ON ANY-KEY OF FILL-IN-AAONR IN FRAME FRAME-A /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-AAONR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AENR WINDOW-1
ON ANY-KEY OF FILL-IN-AENR IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-AENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR WINDOW-1
ON ANY-KEY OF FILL-IN-AONR IN FRAME FRAME-A /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ATRUMMANR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ATRUMMANR WINDOW-1
ON ANY-KEY OF FILL-IN-ATRUMMANR IN FRAME FRAME-A /* Id */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ATRUMMANR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ATRUMMAPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ATRUMMAPLATS WINDOW-1
ON ANY-KEY OF FILL-IN-ATRUMMAPLATS IN FRAME FRAME-A /* Trumma namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ATRUMMAPLATS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TRUMMANR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TRUMMANR WINDOW-1
ON ANY-KEY OF FILL-IN-TRUMMANR IN FRAME FRAME-A /* Id */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TRUMMANR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TRUMMAPLATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TRUMMAPLATS WINDOW-1
ON ANY-KEY OF FILL-IN-TRUMMAPLATS IN FRAME FRAME-A /* Trumma namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TRUMMAPLATS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_AOTVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_AOTVAL WINDOW-1
ON VALUE-CHANGED OF RAD_AOTVAL IN FRAME FRAME-A
DO:
   RAD_AOTVAL = INPUT RAD_AOTVAL.
   IF RAD_AOTVAL = 1 THEN DO: 
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "aotrum.aonr =  '" + vaonr + "' AND aotrum.DELNR = " + STRING(vdelnr) ).
      RUN openbdynspec_UI IN brwproc[4].
      RUN resetcolindex_UI IN brwproc[4].
      RUN title_UI IN brwproc[4].
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "" ).
      RUN openbdynspec_UI IN brwproc[4].
      RUN resetcolindex_UI IN brwproc[4].
      RUN title_UI IN brwproc[4].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VALKOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VALKOP WINDOW-1
ON VALUE-CHANGED OF RAD_VALKOP IN FRAME FRAME-A
DO:   
   RAD_VALKOP = INPUT RAD_VALKOP.
   
   RUN oppna_UI. 
   /*IF RAD_VALKOP = 1 THEN DO:    
      IF TOG_VUTT = TRUE THEN DO:
         IF varifran = 1 THEN DO:                     
            IF TOG_EJNOLL = FALSE THEN DO:               
               hjstrang = "lagertrum.aokopp =  FALSE  ".               
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
            END.
         END.                
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:
            hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".  
         END.                           
      END.
      ELSE DO:
         IF varifran = 1 THEN DO:                       
            IF TOG_EJNOLL = FALSE THEN DO:               
               hjstrang = "lagertrum.aokopp =  FALSE  " + " AND lagertrum.trumma = " + "'" + "S" + "'".               
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
            END.
         END.             
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                                              
      END.                  
   END.   
   ELSE DO: 
      IF TOG_VUTT = TRUE THEN DO:
         IF varifran = 1 THEN DO:                       
            IF TOG_EJNOLL = FALSE THEN DO:               
               hjstrang = "".               
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = " lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
            END.
         END.         
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = " lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.         
      END.
      ELSE DO:
         IF varifran = 1 THEN DO:                       
            IF TOG_EJNOLL = FALSE THEN DO:               
               hjstrang = "lagertrum.trumma = " + "'" + "S" + "'".               
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.trumma = " + "'" + "S" + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
            END.
         END.             
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                                    
      END.               
   END.*/
   RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang ).
   RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").    
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   IF RAD_VALKOP = 1 THEN DO:    
      IF TOG_VUTT = TRUE THEN DO:         
         BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
      END.
      ELSE DO:                  
         BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
      END.                  
   END.   
   ELSE DO:       
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EJNOLL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EJNOLL WINDOW-1
ON VALUE-CHANGED OF TOG_EJNOLL IN FRAME FRAME-A /* Visa ej tomma trummor */
DO:
   TOG_EJNOLL = INPUT TOG_EJNOLL.
   RUN oppna_UI.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang ).
   RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").    
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VUTT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VUTT WINDOW-1
ON VALUE-CHANGED OF TOG_VUTT IN FRAME FRAME-A /* Visa uttag */
DO:
   
   TOG_VUTT = INPUT TOG_VUTT.
   RUN oppna_UI.
   IF TOG_VUTT = TRUE THEN DO:
     lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = TRUE.
   END.  
   ELSE DO:      
      lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = FALSE.
   END.   
   /*IF TOG_VUTT = TRUE THEN DO:
     lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = TRUE.  
      IF RAD_VALKOP = 1 THEN DO:
         IF varifran = 1 THEN DO:
            IF TOG_EJNOLL = FALSE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE ".
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE   AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
            END.
         END.               
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                  
      END.
      ELSE IF RAD_VALKOP = 2 THEN DO:
         IF varifran = 1 THEN DO:
            IF TOG_EJNOLL = FALSE THEN DO:
               hjstrang = " ".
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = " lagertrum.LAGMETER > '" + STRING("0") + "'  ".
            END.
         END.    
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = " lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.
      END.                              
   END.
   ELSE DO:      
      lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = FALSE.
      IF RAD_VALKOP = 1 THEN DO:
         IF varifran = 1 THEN DO:            
            IF TOG_EJNOLL = FALSE THEN DO:               
               hjstrang = "lagertrum.aokopp =  FALSE  " + " AND lagertrum.trumma = " + "'" + "S" + "'".               
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
            END.
         END.             
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                                              
      END.
      ELSE DO:    
         IF varifran = 1 THEN DO:
            IF TOG_EJNOLL = FALSE THEN DO:
               hjstrang = "lagertrum.trumma = " + "'" + "S" + "'".
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = " lagertrum.trumma = " + "'" + "S" + "' +   AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
            END.
         END.                
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                              
      END.         
   END.*/
   RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang ).
   RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").    
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   IF TOG_VUTT = TRUE THEN DO:            
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   END.
   ELSE DO:      
      IF RAD_VALKOP = 1 THEN DO:         
         BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE .
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
      END.
      ELSE DO:               
         BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
      END.
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AOLAGP
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   IF VALID-HANDLE(depaapph) THEN DELETE PROCEDURE depaapph.
   IF VALID-HANDLE(leveransaoapph) THEN DELETE PROCEDURE leveransaoapph NO-ERROR.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   trummauttagen = FALSE.
    
   RUN dnamn_UI IN depaapph (INPUT Guru.GlobalaVariabler:GuruVdepnr ,OUTPUT dnamn ).
                                                           
   ASSIGN WINDOW-1:TITLE = "Trum meny - Bokade trummor på " + CAPS(Guru.Konstanter:gaol) + " " + vaonr + " " + " DELNR: " + STRING(vdelnr,Guru.Konstanter:varforetypchar[1]) + " för " + dnamn.
   
   ASSIGN
   RAD_VALKOP = 1
   RAD_AOTVAL = 1.
   /*TOG_ALLAT = FALSE.*/
   BRW_AOLAGP:TITLE = "Bokade trummor på " + Guru.Konstanter:gaok.
   BRW_LAGERPLATS:TITLE = "Trummor för " + string(Guru.GlobalaVariabler:GuruVdepnr) + "-" +  dnamn.                                   
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok. 
   FILL-IN-AAONR:LABEL = Guru.Konstanter:gaok.   
 
   status-ok = RAD_AOTVAL:DELETE("Alla bokade trummor").      
   status-ok = RAD_AOTVAL:DELETE("Bokade trummor på valt aonr").      
   status-ok = RAD_AOTVAL:ADD-LAST("Bokade trummor på valt " + LC(Guru.Konstanter:gaok), 1).         
   status-ok = RAD_AOTVAL:ADD-LAST("Alla bokade trummor", 2).         
   
   IF varifran = 2 OR varifran = 3 THEN DO:
      
      FILL-IN-UTMETER = "Önskat antal meter: " + STRING(umeter) + " för enr: " + hjenr.
      DISPLAY FILL-IN-UTMETER WITH FRAME FRAME-A.
   END.   
   TOG_EJNOLL = TRUE.
   RUN enable_UI.
         
   IF varifran = 1 THEN DO:
      hjstrang = "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".   
   END.    
   ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
       hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
   END.    
   ELSE IF varifran = 4  THEN DO:       
       hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
   END.    
   RUN setcolsortvar_UI IN brwproc[1] (INPUT hjstrang ).       
    RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").                                                                                                                                            
   RUN openbdynspec_UI IN brwproc[1].
   RUN resetcolindex_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   RUN setcolsortvar_UI IN brwproc[4] 
   (INPUT "aotrum.aonr =  '" + vaonr + "' AND aotrum.DELNR = " + STRING(vdelnr) ).
   
                                                   
   RUN openbdynspec_UI IN brwproc[4]. 
   RUN resetcolindex_UI IN brwproc[4].  
   RUN title_UI IN brwproc[4].
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   FILL-IN-TRUMMANR:HIDDEN = TRUE.
   FILL-IN-ATRUMMANR:HIDDEN = TRUE.
   IF varifran = 3 THEN DO:
      DISABLE BTN_NY WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BACK WITH FRAME {&FRAME-NAME}.
      lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = TRUE.
   END.
   IF varifran = 4 THEN DO:
      
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
      BRW_AOLAGP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      RAD_AOTVAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      RAD_VALKOP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      TOG_VUTT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      TOG_EJNOLL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      
      FBTN_VISA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-ATRUMMANR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-ATRUMMAPLATS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-AENR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-AAONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      
      
      lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = TRUE.
   END.   
   
   IF dtitt = TRUE THEN DO:      
      DISABLE BTN_OVER WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BACK WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_NY WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BACK WITH FRAME {&FRAME-NAME}.      
      lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = TRUE.      
   END.
   {FRMSIZE.I}  
   {musarrow.i}      
   {WIN_M_SLUT.I}
   
   
   /*status-ok = BRW_LAGERPLATS:SELECT-FOCUSED-ROW() NO-ERROR.   */
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
         (INPUT BRW_LAGERPLATS:HANDLE IN FRAME {&FRAME-NAME}).       
   /*RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).*/
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
         (INPUT BRW_AOLAGP:HANDLE IN FRAME {&FRAME-NAME}). 
            
   
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-TRUMMANR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TRUMMANR").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-TRUMMAPLATS:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TRUMMAPLATS").   
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-ENR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ENR").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-AONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONR").
   RUN setcolindex_UI IN brwproc[1] (INPUT "TRUMMAPLATS").    
   RUN addfillin_UI IN brwproc[4] 
      (INPUT FILL-IN-ATRUMMANR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TRUMMANR").
      RUN addfillin_UI IN brwproc[4] 
      (INPUT FILL-IN-ATRUMMAPLATS:HANDLE IN FRAME {&FRAME-NAME}, INPUT "TRUMMAPLATS").
   RUN addfillin_UI IN brwproc[4] 
      (INPUT FILL-IN-AENR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ENR").
   RUN addfillin_UI IN brwproc[4] 
      (INPUT FILL-IN-AAONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONR").
   RUN setcolindex_UI IN brwproc[4] (INPUT "TRUMMAPLATS").   
   
   IF varifran = 1 THEN DO:                 
      IF TOG_EJNOLL = FALSE THEN DO:               
         hjstrang = "lagertrum.aokopp =  FALSE  " + " AND lagertrum.trumma = " + "'" + "S" + "'".               
      END.
      ELSE IF TOG_EJNOLL = TRUE THEN DO:
         hjstrang = "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
      END.
   END.      
   ELSE IF varifran = 2 OR varifran = 3 THEN DO:
      hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".  
   END.                         
         
   IF Guru.Konstanter:appcon THEN DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEPAAPP.P PERSISTENT SET depaapph.
   END.  
    IF Guru.Konstanter:appcon THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BESTDEPAO", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "BESTDEPAO", OUTPUT logresult).
   END.   
   IF logresult = TRUE THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:
         RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph.
      END.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-UTMETER TOG_VUTT TOG_EJNOLL RAD_VALKOP RAD_AOTVAL 
          FILL-IN-ATRUMMAPLATS FILL-IN-TRUMMAPLATS FILL-IN-AENR FILL-IN-ENR 
          FILL-IN-AAONR FILL-IN-AONR 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE IMAGE-6 RECT_SOK1 RECT_SOK2 IMAGE-16 TOG_VUTT TOG_EJNOLL RAD_VALKOP 
         RAD_AOTVAL BRW_LAGERPLATS BRW_AOLAGP btn_over btn_back BTN_NY BTN_BORT 
         FBTN_VISA FILL-IN-ATRUMMAPLATS FILL-IN-TRUMMAPLATS FILL-IN-AENR 
         FILL-IN-ENR FILL-IN-AAONR FILL-IN-AONR FBTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppna_UI WINDOW-1 
PROCEDURE oppna_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_VUTT = TRUE THEN DO:
     /*lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = TRUE.*/  
      IF RAD_VALKOP = 1 THEN DO:
         IF varifran = 1 THEN DO:
            IF TOG_EJNOLL = FALSE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE ".
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE   AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
            END.
         END.               
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                  
      END.
      ELSE IF RAD_VALKOP = 2 THEN DO:
         IF varifran = 1 THEN DO:
            IF TOG_EJNOLL = FALSE THEN DO:
               hjstrang = " ".
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = " lagertrum.LAGMETER > '" + STRING("0") + "'  ".
            END.
         END.    
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = " lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.
      END.                              
   END.
   ELSE DO:      
      /*lagertrum.TRUMMAPLATS:READ-ONLY IN BROWSE BRW_LAGERPLATS = FALSE.*/
      IF RAD_VALKOP = 1 THEN DO:
         IF varifran = 1 THEN DO:            
            IF TOG_EJNOLL = FALSE THEN DO:               
               hjstrang = "lagertrum.aokopp =  FALSE  " + " AND lagertrum.trumma = " + "'" + "S" + "'".               
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:
               hjstrang = "lagertrum.aokopp =  FALSE " + " AND lagertrum.trumma = " + "'" + "S" + "'  AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".               
            END.
         END.             
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.aokopp =  FALSE  AND  lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                                              
      END.
      ELSE DO:    
         IF varifran = 1 THEN DO:
            IF TOG_EJNOLL = FALSE THEN DO:
               hjstrang = "lagertrum.trumma = " + "'" + "S" + "'".
            END.
            ELSE IF TOG_EJNOLL = TRUE THEN DO:               
               /*hjstrang = " lagertrum.trumma = " + "'" + "S" + "' + " AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".*/
               
               hjstrang = " lagertrum.trumma = 'S' " +  " AND lagertrum.LAGMETER > " + STRING("0")  .               
            END.
         END.                
         ELSE IF varifran = 2 OR varifran = 3 THEN DO:       
             hjstrang = "lagertrum.ENR = '" +  STRING(hjenr) + "'  AND lagertrum.trumma = " + "'" + "S" + "' AND lagertrum.LAGMETER > '" + STRING("0") + "'  ".
         END.                              
      END.         
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
      FIND FIRST aotrum WHERE aotrum.TRUMMANR = lagertrum.TRUMMANR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE aotrum THEN DO:         
         CREATE aotrum.
         ASSIGN 
         aotrum.aonr = vaonr
         aotrum.TRUMMANR = lagertrum.TRUMMANR
         aotrum.TRUMMAPLATS = lagertrum.TRUMMAPLATS
         aotrum.delnr =  vdelnr
         aotrum.ENR =  lagertrum.ENR
         aotrum.BENAMNING =  lagertrum.BENAMNING
         aotrum.ENHET =  lagertrum.ENHET
         aotrum.DATUMUT =  TODAY
         aotrum.STMETER =  lagertrum.LAGMETER
         aotrum.LAGERPLATSI =  lagertrum.LAGERPLATSI
         aotrum.LAGERPLATSC =  lagertrum.LAGERPLATSC
         aotrum.PRIS =  lagertrum.PRIS
         aotrum.BESTALLARE =  Guru.Konstanter:globanv.      
         spec_rowid = ROWID(aotrum).                  
         /* spara uttag till pnr*/
         aotrum.DEPNR = Guru.GlobalaVariabler:GuruVdepnr. 
      END. 
      lagertrum.AOKOPP = TRUE.     
      /*spara om lagerplatsen blivit kopplad till pnr*/
      RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum). 
      antal_raknare = antal_raknare + 1.              
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF AVAILABLE lagertrum THEN DO:     
      
      DISPLAY        
      lagertrum.TRUMMANR
      lagertrum.TRUMMAPLATS
      lagertrum.benamning
      lagertrum.stmeter
      lagertrum.lagmeter
      lagertrum.utmeter
      lagertrum.aokopp       
      lagertrum.AONR 
      lagertrum.DELNR
      lagertrum.DATUM  WITH BROWSE BRW_LAGERPLATS.
                      
   END.
   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

