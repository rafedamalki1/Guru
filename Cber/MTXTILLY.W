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

  Created: 02/14/97 -  3:20 pm

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
{BYTENRTEMP.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{DEFSOK.I} 
{BMTRL.I}
&Scoped-define SHARED SHARED
{HUVLEVTEMP.I}
{LEVTEMP.I}
{KONSTRMTRL.I}
&Scoped-define NEW NEW
{SMTRL.I}
{MTRLTEMP.I}
DEFINE NEW SHARED VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE lista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE varalla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valgrupp AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE erslev AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.   
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE temprow AS ROWID NO-UNDO.
DEFINE VARIABLE kgrvar AS CHARACTER NO-UNDO.
/*MATERIEL DEFINITIONER*/
DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_levtill AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.  
DEFINE VARIABLE vald_levfran AS CHARACTER NO-UNDO.  
DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE sokstring AS CHARACTER FORMAT "x(40)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE nyaquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE appqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE valdkommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbufmerah AS HANDLE NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE mtxtillyapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE nettobredd AS DECIMAL NO-UNDO.
DEFINE VARIABLE nettobreddorg AS DECIMAL NO-UNDO.
DEFINE VARIABLE nettobreddsats AS DECIMAL NO-UNDO.
DEFINE VARIABLE nettobreddsatsorg AS DECIMAL NO-UNDO.
DEFINE VARIABLE enrspar AS CHARACTER NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE VARIABLE flev AS CHARACTER NO-UNDO.
DEFINE VARIABLE tlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.

DEFINE SHARED TEMP-TABLE grupp_temp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp mtrlbertemp satstemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.BERKVANT mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV mtrltemp.BERKVANT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_HLEV2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV2 mtrlbertemp.ENR ~
mtrlbertemp.BENAMNING mtrlbertemp.ENHET mtrlbertemp.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV2 
&Scoped-define QUERY-STRING-BRW_HLEV2 FOR EACH mtrlbertemp NO-LOCK ~
    BY mtrlbertemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV2 OPEN QUERY BRW_HLEV2 FOR EACH mtrlbertemp NO-LOCK ~
    BY mtrlbertemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV2 mtrlbertemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV2 mtrlbertemp


/* Definitions for BROWSE BRW_SATS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS satstemp.KOD satstemp.BENAMNING ~
satstemp.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS satstemp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_SATS satstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_SATS satstemp
&Scoped-define QUERY-STRING-BRW_SATS FOR EACH satstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SATS OPEN QUERY BRW_SATS FOR EACH satstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SATS satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS satstemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 IMAGE-10 IMAGE-11 BRW_HLEV2 ~
BRW_HLEV BRW_SATS RAD_VAL BTN_VISA BTN_BYT BTN_TILL BTN_BORT BTN_BYTFIL ~
BTN_UPP BTN_SENASTKOP CMB_LEV2 CMB_LEV FILL-IN-ENR2 FILL-IN-KOD FILL-IN-ENR ~
FILL-IN-BEN2 FILL-IN-BEN RAD_SOK2 RAD_SOK BTN_AVB FILL-IN-SOKALT ~
FILL-IN-SOKALT-2 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL CMB_LEV2 CMB_LEV FILL-IN-ENR2 ~
FILL-IN-KOD FILL-IN-ENR FILL-IN-BEN2 FILL-IN-BEN RAD_SOK2 RAD_SOK ~
FILL-IN-SOKALT FILL-IN-SOKALT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_HLEV 
       MENU-ITEM m_AvmarkeraHLEV LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_HLEV2 
       MENU-ITEM m_AvmarkeraHLEV2 LABEL "Avmarkera"     .

DEFINE MENU POPUP-MENU-BRW_SATS 
       MENU-ITEM m_Sats_information LABEL "Satsinformation".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt mtrl" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BYTFIL 
     LABEL "Byt mtrl från fil" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SENASTKOP 
     LABEL "Senast inköpt" 
     SIZE 14 BY 1 TOOLTIP "Senaste inköpet för samtliga artiklar på vänstra sidan".

DEFINE BUTTON BTN_TILL 
     LABEL "Lägg till mtrl" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Uppdat. katalog" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa / Uppdat antal" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-11
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_SOK2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 30.88 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ersätt", 1,
"Lägg till", 2,
"Ta bort", 3
     SIZE 13.5 BY 3.04 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 3.83
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60.63 BY 3.83
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_HLEV2 FOR 
      mtrlbertemp SCROLLING.

DEFINE QUERY BRW_SATS FOR 
      satstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 32
      mtrltemp.Enhet COLUMN-LABEL "Enh" FORMAT "x(5)":U WIDTH 4
      mtrltemp.BERKVANT FORMAT ">>>>>>9":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
  ENABLE
      mtrltemp.BERKVANT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 55.25 BY 21
         TITLE "Materiellista huvudleverantör.".

DEFINE BROWSE BRW_HLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV2 WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV2 NO-LOCK DISPLAY
      mtrlbertemp.ENR FORMAT "X(11)":U
      mtrlbertemp.BENAMNING FORMAT "x(256)":U WIDTH 31
      mtrlbertemp.ENHET COLUMN-LABEL "Enh" FORMAT "x(5)":U WIDTH 4
      mtrlbertemp.PRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 53 BY 21
         TITLE "Materiellista huvudleverantör.".

DEFINE BROWSE BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS WINDOW-1 _STRUCTURED
  QUERY BRW_SATS NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(256)":U WIDTH 40
      satstemp.ANTAL FORMAT ">>>,>>9":U
  ENABLE
      satstemp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 55.25 BY 21
         TITLE "Materielsatser".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_HLEV2 AT ROW 2.5 COL 1.5
     BRW_HLEV AT ROW 2.5 COL 55.5
     BRW_SATS AT ROW 2.5 COL 55.5
     RAD_VAL AT ROW 2.63 COL 111.38 NO-LABEL
     BTN_VISA AT ROW 7.54 COL 111.38
     BTN_BYT AT ROW 9 COL 111.38
     BTN_TILL AT ROW 10.08 COL 111.38
     BTN_BORT AT ROW 11.29 COL 111.38
     BTN_BYTFIL AT ROW 12.75 COL 111.5 WIDGET-ID 4
     BTN_UPP AT ROW 18.75 COL 111.38 WIDGET-ID 2
     BTN_SENASTKOP AT ROW 20.08 COL 111.38 WIDGET-ID 6
     CMB_LEV2 AT ROW 24 COL 20 COLON-ALIGNED
     CMB_LEV AT ROW 24 COL 66.13
     FILL-IN-ENR2 AT ROW 25.46 COL 16.13 COLON-ALIGNED
     FILL-IN-KOD AT ROW 25.46 COL 71.25 COLON-ALIGNED
     FILL-IN-ENR AT ROW 25.46 COL 71.25 COLON-ALIGNED
     FILL-IN-BEN2 AT ROW 26.5 COL 16.13 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.5 COL 71.25 COLON-ALIGNED
     RAD_SOK2 AT ROW 27.88 COL 16.88 NO-LABEL
     RAD_SOK AT ROW 27.88 COL 68.88 NO-LABEL
     BTN_AVB AT ROW 28 COL 111.38
     FILL-IN-SOKALT AT ROW 27.92 COL 2.5 NO-LABEL
     FILL-IN-SOKALT-2 AT ROW 27.92 COL 51.25 NO-LABEL
     "Till" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 1.25 COL 71.5
          FONT 17
     "Från" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 1.25 COL 21.25
          FONT 17
     RECT-3 AT ROW 25.25 COL 1.5
     RECT-4 AT ROW 25.25 COL 50.13
     IMAGE-10 AT ROW 25.46 COL 2.25
     IMAGE-11 AT ROW 25.46 COL 50.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrlbertemp T "?" NO-UNDO temp-db mtrlbertemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 1"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 240
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 240
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
/* BROWSE-TAB BRW_HLEV2 IMAGE-11 FRAME-A */
/* BROWSE-TAB BRW_HLEV BRW_HLEV2 FRAME-A */
/* BROWSE-TAB BRW_SATS BRW_HLEV FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_HLEV:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_HLEV2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV2:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_HLEV2:HANDLE
       BRW_HLEV2:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_SATS:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_SATS:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_SATS:HANDLE
       BRW_SATS:MAX-DATA-GUESS IN FRAME FRAME-A         = 50000
       BRW_SATS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEV IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT-2 IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT-2:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "32" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" "Enh" ? "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.mtrltemp.BERKVANT
"mtrltemp.BERKVANT" ? ">>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV2
/* Query rebuild information for BROWSE BRW_HLEV2
     _TblList          = "Temp-Tables.mtrlbertemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrlbertemp.Enr|yes"
     _FldNameList[1]   = Temp-Tables.mtrlbertemp.ENR
     _FldNameList[2]   > Temp-Tables.mtrlbertemp.BENAMNING
"mtrlbertemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "31" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrlbertemp.ENHET
"mtrlbertemp.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.mtrlbertemp.PRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.satstemp.ANTAL
"satstemp.ANTAL" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON ROW-LEAVE OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
   DISPLAY mtrltemp.BERKVANT WITH BROWSE BRW_HLEV.
   mtrltemp.BERKVANT = INPUT BROWSE BRW_HLEV mtrltemp.BERKVANT.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON VALUE-CHANGED OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
   status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrltemp.BERKVANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BERKVANT BRW_HLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrltemp.BERKVANT IN BROWSE BRW_HLEV /* Antal */
DO:   
   DISPLAY mtrltemp.BERKVANT WITH BROWSE BRW_HLEV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrltemp.BERKVANT BRW_HLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrltemp.BERKVANT IN BROWSE BRW_HLEV /* Antal */
DO:   
   mtrltemp.BERKVANT = INPUT BROWSE BRW_HLEV mtrltemp.BERKVANT.
   DISPLAY mtrltemp.BERKVANT WITH BROWSE BRW_HLEV.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV2
&Scoped-define SELF-NAME BRW_HLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV2 WINDOW-1
ON VALUE-CHANGED OF BRW_HLEV2 IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SATS
&Scoped-define SELF-NAME BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON ROW-LEAVE OF BRW_SATS IN FRAME FRAME-A /* Materielsatser */
DO:
   DISPLAY satstemp.ANTAL WITH BROWSE BRW_SATS. 
   satstemp.ANTAL = INPUT BROWSE BRW_SATS satstemp.ANTAL.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON VALUE-CHANGED OF BRW_SATS IN FRAME FRAME-A /* Materielsatser */
DO:
   status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME satstemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL satstemp.ANTAL BRW_SATS _BROWSE-COLUMN WINDOW-1
ON ENTRY OF satstemp.ANTAL IN BROWSE BRW_SATS /* Antal */
DO:
   DISPLAY satstemp.ANTAL WITH BROWSE BRW_SATS.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL satstemp.ANTAL BRW_SATS _BROWSE-COLUMN WINDOW-1
ON LEAVE OF satstemp.ANTAL IN BROWSE BRW_SATS /* Antal */
DO:
   satstemp.ANTAL = INPUT BROWSE BRW_SATS satstemp.ANTAL.  
   DISPLAY satstemp.ANTAL WITH BROWSE BRW_SATS.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:   
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE mtrlbertemp THEN DO:
      MESSAGE "Välj något!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {muswait.i}
   mtrl_rowid = mtrlbertemp.MTRLROW.   
   RUN kollmtrl_UI IN mtxtillyapph (INPUT mtrl_rowid,OUTPUT TABLE spec_mtrl,INPUT varalla, INPUT erslev).                   
   IF valgrupp NE ? THEN DO:
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = valgrupp NO-LOCK NO-ERROR.
      MESSAGE "Är det korrekt att Ni vill ta bort E-nummer:" + spec_mtrl.ENR + 
      " på alla ställen där det förekommer inom valda konstruktionsgrupper?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
   END.
   ELSE DO:
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      MESSAGE "Är det korrekt att Ni vill ta bort E-nummer:" + spec_mtrl.ENR + 
      " på alla ställen där det förekommer?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
   END.
   IF svar THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN ENRKONST4.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valgrupp, INPUT TABLE spec_mtrl, INPUT TABLE grupp_temp,INPUT varalla).
      END.
      ELSE DO:
         RUN ENRKONST4.P
         (INPUT valgrupp, INPUT TABLE spec_mtrl, INPUT TABLE grupp_temp,INPUT varalla).
      END.    
      DELETE mtrlbertemp.
      RUN selnextprevrow_UI IN brwproc[1]. 
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1]. 
      RUN title_UI IN brwproc[1].
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WINDOW-1
ON CHOOSE OF BTN_BYT IN FRAME FRAME-A /* Byt mtrl */
DO:   
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR. 
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE mtrlbertemp THEN DO:
      MESSAGE "Välj något materiel." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   mtrl_rowid = mtrlbertemp.MTRLROW.
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = mtrlbertemp.ENR
   spec_mtrl.BENAMNING = mtrlbertemp.BENAMNING
   spec_mtrl.ENHET = mtrlbertemp.ENHET
   spec_mtrl.NPRIS = mtrlbertemp.PRIS
   spec_mtrl.LEVKOD = mtrlbertemp.LEVKOD
   spec_mtrl.BERKVANT = 1
   spec_mtrl.SATS = FALSE.
  
   FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
   IF satsvar = FALSE THEN DO:       
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:
         mtrl_rowid = mtrltemp.MTRLROW.
         CREATE byt_mtrl.
         ASSIGN
         byt_mtrl.ENR = mtrltemp.ENR
         byt_mtrl.BENAMNING = mtrltemp.BENAMNING
         byt_mtrl.ENHET = mtrltemp.ENHET
         byt_mtrl.NPRIS = mtrltemp.NPRIS
         byt_mtrl.LEVKOD = mtrltemp.LEVKOD
         byt_mtrl.BERKVANT = 1
         byt_mtrl.SATS = FALSE.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt något materiel!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE satstemp THEN DO:
         mtrl_rowid = satstemp.SATSROW.
         CREATE byt_mtrl.
         ASSIGN
         byt_mtrl.ENR = satstemp.ENR
         byt_mtrl.BENAMNING = satstemp.BENAMNING
         byt_mtrl.ENHET = satstemp.ENHET
         byt_mtrl.NPRIS = satstemp.PRIS
         byt_mtrl.LEVKOD = satstemp.LEVKOD.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt något materiel!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.     
   IF byt_mtrl.ENR = spec_mtrl.ENR AND byt_mtrl.LEVKOD = spec_mtrl.LEVKOD THEN DO:
      MESSAGE "Du kan inte ersätta samma " LC(Guru.Konstanter:genk) "!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST byt_mtrl NO-LOCK NO-ERROR.
   IF valgrupp = ? THEN DO:            
      MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " leverantör " + spec_mtrl.LEVKOD +
      " med " + LC(Guru.Konstanter:genk) + ":" + byt_mtrl.ENR  + " leverantör " + byt_mtrl.LEVKOD + " på alla konstruktioner?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR  + " leverantör " + spec_mtrl.LEVKOD +
       " med " + LC(Guru.Konstanter:genk) + ":" + byt_mtrl.ENR  + " leverantör " + byt_mtrl.LEVKOD + " på alla valda konstruktionsgrupper?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:
      {muswait.i}
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN BYTXTY.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valgrupp, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl, INPUT TABLE grupp_temp,INPUT varalla, INPUT erslev).
      END.
      ELSE DO:
         RUN BYTXTY.P
         (INPUT valgrupp, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl, INPUT TABLE grupp_temp,INPUT varalla, INPUT erslev).
      END.
      enrspar = byt_mtrl.ENR.
      /*
      RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_lev,INPUT valgrupp,OUTPUT TABLE mtrlbertemp).
      */
      IF byt_mtrl.ENR = spec_mtrl.ENR THEN DO:
         DELETE mtrlbertemp.
         RUN selnextprevrow_UI IN brwproc[1]. 
         RUN refreshbrw_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1]. 
         RUN title_UI IN brwproc[1].
      END.
      ELSE DO:
         IF vald_levfran NE vald_levtill THEN DO:
            DELETE mtrlbertemp.
            RUN selnextprevrow_UI IN brwproc[1]. 
            RUN refreshbrw_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1]. 
            RUN title_UI IN brwproc[1].
         END.
         ELSE DO:
            
            RUN selnextprevrow_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].
            IF AVAILABLE mtrlbertemp THEN enrspar = mtrlbertemp.ENR.
            
            RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_levfran,INPUT TABLE grupp_temp,OUTPUT TABLE mtrlbertemp,INPUT varalla, INPUT erslev).
            IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
               /*nettopris beredning adm*/                
               EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
               RUN mbnettomark_UI IN nettoh (INPUT TABLE mtrlbertemp, INPUT-OUTPUT TABLE ikmtrltemp ).                  
            END.
            
            RUN openbdynspec_UI IN brwproc[1].             
            FIND FIRST mtrlbertemp WHERE mtrlbertemp.ENR = enrspar NO-LOCK NO-ERROR.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrlbertemp)).
            RUN lastselectdyn_UI IN brwproc[1].
            
            
         END.
      END.
      MESSAGE "Byte av artikel klart!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYTFIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTFIL WINDOW-1
ON CHOOSE OF BTN_BYTFIL IN FRAME FRAME-A /* Byt mtrl från fil */
DO:
      
   lev = INPUT CMB_LEV.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lev,6,20) USE-INDEX LEV NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
       tlev = levtemp.LEVKOD.
   END.      
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
    
   EMPTY TEMP-TABLE felemex NO-ERROR. 
   fildir = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den excelfil som Ni vill läsa in från"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                      
      {muswait.i}     
      EMPTY TEMP-TABLE felemex NO-ERROR.    
      RUN BYTENREXELIN.P (INPUT fildir,INPUT flev,input tlev, OUTPUT TABLE bytenrtemp ).
               
      RUN bytenrexcel_UI.
      
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].  
      RUN title_UI IN brwproc[1].
      FIND FIRST felemex NO-ERROR.
      IF AVAILABLE felemex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".  
         {AMERICANEUROPEAN.I}     
         OUTPUT TO VALUE(felfil).         
         PUT "Dessa översättningar blev ej inlästa från excel . Var god kontrollera!" AT 6.         
         PUT SKIP.         
         FOR EACH felemex:           
            PUT UNFORMATTED felemex.FLEV AT 6 felemex.FENR AT 10 felemex.TLEV AT 20 felemex.TENR AT 24.  
            PUT SKIP.
         END.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
   END.               
   {musarrow.i}  
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SENASTKOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SENASTKOP WINDOW-1
ON CHOOSE OF BTN_SENASTKOP IN FRAME FRAME-A /* Senast inköpt */
DO:
  {muswait.i}
  {AVBGOM.I} 
  RUN ENRSENASTKOP.W.
  {AVBFRAM.I}
  {musarrow.i}
  
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TILL WINDOW-1
ON CHOOSE OF BTN_TILL IN FRAME FRAME-A /* Lägg till mtrl */
DO:   
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR. 
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE mtrlbertemp THEN DO:
      MESSAGE "Välj något materiel." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   mtrl_rowid = mtrlbertemp.MTRLROW.
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = mtrlbertemp.ENR
   spec_mtrl.BENAMNING = mtrlbertemp.BENAMNING
   spec_mtrl.ENHET = mtrlbertemp.ENHET
   spec_mtrl.NPRIS = mtrlbertemp.PRIS
   spec_mtrl.LEVKOD = mtrlbertemp.LEVKOD
   spec_mtrl.BERKVANT = 1
   spec_mtrl.SATS = FALSE.
  
   FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
   IF satsvar = FALSE THEN DO: 
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:
         mtrl_rowid = mtrltemp.MTRLROW.
         CREATE byt_mtrl.
         ASSIGN
         byt_mtrl.ENR = mtrltemp.ENR
         byt_mtrl.BENAMNING = mtrltemp.BENAMNING
         byt_mtrl.ENHET = mtrltemp.ENHET
         byt_mtrl.NPRIS = mtrltemp.NPRIS
         byt_mtrl.LEVKOD = mtrltemp.LEVKOD
         byt_mtrl.BERKVANT = mtrltemp.BERKVANT
         byt_mtrl.SATS = FALSE.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt något materiel!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE DO:
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE satstemp THEN DO:
         mtrl_rowid = satstemp.SATSROW.
         CREATE byt_mtrl.
         ASSIGN
         byt_mtrl.ENR = satstemp.ENR
         byt_mtrl.BENAMNING = satstemp.BENAMNING
         byt_mtrl.ENHET = satstemp.ENHET
         byt_mtrl.NPRIS = satstemp.PRIS
         byt_mtrl.LEVKOD = satstemp.LEVKOD
         byt_mtrl.BERKVANT = satstemp.ANTAL.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt något materiel!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.     
   IF byt_mtrl.ENR = spec_mtrl.ENR THEN DO:
      MESSAGE "Du kan inte lägga till samma " LC(Guru.Konstanter:genk) "!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST byt_mtrl NO-LOCK NO-ERROR.
   IF valgrupp = ? THEN DO:            
      MESSAGE "Är det korrekt att Ni vill lägga till " + LC(Guru.Konstanter:genk) + ":" + byt_mtrl.ENR + " där " + LC(Guru.Konstanter:genk) + ":" +
      spec_mtrl.ENR + " finns på alla konstruktioner?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "Är det korrekt att Ni vill lägga till " + LC(Guru.Konstanter:genk) + ":" + byt_mtrl.ENR + " där " + LC(Guru.Konstanter:genk) + ":" +
      spec_mtrl.ENR + " finns på alla valda konstruktionsgrupper?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:
      {muswait.i}         
      RUN laggtill_UI IN mtxtillyapph (INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl, INPUT TABLE grupp_temp,INPUT varalla).                  
      {musarrow.i}
      enrspar = byt_mtrl.ENR.           
      RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_levfran,INPUT TABLE grupp_temp,OUTPUT TABLE mtrlbertemp,INPUT varalla, INPUT erslev).         
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning adm*/                
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
         RUN mbnettomark_UI IN nettoh (INPUT TABLE mtrlbertemp, INPUT-OUTPUT TABLE ikmtrltemp ).                           
      END.
      RUN openbdynspec_UI IN brwproc[1].             
      FIND FIRST mtrlbertemp WHERE mtrlbertemp.ENR = enrspar NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrlbertemp)).
      RUN lastselectdyn_UI IN brwproc[1].
      MESSAGE "Artikel har lagts till!"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Uppdat. katalog */
DO:
 
   IF leverant = "" THEN DO:
      MESSAGE "Markera den leverantör ni vill lägga in ny katalog för." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:   
      {muswait.i}  
      {AVBGOM.I}  
      {AMERICANEUROPEAN.I}            
      RUN UPPMTRLV.W.     
      {EUROPEANAMERICAN.I}     
      {AVBFRAM.I}     
      {musarrow.i}   
   END.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa / Uppdat antal */
DO:   
   
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE mtrlbertemp THEN DO:
      {muswait.i}
      mtrl_rowid = mtrlbertemp.MTRLROW.
      RUN kollmtrl_UI IN mtxtillyapph (INPUT mtrl_rowid,OUTPUT TABLE spec_mtrl,INPUT varalla, INPUT erslev).
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      {AVBGOM.I}     
      RUN ENRKONSTU3.W.     
      {AVBFRAM.I}
      {musarrow.i}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO: 
   ASSIGN           
   lev = INPUT CMB_LEV.   
   IF SUBSTRING(lev,1,5) = "Sats " THEN DO:
      ASSIGN                      
      BRW_HLEV:HIDDEN = TRUE               
      FILL-IN-ENR:HIDDEN = TRUE      
      BRW_SATS:HIDDEN = FALSE         
      FILL-IN-KOD:HIDDEN = FALSE 
      satsvar = TRUE.
      RUN setorgtitle_UI IN brwproc[3] (INPUT "Satslista " + lev).
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lev,6,20) USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE levtemp THEN DO:
         vald_levtill = levtemp.LEVKOD.
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE satstemp.LEVKOD = '" + vald_levtill + "' AND satstemp.SATS = TRUE").
         RUN openbdynspec_UI IN brwproc[3].                
      END.
   END.
   ELSE DO:
      ASSIGN                      
      BRW_HLEV:HIDDEN = FALSE           
      BRW_SATS:HIDDEN = TRUE
      FILL-IN-KOD:HIDDEN = TRUE
      satsvar = FALSE.
      IF lev = Guru.Konstanter:HuvudLeverantorName THEN DO:
         RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista huvudleverantör " + lev).         
      END.
      ELSE DO:
         RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + lev).                  
      END.  
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev USE-INDEX LEV NO-LOCK NO-ERROR.
      IF AVAILABLE levtemp THEN DO:
         vald_levtill = levtemp.LEVKOD. 
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + vald_levtill + "' AND mtrltemp.KALKNR = 0").
         RUN openbdynspec_UI IN brwproc[2].                
      END.
   END.  
  leverant = vald_levtill.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV2 WINDOW-1
ON VALUE-CHANGED OF CMB_LEV2 IN FRAME FRAME-A /* Leverantörer */
DO:  
   {muswait.i}
   ASSIGN           
   lev = INPUT CMB_LEV2.   
   ASSIGN                      
   BRW_HLEV2:HIDDEN = FALSE.
   IF lev = Guru.Konstanter:HuvudLeverantorName THEN DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista huvudleverantör " + lev).         
   END.
   ELSE DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista " + lev).                  
   END.  
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev USE-INDEX LEV NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
      vald_levfran = levtemp.LEVKOD.
      EMPTY TEMP-TABLE mtrlbertemp NO-ERROR.            
      RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_levfran,INPUT TABLE grupp_temp,OUTPUT TABLE mtrlbertemp,INPUT varalla, INPUT erslev).
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning adm*/                
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
         RUN mbnettomark_UI IN nettoh (INPUT TABLE mtrlbertemp, INPUT-OUTPUT TABLE ikmtrltemp ).                  
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE mtrlbertemp.LEVKOD = '" + vald_levfran + "' USE-INDEX ENR").
      RUN openbdynspec_UI IN brwproc[1].             
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Ben */
DO: 
   {muswait.i}
   ASSIGN
   RAD_SOK = INPUT RAD_SOK
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   sokstring[1] = FILL-IN-BEN.
   RUN sokoch_UI (sokstring[1]).
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      sokstring[1] = sokstring[1].
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      sokstring[1] = "*" + sokstring[1] + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      sokstring[1] = "*" + sokstring[1].
      begvar = FALSE.
   END.      
   felmedd = "".
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   EMPTY TEMP-TABLE satstemp NO-ERROR. 
   RUN benhmt_UI IN mtrlhmtapph (INPUT sokstring[1],INPUT satsvar,INPUT begvar,INPUT vald_levtill,
                 OUTPUT felmedd,OUTPUT TABLE mtrltemp,OUTPUT TABLE satstemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END. 
   ELSE DO:
      IF sokstring[2] = "" THEN .
      ELSE RUN sokmera_UI.
      IF satsvar = FALSE THEN DO:         
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + vald_levtill + "' AND mtrltemp.KALKNR = 0").
         RUN openbdynspec_UI IN brwproc[2].                
         FIND FIRST mtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.                 
      END.
      ELSE DO:
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE satstemp.LEVKOD = '" + vald_levtill + "' AND satstemp.SATS = TRUE").
         RUN openbdynspec_UI IN brwproc[3].                
         FIND FIRST satstemp NO-LOCK NO-ERROR.
         IF AVAILABLE satstemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(satstemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.
      END.
   END.
   {musarrow.i}                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-1
ON ANY-KEY OF FILL-IN-BEN2 IN FRAME FRAME-A /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN2 IN FRAME FRAME-A /* Ben */
DO: 
   ASSIGN
   RAD_SOK2 = INPUT RAD_SOK2
   FILL-IN-BEN2 = INPUT FILL-IN-BEN2.
   IF AVAILABLE mtrlbertemp THEN temprow = ROWID(mtrlbertemp).
   IF FILL-IN-BEN2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   
   IF RAD_SOK2 = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK2 = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK2 = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN2.
      begvar = FALSE.
   END.      
   status-ok = BRW_HLEV2:DESELECT-ROWS(). 
   IF begvar = FALSE THEN DO:
      FIND mtrlbertemp WHERE ROWID(mtrlbertemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrlbertemp WHERE mtrlbertemp.BENAMNING MATCHES aosok AND mtrlbertemp.LEVKOD = vald_levfran
      USE-INDEX KOD NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrlbertemp THEN DO:      
         FIND FIRST mtrlbertemp WHERE mtrlbertemp.BENAMNING MATCHES aosok AND mtrlbertemp.LEVKOD = vald_levfran
         USE-INDEX KOD NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrlbertemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.
   END.   
   ELSE DO:
      FIND mtrlbertemp WHERE ROWID(mtrlbertemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrlbertemp WHERE mtrlbertemp.BENAMNING BEGINS aosok AND mtrlbertemp.LEVKOD = vald_levfran
      USE-INDEX KOD NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrlbertemp THEN DO:      
         FIND FIRST mtrlbertemp WHERE mtrlbertemp.BENAMNING BEGINS aosok AND mtrlbertemp.LEVKOD = vald_levfran
         USE-INDEX KOD NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrlbertemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.      
   END.
   IF AVAILABLE mtrlbertemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrlbertemp)).
      RUN lastselectdyn_UI IN brwproc[1].
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO: 
   {muswait.i}
   {ENRHMT2.I}
   felmedd = "".
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   RUN enrhmt_UI IN mtrlhmtapph (INPUT posok,INPUT begvar,INPUT vald_levtill,
                                    OUTPUT felmedd,OUTPUT TABLE mtrltemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + vald_levtill + "' AND mtrltemp.KALKNR = 0").
      RUN openbdynspec_UI IN brwproc[2].                
      FIND FIRST mtrltemp NO-LOCK NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:      
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
         RUN lastselectdyn_UI IN brwproc[2].
      END.
   END.
   {musarrow.i}            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON ANY-KEY OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO: 
   ASSIGN
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2
   RAD_SOK2 = INPUT RAD_SOK2.
   IF FILL-IN-ENR2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE mtrlbertemp THEN DO:
      ASSIGN temprow = ROWID(mtrlbertemp).
      status-ok = BRW_HLEV2:DESELECT-ROWS().
   END.
   stjarnvar = INDEX(FILL-IN-ENR2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK2 = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-ENR2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK2 = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK2 = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR2.
      begvar = FALSE.
   END.
   IF begvar = FALSE THEN DO:
      FIND mtrlbertemp WHERE ROWID(mtrlbertemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrlbertemp WHERE mtrlbertemp.ENR MATCHES posok AND mtrlbertemp.LEVKOD = vald_levfran
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrlbertemp THEN DO:      
         FIND FIRST mtrlbertemp WHERE mtrlbertemp.ENR MATCHES posok AND mtrlbertemp.LEVKOD = vald_levfran
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrlbertemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.
   END.   
   ELSE DO:
      FIND mtrlbertemp WHERE ROWID(mtrlbertemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrlbertemp WHERE mtrlbertemp.ENR BEGINS posok AND mtrlbertemp.LEVKOD = vald_levfran
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrlbertemp THEN DO:      
         FIND FIRST mtrlbertemp WHERE mtrlbertemp.ENR BEGINS posok AND mtrlbertemp.LEVKOD = vald_levfran
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrlbertemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.      
   END.
   IF AVAILABLE mtrlbertemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrlbertemp)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON ANY-KEY OF FILL-IN-KOD IN FRAME FRAME-A /* Kod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME FRAME-A /* Kod */
DO: 
   {muswait.i}
   {ENRHMT3.I}
   felmedd = "".   
   EMPTY TEMP-TABLE satstemp NO-ERROR. 
   RUN kodhmt_UI IN mtrlhmtapph (INPUT posok,INPUT begvar,
                                    INPUT vald_levtill,OUTPUT felmedd,OUTPUT TABLE satstemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE satstemp.LEVKOD = '" + vald_levtill + "' AND satstemp.SATS = TRUE").
      RUN openbdynspec_UI IN brwproc[3].                
      FIND FIRST satstemp NO-LOCK NO-ERROR.
      IF AVAILABLE satstemp THEN DO:  
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(satstemp)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.
   END.        
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraHLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraHLEV WINDOW-1
ON CHOOSE OF MENU-ITEM m_AvmarkeraHLEV /* Avmarkera */
DO:
  RUN avmark_UI (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraHLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraHLEV2 WINDOW-1
ON CHOOSE OF MENU-ITEM m_AvmarkeraHLEV2 /* Avmarkera */
DO:
  RUN avmark_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sats_information
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sats_information WINDOW-1
ON CHOOSE OF MENU-ITEM m_Sats_information /* Satsinformation */
DO:
   {muswait.i}
   
   satstrue = TRUE.
   RUN SATSINNEU.W (INPUT ROWID(satstemp)).
   satstrue = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK WINDOW-1
ON VALUE-CHANGED OF RAD_SOK IN FRAME FRAME-A
DO:
   RAD_SOK = INPUT RAD_SOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SOK2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK2 WINDOW-1
ON VALUE-CHANGED OF RAD_SOK2 IN FRAME FRAME-A
DO:
  RAD_SOK2 = INPUT RAD_SOK2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-A
DO:
   IF mtrltemp.BERKVANT:VISIBLE IN BROWSE BRW_HLEV = TRUE THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
         APPLY "LEAVE" TO mtrltemp.BERKVANT IN BROWSE BRW_HLEV.
         mtrltemp.BERKVANT:VISIBLE IN BROWSE BRW_HLEV = FALSE.
      END.
      ELSE DO:
         IF AVAILABLE satstemp THEN DO:
            APPLY "LEAVE" TO satstemp.ANTAL IN BROWSE BRW_SATS.
            satstemp.ANTAL:VISIBLE IN BROWSE BRW_SATS = FALSE.
         END.
      END.
   END.
   ASSIGN
   satstemp.BENAMNING:WIDTH IN BROWSE BRW_SATS = nettobreddsats 
   mtrltemp.NPRIS:WIDTH IN BROWSE BRW_HLEV = nettobredd 
   satstemp.ANTAL:VISIBLE IN BROWSE BRW_SATS = FALSE
   mtrltemp.BERKVANT:VISIBLE IN BROWSE BRW_HLEV = FALSE
   CMB_LEV:HIDDEN = FALSE 
   BRW_HLEV:HIDDEN = FALSE
   BTN_BYT:HIDDEN = TRUE
   BTN_TILL:HIDDEN = TRUE
   BTN_BORT:HIDDEN = TRUE. 
   
   IF SUBSTRING(CMB_LEV,1,5) = "Sats " THEN DO:
      ASSIGN
      BRW_SATS:HIDDEN = FALSE
      BRW_HLEV:HIDDEN = TRUE.
   END.
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:
      BTN_BYT:HIDDEN = FALSE.      
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      ASSIGN
      satstemp.BENAMNING:WIDTH IN BROWSE BRW_SATS = nettobreddsatsorg 
      mtrltemp.NPRIS:WIDTH IN BROWSE BRW_HLEV = nettobreddorg 
      satstemp.ANTAL:VISIBLE IN BROWSE BRW_SATS = TRUE
      mtrltemp.BERKVANT:VISIBLE IN BROWSE BRW_HLEV = TRUE
      BTN_TILL:HIDDEN = FALSE.      
   END.
   ELSE DO:
      ASSIGN
      CMB_LEV:HIDDEN = TRUE
      BRW_SATS:HIDDEN = TRUE
      BRW_HLEV:HIDDEN = TRUE
      BTN_BORT:HIDDEN = FALSE.      
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtxtillyapph) THEN DELETE PROCEDURE mtxtillyapph.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh NO-ERROR.      
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
   IF valgrupp = ? THEN DO:
      {&WINDOW-NAME}:TITLE = "Byte av artikel på ALLA konstruktioner".      
   END.
   ELSE DO:
      kgrvar = "".
      FOR EACH grupp_temp:
         kgrvar = kgrvar + " " + grupp_temp.BENAMNING.
      END.
      FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = valgrupp 
      NO-LOCK NO-ERROR.
      {&WINDOW-NAME}:TITLE = "Byte av artikel på valda konstruktionsgrupper: " + kgrvar.
   END.  
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR.
   EMPTY TEMP-TABLE mtrlbertemp NO-ERROR.
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   EMPTY TEMP-TABLE satstemp NO-ERROR.
   EMPTY TEMP-TABLE ikmtrltemp NO-ERROR.
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.
   ASSIGN
   mtrltemp.BERKVANT:LABEL IN BROWSE BRW_HLEV = "Antal"
   BTN_BYT:LABEL = "Ersätt mtrl"
   BTN_TILL:LABEL = "Lägg till mtrl"
   BTN_BORT:LABEL = "Ta bort mtrl"
   mtrlbertemp.ENR:LABEL IN BROWSE BRW_HLEV2 = Guru.Konstanter:genk
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk.  
   ASSIGN
   status-ok = CMB_LEV:DELETE("0")
   status-ok = CMB_LEV2:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN
   status-ok = CMB_LEV2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV2:SCREEN-VALUE = levtemp.LEVNAMN.
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.      
      status-ok = CMB_LEV2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.      
   END.        
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND
   levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      IF AVAILABLE huvlevtemp AND  huvlevtemp.LEVKOD = levtemp.LEVKOD THEN status-ok = CMB_LEV:ADD-LAST("Sats " + levtemp.LEVNAMN) IN FRAME {&FRAME-NAME}.
      ELSE IF levtemp.LEVKOD = vald_kundlev THEN status-ok = CMB_LEV:ADD-LAST("Sats " + levtemp.LEVNAMN) IN FRAME {&FRAME-NAME}.
            
   END.        
   ASSIGN 
   CMB_LEV = INPUT CMB_LEV
   CMB_LEV2 = INPUT CMB_LEV2
   vald_levtill = vald_kundlev
   vald_levfran = vald_kundlev
   bestant = FALSE.
   RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_levfran,INPUT TABLE grupp_temp,OUTPUT TABLE mtrlbertemp,INPUT varalla, INPUT erslev).
   FIND FIRST mtrlbertemp NO-LOCK NO-ERROR.
   IF AVAILABLE mtrlbertemp THEN DO:
       IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning adm*/                
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
         RUN mbnettomark_UI IN nettoh (INPUT TABLE mtrlbertemp, INPUT-OUTPUT TABLE ikmtrltemp ).                  
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE mtrlbertemp.LEVKOD = '" + vald_levfran + "' USE-INDEX ENR").
      RUN openbdynspec_UI IN brwproc[1].        
   END.
   RUN enable_UI.   
   {FRMSIZE.I}         
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_VISA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).            
   Guru.GlobalaVariabler:colrighth = BTN_BYT:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   Guru.GlobalaVariabler:colrighth = BTN_UPP:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SENASTKOP:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_BYTFIL:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   ASSIGN
   BTN_TILL:ROW = BTN_BYT:ROW
   BTN_BORT:ROW = BTN_BYT:ROW   
   nettobreddsats = satstemp.BENAMNING:WIDTH IN BROWSE BRW_SATS
   nettobreddorg = mtrltemp.NPRIS:WIDTH IN BROWSE BRW_HLEV.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   APPLY "VALUE-CHANGED" TO CMB_LEV.
   APPLY "VALUE-CHANGED" TO CMB_LEV2.
   ASSIGN      
   nettobredd = mtrltemp.NPRIS:WIDTH IN BROWSE BRW_HLEV
   nettobreddsatsorg = satstemp.BENAMNING:WIDTH IN BROWSE BRW_SATS
   FILL-IN-KOD:HIDDEN = TRUE. 
   BTN_BYTFIL:HIDDEN = TRUE.
   {musarrow.i}  
   {WIN_M_SLUT.I}
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
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_HLEV2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).      
   IF Guru.Konstanter:globnetprissortvar = 1 THEN  RUN setcolindex_UI IN brwproc[2] (INPUT "kund DESCENDING by enr").   
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_SATS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[3] (INPUT THIS-PROCEDURE).       
   RUN settitlenum_UI IN brwproc[1] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[3] (INPUT TRUE).

   IF Guru.Konstanter:appcon THEN DO:
      RUN MTXTILLYAPP.P PERSISTENT SET mtxtillyapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTXTILLYAPP.P PERSISTENT SET mtxtillyapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmark_UI WINDOW-1 
PROCEDURE avmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER vber AS INTEGER NO-UNDO.
   IF vber = 1 THEN DO:
      status-ok = BRW_HLEV:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   END.
   IF vber = 2 THEN DO:
      status-ok = BRW_HLEV2:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytenrexcel_UI WINDOW-1 
PROCEDURE bytenrexcel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE delubytenrtemp NO-ERROR.
   EMPTY TEMP-TABLE delmbytenrtemp NO-ERROR. 
   RUN kollenr_UI IN mtxtillyapph ( INPUT-OUTPUT TABLE bytenrtemp, OUTPUT TABLE  delmbytenrtemp ).
   
   FOR EACH bytenrtemp:        
      FIND FIRST mtrlbertemp WHERE mtrlbertemp.ENR = bytenrtemp.FENR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mtrlbertemp THEN DO:
         CREATE delubytenrtemp.
         BUFFER-COPY bytenrtemp TO delubytenrtemp.
         DELETE bytenrtemp.
      END. 
      ELSE DO:
         bytenrtemp.FLEV = mtrlbertemp.LEVKOD.
      END.     
   END.       
      
   EMPTY TEMP-TABLE bytenrtemp NO-ERROR.
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
  DISPLAY RAD_VAL CMB_LEV2 CMB_LEV FILL-IN-ENR2 FILL-IN-KOD FILL-IN-ENR 
          FILL-IN-BEN2 FILL-IN-BEN RAD_SOK2 RAD_SOK FILL-IN-SOKALT 
          FILL-IN-SOKALT-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-3 RECT-4 IMAGE-10 IMAGE-11 BRW_HLEV2 BRW_HLEV BRW_SATS RAD_VAL 
         BTN_VISA BTN_BYT BTN_TILL BTN_BORT BTN_BYTFIL BTN_UPP BTN_SENASTKOP 
         CMB_LEV2 CMB_LEV FILL-IN-ENR2 FILL-IN-KOD FILL-IN-ENR FILL-IN-BEN2 
         FILL-IN-BEN RAD_SOK2 RAD_SOK BTN_AVB FILL-IN-SOKALT FILL-IN-SOKALT-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_HLEV" THEN valbrw = 1.  
   IF brwname = "BRW_HLEV2" THEN valbrw = 2. 
   IF brwname = "BRW_SATS" THEN valbrw = 3.   
   IF valbrw = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 2 THEN DO:
      status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrlbertemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrlbertemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 3 THEN DO:
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = satstemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = satstemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kommandoskap_UI WINDOW-1 
PROCEDURE kommandoskap_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT  PARAMETER sokmera AS CHARACTER NO-UNDO.
   DEFINE VARIABLE extrafalth5 AS HANDLE NO-UNDO.
   DEFINE VARIABLE faltvar AS CHARACTER NO-UNDO.
   faltvar = "benamning".
   valdkommandoquery = "".
   IF sokstring[1] NE "" THEN DO:
      extrafalth5 = tempbufh:BUFFER-FIELD(faltvar).
      IF begvar = FALSE THEN DO:
         IF extrafalth5:DATA-TYPE = "CHARACTER" THEN DO:
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES '" + sokmera + "'".
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES '" + sokmera + "' AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "INTEGER" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "LOGICAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.          
         ELSE IF extrafalth5:DATA-TYPE = "DECIMAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.          
      END.
      ELSE DO:
         IF extrafalth5:DATA-TYPE = "CHARACTER" THEN DO:
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS '" + sokmera + "'".
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS '" + sokmera + "' AND " +  nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "INTEGER" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "LOGICAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.       
         ELSE IF extrafalth5:DATA-TYPE = "DECIMAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.       
      END.
      DELETE OBJECT extrafalth5 NO-ERROR. 
      extrafalth5 = ?.
   END.
   ELSE DO:
      valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + nyaquery.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-1 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_HLEV" THEN RUN rowdispmtrl_UI (INPUT 1).    
   IF brwh:NAME = "BRW_HLEV2" THEN RUN rowdispmtrl_UI (INPUT 2).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI WINDOW-1 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   
   IF vad = 1 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
         IF mtrltemp.KUND = TRUE THEN DO: 
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28].
         END.
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38].
         END.
      END.
   END.
   IF vad = 2 THEN DO:
      IF AVAILABLE mtrlbertemp THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrlbertemp.LEVKOD AND ikmtrltemp.ENR = mtrlbertemp.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:        
            ASSIGN
            mtrlbertemp.ENR:BGCOLOR IN BROWSE BRW_HLEV2 = Guru.Konstanter:varforetypval[28].                   
         END.
         ELSE DO:         
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrlbertemp.LEVKOD AND ikmtrltemp.ENR = mtrlbertemp.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:        
               ASSIGN
               mtrlbertemp.ENR:BGCOLOR IN BROWSE BRW_HLEV2 = Guru.Konstanter:varforetypval[38].                   
            END.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokmera_UI WINDOW-1 
PROCEDURE sokmera_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DELETE WIDGET-POOL "dynpool" NO-ERROR.
   IF satsvar = FALSE THEN tempbufh = TEMP-TABLE mtrltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   ELSE tempbufh = TEMP-TABLE satstemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(tempbufh).
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia").
   tempbufmerah = ttcopyh:DEFAULT-BUFFER-HANDLE.
   nyaquery = "".
   rkollvar = 2.
   REPEAT:       
      orgbufh = tempbufh.
      IF sokstring[rkollvar] = "" THEN LEAVE.
      RUN starwars_UI (INPUT sokstring[rkollvar]).
      IF begvar = ? THEN LEAVE.
      RUN kommandoskap_UI (INPUT sokstring[rkollvar]).
      /* Skapar query och kopplar queryn till den skarpa tabellens buffer */
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(orgbufh).
      appqueh:QUERY-PREPARE(valdkommandoquery).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      IF appqueh:QUERY-OFF-END = TRUE THEN DO:
         orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
         felmedd = "Det finns inget på sökbegreppet.". 
         LEAVE.     
      END.
      ELSE DO:
         tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
         DO WHILE appqueh:QUERY-OFF-END = FALSE:
            tempbufmerah:BUFFER-CREATE().
            tempbufmerah:BUFFER-COPY(orgbufh) NO-ERROR.
            appqueh:GET-NEXT(NO-LOCK).
         END.         
      END.
      orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(tempbufmerah).
      appqueh:QUERY-PREPARE("FOR EACH " + tempbufmerah:TABLE).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      DO WHILE appqueh:QUERY-OFF-END = FALSE:
         orgbufh:BUFFER-CREATE().
         orgbufh:BUFFER-COPY(tempbufmerah) NO-ERROR.
         appqueh:GET-NEXT(NO-LOCK).             
      END.      
      tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokoch_UI WINDOW-1 
PROCEDURE sokoch_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT  PARAMETER sokstringorg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE forsta AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sista AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   IF INDEX(sokstringorg,"&&") = 0  THEN RETURN.
   IF SUBSTRING(sokstringorg,1,1) = "*" THEN  forsta = "*".
   IF SUBSTRING(sokstringorg,LENGTH(sokstringorg),1) = "*" THEN  sista = "*".
   sokstringorg = REPLACE(sokstringorg,"*","").
   rkollvar = 1.
   kollvar = 1.
   REPEAT: 
      IF kollvar >= LENGTH(sokstringorg) THEN LEAVE.
      IF rkollvar > 1 THEN DO:
         ASSIGN
         forsta = "*"
         sista = "*".
      END.   
      IF INDEX(sokstringorg,"&&",kollvar) = 0 THEN DO: 
         sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar) + sista).
         LEAVE.
      END.
      sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar,INDEX(sokstringorg,"&&",kollvar) - kollvar) + sista).
      kollvar = INDEX(sokstringorg,"&&",kollvar) + 2.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE starwars_UI WINDOW-1 
PROCEDURE starwars_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT  PARAMETER stringkoll AS CHARACTER NO-UNDO.
   IF INDEX(stringkoll,"*",1) = 0 THEN DO:
      begvar = TRUE.
      RETURN.
   END.
   IF SUBSTRING(stringkoll,1,1) = "*" THEN DO:
      begvar = FALSE.
      RETURN.      
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

