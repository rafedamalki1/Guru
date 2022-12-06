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
{ALLDEF.I}
{EXTRADATA.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{SCADMIN.I}
{WHANDLTEMP.I}  

DEFINE NEW SHARED VARIABLE schapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE ejanv AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE ingenkod AS LOGICAL NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
DEFINE VARIABLE schdynh AS HANDLE NO-UNDO.
DEFINE VARIABLE tabellhandle AS HANDLE NO-UNDO.
DEFINE VARIABLE vstartad AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_SKRIVVAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES skrivhdschakttemp hdschakttemp

/* Definitions for BROWSE BRW_SKRIVVAL                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_SKRIVVAL skrivhdschakttemp.SID ~
skrivhdschakttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SKRIVVAL 
&Scoped-define QUERY-STRING-BRW_SKRIVVAL FOR EACH skrivhdschakttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SKRIVVAL OPEN QUERY BRW_SKRIVVAL FOR EACH skrivhdschakttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SKRIVVAL skrivhdschakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SKRIVVAL skrivhdschakttemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL hdschakttemp.SID ~
hdschakttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL hdschakttemp.BENAMNING 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VAL hdschakttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VAL hdschakttemp
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH hdschakttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH hdschakttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL hdschakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL hdschakttemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL MBTN_SPARA BTN_OK BRW_VAL ~
BRW_SKRIVVAL MBTN_PROFIL BTN_OVER BTN_AVB BTN_MANSCBORT BTN_MANNY BTN_BACK ~
TOG_SORTRUB TOG_SPROFIL MBTN_RAKNA BTN_IMPSTRACK BTN_MANSTRACK ~
BTN_MANSTRBORT BTN_MANRUBRIK CMB_RUBRIK BTN_BORTRUBRIK BTN_VISA ~
BTN_VISAMARK BTN_SKRIV BTN_EXCEL MBTN_HAND MBTN_KALK MBTN_KORD MBTN_SCH ~
FILL-IN_AONR FILL-IN_DATUM 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL TOG_SORTRUB TOG_SPROFIL CMB_RUBRIK ~
FILL-IN_AONR FILL-IN_DATUM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "Ta bort" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BORTRUBRIK 
     LABEL "Ta Bort Rubrik" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "excel" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_IMPSTRACK 
     LABEL "Imp.Sträcka från Bered." 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_MANNY 
     LABEL "Nytt Schakt" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MANRUBRIK 
     LABEL "Ny Rubrik" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_MANSCBORT 
     LABEL "Ta Bort Schakt" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MANSTRACK 
     LABEL "Ny Sträcka" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_MANSTRBORT 
     LABEL "Ta Bort Sträcka" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "OK" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "Skapa" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_SKRIV 
     LABEL "SKRIV UT" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 15 BY 1.

DEFINE BUTTON BTN_VISAMARK 
     LABEL "Visa markerade sträckor" 
     SIZE 15 BY 1.

DEFINE BUTTON MBTN_HAND  NO-FOCUS
     LABEL "Btn_sch 2" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_KALK  NO-FOCUS
     LABEL "Btn_sch 4" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_KORD  NO-FOCUS
     LABEL "Btn_sch 3" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_PROFIL  NO-FOCUS
     LABEL "Profil" 
     SIZE 7.5 BY 2.33 TOOLTIP "Schaktprofiler".

DEFINE BUTTON MBTN_RAKNA  NO-FOCUS
     LABEL "Btn_sch 5" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_SCH  NO-FOCUS
     LABEL "Button 1" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_SPARA 
     LABEL "Snabbspara" 
     SIZE 7.5 BY 2.33.

DEFINE VARIABLE CMB_RUBRIK AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 62.13 BY .63
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "9999/99/99":U 
     LABEL "Datum" 
      VIEW-AS TEXT 
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Schaktprotokoll", 1,
"Händelser", 2
     SIZE 45 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_SORTRUB AS LOGICAL INITIAL no 
     LABEL "Rubriker sorterade i bokstavsordning" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .79 TOOLTIP "Sortering i bokstavsordning eller i Förlaggning, Händelse, Ytbelägg" NO-UNDO.

DEFINE VARIABLE TOG_SPROFIL AS LOGICAL INITIAL no 
     LABEL "Schaktprofiler" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .79 TOOLTIP "Automatisk beräkning av volym med hjälp av schaktprofiler" NO-UNDO.

DEFINE BUTTON BTN_ANDKALK 
     LABEL "Ändra" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORTKALK 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_GENKALK 
     LABEL "Generera kalkyl" 
     SIZE 12 BY 1 TOOLTIP "Skapa en ny kalkyl för visat schakt. Alla ändringar tas bort!"
     BGCOLOR 8 .

DEFINE BUTTON BTN_GENKALKV 
     LABEL "Generera kalkyl för valda schakt!" 
     SIZE 22 BY 1 TOOLTIP "Skapa en ny kalkyl för valda schakt.Alla ändringar tas bort!"
     BGCOLOR 8 .

DEFINE BUTTON BTN_GRUNDUPP 
     LABEL "Hämta Kalkylkoder" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORTPROFIL 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_NYPROFIL 
     LABEL "Skapa Profil" 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-BB AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Bottenbredd i cm" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DJM AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Djup för massor i %" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Djup för utbyte av massor i %" NO-UNDO.

DEFINE VARIABLE FILL-IN-DJP AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Packdjup i cm" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROCENTTK AS CHARACTER FORMAT "X(256)":U INITIAL "%" 
      VIEW-AS TEXT 
     SIZE 3 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TB AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Toppbredd i cm" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_SKRIVVAL FOR 
      skrivhdschakttemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      hdschakttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_SKRIVVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SKRIVVAL C-Win _STRUCTURED
  QUERY BRW_SKRIVVAL NO-LOCK DISPLAY
      skrivhdschakttemp.SID COLUMN-LABEL "Nr" FORMAT ">>>9":U
      skrivhdschakttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 30 BY 8
         TITLE "Valda Schakt" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      hdschakttemp.SID COLUMN-LABEL "Nr" FORMAT ">>>9":U
      hdschakttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
  ENABLE
      hdschakttemp.BENAMNING
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 30 BY 8
         TITLE "Schakt" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_VAL AT ROW 1.21 COL 65.5 NO-LABEL
     MBTN_SPARA AT ROW 1.25 COL 102.63
     BTN_OK AT ROW 1.25 COL 110.5
     BRW_VAL AT ROW 1.83 COL 34.38
     BRW_SKRIVVAL AT ROW 1.83 COL 69.75
     MBTN_PROFIL AT ROW 1.25 COL 26.38 WIDGET-ID 20
     BTN_OVER AT ROW 2.5 COL 65.25
     BTN_AVB AT ROW 2.5 COL 110.5
     BTN_MANSCBORT AT ROW 4 COL 7
     BTN_MANNY AT ROW 4 COL 20
     BTN_BACK AT ROW 4 COL 65.25
     TOG_SORTRUB AT ROW 5.75 COL 1.88 WIDGET-ID 10
     TOG_SPROFIL AT ROW 5.75 COL 3 WIDGET-ID 16
     MBTN_RAKNA AT ROW 1.25 COL 26.38 WIDGET-ID 8
     BTN_IMPSTRACK AT ROW 27 COL 2.25 WIDGET-ID 12
     BTN_MANSTRACK AT ROW 28.21 COL 2
     BTN_MANSTRBORT AT ROW 28.21 COL 17.25
     BTN_MANRUBRIK AT ROW 28.21 COL 32.5
     CMB_RUBRIK AT ROW 28.21 COL 47.75 NO-LABEL WIDGET-ID 6
     BTN_BORTRUBRIK AT ROW 28.21 COL 64 WIDGET-ID 4
     BTN_VISA AT ROW 28.21 COL 79.25 WIDGET-ID 2
     BTN_VISAMARK AT ROW 28.21 COL 79.25 WIDGET-ID 14
     BTN_SKRIV AT ROW 28.21 COL 94.63
     BTN_EXCEL AT ROW 28.21 COL 110
     MBTN_HAND AT ROW 1.25 COL 9.13
     MBTN_KALK AT ROW 1.25 COL 21.5
     MBTN_KORD AT ROW 1.25 COL 16.88
     MBTN_SCH AT ROW 1.25 COL 1.5
     FILL-IN_AONR AT ROW 1 COL 51.5 NO-LABEL
     FILL-IN_DATUM AT ROW 3.88 COL 107.75 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 28.38.

DEFINE FRAME FRAME-PROFIL
     FILL-IN-BB AT ROW 1.25 COL 20 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-TB AT ROW 2.5 COL 20 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-DJP AT ROW 3.75 COL 20 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-DJM AT ROW 5 COL 20 COLON-ALIGNED WIDGET-ID 12
     BTN_NYPROFIL AT ROW 5 COL 63 WIDGET-ID 2
     BTN_BORTPROFIL AT ROW 19.75 COL 63 WIDGET-ID 4
     FILL-IN-PROCENTTK AT ROW 5.25 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     "profiler kan det" VIEW-AS TEXT
          SIZE 21.5 BY 1.5 AT ROW 11.5 COL 54 WIDGET-ID 18
          FGCOLOR 12 FONT 17
     "påverka andra" VIEW-AS TEXT
          SIZE 21.5 BY 1.5 AT ROW 13.25 COL 54 WIDGET-ID 20
          FGCOLOR 12 FONT 17
     "beredningar!" VIEW-AS TEXT
          SIZE 21.5 BY 1.5 AT ROW 15 COL 54 WIDGET-ID 22
          FGCOLOR 12 FONT 17
     "Obs! Vid ändring av" VIEW-AS TEXT
          SIZE 21.5 BY 1.5 AT ROW 9.75 COL 54 WIDGET-ID 16
          FGCOLOR 12 FONT 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24.25 ROW 6.71
         SIZE 76 BY 21.33 WIDGET-ID 100.

DEFINE FRAME FRAME-KOORD
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 6.71
         SIZE 124.75 BY 21.33.

DEFINE FRAME FRAME-HAND
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 6.71
         SIZE 124.25 BY 21.33.

DEFINE FRAME FRAME-SCHAKT
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 6.71
         SIZE 124.75 BY 19.79.

DEFINE FRAME FRAME-KALK
     BTN_ANDKALK AT ROW 19.75 COL 49
     BTN_GRUNDUPP AT ROW 19.75 COL 63.5 WIDGET-ID 2
     BTN_BORTKALK AT ROW 19.75 COL 63.63
     BTN_GENKALK AT ROW 19.75 COL 78.25
     BTN_GENKALKV AT ROW 19.75 COL 91.5 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 6.71
         SIZE 124.75 BY 19.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: hdschakttemp T "?" NO-UNDO temp-db hdschakttemp
      TABLE: skrivhdschakttemp T "?" NO-UNDO temp-db skrivhdschakttemp
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-HAND:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-KALK:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-KOORD:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-PROFIL:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-SCHAKT:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-KALK:MOVE-AFTER-TAB-ITEM (TOG_SPROFIL:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-PROFIL:MOVE-BEFORE-TAB-ITEM (BTN_IMPSTRACK:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-HAND:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-PROFIL:HANDLE)
       XXTABVALXX = FRAME FRAME-SCHAKT:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-HAND:HANDLE)
       XXTABVALXX = FRAME FRAME-KOORD:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-SCHAKT:HANDLE)
       XXTABVALXX = FRAME FRAME-KALK:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-KOORD:HANDLE)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BRW_VAL BTN_OK DEFAULT-FRAME */
/* BROWSE-TAB BRW_SKRIVVAL BRW_VAL DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CMB_RUBRIK IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_AONR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN_AONR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_DATUM:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME FRAME-HAND
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-HAND:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-KALK
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-KALK:HIDDEN           = TRUE.

ASSIGN 
       BTN_ANDKALK:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_BORTKALK:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_GENKALK:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_GENKALKV:HIDDEN IN FRAME FRAME-KALK           = TRUE.

ASSIGN 
       BTN_GRUNDUPP:HIDDEN IN FRAME FRAME-KALK           = TRUE.

/* SETTINGS FOR FRAME FRAME-KOORD
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-KOORD:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-PROFIL
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-PROFIL:HIDDEN           = TRUE.

ASSIGN 
       BTN_BORTPROFIL:HIDDEN IN FRAME FRAME-PROFIL           = TRUE.

ASSIGN 
       BTN_NYPROFIL:HIDDEN IN FRAME FRAME-PROFIL           = TRUE.

ASSIGN 
       FILL-IN-PROCENTTK:HIDDEN IN FRAME FRAME-PROFIL           = TRUE
       FILL-IN-PROCENTTK:READ-ONLY IN FRAME FRAME-PROFIL        = TRUE.

/* SETTINGS FOR FRAME FRAME-SCHAKT
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-SCHAKT:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SKRIVVAL
/* Query rebuild information for BROWSE BRW_SKRIVVAL
     _TblList          = "Temp-Tables.skrivhdschakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.skrivhdschakttemp.SID
"skrivhdschakttemp.SID" "Nr" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.skrivhdschakttemp.BENAMNING
"skrivhdschakttemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SKRIVVAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.hdschakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.hdschakttemp.SID
"hdschakttemp.SID" "Nr" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.hdschakttemp.BENAMNING
"hdschakttemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-HAND
/* Query rebuild information for FRAME FRAME-HAND
     _Query            is NOT OPENED
*/  /* FRAME FRAME-HAND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KALK
/* Query rebuild information for FRAME FRAME-KALK
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KOORD
/* Query rebuild information for FRAME FRAME-KOORD
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KOORD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-PROFIL
/* Query rebuild information for FRAME FRAME-PROFIL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-PROFIL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-SCHAKT
/* Query rebuild information for FRAME FRAME-SCHAKT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-SCHAKT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Guru-Projekthuvud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
   IF ejanv = TRUE THEN RETURN NO-APPLY.
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
  IF ejanv = TRUE THEN RETURN NO-APPLY.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   Guru.GlobalaVariabler:NyBerKalkyl = FALSE.
   RUN KollVolymStart_UI IN schdynh (OUTPUT vstartad).
   IF vstartad = TRUE THEN. 
   ELSE DO:   
      IF ejanv = TRUE THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RETURN NO-APPLY.
      END.   
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO BTN_OK.         
     END.
     ELSE IF NOT svar THEN DO:          
     END.                    
     ELSE DO:
        RETURN NO-APPLY.
     END.    
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  
   Guru.GlobalaVariabler:NyBerKalkyl = FALSE.
   RUN KollVolymStart_UI IN schdynh (OUTPUT vstartad).
   IF vstartad = TRUE THEN. 
   ELSE RUN sparplan_UI (INPUT TRUE).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SKRIVVAL
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
   IF VALID-HANDLE(schdynh) THEN RUN avs_UI IN schdynh.
   {BORTBRWPROC.I}   
   IF VALID-HANDLE(schdynh) THEN DO:
      RUN releaseh_UI IN schdynh.
      DELETE PROCEDURE schdynh NO-ERROR.  
   END.
   IF VALID-HANDLE(schapph) THEN DO:
      RUN DelPool_UI IN schapph.
      RUN BerkoppbufAvs_UI IN schapph.
      DELETE PROCEDURE schapph NO-ERROR.  
   END.   
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
   FIND FIRST hdschakttemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hdschakttemp THEN DO:      
   END.
   FIND FIRST hdschakprottemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hdschakprottemp THEN DO:      
   END.
   RAD_VAL = 1.
   RAD_VAL:ADD-LAST("Koordinater",3).
   RAD_VAL:ADD-LAST("Kalkyl",4).
   RAD_VAL:ADD-LAST("volym",5).
   RAD_VAL:ADD-LAST("profil",6).
   {&WINDOW-NAME}:TITLE = "".
   {BERTITLE.I}
   ASSIGN
   FILL-IN_DATUM = TODAY
   FILL-IN_AONR = {&WINDOW-NAME}:TITLE.
   {&WINDOW-NAME}:TITLE = "Schaktprotokoll".
   BTN_MANNY:TOOLTIP = "Lägg upp ett nytt schakt. Lägg sedan till de rubriker som skall finnas med, samt ingående sträckor.".
   BTN_MANSCBORT:TOOLTIP = "Ta bort markerat schakt med ingående rubriker och sträckor.".
   BTN_MANRUBRIK:TOOLTIP = "Lägg upp den/de rubriker som skall vara med i aktuellt schakt.".
   BTN_BORTRUBRIK:TOOLTIP = "Ta bort vald rubrik i valt schakt.".
   BTN_MANSTRACK:TOOLTIP = "Lägg upp den/de sträckor som skall vara med i valt schakt.".
   BTN_MANSTRBORT:TOOLTIP = "Ta bort markerad sträcka i valt schakt.".
   BTN_IMPSTRACK:TOOLTIP = "Importera sträckor från Beredningskonstruktioner.".   
   ingenkod = FALSE.
   RUN enable_UI.   
   FRAME FRAME-SCHAKT:HIDDEN = FALSE.  
   {FRMSIZE.I}
   RUN getstart_UI.
   IF NOT AVAILABLE hdschakttemp THEN DO: /* skapa nytt schakt om det inte finns något*/
      RUN mannyschakt_UI IN schdynh.
   END.    
   ASSIGN
   BTN_GENKALKV:ROW = BTN_ANDKALK:ROW
   BTN_GENKALK:ROW = BTN_ANDKALK:ROW
   Guru.GlobalaVariabler:collefth = BTN_GENKALKV:HANDLE.   
   BTN_IMPSTRACK:ROW = BTN_MANSTRACK:ROW.     
   RUN startschakt_UI IN schdynh.
   RUN manknapp_UI IN schdynh.
   APPLY 'VALUE-CHANGED' TO BRW_VAL.   
   Guru.GlobalaVariabler:StartRadForKnappar = FILL-IN_DATUM:ROW. 
   Guru.Konstanter:LabelFlytt(FILL-IN_DATUM:HANDLE).
   {musarrow.i}
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
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
      RUN SCHAKTPROAPP.P PERSISTENT SET schapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:varforetypchar[48], INPUT Guru.Konstanter:globanv). 
   END.
   ELSE DO:
      RUN SCHAKTPROAPP.P PERSISTENT SET schapph (INPUT Guru.Konstanter:varforetypchar[48], INPUT Guru.Konstanter:globanv). 
   END.
   RUN hamtpro_UI IN schapph (INPUT INTEGER(valaonr),INPUT valomrade,OUTPUT TABLE hdschakprottemp,OUTPUT TABLE hdschakprothandtemp,
                              OUTPUT TABLE hdprotkopbertemp,OUTPUT TABLE schkordstartsluttemp,OUTPUT TABLE hdschakttemp).   
   FOR EACH hdschakttemp NO-LOCK:  
      FIND FIRST hdkalktemp WHERE hdkalktemp.SID = hdschakttemp.SID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE hdkalktemp THEN DO:
         RUN kalktolk_UI IN schapph (INPUT INTEGER(valaonr),INPUT valomrade,INPUT hdschakttemp.SID,OUTPUT TABLE hdkalktemp APPEND).
      END.
   END.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
       
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
  DISPLAY RAD_VAL TOG_SORTRUB TOG_SPROFIL CMB_RUBRIK FILL-IN_AONR FILL-IN_DATUM 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_VAL MBTN_SPARA BTN_OK BRW_VAL BRW_SKRIVVAL MBTN_PROFIL BTN_OVER 
         BTN_AVB BTN_MANSCBORT BTN_MANNY BTN_BACK TOG_SORTRUB TOG_SPROFIL 
         MBTN_RAKNA BTN_IMPSTRACK BTN_MANSTRACK BTN_MANSTRBORT BTN_MANRUBRIK 
         CMB_RUBRIK BTN_BORTRUBRIK BTN_VISA BTN_VISAMARK BTN_SKRIV BTN_EXCEL 
         MBTN_HAND MBTN_KALK MBTN_KORD MBTN_SCH FILL-IN_AONR FILL-IN_DATUM 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BTN_ANDKALK BTN_GRUNDUPP BTN_BORTKALK BTN_GENKALK BTN_GENKALKV 
      WITH FRAME FRAME-KALK IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALK}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KOORD}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SCHAKT}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-HAND}
  DISPLAY FILL-IN-BB FILL-IN-TB FILL-IN-DJP FILL-IN-DJM FILL-IN-PROCENTTK 
      WITH FRAME FRAME-PROFIL IN WINDOW C-Win.
  ENABLE FILL-IN-BB FILL-IN-TB FILL-IN-DJP FILL-IN-DJM BTN_NYPROFIL 
         BTN_BORTPROFIL FILL-IN-PROCENTTK 
      WITH FRAME FRAME-PROFIL IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PROFIL}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getstart_UI C-Win 
PROCEDURE getstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  EMPTY TEMP-TABLE whandltemp NO-ERROR.         
  CREATE whandltemp.
   ordningnr = 1.
   RUN whandle_UI (INPUT ordningnr, {&WINDOW-NAME}:HANDLE).
   RUN whandle_UI (INPUT ordningnr,FRAME DEFAULT-FRAME:HANDLE).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-SCHAKT:HANDLE).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-HAND:HANDLE).
   RUN whandle_UI (INPUT ordningnr,RAD_VAL:HANDLE IN FRAME DEFAULT-FRAME).  
   RUN whandle_UI (INPUT ordningnr,BTN_EXCEL:HANDLE IN FRAME DEFAULT-FRAME).   
   RUN whandle_UI (INPUT ordningnr,BTN_AVB:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_SKRIV:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,FILL-IN_AONR:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,FILL-IN_DATUM:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KOORD:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BRW_VAL:HANDLE IN FRAME DEFAULT-FRAME).  
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KALK:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_ANDKALK:HANDLE IN FRAME FRAME-KALK).
   RUN whandle_UI (INPUT ordningnr,BTN_BORTKALK:HANDLE IN FRAME FRAME-KALK).
   RUN whandle_UI (INPUT ordningnr,BTN_GRUNDUPP:HANDLE IN FRAME FRAME-KALK).
   RUN whandle_UI (INPUT ordningnr,MBTN_SPARA:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,MBTN_SCH:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,MBTN_HAND:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,MBTN_KORD:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,MBTN_KALK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BRW_SKRIVVAL:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_OVER:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_BACK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_MANSTRBORT:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_MANNY:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_MANSCBORT:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_MANSTRACK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_IMPSTRACK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_MANRUBRIK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_BORTRUBRIK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,CMB_RUBRIK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,hdschakttemp.BENAMNING:HANDLE IN BROWSE BRW_VAL).
   RUN whandle_UI (INPUT ordningnr,BTN_GENKALK:HANDLE IN FRAME FRAME-KALK).
   RUN whandle_UI (INPUT ordningnr,BTN_GENKALKV:HANDLE IN FRAME FRAME-KALK).   
   RUN whandle_UI (INPUT ordningnr,BTN_VISA:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,BTN_VISAMARK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,MBTN_RAKNA:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,TOG_SORTRUB:HANDLE IN FRAME DEFAULT-FRAME).   
   RUN whandle_UI (INPUT ordningnr,TOG_SPROFIL:HANDLE IN FRAME DEFAULT-FRAME).   
   RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,MBTN_PROFIL:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT ordningnr,FRAME FRAME-PROFIL:HANDLE).
   RUN whandle_UI (INPUT ordningnr,BTN_NYPROFIL:HANDLE IN FRAME FRAME-PROFIL).
   RUN whandle_UI (INPUT ordningnr,BTN_BORTPROFIL:HANDLE IN FRAME FRAME-PROFIL).
   RUN whandle_UI (INPUT ordningnr,FILL-IN-BB:HANDLE IN FRAME FRAME-PROFIL).
   RUN whandle_UI (INPUT ordningnr,FILL-IN-TB:HANDLE IN FRAME FRAME-PROFIL).
   RUN whandle_UI (INPUT ordningnr,FILL-IN-DJP:HANDLE IN FRAME FRAME-PROFIL).
   RUN whandle_UI (INPUT ordningnr,FILL-IN-DJM:HANDLE IN FRAME FRAME-PROFIL).
   RUN whandle_UI (INPUT ordningnr,FILL-IN-PROCENTTK:HANDLE IN FRAME FRAME-PROFIL).
   
   RUN SCHAKTPRODYN.P PERSISTENT SET schdynh (INPUT THIS-PROCEDURE,INPUT framesizeh, INPUT FRAME DEFAULT-FRAME:HANDLE,INPUT TABLE whandltemp).
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musa C-Win 
PROCEDURE musa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {musarrow.i}   
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE musw C-Win 
PROCEDURE musw :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   {muswait.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparplan_UI C-Win 
PROCEDURE sparplan_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:
  Notes:       
-------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER avsluta AS LOGICAL NO-UNDO.
   RUN fbtnsnabb_UI IN schdynh.
  
   IF avsluta = TRUE THEN DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI C-Win 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

