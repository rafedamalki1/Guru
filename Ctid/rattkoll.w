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

  Created: 04/08/97 -  8:47 am

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
&Scoped-define NEW

{TIDALLT.I}
DEFINE TEMP-TABLE orginaltemp NO-UNDO LIKE extratidallt.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valet AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
DEFINE INPUT PARAMETER TABLE FOR orginaltemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define SHARED SHARED
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I} 
DEFINE NEW SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE diroverrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE dirtidoveraknare AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE dirtid AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE dirtidrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE allatidvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE flexav AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE openqtid AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE aonrdelnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE allval AS INTEGER NO-UNDO.
DEFINE VARIABLE allatider AS INTEGER NO-UNDO.

{TIDUTTTNEW.I}
DEFINE NEW SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar för perioden", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 31.5 BY 2
     BGCOLOR 8  NO-UNDO.
DEFINE NEW SHARED VARIABLE TOGGLE-MONTH AS LOGICAL INITIAL ? 
     LABEL "Två tidsedlar vid månadasskifte":L 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY 1 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_FRAN

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES orginaltemp extratidallt

/* Definitions for BROWSE BRW_FRAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FRAN orginaltemp.DATUM orginaltemp.DAG ~
orginaltemp.START orginaltemp.SLUT orginaltemp.VILART ~
orginaltemp.BEREDSKAPSTART orginaltemp.BEREDSKAPSLUT orginaltemp.BERANTAL ~
orginaltemp.AONR orginaltemp.DELNR orginaltemp.TOTALT ~
orginaltemp.LONTILLANTAL orginaltemp.TRAKTANTAL orginaltemp.ANVANDARE ~
orginaltemp.GODKAND orginaltemp.VIBEFATTNING orginaltemp.PRISTYP ~
orginaltemp.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRAN orginaltemp.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FRAN orginaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FRAN orginaltemp
&Scoped-define QUERY-STRING-BRW_FRAN FOR EACH orginaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FRAN OPEN QUERY BRW_FRAN FOR EACH orginaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FRAN orginaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRAN orginaltemp


/* Definitions for BROWSE BRW_TILL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TILL extratidallt.DATUM extratidallt.DAG ~
extratidallt.START extratidallt.SLUT extratidallt.VILART ~
extratidallt.BEREDSKAPSTART extratidallt.BEREDSKAPSLUT ~
extratidallt.BERANTAL extratidallt.AONR extratidallt.DELNR ~
extratidallt.TOTALT extratidallt.LONTILLANTAL extratidallt.TRAKTANTAL ~
extratidallt.ANVANDARE extratidallt.GODKAND extratidallt.VIBEFATTNING ~
extratidallt.PRISTYP extratidallt.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILL extratidallt.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TILL extratidallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TILL extratidallt
&Scoped-define QUERY-STRING-BRW_TILL FOR EACH extratidallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TILL OPEN QUERY BRW_TILL FOR EACH extratidallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TILL extratidallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILL extratidallt


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_FRAN BTN_SKRIV BRW_TILL BTN_AVB BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN_FORNAMN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AR AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FRAN FOR 
      orginaltemp SCROLLING.

DEFINE QUERY BRW_TILL FOR 
      extratidallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRAN WINDOW-1 _STRUCTURED
  QUERY BRW_FRAN NO-LOCK DISPLAY
      orginaltemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      orginaltemp.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      orginaltemp.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      orginaltemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
      orginaltemp.VILART COLUMN-LABEL "Lart" FORMAT "X(5)":U
      orginaltemp.BEREDSKAPSTART COLUMN-LABEL "Start" FORMAT "99.99":U
      orginaltemp.BEREDSKAPSLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
            WIDTH 5.5
      orginaltemp.BERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U
            WIDTH 5.5
      orginaltemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      orginaltemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      orginaltemp.TOTALT COLUMN-LABEL "Timmar!100" FORMAT "99.99":U
      orginaltemp.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>9.<<":U
      orginaltemp.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "-99.9":U
      orginaltemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      orginaltemp.GODKAND COLUMN-LABEL "Godkänd" FORMAT "x(4)":U
      orginaltemp.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(15)":U
      orginaltemp.PRISTYP COLUMN-LABEL "Debitering" FORMAT "X(9)":U
      orginaltemp.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9":U
  ENABLE
      orginaltemp.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 85 BY 7
         TITLE "Från".

DEFINE BROWSE BRW_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILL WINDOW-1 _STRUCTURED
  QUERY BRW_TILL NO-LOCK DISPLAY
      extratidallt.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      extratidallt.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      extratidallt.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      extratidallt.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
      extratidallt.VILART COLUMN-LABEL "Lart" FORMAT "X(5)":U
      extratidallt.BEREDSKAPSTART COLUMN-LABEL "Start" FORMAT "99.99":U
      extratidallt.BEREDSKAPSLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
            WIDTH 5.5
      extratidallt.BERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U
      extratidallt.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      extratidallt.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      extratidallt.TOTALT COLUMN-LABEL "Timmar!100" FORMAT "99.99":U
      extratidallt.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>9.<<":U
      extratidallt.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "-99.9":U
      extratidallt.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      extratidallt.GODKAND COLUMN-LABEL "Godkänd" FORMAT "x(4)":U
      extratidallt.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(15)":U
      extratidallt.PRISTYP COLUMN-LABEL "Debitering" FORMAT "X(9)":U
      extratidallt.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9":U
  ENABLE
      extratidallt.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 85 BY 7
         TITLE "Till".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-PKOD AT ROW 2.08 COL 11.5 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 2.08 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN_AR AT ROW 3.46 COL 15.88 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 3.46 COL 25.25 COLON-ALIGNED NO-LABEL
     BRW_FRAN AT ROW 7.75 COL 1.5
     BTN_SKRIV AT ROW 8 COL 87.25
     BRW_TILL AT ROW 15.5 COL 1.5
     BTN_AVB AT ROW 23.17 COL 87.25
     BTN_OK AT ROW 23.25 COL 72.25
     "Är det korrekt att Ni vill göra följande rättning?" VIEW-AS TEXT
          SIZE 50.25 BY 1.54 AT ROW 6.04 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.13 ROW 1
         SIZE 103.88 BY 24.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: extratidallt T "?" NO-UNDO temp-db extratidallt
      TABLE: orginaltemp T "?" NO-UNDO temp-db orginaltemp
      TABLE: ? T "?" NO-UNDO temp-db tidallt
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändra tidregistrering"
         HEIGHT             = 24.13
         WIDTH              = 104.38
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 122.88
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 122.88
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
  NOT-VISIBLE,                                                          */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FRAN FILL-IN-MANAD FRAME-A */
/* BROWSE-TAB BRW_TILL BTN_SKRIV FRAME-A */
ASSIGN 
       BRW_FRAN:HIDDEN  IN FRAME FRAME-A                = TRUE.

ASSIGN 
       BRW_TILL:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRAN
/* Query rebuild information for BROWSE BRW_FRAN
     _TblList          = "Temp-Tables.orginaltemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.orginaltemp.DATUM
"orginaltemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.orginaltemp.DAG
"orginaltemp.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.orginaltemp.START
"orginaltemp.START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.orginaltemp.SLUT
"orginaltemp.SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.orginaltemp.VILART
"orginaltemp.VILART" "Lart" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.orginaltemp.BEREDSKAPSTART
"orginaltemp.BEREDSKAPSTART" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.orginaltemp.BEREDSKAPSLUT
"orginaltemp.BEREDSKAPSLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no "5.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.orginaltemp.BERANTAL
"orginaltemp.BERANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no "5.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.orginaltemp.AONR
"orginaltemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.orginaltemp.DELNR
"orginaltemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.orginaltemp.TOTALT
"orginaltemp.TOTALT" "Timmar!100" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.orginaltemp.LONTILLANTAL
"orginaltemp.LONTILLANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.orginaltemp.TRAKTANTAL
"orginaltemp.TRAKTANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.orginaltemp.ANVANDARE
"orginaltemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.orginaltemp.GODKAND
"orginaltemp.GODKAND" "Godkänd" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.orginaltemp.VIBEFATTNING
"orginaltemp.VIBEFATTNING" "Befattning" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.orginaltemp.PRISTYP
"orginaltemp.PRISTYP" "Debitering" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.orginaltemp.PRIS
"orginaltemp.PRIS" "Pris" ">>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILL
/* Query rebuild information for BROWSE BRW_TILL
     _TblList          = "Temp-Tables.extratidallt"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.extratidallt.DATUM
"extratidallt.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.extratidallt.DAG
"extratidallt.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.extratidallt.START
"extratidallt.START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.extratidallt.SLUT
"extratidallt.SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.extratidallt.VILART
"extratidallt.VILART" "Lart" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.extratidallt.BEREDSKAPSTART
"extratidallt.BEREDSKAPSTART" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.extratidallt.BEREDSKAPSLUT
"extratidallt.BEREDSKAPSLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no "5.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.extratidallt.BERANTAL
"extratidallt.BERANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.extratidallt.AONR
"extratidallt.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.extratidallt.DELNR
"extratidallt.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.extratidallt.TOTALT
"extratidallt.TOTALT" "Timmar!100" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.extratidallt.LONTILLANTAL
"extratidallt.LONTILLANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.extratidallt.TRAKTANTAL
"extratidallt.TRAKTANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.extratidallt.ANVANDARE
"extratidallt.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.extratidallt.GODKAND
"extratidallt.GODKAND" "Godkänd" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.extratidallt.VIBEFATTNING
"extratidallt.VIBEFATTNING" "Befattning" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.extratidallt.PRISTYP
"extratidallt.PRISTYP" "Debitering" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.extratidallt.PRIS
"extratidallt.PRIS" "Pris" ">>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TILL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_FRAN
&Scoped-define SELF-NAME BRW_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRAN WINDOW-1
ON VALUE-CHANGED OF BRW_FRAN IN FRAME FRAME-A /* Från */
DO:   
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TILL
&Scoped-define SELF-NAME BRW_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TILL WINDOW-1
ON VALUE-CHANGED OF BRW_TILL IN FRAME FRAME-A /* Till */
DO:   
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:   
   musz = TRUE.    
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO: 
   {muswait.i}
   musz = FALSE.
   APPLY "CLOSE":U TO THIS-PROCEDURE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-1
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:        
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:   
      {muswait.i}
      skrivut = TRUE.
      RUN RATTVISA.W (INPUT pkod,INPUT allval, INPUT TABLE extratidallt, INPUT TABLE orginaltemp).
      {musarrow.i}
   END.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FRAN
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   FOR EACH orginaltemp:
      FIND FIRST extratidallt WHERE extratidallt.RECTIDVIS = orginaltemp.RECTIDVIS NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extratidallt THEN DELETE orginaltemp.
   END.   
   ASSIGN      
   &Scoped-define BROWSE-NAME BRW_FRAN
   orginaltemp.DATUM:READ-ONLY IN BROWSE   {&BROWSE-NAME} = TRUE.
   &Scoped-define BROWSE-NAME BRW_TILL
   extratidallt.DATUM:READ-ONLY IN BROWSE   {&BROWSE-NAME} = TRUE.
   ASSIGN
   allatider = valet
   allval = valet
   brwbdatum = bdatum.
   brwbdatum = DATE(MONTH(bdatum),01,YEAR(bdatum)).
   orginaltemp.AONR:LABEL IN BROWSE BRW_FRAN = Guru.Konstanter:gaok.
   extratidallt.AONR:LABEL IN BROWSE BRW_TILL = Guru.Konstanter:gaok.
   &Scoped-define FORMATNAMN orginaltemp.AONR
   &Scoped-define BROWSE-NAME BRW_FRAN
   {AOFORMAT1.I}
   &Scoped-define FORMATNAMN extratidallt.AONR
   &Scoped-define BROWSE-NAME BRW_TILL
   {AOFORMAT1.I}
   brwavdatum = avdatum.    
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   regmnr = MONTH(bdatum).
   RUN MANNAMN.P.
   ASSIGN
   vart = "AND"  
   FILL-IN_FORNAMN-2 = tidpers.FORNAMN + " " + tidpers.EFTERNAMN      
   FILL-IN-PKOD = tidpers.PERSONALKOD
   FILL-IN_AR = YEAR(bdatum)
   FILL-IN-MANAD = regmannamn.        
   RUN enable_UI.   
   {FRMSIZE.I}    
   RUN open_UI.
   DISPLAY FILL-IN_AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.    
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/      
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FRAN:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
   (INPUT BRW_TILL:HANDLE IN FRAME {&FRAME-NAME}).
      BRW_TILL:NO-EMPTY-SPACE = FALSE.
      BRW_FRAN:NO-EMPTY-SPACE = FALSE.  
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
  DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_FRAN BTN_SKRIV BRW_TILL BTN_AVB BTN_OK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openb_UI WINDOW-1 
PROCEDURE openb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_TILL 
   IF allatider = 1 THEN DO:      
       OPEN QUERY {&BROWSE-NAME} FOR EACH extratidallt WHERE extratidallt.TIDLOG = TRUE NO-LOCK.
   END.     
   IF allatider = 2 THEN DO:
      OPEN QUERY {&BROWSE-NAME} FOR EACH extratidallt WHERE extratidallt.TIDLOG = FALSE AND extratidallt.LONTILLAGG NE "" NO-LOCK.
   END.   
   IF allatider = 3 THEN DO:
       OPEN QUERY {&BROWSE-NAME} FOR EACH extratidallt WHERE extratidallt.TYP = "BER"  NO-LOCK.
   END.    
   IF allatider = 4 THEN DO:
       OPEN QUERY {&BROWSE-NAME} FOR EACH extratidallt WHERE extratidallt.TIDLOG = FALSE AND extratidallt.TRAKTKOD NE "" NO-LOCK.
   END.                
   GET FIRST {&BROWSE-NAME}.
   OPEN QUERY BRW_FRAN FOR EACH orginaltemp NO-LOCK.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI WINDOW-1 
PROCEDURE open_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   ASSIGN
   extratidallt.START:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.SLUT:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.VILART:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.BERANTAL:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.AONR:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.DELNR:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.TOTALT:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.LONTILLANTAL:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.TRAKTANTAL:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.PRISTYP:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.VIBEFATTNING:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.PRIS:VISIBLE IN BROWSE BRW_TILL = FALSE
   extratidallt.GODKAND:VISIBLE IN BROWSE BRW_TILL = FALSE.   
   ASSIGN
   orginaltemp.START:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.SLUT:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.VILART:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.BERANTAL:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.AONR:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.DELNR:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.TOTALT:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.LONTILLANTAL:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.TRAKTANTAL:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.ANVANDARE:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.PRISTYP:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.VIBEFATTNING:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.PRIS:VISIBLE IN BROWSE BRW_FRAN = FALSE
   orginaltemp.GODKAND:VISIBLE IN BROWSE BRW_FRAN = FALSE.   
   IF allval = 1 THEN DO:   
      RUN openb_UI.
      ASSIGN
      extratidallt.START:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.SLUT:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.AONR:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.DELNR:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.TOTALT:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.PRISTYP:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.VIBEFATTNING:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.PRIS:VISIBLE IN BROWSE BRW_TILL = TRUE.     
      ASSIGN      
      orginaltemp.START:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.SLUT:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.AONR:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.DELNR:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.TOTALT:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.PRISTYP:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.VIBEFATTNING:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.PRIS:VISIBLE IN BROWSE BRW_FRAN = TRUE.
   END.
   ELSE IF allval = 2 THEN DO:   
      RUN openb_UI.
      ASSIGN   
      extratidallt.VILART:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.AONR:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.DELNR:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.LONTILLANTAL:VISIBLE IN BROWSE BRW_TILL = TRUE.      
      ASSIGN   
      orginaltemp.VILART:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.AONR:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.DELNR:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.LONTILLANTAL:VISIBLE IN BROWSE BRW_FRAN = TRUE.      
   END.
   ELSE IF allval = 3 THEN DO:   
      RUN openb_UI.
      ASSIGN
      extratidallt.VILART:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.BERANTAL:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TILL = TRUE.      
      ASSIGN
      orginaltemp.VILART:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.BERANTAL:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.ANVANDARE:VISIBLE IN BROWSE BRW_FRAN = TRUE.      
   END.
   ELSE IF allval = 4 THEN DO:   
      RUN openb_UI.      
      ASSIGN
      extratidallt.VILART:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.AONR:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.DELNR:VISIBLE IN BROWSE BRW_TILL = TRUE
      extratidallt.TRAKTANTAL:VISIBLE IN BROWSE BRW_TILL = TRUE.   
      ASSIGN
      orginaltemp.VILART:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.AONR:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.DELNR:VISIBLE IN BROWSE BRW_FRAN = TRUE
      orginaltemp.TRAKTANTAL:VISIBLE IN BROWSE BRW_FRAN = TRUE.      
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

