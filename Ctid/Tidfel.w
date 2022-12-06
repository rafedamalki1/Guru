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

  Created: 04/08/97 -  4:19 pm

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
&Scoped-define NEW NEW
{TIDFALLT.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{TIDPERS.I}
{OMRTEMPW.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
DEFINE SHARED VARIABLE andvisdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE dirtid AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE dirtidrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE muszval AS INTEGER NO-UNDO.
DEFINE VARIABLE tempmanad AS INTEGER NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE VARIABLE prepmanad AS CHARACTER NO-UNDO.
DEFINE VARIABLE prepalltid AS CHARACTER NO-UNDO.
DEFINE VARIABLE prepquerystr AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.

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
&Scoped-define BROWSE-NAME BRW_FEL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidfeltemp

/* Definitions for BROWSE BRW_FEL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FEL tidfeltemp.DATUM tidfeltemp.DAG ~
tidfeltemp.PERSONALKOD tidfeltemp.START tidfeltemp.SLUT tidfeltemp.VILART ~
tidfeltemp.LONTILLANTAL tidfeltemp.TRAKTANTAL tidfeltemp.BERANTAL ~
tidfeltemp.AONR tidfeltemp.DELNR tidfeltemp.BEREDSKAPSTART ~
tidfeltemp.BEREDSKAPSLUT tidfeltemp.TOTALT tidfeltemp.DEBET ~
tidfeltemp.FELDATUM tidfeltemp.FELKORD tidfeltemp.FELANVAND ~
tidfeltemp.VIBEFATTNING tidfeltemp.PRISTYP tidfeltemp.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FEL tidfeltemp.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FEL tidfeltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FEL tidfeltemp
&Scoped-define QUERY-STRING-BRW_FEL FOR EACH tidfeltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FEL OPEN QUERY BRW_FEL FOR EACH tidfeltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FEL tidfeltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FEL tidfeltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_KORD BTN_VISA BRW_FEL BTN_NY BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_KORD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa summering":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_BER  NO-FOCUS FLAT-BUTTON
     LABEL "Beredskap" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_LON  NO-FOCUS FLAT-BUTTON
     LABEL "Lönetillägg" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_TID  NO-FOCUS FLAT-BUTTON
     LABEL "Tidregistreringar" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_TRA  NO-FOCUS FLAT-BUTTON
     LABEL "Traktamente" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 7 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE CMB_MANAD AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 13
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti","september","oktober","november","december","hela året" 
     DROP-DOWN-LIST
     SIZE 15 BY 1
     FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FILL-IN-text AS CHARACTER FORMAT "X(256)":U INITIAL "Det finns inga rättningar" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 70 BY 1.04 NO-UNDO.

DEFINE VARIABLE TOG_KORD AS LOGICAL INITIAL no 
     LABEL "Visa redan körda" 
     VIEW-AS TOGGLE-BOX
     SIZE 19.5 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FEL FOR 
      tidfeltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FEL WINDOW-1 _STRUCTURED
  QUERY BRW_FEL NO-LOCK DISPLAY
      tidfeltemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
            WIDTH 7
      tidfeltemp.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      tidfeltemp.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "x(5)":U
      tidfeltemp.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      tidfeltemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
      tidfeltemp.VILART COLUMN-LABEL "Lart" FORMAT "X(4)":U
      tidfeltemp.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT "->>>9.<<":U
      tidfeltemp.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "-99.9":U
      tidfeltemp.BERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U
      tidfeltemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U WIDTH 5
      tidfeltemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      tidfeltemp.BEREDSKAPSTART COLUMN-LABEL "Start" FORMAT "99.99":U
      tidfeltemp.BEREDSKAPSLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
      tidfeltemp.TOTALT COLUMN-LABEL "Timmar!100" FORMAT "-99.99":U
      tidfeltemp.DEBET FORMAT "Ja/Nej":U
      tidfeltemp.FELDATUM COLUMN-LABEL "Rättad" FORMAT "99/99/99":U
      tidfeltemp.FELKORD COLUMN-LABEL "Körd" FORMAT "x(9)":U
      tidfeltemp.FELANVAND FORMAT "X(12)":U WIDTH 9
      tidfeltemp.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(15)":U
      tidfeltemp.PRISTYP COLUMN-LABEL "Debitering" FORMAT "X(10)":U
      tidfeltemp.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9":U WIDTH 10
  ENABLE
      tidfeltemp.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 108.5 BY 14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FBTN_BER AT ROW 10.21 COL 111.5
     FBTN_LON AT ROW 9.08 COL 111.5
     FBTN_TID AT ROW 8 COL 111.5
     FBTN_TRA AT ROW 11.29 COL 111.5
     RAD_ALLTID AT ROW 2.79 COL 42.38 NO-LABEL
     CMB_AR AT ROW 4.25 COL 33.88 NO-LABEL
     CMB_MANAD AT ROW 4.25 COL 41 COLON-ALIGNED NO-LABEL
     TOG_KORD AT ROW 4.29 COL 64.5
     BTN_VISA AT ROW 5.25 COL 111.5 WIDGET-ID 2
     BRW_FEL AT ROW 6 COL 1.5
     FILL-IN-text AT ROW 10.67 COL 16 COLON-ALIGNED NO-LABEL
     BTN_NY AT ROW 20.33 COL 45.5
     BTN_AVB AT ROW 20.33 COL 111.5
     "Ändra eller registrera rättningar" VIEW-AS TEXT
          SIZE 34.5 BY 1.54 AT ROW 2.71 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 20.58.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tidfeltemp T "?" NO-UNDO temp-db tidfeltemp
      TABLE: ? T "?" NO-UNDO temp-db overtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändra eller registrera rättningar"
         HEIGHT             = 20.63
         WIDTH              = 125.38
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 125.38
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 125.38
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
/* BROWSE-TAB BRW_FEL BTN_VISA FRAME-A */
/* SETTINGS FOR COMBO-BOX CMB_AR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       CMB_AR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_MANAD IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_MANAD:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_BER IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_BER:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_LON IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_LON:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_TID IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_TID:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_TRA IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_TRA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-text IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-text:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLTID IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_ALLTID:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FEL
/* Query rebuild information for BROWSE BRW_FEL
     _TblList          = "Temp-Tables.tidfeltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidfeltemp.DATUM
"tidfeltemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidfeltemp.DAG
"tidfeltemp.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidfeltemp.PERSONALKOD
"tidfeltemp.PERSONALKOD" "Enhet!/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tidfeltemp.START
"tidfeltemp.START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tidfeltemp.SLUT
"tidfeltemp.SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tidfeltemp.VILART
"tidfeltemp.VILART" "Lart" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tidfeltemp.LONTILLANTAL
"tidfeltemp.LONTILLANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tidfeltemp.TRAKTANTAL
"tidfeltemp.TRAKTANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tidfeltemp.BERANTAL
"tidfeltemp.BERANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tidfeltemp.AONR
"tidfeltemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tidfeltemp.DELNR
"tidfeltemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tidfeltemp.BEREDSKAPSTART
"tidfeltemp.BEREDSKAPSTART" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.tidfeltemp.BEREDSKAPSLUT
"tidfeltemp.BEREDSKAPSLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.tidfeltemp.TOTALT
"tidfeltemp.TOTALT" "Timmar!100" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   = Temp-Tables.tidfeltemp.DEBET
     _FldNameList[16]   > Temp-Tables.tidfeltemp.FELDATUM
"tidfeltemp.FELDATUM" "Rättad" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.tidfeltemp.FELKORD
"tidfeltemp.FELKORD" "Körd" "x(9)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.tidfeltemp.FELANVAND
"tidfeltemp.FELANVAND" ? "X(12)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.tidfeltemp.VIBEFATTNING
"tidfeltemp.VIBEFATTNING" "Befattning" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.tidfeltemp.PRISTYP
"tidfeltemp.PRISTYP" "Debitering" "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.tidfeltemp.PRIS
"tidfeltemp.PRIS" "Pris" ">>>>9" "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FEL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO: 
   ASSIGN
   tidtabrec = ?
   tidtabrec2 = ?.    
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   {muswait.i} 
   RUN datum_UI.
   andvisdatum = regdatum. 
   {AVBGOM.I}
   IF BRW_FEL:VISIBLE = TRUE THEN DO:
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.        
      RUN FELVAL.W (INPUT tidfeltemp.PERSONALKOD).
   END.
   ELSE RUN FELVAL.W (INPUT "").
   RUN datum_UI.
   RUN hamttid_UI (INPUT 1).
   RUN globman2_UI.
   RUN open_UI.
   {AVBFRAM.I}
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa summering */
DO:
   {muswait.i} 
   {AVBGOM.I}
   RUN datum_UI.
   RUN VISTIDFSUM.W (INPUT brwbdatum, INPUT prepmanad,INPUT CMB_MANAD,INPUT TOG_KORD ).   
   {AVBFRAM.I}
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR WINDOW-1
ON LEAVE OF CMB_AR IN FRAME FRAME-A
DO:
   CMB_AR = INPUT CMB_AR.
   regar = CMB_AR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AR WINDOW-1
ON VALUE-CHANGED OF CMB_AR IN FRAME FRAME-A
DO:
   CMB_AR = INPUT CMB_AR.
   regar = CMB_AR.   
   RUN datum_UI.
   RUN hamttid_UI (INPUT 1).
   RUN open_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD WINDOW-1
ON LEAVE OF CMB_MANAD IN FRAME FRAME-A
DO:
   CMB_MANAD = INPUT CMB_MANAD.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_MANAD WINDOW-1
ON VALUE-CHANGED OF CMB_MANAD IN FRAME FRAME-A
DO:
   CMB_MANAD = INPUT CMB_MANAD. 
   RUN datum_UI.
   IF CMB_MANAD = "hela året" THEN DO:
      prepmanad = "".
   END.
   ELSE DO:
      prepmanad = " MONTH(tidfeltemp.DATUM) = MONTH(" + STRING(brwbdatum) + ") AND ".
   END.
   RUN open_UI.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BER WINDOW-1
ON CHOOSE OF FBTN_BER IN FRAME FRAME-A /* Beredskap */
DO:
  RAD_ALLTID = 3.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_LON WINDOW-1
ON CHOOSE OF FBTN_LON IN FRAME FRAME-A /* Lönetillägg */
DO:
  RAD_ALLTID = 2.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_TID WINDOW-1
ON CHOOSE OF FBTN_TID IN FRAME FRAME-A /* Tidregistreringar */
DO:
  RAD_ALLTID = 1.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_TRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_TRA WINDOW-1
ON CHOOSE OF FBTN_TRA IN FRAME FRAME-A /* Traktamente */
DO:
  RAD_ALLTID = 4.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ALLTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ALLTID WINDOW-1
ON VALUE-CHANGED OF RAD_ALLTID IN FRAME FRAME-A
DO:
   RUN chbstate_UI.
   IF RAD_ALLTID = 1 THEN DO:
      prepalltid = " tidfeltemp.TIDLOG = TRUE AND ".
   END.
   ELSE IF RAD_ALLTID = 2 THEN DO:
      prepalltid = " tidfeltemp.LONTILLAGG NE """" AND ".
   END.
   ELSE IF RAD_ALLTID = 3 THEN DO:
      prepalltid = " tidfeltemp.BEREDSKAP NE """" AND ".
   END.
   ELSE IF RAD_ALLTID = 4 THEN DO:
      prepalltid = " tidfeltemp.TRAKTKOD NE """" AND ".
   END.
   RUN open_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KORD WINDOW-1
ON VALUE-CHANGED OF TOG_KORD IN FRAME FRAME-A /* Visa redan körda */
DO:
  TOG_KORD = INPUT TOG_KORD.
  IF TOG_KORD = FALSE THEN DO:
     BTN_NY:HIDDEN = FALSE.      
  END.
  ELSE DO:  
     BTN_NY:HIDDEN = TRUE.      
  END. 
  RUN open_UI.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FEL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


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
   ASSIGN
   tidtabrec = 0
   brwbdatum = bdatum
   brwavdatum = avdatum
   tidfeltemp.FELANVAND:LABEL IN BROWSE BRW_FEL = "Användare"
   tidfeltemp.DEBET:LABEL IN BROWSE BRW_FEL = "Debet"
   status-ok = RAD_ALLTID:DELETE("")
   status-ok = RAD_ALLTID:ADD-LAST("Tidregistreringar", 1)
   status-ok = RAD_ALLTID:ADD-LAST("Lönetillägg", 2) 
   status-ok = RAD_ALLTID:ADD-LAST("Beredskap", 3)  
   status-ok = RAD_ALLTID:ADD-LAST("Traktamente",4 )                         
   RAD_ALLTID = 1.
   FBTN_TID:LOAD-IMAGE-UP("BILDER\BTN_tidreg.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_TID:LOAD-IMAGE-DOWN("BILDER\BTN_tidreg_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_LON:LOAD-IMAGE-UP("BILDER\BTN_lontill.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_LON:LOAD-IMAGE-DOWN("BILDER\BTN_lontill_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_BER:LOAD-IMAGE-UP("BILDER\BTN_bereskap.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_BER:LOAD-IMAGE-DOWN("BILDER\BTN_bereskap_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_TRA:LOAD-IMAGE-UP("BILDER\BTN_trakt.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   FBTN_TRA:LOAD-IMAGE-DOWN("BILDER\BTN_trakt_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO RAD_ALLTID IN FRAME {&FRAME-NAME}.
   ENABLE CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}. 
   RUN REGVEC.P.    
   RUN enable_UI.   
   {FRMSIZE.I}              
   RUN globman_UI.
   RUN datum_UI.
   RUN hamttid_UI (INPUT 1).
   prepalltid = " tidfeltemp.TIDLOG = TRUE AND ".
   APPLY "VALUE-CHANGED" TO CMB_MANAD IN FRAME {&FRAME-NAME}.   
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
   tidfeltemp.DATUM:READ-ONLY IN BROWSE BRW_FEL = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FEL:HANDLE IN FRAME {&FRAME-NAME}).   
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chbstate_UI WINDOW-1 
PROCEDURE chbstate_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_ALLTID = 1 THEN DO:            
      FBTN_TID:LOAD-IMAGE("BILDER\BTN_TIDREG_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_ALLTID = 2 THEN DO:      
      FBTN_LON:LOAD-IMAGE("BILDER\BTN_LONTILL_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_ALLTID = 3 THEN DO:      
      FBTN_BER:LOAD-IMAGE("BILDER\btn_bereskap_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_ALLTID = 4 THEN DO:     
      FBTN_TRA:LOAD-IMAGE("BILDER\BTN_TRAKT_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   IF radspar NE RAD_ALLTID THEN DO:
      IF radspar = 1 THEN FBTN_TID:LOAD-IMAGE("BILDER\BTN_TIDREG.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      IF radspar = 2 THEN FBTN_LON:LOAD-IMAGE("BILDER\BTN_LONTILL.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      IF radspar = 3 THEN FBTN_BER:LOAD-IMAGE("BILDER\btn_bereskap.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      IF radspar = 4 THEN FBTN_TRA:LOAD-IMAGE("BILDER\BTN_TRAKT.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   radspar = RAD_ALLTID.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE datum_UI WINDOW-1 
PROCEDURE datum_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF CMB_MANAD = "hela året" THEN RETURN.
   regmannamn = CMB_MANAD.
   RUN MANNR.P.
   ASSIGN
   regdatum = DATE(regmnr,01,CMB_AR)
   bdatum = regdatum
   brwbdatum = DATE(MONTH(bdatum),01,YEAR(bdatum)).
   IF MONTH(regdatum) = 12 THEN avdatum = DATE(12,31,YEAR(regdatum)). 
   ELSE avdatum = DATE((MONTH(regdatum) + 1),01,YEAR(regdatum)) - 1.
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
  DISPLAY TOG_KORD 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE TOG_KORD BTN_VISA BRW_FEL BTN_NY BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE globman2_UI WINDOW-1 
PROCEDURE globman2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN      
   CMB_AR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(YEAR(andvisdatum),"9999")
   CMB_AR = INPUT FRAME {&FRAME-NAME} CMB_AR
   regar = CMB_AR.  
   tempmanad = MONTH(andvisdatum).
   CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(tempmanad,CMB_MANAD:LIST-ITEMS).
   CMB_MANAD = INPUT CMB_MANAD.    
   DISPLAY CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}.
   DISPLAY CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO CMB_MANAD IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE globman_UI WINDOW-1 
PROCEDURE globman_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ENABLE CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}.
   ASSIGN  /*LADDAR ÅR I CMB_AR*/
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 4,"9999")) 
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 3,"9999")) 
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 2,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) - 1,"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY),"9999"))
   status-ok = CMB_AR:ADD-LAST(STRING(YEAR(TODAY) + 1,"9999"))
   status-ok = CMB_AR:DELETE("0")
   CMB_AR:SCREEN-VALUE = STRING(YEAR(andvisdatum),"9999")
   CMB_AR = INPUT CMB_AR. 
   regar = CMB_AR.   
   tempmanad = MONTH(andvisdatum).
   CMB_MANAD:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(tempmanad,CMB_MANAD:LIST-ITEMS).
   CMB_MANAD = INPUT CMB_MANAD.    
   DISPLAY CMB_MANAD CMB_AR WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamttid_UI WINDOW-1 
PROCEDURE hamttid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   brwbdatum = DATE(01,01,CMB_AR).
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDFHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vadgora,INPUT "",INPUT brwbdatum,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE tidfeltemp).
   END.
   ELSE DO:
      RUN TIDFHMT.P  
      (INPUT vadgora,INPUT "",INPUT brwbdatum,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE tidfeltemp).
   END.
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
   tidfeltemp.START:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.SLUT:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.VILART:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.LONTILLANTAL:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.TRAKTANTAL:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.BERANTAL:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_FEL = FALSE
   tidfeltemp.TOTALT:VISIBLE IN BROWSE BRW_FEL = FALSE.
   prepquerystr = "YEAR(tidfeltemp.DATUM) = YEAR(" + STRING(brwbdatum) + ") AND " +
                  prepmanad + prepalltid + " tidfeltemp.TKORD = " + STRING(TOG_KORD).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT prepquerystr).
   RUN openbdynspec_UI IN brwproc[1].
   IF RAD_ALLTID = 1 THEN DO:
      ASSIGN
      tidfeltemp.START:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.SLUT:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.TOTALT:VISIBLE IN BROWSE BRW_FEL = TRUE.       
   END.   
   IF RAD_ALLTID = 2 THEN DO:
      ASSIGN
      tidfeltemp.VILART:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.LONTILLANTAL:VISIBLE IN BROWSE BRW_FEL = TRUE.              
   END.
   IF RAD_ALLTID = 3 THEN DO:    
      ASSIGN
      tidfeltemp.BERANTAL:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_FEL = TRUE.                       
   END.
   IF RAD_ALLTID = 4 THEN DO:
      ASSIGN
      tidfeltemp.VILART:VISIBLE IN BROWSE BRW_FEL = TRUE
      tidfeltemp.TRAKTANTAL:VISIBLE IN BROWSE BRW_FEL = TRUE.      
   END.     
   GET FIRST BRW_FEL NO-LOCK.
   IF NOT AVAILABLE tidfeltemp THEN DO:
      ASSIGN 
      FILL-IN-text:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_FEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.          
   END.
   ELSE DO :                          
      ENABLE BRW_FEL WITH FRAME {&FRAME-NAME}. 
      ASSIGN 
      FILL-IN-text:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_FEL:HIDDEN = FALSE.                    
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storlek_UI WINDOW-1 
PROCEDURE storlek_UI :
DEFINE INPUT PARAMETER vh AS HANDLE NO-UNDO.
   vh:HEIGHT-CHARS = vh:HEIGHT-CHARS + 0.05.
   IF vh:ROW - 0.04 > 1 THEN vh:ROW = vh:ROW - 0.04.
   vh:COLUMN = vh:COLUMN - 0.08.
   vh:WIDTH-CHARS = vh:WIDTH-CHARS + 0.25.       
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdgod_UI WINDOW-1 
PROCEDURE valdgod_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   antal_valda = BRW_FEL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen registrering är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   ELSE DO:  
      antal_raknare = 1.     
      DO WHILE antal_raknare LE antal_valda :         
         status-ok = BRW_FEL:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.         
         antal_raknare = antal_raknare + 1.
      END.          
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

