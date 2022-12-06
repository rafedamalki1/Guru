&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1






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
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{PERBEF.I}
{TIDALLT.I}
DEFINE TEMP-TABLE orginaltemp NO-UNDO LIKE extratidallt.
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
{FLEXTAB.I}
{DIRDEF.I}
{UPPGHMT.I}
{PHMT.I}
DEFINE NEW SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE varaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vardelnr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE vartrakt AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE varpris AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE varslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE varpristyp  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varfabef AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varutryck AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE vartotalt AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE korda AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE debitering AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE startvar AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE slutvar AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE varlon AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varlant AS DECIMAL NO-UNDO.
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
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE allatider AS INTEGER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE VARIABLE felvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjalpvar AS RECID  NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
{TIDUTTTNEW.I}
DEFINE INPUT PARAMETER valet AS INTEGER NO-UNDO.

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
&Scoped-define BROWSE-NAME BRW_TIDANDG

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidallt

/* Definitions for BROWSE BRW_TIDANDG                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDANDG tidallt.DATUM tidallt.DAG ~
tidallt.START tidallt.SLUT tidallt.VILART tidallt.BEREDSKAPSTART ~
tidallt.BEREDSKAPSLUT tidallt.BERANTAL tidallt.AONR tidallt.DELNR ~
tidallt.TOTALT tidallt.LONTILLANTAL tidallt.TRAKTANTAL tidallt.ANVANDARE ~
tidallt.GODKAND SUBSTRING (tidallt.VECKOKORD,4,6) tidallt.VIBEFATTNING ~
tidallt.PRISTYP tidallt.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDANDG tidallt.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TIDANDG tidallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TIDANDG tidallt
&Scoped-define QUERY-STRING-BRW_TIDANDG FOR EACH tidallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TIDANDG OPEN QUERY BRW_TIDANDG FOR EACH tidallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TIDANDG tidallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDANDG tidallt


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_TIDREG RECT-29 MBTN_LONTILL ~
MBTN_BEREDSKAP MBTN_TRAKT BTN_DELA BTN_BYTA BTN_VISA BTN_REG BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN_FORNAMN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock100 WINDOW-1 
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock60 WINDOW-1 
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BYTA 
     LABEL "Byta aonr":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_DELA 
     LABEL "Dela upp":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "Registrera rättning!":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa rättning":L 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_BEREDSKAP 
     LABEL "Beredskap" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_LONTILL 
     LABEL "Lönetillägg" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_TIDREG 
     LABEL "Tidregistreringar" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_TRAKT 
     LABEL "Traktamente" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_AR AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 70 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-29
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124.5 BY .08.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TIDANDG FOR 
      tidallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TIDANDG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDANDG WINDOW-1 _STRUCTURED
  QUERY BRW_TIDANDG NO-LOCK DISPLAY
      tidallt.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      tidallt.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      tidallt.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      tidallt.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
      tidallt.VILART COLUMN-LABEL "Lart" FORMAT "X(5)":U
      tidallt.BEREDSKAPSTART COLUMN-LABEL "Start" FORMAT "99.99":U
      tidallt.BEREDSKAPSLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
            WIDTH 5.5
      tidallt.BERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U
      tidallt.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      tidallt.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      tidallt.TOTALT COLUMN-LABEL "Timmar!100" FORMAT "99.99":U
      tidallt.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>9.<<":U
      tidallt.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "-99.9":U
      tidallt.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      tidallt.GODKAND COLUMN-LABEL "Godkänd" FORMAT "x(4)":U
      SUBSTRING (tidallt.VECKOKORD,4,6) COLUMN-LABEL "Till lön!eko.sys"
      tidallt.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(15)":U
      tidallt.PRISTYP COLUMN-LABEL "Debitering" FORMAT "X(9)":U
      tidallt.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9":U
  ENABLE
      tidallt.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 108 BY 12.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     MBTN_TIDREG AT ROW 1.08 COL 1
     MBTN_LONTILL AT ROW 1.08 COL 15
     MBTN_BEREDSKAP AT ROW 1.08 COL 29
     MBTN_TRAKT AT ROW 1.08 COL 43
     RAD_ALLTID AT ROW 2.25 COL 26 NO-LABEL
     FILL-IN-PKOD AT ROW 3.46 COL 24 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 3.46 COL 33 COLON-ALIGNED NO-LABEL
     FILL-IN_AR AT ROW 4.83 COL 28.75 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 4.83 COL 38.13 COLON-ALIGNED NO-LABEL
     BRW_TIDANDG AT ROW 6.04 COL 1.5
     BTN_DELA AT ROW 8 COL 110.88
     BTN_BYTA AT ROW 9.08 COL 110.88
     BTN_VISA AT ROW 10.21 COL 110.88
     BTN_REG AT ROW 11.29 COL 110.88
     BTN_AVB AT ROW 19 COL 110.88
     FILL-IN-TEXT AT ROW 19.17 COL 1.5 NO-LABEL
     RECT-29 AT ROW 2.08 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 19.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tidallt T "?" NO-UNDO temp-db tidallt
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändra tidregistrering"
         HEIGHT             = 19.46
         WIDTH              = 125
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 125
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
/* BROWSE-TAB BRW_TIDANDG FILL-IN-MANAD FRAME-A */
/* SETTINGS FOR BROWSE BRW_TIDANDG IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TIDANDG:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
/* SETTINGS FOR FILL-IN FILL-IN_AR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       MBTN_BEREDSKAP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       MBTN_LONTILL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       MBTN_TIDREG:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       MBTN_TRAKT:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLTID IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_ALLTID:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDANDG
/* Query rebuild information for BROWSE BRW_TIDANDG
     _TblList          = "Temp-Tables.tidallt"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.tidallt.DATUM
"DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidallt.DAG
"DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidallt.START
"START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tidallt.SLUT
"SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tidallt.VILART
"VILART" "Lart" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tidallt.BEREDSKAPSTART
"BEREDSKAPSTART" "Start" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tidallt.BEREDSKAPSLUT
"BEREDSKAPSLUT" "Slut" ? "decimal" ? ? ? ? ? ? no ? no no "5.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tidallt.BERANTAL
"BERANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tidallt.AONR
"AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tidallt.DELNR
"DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tidallt.TOTALT
"TOTALT" "Timmar!100" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tidallt.LONTILLANTAL
"LONTILLANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.tidallt.TRAKTANTAL
"TRAKTANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.tidallt.ANVANDARE
"ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.tidallt.GODKAND
"GODKAND" "Godkänd" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"SUBSTRING (tidallt.VECKOKORD,4,6)" "Till lön!eko.sys" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.tidallt.VIBEFATTNING
"VIBEFATTNING" "Befattning" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.tidallt.PRISTYP
"PRISTYP" "Debitering" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.tidallt.PRIS
"PRIS" "Pris" ">>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TIDANDG */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_TIDANDG
&Scoped-define SELF-NAME BRW_TIDANDG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TIDANDG WINDOW-1
ON VALUE-CHANGED OF BRW_TIDANDG IN FRAME FRAME-A
DO:   
   RUN vchangebrw_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:      
   FIND FIRST extratidallt NO-LOCK NO-ERROR.
   IF AVAILABLE extratidallt THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:      
         APPLY "CHOOSE" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:       
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                    
      ELSE DO:
         Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      END.
   END.
   ELSE APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTA WINDOW-1
ON CHOOSE OF BTN_BYTA IN FRAME FRAME-A /* Byta aonr */
DO:
   RUN valdbyt_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DELA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DELA WINDOW-1
ON CHOOSE OF BTN_DELA IN FRAME FRAME-A /* Dela upp */
DO: 
   RUN valdela_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG WINDOW-1
ON CHOOSE OF BTN_REG IN FRAME FRAME-A /* Registrera rättning! */
DO:
   {muswait.i}
   {AVBGOM.I}     
   RUN RATTKOLL.W (INPUT pkod,INPUT allatider,INPUT TABLE extratidallt,INPUT TABLE orginaltemp).
   {AVBFRAM.I}
   {musarrow.i}      
   IF musz = FALSE THEN DO:
      FOR EACH extratidallt:
         extratidallt.TOTALT = klock60(extratidallt.TOTALT).
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN RATTSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv,INPUT TABLE extratidallt, INPUT TABLE orginaltemp).   
      END.
      ELSE DO:
         RUN RATTSKAP.P
         (INPUT Guru.Konstanter:globanv,INPUT TABLE extratidallt, INPUT TABLE orginaltemp).   
      END.
      APPLY "CLOSE":U TO THIS-PROCEDURE. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa rättning */
DO:
   {AVBGOM.I}
   RUN VISARATT.W (INPUT pkod,INPUT allatider, INPUT tidallt.RECTIDVIS, INPUT TABLE tidallt).
   {AVBFRAM.I}                                                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_BEREDSKAP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_BEREDSKAP WINDOW-1
ON CHOOSE OF MBTN_BEREDSKAP IN FRAME FRAME-A /* Beredskap */
DO:
  RAD_ALLTID = 3.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_LONTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_LONTILL WINDOW-1
ON CHOOSE OF MBTN_LONTILL IN FRAME FRAME-A /* Lönetillägg */
DO:
  RAD_ALLTID = 2.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_TIDREG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_TIDREG WINDOW-1
ON CHOOSE OF MBTN_TIDREG IN FRAME FRAME-A /* Tidregistreringar */
DO:
  RAD_ALLTID = 1.
  APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_TRAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_TRAKT WINDOW-1
ON CHOOSE OF MBTN_TRAKT IN FRAME FRAME-A /* Traktamente */
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
   FIND FIRST extratidallt NO-LOCK NO-ERROR.
   IF AVAILABLE extratidallt THEN DO:
      MESSAGE "OBS! Du måste registrera dina förändringar innan du byter funktion!"
      VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      allatider = RAD_ALLTID.
      RUN hmttid_UI (INPUT 4).
      RUN changecolsort_UI.
      RUN vchangebrw_UI.         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


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
   {ALLSTARTDYN.I}  
   {muswait.i}   
   ASSIGN
   korda = 0
   bilforare = FALSE
   brwbdatum = bdatum
   status-ok = RAD_ALLTID:DELETE("")
   status-ok = RAD_ALLTID:ADD-LAST("Tidregistreringar", 1)
   status-ok = RAD_ALLTID:ADD-LAST("Lönetillägg", 2) 
   status-ok = RAD_ALLTID:ADD-LAST("Beredskap", 3)  
   status-ok = RAD_ALLTID:ADD-LAST("Traktamente",4 )                         
   RAD_ALLTID = valet
   allatider = valet.
   MBTN_TIDREG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   MBTN_LONTILL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   MBTN_BEREDSKAP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   MBTN_TRAKT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
   ASSIGN
   brwbdatum = DATE(MONTH(bdatum),01,YEAR(bdatum))  
   brwavdatum = avdatum.    
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   persrec = tidpers.TIDPERSREC.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.     
   tidallt.AONR:LABEL IN BROWSE BRW_TIDANDG = Guru.Konstanter:gaok.
   BTN_BYTA:LABEL = "Byta " + LC(Guru.Konstanter:gaok).
   &Scoped-define FORMATNAMN tidallt.AONR
   &Scoped-define BROWSE-NAME BRW_TIDANDG
   {AOFORMAT1.I}
   regmnr = MONTH(bdatum).
   RUN MANNAMN.P.
   ASSIGN
   vart = "AND"  
   FILL-IN_FORNAMN-2 = tidpers.FORNAMN + " " + tidpers.EFTERNAMN      
   FILL-IN-PKOD = tidpers.PERSONALKOD
   FILL-IN_AR = YEAR(bdatum)
   FILL-IN-MANAD = regmannamn.        
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PERBEFPRIS2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT FILL-IN-PKOD,INPUT bdatum,OUTPUT TABLE perspristemp,OUTPUT TABLE befvaltemp).   
   END.
   ELSE DO:
      RUN PERBEFPRIS2.P 
      (INPUT FILL-IN-PKOD,INPUT bdatum,OUTPUT TABLE perspristemp,OUTPUT TABLE befvaltemp).   
   END.
   RUN enable_UI.   
   {FRMSIZE.I}  
   RUN hmttid_UI (INPUT 4).
   RUN changecolsort_UI.   
   status-ok = BRW_TIDANDG:DESELECT-FOCUSED-ROW() NO-ERROR.
   DISPLAY FILL-IN_AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}. 
   DISABLE BTN_BYTA BTN_DELA BTN_REG BTN_VISA WITH FRAME {&FRAME-NAME}.
   DISPLAY BTN_BYTA BTN_DELA BTN_REG BTN_VISA  WITH FRAME {&FRAME-NAME}. 
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
   tidallt.DATUM:READ-ONLY IN BROWSE BRW_TIDANDG = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TIDANDG:HANDLE IN FRAME {&FRAME-NAME}).     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changecolsort_UI WINDOW-1 
PROCEDURE changecolsort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   ASSIGN
   tidallt.START:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.SLUT:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BERANTAL:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.TOTALT:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.LONTILLANTAL:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.TRAKTANTAL:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.PRISTYP:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.VIBEFATTNING:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.PRIS:VISIBLE IN BROWSE BRW_TIDANDG = FALSE.   
   IF allatider = 1  THEN DO:   
      RUN openbdyn_UI IN brwproc[1] (INPUT " WHERE tidallt.TIDLOG = TRUE").
      ASSIGN
      tidallt.START:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.SLUT:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.TOTALT:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.PRISTYP:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.VIBEFATTNING:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.PRIS:VISIBLE IN BROWSE BRW_TIDANDG = TRUE.   
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DATUM",INPUT "tidallt.TIDLOG = TRUE").    
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DAG",INPUT "tidallt.TIDLOG = TRUE").          
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "AONR",INPUT "tidallt.TIDLOG = TRUE").                          
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DELNR",INPUT "tidallt.TIDLOG = TRUE").
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "GODKAND",INPUT "tidallt.TIDLOG = TRUE").  
   END.
   ELSE IF allatider = 2  THEN DO:   
      RUN openbdyn_UI IN brwproc[1] (INPUT " WHERE tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """"").
      ASSIGN   
      tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.LONTILLANTAL:VISIBLE IN BROWSE BRW_TIDANDG = TRUE. 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DATUM",INPUT "tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """"").  
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DAG",INPUT "tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """"").          
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "VILART",INPUT "tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """"").                   
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "AONR",INPUT "tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """"").                 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DELNR",INPUT "tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """""). 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "GODKAND",INPUT "tidallt.TIDLOG = FALSE AND tidallt.LONTILLAGG NE """"").      
   END.
   ELSE IF allatider = 3  THEN DO:   
      RUN openbdyn_UI IN brwproc[1] (INPUT " WHERE tidallt.TYP = ""BER""").
      ASSIGN
      tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BERANTAL:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TIDANDG = TRUE. 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DATUM",INPUT "tidallt.TYP = ""BER""").   
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DAG",INPUT " tidallt.TYP = ""BER""").     
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "VILART",INPUT "tidallt.TYP = ""BER""").                 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "AONR",INPUT "tidallt.TYP = ""BER"""). 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DELNR",INPUT "tidallt.TYP = ""BER"""). 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "GODKAND",INPUT "tidallt.TYP = ""BER""").  
   END.
   ELSE IF allatider = 4  THEN DO:  
      RUN openbdyn_UI IN brwproc[1] (INPUT " WHERE tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """"").
      ASSIGN
      tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.TRAKTANTAL:VISIBLE IN BROWSE BRW_TIDANDG = TRUE.      
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DATUM",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """""). 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DAG",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """"").   
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "VILART",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """"").                 
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "AONR",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """"").     
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "DELNR",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """"").     
      RUN setpdatatabvar_UI IN brwproc[1] (INPUT "GODKAND",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """""). 
   END.
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "START",INPUT "tidallt.TIDLOG = TRUE").
   RUN setpdatacolsort_UI IN brwproc[1] (INPUT "START",INPUT "tidallt.DAG BY tidallt.AONR").
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "SLUT",INPUT "tidallt.TIDLOG = TRUE").    
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "BEREDSKAPSTART",INPUT "tidallt.TYP = ""BER""").                
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "BEREDSKAPSLUT",INPUT "tidallt.TYP = ""BER""").                 
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "BERANTAL",INPUT "tidallt.TYP = ""BER""").  
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "LONTILLANTAL",INPUT "tidallt.LONTILLAGG NE """"").                
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "TRAKTANTAL",INPUT "tidallt.TIDLOG = FALSE AND tidallt.TRAKTKOD NE """"").   
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "ANVANDARE",INPUT "tidallt.TYP = ""BER"""). 
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "PRISTYP",INPUT "tidallt.TIDLOG = TRUE").                  
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "PRIS",INPUT "tidallt.TIDLOG = TRUE").                     
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "VIBEFATTNING",INPUT "tidallt.TIDLOG = TRUE").             
   RUN setpdatatabvar_UI IN brwproc[1] (INPUT "TOTALT",INPUT "tidallt.TIDLOG = TRUE").  
   GET FIRST BRW_TIDANDG NO-LOCK.
   IF AVAILABLE tidallt THEN DO:
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
      BRW_TIDANDG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      DISABLE BTN_BYTA BTN_VISA BTN_DELA BTN_REG WITH FRAME {&FRAME-NAME}.
      BRW_TIDANDG:HIDDEN = TRUE.
      IF allatider = 1 THEN MESSAGE "Ingen tid är registrerad." VIEW-AS ALERT-BOX.             
      IF allatider = 2 THEN MESSAGE "Inga manuella lönetillägg är registrerat." VIEW-AS ALERT-BOX.              
      IF allatider = 3 THEN MESSAGE "Ingen beredskap är registrerad." VIEW-AS ALERT-BOX.             
      IF allatider = 4 THEN MESSAGE "Inga manuella traktamenten är registrerade." VIEW-AS ALERT-BOX.             
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
  DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE MBTN_TIDREG RECT-29 MBTN_LONTILL MBTN_BEREDSKAP MBTN_TRAKT BTN_DELA 
         BTN_BYTA BTN_VISA BTN_REG BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmttid_UI WINDOW-1 
PROCEDURE hmttid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vadgora,INPUT pkod,INPUT brwbdatum,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE overtemp,INPUT-OUTPUT TABLE tidallt).
   END.
   ELSE DO:
      RUN TIDHMT.P  
      (INPUT vadgora,INPUT pkod,INPUT brwbdatum,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE overtemp,INPUT-OUTPUT TABLE tidallt).
   END.   
   FOR EACH tidallt:
      tidallt.TOTALT = klock100(tidallt.TOTALT).
   END.                                                                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdbyt_UI WINDOW-1 
PROCEDURE valdbyt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_raknare = 1.
   avdatum = brwbdatum.
   ASSIGN 
   varaonr = tidallt.AONR 
   vardelnr = tidallt.DELNR
   datvar = tidallt.DATUM
   startvar = tidallt.START
   slutvar = tidallt.SLUT.
   debitering = 1. 
   {AVBGOMD.I}
   FIND FIRST orginaltemp WHERE orginaltemp.RECTIDVIS = tidallt.RECTIDVIS NO-LOCK NO-ERROR.
   IF NOT AVAILABLE orginaltemp THEN DO:
      CREATE orginaltemp.
      BUFFER-COPY tidallt TO orginaltemp.
   END.
   RUN FELBYTAO.W (INPUT pkod,INPUT tidallt.PRISTYP, INPUT tidallt.PRIS,INPUT tidallt.OVERTIDTILL).
   {AVBFRAMD.I}     
   IF musz = FALSE THEN DO:
      antal_raknare = 1.
      IF allatider = 1  THEN DO:
         ASSIGN
         tidallt.PRIS = varpris
         tidallt.PRISTYP = varpristyp                             
         tidallt.AONR = varaonr
         tidallt.DELNR = vardelnr
         tidallt.VIBEFATTNING = varfabef.                      
      END.
      ELSE IF allatider = 2  THEN DO:
         ASSIGN
         tidallt.AONR = varaonr
         tidallt.DELNR = vardelnr
         tidallt.PRISTYP = varpristyp
         tidallt.PRIS = varpris.
         IF tidallt.TIDLOG = FALSE THEN DO:
            tidallt.PRIS = 0.
         END.            
         ELSE DO:
            tidallt.VIBEFATTNING = varfabef.
         END.
      END.
      ELSE IF allatider = 3  THEN DO:
         ASSIGN
         tidallt.AONR = varaonr
         tidallt.DELNR = vardelnr
         tidallt.PRISTYP = varpristyp
         tidallt.PRIS = varpris.
         IF tidallt.TIDLOG = FALSE THEN DO:
            tidallt.PRIS = 0.
         END.                                    
         ELSE DO:
            tidallt.VIBEFATTNING = varfabef.
         END.
      END.
      ELSE IF allatider = 4  THEN DO:            
         ASSIGN
         tidallt.AONR = varaonr
         tidallt.DELNR = vardelnr
         tidallt.PRISTYP = varpristyp
         tidallt.PRIS = varpris.
         IF tidallt.TIDLOG = FALSE THEN DO:
            tidallt.PRIS = 0.
         END.                                   
         ELSE DO:
            tidallt.VIBEFATTNING = varfabef.
         END.
      END.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN changecolsort_UI.
      RUN lastselectdyn_UI IN brwproc[1]. 
      FIND FIRST extratidallt WHERE extratidallt.DATUM = tidallt.DATUM  AND                        
      extratidallt.START = tidallt.START AND extratidallt.SLUT = tidallt.SLUT NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE extratidallt THEN DO:
         CREATE extratidallt.      
      END.
      BUFFER-COPY tidallt TO extratidallt.
      RUN vchangebrw_UI.   
   END. 
   avdatum = brwavdatum.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdela_UI WINDOW-1 
PROCEDURE valdela_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   ASSIGN
   vart = "DEL"     
   tidtabrec2 = RECID(tidallt)
   regdatum = tidallt.DATUM
   felvar = FALSE.
   FIND FIRST orginaltemp WHERE orginaltemp.RECTIDVIS = tidallt.RECTIDVIS NO-LOCK NO-ERROR.
   IF NOT AVAILABLE orginaltemp THEN DO:
      CREATE orginaltemp.
      BUFFER-COPY tidallt TO orginaltemp.
   END.
   RUN REGVEC.P.
   {SLUTARBW.I}
   IF tidallt.OVERTIDUTTAG = "F" THEN .
   ELSE DO:   
      IF tidallt.START < regstart THEN felvar = TRUE.
      IF tidallt.SLUT > regslut THEN felvar = TRUE.   
      IF felvar = TRUE THEN DO:
         MESSAGE "Registreringen ligger utanför ordinarie arbetstid och går ej att dela upp."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END. 
   END.
   {muswait.i}  
   {AVBGOMD.I}
   RUN FELDELA.W (INPUT pkod).      
   {AVBFRAMD.I}
   IF musz = FALSE THEN DO:
      FIND tidallt WHERE RECID(tidallt) = tidtabrec2
      NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      hjalpvar = tidallt.RECTIDVIS.
      /* tar bort orginalen*/
      FOR EACH extratidallt WHERE extratidallt.RECTIDVIS = hjalpvar:
         DELETE extratidallt.
      END.
      /*den uppdelade skapas här*/
      FOR EACH tidallt WHERE tidallt.RECTIDVIS = hjalpvar:                                                 
         CREATE extratidallt.
         BUFFER-COPY tidallt TO extratidallt.
      END.                         
      RUN changecolsort_UI.
      RUN lastselectdyn_UI IN brwproc[1].  
      RUN vchangebrw_UI.   
   END.
   {musarrow.i}
   musz = FALSE.                        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vchangebrw_UI WINDOW-1 
PROCEDURE vchangebrw_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      RETURN.                
   END.
   ELSE DO: 
      ASSIGN 
      varaonr = tidallt.AONR 
      vardelnr = tidallt.DELNR
      datvar = tidallt.DATUM.      
      debitering = 1. 
      IF allatider = 1 THEN DO:
         ASSIGN
         startvar = tidallt.START
         slutvar = tidallt.SLUT.
      END.
      ELSE IF allatider = 2 THEN DO:
         ASSIGN
         varlon = tidallt.LONTILLAGG
         varlant = tidallt.LONTILLANTAL
         debitering = 1. 
      END.
      ELSE IF allatider = 3  THEN DO:
         ASSIGN
         varlon = tidallt.BEREDSKAP
         varlant = tidallt.BERANT
         debitering = 1. 
      END.
      ELSE IF allatider = 4  THEN DO:
         ASSIGN 
         varlon = tidallt.TRAKTKOD
         varlant = tidallt.TRAKTANTAL
         debitering = 1. 
      END.
      IF tidallt.AONR NE "" THEN DO:  
         IF AVAILABLE utsokaonr THEN DO: 
            regdatum = tidallt.DATUM.
            {AOKOLLERS.I}         
            IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
            utsokaonr.AONRAVDATUM >= tidallt.DATUM THEN musz = musz.
            ELSE DO:
               MESSAGE Guru.Konstanter:gaol tidallt.AONR STRING(tidallt.DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.         
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.          
      END.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 18
      soktemp.SOKDATE[1] = tidallt.DATUM
      soktemp.SOKCHAR[1] = tidallt.PERSONALKOD
      soktemp.SOKINT[1] = allatider
      soktemp.SOKINT[2] = INTEGER(tidallt.RECTIDVIS).
      {SOKANROP.I}      
      IF soktemp.SOKLOG[1] = TRUE THEN DO:
         DISABLE BTN_BYTA BTN_DELA BTN_REG WITH FRAME {&FRAME-NAME}.
         ENABLE BTN_VISA WITH FRAME {&FRAME-NAME}.  
      END.
      ELSE DO:
         DISABLE BTN_VISA BTN_DELA WITH FRAME {&FRAME-NAME}. 
         IF allatider = 1 THEN ENABLE BTN_DELA WITH FRAME {&FRAME-NAME}.
         ENABLE BTN_BYTA WITH FRAME {&FRAME-NAME}.                 
      END.
      FIND FIRST extratidallt NO-LOCK NO-ERROR.
      IF AVAILABLE extratidallt THEN ENABLE BTN_REG WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_BYTA BTN_DELA BTN_REG BTN_VISA WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock100 WINDOW-1 
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock60 WINDOW-1 
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

