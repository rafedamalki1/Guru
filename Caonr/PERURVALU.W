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
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
{SOKDEF.I}
DEFINE TEMP-TABLE omrtemp2 NO-UNDO LIKE omrtemp.
{PHMT.I}
&Scoped-define SHARED SHARED
{KODERAVT.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE valaonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.  
DEFINE SHARED VARIABLE tillrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.  
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Arbetsorder-tid-övertid-traktamenten",
                "Tid-övertid-traktamenten / område", 
                "Det.lista Tid-övertid-trakt./område",
                "Arbetsorder-kostnadsreg.-kalkyl",
                "Arbetsorder-personal",
                "Arbetstidens fördelning",
                "Debiteringsgrad-område",
                "Personal-lönetillägg-personal",
                "Område-interna intäkter/kostnader"
                
     SIZE 39.5 BY 9.5
       NO-UNDO.
DEFINE VARIABLE FILL-IN-AR AS INTEGER NO-UNDO.     
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_valda2 AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare2 AS INTEGER NO-UNDO.
DEFINE VARIABLE buff AS CHARACTER NO-UNDO.
DEFINE VARIABLE perurvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.                     

DEFINE SHARED TEMP-TABLE perstill    
   FIELD EFTERNAMN AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD PERSONALKOD AS CHARACTER   
   FIELD PERSTILLREC AS RECID  
   FIELD OMRADE AS CHARACTER
   FIELD ANSVARIGTIDR AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD ASCENDING. 
   
DEFINE SHARED TEMP-TABLE tillpers
   FIELD LONTILLAGG AS CHARACTER
   FIELD VILART AS CHARACTER
   FIELD LONKODTEXT AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD TILLPERSREC AS RECID
   INDEX LONTILLAGG IS PRIMARY LONTILLAGG ASCENDING. 
   
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ANSV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ansvarigtemp omrtemp2 personaltemp koder

/* Definitions for BROWSE BRW_ANSV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_ANSV ansvarigtemp.PERSONALKOD ~
ansvarigtemp.FORNAMN ansvarigtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANSV 
&Scoped-define QUERY-STRING-BRW_ANSV FOR EACH ansvarigtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANSV OPEN QUERY BRW_ANSV FOR EACH ansvarigtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANSV ansvarigtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANSV ansvarigtemp


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp2.OMRADE omrtemp2.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp2


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK ~
    BY personaltemp.PERSONALKOD INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK ~
    BY personaltemp.PERSONALKOD INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for BROWSE BRW_TILL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TILL koder.VILART koder.LONKODTEXT ~
koder.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILL 
&Scoped-define QUERY-STRING-BRW_TILL FOR EACH koder NO-LOCK ~
    BY koder.VILART INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TILL OPEN QUERY BRW_TILL FOR EACH koder NO-LOCK ~
    BY koder.VILART INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TILL koder
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILL koder


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_ANSV}~
    ~{&OPEN-QUERY-BRW_OMR}~
    ~{&OPEN-QUERY-BRW_TILL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 RECT-23 RAD_SORT TOG_EJSUM BTN_NVE ~
BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 RAD_ALLVAL ~
RAD_ALLTID FBTN_VISA BRW_PERS BRW_TILL FBTN_EX FILL-IN_SFORNAMN ~
FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN FILL-IN-KOD FILL-IN-TEXT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_SORT TOG_EJSUM FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT RAD_ALLVAL RAD_ALLTID FILL-IN_SFORNAMN ~
FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN FILL-IN-KOD FILL-IN-TEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "x(4)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY .83.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Period från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "x(20)" 
     LABEL "Text" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY .83.

DEFINE VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 79 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          " ", 0
     SIZE 64.5 BY .75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_SORT AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Personal", 1,
"Tillägg", 2
     SIZE 28.5 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 2.42
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.5 BY 1.42
     BGCOLOR 8 .

DEFINE VARIABLE TOG_EJSUM AS LOGICAL INITIAL no 
     LABEL "Summera ej" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANSV FOR 
      ansvarigtemp SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      omrtemp2 SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.

DEFINE QUERY BRW_TILL FOR 
      koder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANSV C-Win _STRUCTURED
  QUERY BRW_ANSV NO-LOCK DISPLAY
      ansvarigtemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      ansvarigtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 15
      ansvarigtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 55 BY 13.58
         TITLE "Ansvariga tidredovisare".

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrtemp2.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp2.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 55 BY 13.58
         TITLE "Område/Organisation".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      personaltemp.EFTERNAMN FORMAT "x(256)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 55 BY 13.58
         TITLE "Personal".

DEFINE BROWSE BRW_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILL C-Win _STRUCTURED
  QUERY BRW_TILL NO-LOCK DISPLAY
      koder.VILART COLUMN-LABEL "Lönetillägg" FORMAT "X(8)":U
      koder.LONKODTEXT COLUMN-LABEL "Text" FORMAT "x(20)":U WIDTH 30
      koder.ENHET COLUMN-LABEL "Sort" FORMAT "x(2)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 50.5 BY 13.58
         TITLE "Lönetillägg".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_SORT AT ROW 2.25 COL 15.13 NO-LABEL
     TOG_EJSUM AT ROW 2.75 COL 71.13 WIDGET-ID 2
     BTN_NVE AT ROW 3.71 COL 25.25
     BTN_NVE-2 AT ROW 3.71 COL 43.88
     FILL-IN-STARTDAT AT ROW 3.96 COL 13.13 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 3.96 COL 32.13 COLON-ALIGNED
     BTN_FVE AT ROW 4.58 COL 25.25
     BTN_FVE-2 AT ROW 4.58 COL 43.88
     RAD_ALLVAL AT ROW 5.96 COL 1.5 NO-LABEL
     RAD_ALLTID AT ROW 7.21 COL 1.5 NO-LABEL
     FBTN_VISA AT ROW 8 COL 109.25
     BRW_PERS AT ROW 8.46 COL 1.5
     BRW_ANSV AT ROW 8.46 COL 1.5
     BRW_TILL AT ROW 8.46 COL 58.13
     BRW_OMR AT ROW 8.5 COL 1.5
     FBTN_EX AT ROW 9.08 COL 109.25
     FILL-IN_SFORNAMN AT ROW 22.33 COL 36 COLON-ALIGNED
     FILL-IN_SPERSONALKOD AT ROW 23.38 COL 17.5 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 23.38 COL 36 COLON-ALIGNED
     FILL-IN-KOD AT ROW 23.38 COL 75.5 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 23.38 COL 88.5 COLON-ALIGNED
     BTN_AVB AT ROW 23.54 COL 109.25
     "Sortera på:" VIEW-AS TEXT
          SIZE 11 BY .83 AT ROW 2.25 COL 2.25
     "Sök på:" VIEW-AS TEXT
          SIZE 8.13 BY .83 AT ROW 23.38 COL 59.38
     "Välj funktion:" VIEW-AS TEXT
          SIZE 23 BY 3 AT ROW 11.83 COL 9
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 8.13 BY .83 AT ROW 22.38 COL 3.13
     RECT-22 AT ROW 22.13 COL 1.5
     RECT-23 AT ROW 23.13 COL 58.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123 BY 23.96.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ansvarigtemp T "?" NO-UNDO temp-db ansvarigtemp
      TABLE: koder T "?" NO-UNDO temp-db koder
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: omrtemp2 T "?" NO-UNDO temp-db omrtemp2
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Välj personal / lönekod"
         HEIGHT             = 23.92
         WIDTH              = 123.25
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 123.75
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 123.75
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
/* BROWSE-TAB BRW_PERS FBTN_VISA DEFAULT-FRAME */
/* BROWSE-TAB BRW_ANSV BRW_PERS DEFAULT-FRAME */
/* BROWSE-TAB BRW_TILL BRW_ANSV DEFAULT-FRAME */
/* BROWSE-TAB BRW_OMR BRW_TILL DEFAULT-FRAME */
/* SETTINGS FOR BROWSE BRW_ANSV IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_ANSV:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_ANSV:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_ANSV:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_OMR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_OMR:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_OMR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_TILL:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_TILL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 500
       BRW_TILL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN_SEFTERNAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN_SFORNAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN_SPERSONALKOD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME DEFAULT-FRAME
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_SORT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANSV
/* Query rebuild information for BROWSE BRW_ANSV
     _TblList          = "Temp-Tables.ansvarigtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ansvarigtemp.PERSONALKOD
"ansvarigtemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ansvarigtemp.FORNAMN
"ansvarigtemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ansvarigtemp.EFTERNAMN
"ansvarigtemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_ANSV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrtemp2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp2.OMRADE
"omrtemp2.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.omrtemp2.NAMN
"omrtemp2.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no "ANGE ORAGNISTIONENSBENÄMNING" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.personaltemp.PERSONALKOD|yes"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILL
/* Query rebuild information for BROWSE BRW_TILL
     _TblList          = "Temp-Tables.koder"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.koder.VILART|yes"
     _FldNameList[1]   > Temp-Tables.koder.VILART
"koder.VILART" "Lönetillägg" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.koder.LONKODTEXT
"koder.LONKODTEXT" "Text" ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.koder.ENHET
"koder.ENHET" "Sort" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_TILL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Välj personal / lönekod */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Välj personal / lönekod */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE C-Win
ON CHOOSE OF BTN_FVE IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 C-Win
ON CHOOSE OF BTN_FVE-2 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE C-Win
ON CHOOSE OF BTN_NVE IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 C-Win
ON CHOOSE OF BTN_NVE-2 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX C-Win
ON CHOOSE OF FBTN_EX IN FRAME DEFAULT-FRAME /* Visa I EXCEL */
DO: 
   visvalvar = 2.
   RUN btnok_UI.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO: 
   visvalvar = 1.
   RUN btnok_UI.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   musz = FALSE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON ANY-KEY OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Löneart */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Löneart */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN-KOD = INPUT FILL-IN-KOD.          
   IF AVAILABLE koder THEN tillrec = RECID(koder).
   IF FILL-IN-KOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN-KOD + '*'.        
   FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR.
   FIND NEXT koder WHERE koder.VILART MATCHES posok       
   USE-INDEX LONTIL NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE koder THEN DO:      
      FIND FIRST koder WHERE koder.VILART MATCHES posok       
      USE-INDEX LONTIL NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE koder THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END. 
   END.     
   IF AVAILABLE koder THEN DO: 
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(koder)).
      RUN lastselectdyn_UI IN brwproc[4].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Period från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Period från */
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


&Scoped-define SELF-NAME FILL-IN-STOPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* till */
DO:
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STOPPDAT.
   RUN AlmanBtn.w.
   FILL-IN-STOPPDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEXT C-Win
ON ANY-KEY OF FILL-IN-TEXT IN FRAME DEFAULT-FRAME /* Text */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TEXT IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEXT C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-TEXT IN FRAME DEFAULT-FRAME /* Text */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN-TEXT = INPUT FILL-IN-TEXT.     
   IF AVAILABLE koder THEN tillrec = RECID(koder).
   IF FILL-IN-TEXT = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-TEXT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN-TEXT + '*'.
   FIND koder WHERE RECID(koder) = tillrec NO-LOCK NO-ERROR.
   FIND NEXT koder WHERE koder.LONKODTEXT MATCHES posok       
   USE-INDEX LONTIL NO-LOCK NO-ERROR.
   IF NOT AVAILABLE koder THEN DO:      
      FIND FIRST koder WHERE koder.LONKODTEXT MATCHES posok       
      USE-INDEX LONTIL NO-LOCK NO-ERROR.     
      IF NOT AVAILABLE koder THEN DO:      
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-TEXT IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.   
   IF AVAILABLE koder THEN DO:  
      status-ok = BRW_TILL:DESELECT-SELECTED-ROW(1).      
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(koder)).
      RUN lastselectdyn_UI IN brwproc[4].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN C-Win
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME DEFAULT-FRAME /* Efternamn */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME DEFAULT-FRAME /* Efternamn */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.  
   IF AVAILABLE personaltemp THEN persrec = RECID(personaltemp).   
   IF FILL-IN_SEFTERNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SEFTERNAMN + '*'.
   FIND personaltemp WHERE RECID(personaltemp) = persrec NO-LOCK NO-ERROR.
   FIND NEXT personaltemp WHERE personaltemp.EFTERNAMN MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.EFTERNAMN MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   END.   
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE personaltemp THEN DO:      
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(personaltemp)).
      RUN lastselectdyn_UI IN brwproc[3].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN C-Win
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.   
   IF AVAILABLE personaltemp THEN persrec = RECID(personaltemp).
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SFORNAMN + '*'.
   FIND personaltemp WHERE RECID(personaltemp) = persrec NO-LOCK NO-ERROR.
   FIND NEXT personaltemp WHERE personaltemp.FORNAMN MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:      
      FIND FIRST personaltemp WHERE personaltemp.FORNAMN MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   END.   
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE personaltemp THEN DO:
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(personaltemp)).
      RUN lastselectdyn_UI IN brwproc[3].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD C-Win
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.   
   status-ok = BRW_PERS:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_PERS.
   IF AVAILABLE personaltemp THEN persrec = RECID(personaltemp).
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SPERSONALKOD + '*'.
   FIND personaltemp WHERE RECID(personaltemp) = persrec NO-LOCK NO-ERROR.
   FIND NEXT personaltemp WHERE personaltemp.PERSONALKOD MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.    
   END.
   IF AVAILABLE personaltemp THEN DO:
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(personaltemp)).
      RUN lastselectdyn_UI IN brwproc[3].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ALLTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ALLTID C-Win
ON VALUE-CHANGED OF RAD_ALLTID IN FRAME DEFAULT-FRAME
DO:
   RAD_ALLTID = INPUT RAD_ALLTID.   
   EMPTY TEMP-TABLE koder NO-ERROR.    
   IF RAD_ALLTID = 1 THEN BRW_TILL:TITLE = "Lönetillägg".
   IF RAD_ALLTID = 2 THEN BRW_TILL:TITLE = "Beredskap".
   IF RAD_ALLTID = 3 THEN BRW_TILL:TITLE = "Traktamente".
   IF RAD_ALLTID = 4 THEN BRW_TILL:TITLE = "Övertid".
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KODSKAP2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT RAD_ALLTID ,OUTPUT TABLE koder).
   END.
   ELSE DO:
      RUN KODSKAP2.P 
      (INPUT RAD_ALLTID ,OUTPUT TABLE koder).                  
   END.
   OPEN QUERY BRW_TILL FOR EACH koder WHERE koder.VALBAR = TRUE BY koder.VILART.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ALLVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ALLVAL C-Win
ON VALUE-CHANGED OF RAD_ALLVAL IN FRAME DEFAULT-FRAME
DO:
   ASSIGN RAD_ALLVAL = INPUT RAD_ALLVAL.   
   IF RAD_ALLVAL = 2 THEN DO:
      ASSIGN
      BRW_ANSV:HIDDEN = TRUE  
      BRW_PERS:HIDDEN = TRUE
      FILL-IN_SEFTERNAMN:HIDDEN = TRUE
      FILL-IN_SFORNAMN:HIDDEN = TRUE
      FILL-IN_SPERSONALKOD:HIDDEN = TRUE     
      BRW_OMR:HIDDEN = FALSE. 
      DISPLAY BRW_OMR WITH FRAME {&FRAME-NAME}.
      ENABLE BRW_OMR WITH FRAME {&FRAME-NAME}.
      IF AVAILABLE omrtemp2 THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(omrtemp2)).
         RUN lastselectdyn_UI IN brwproc[2].
      END.
      ELSE DO:
         APPLY "HOME" TO BRW_OMR.
         status-ok = BRW_OMR:SELECT-FOCUSED-ROW() NO-ERROR.
      END.       
   END.        
   IF RAD_ALLVAL = 3 THEN DO:
      ASSIGN
      FILL-IN_SEFTERNAMN:HIDDEN = TRUE
      FILL-IN_SFORNAMN:HIDDEN = TRUE
      FILL-IN_SPERSONALKOD:HIDDEN = TRUE
      BRW_ANSV:HIDDEN = TRUE  
      BRW_PERS:HIDDEN = TRUE    
      BRW_OMR:HIDDEN = TRUE.
   END.
   IF RAD_ALLVAL = 4 THEN DO:
      ASSIGN
      BRW_ANSV:HIDDEN = TRUE  
      BRW_OMR:HIDDEN = TRUE
      FILL-IN_SEFTERNAMN:HIDDEN = FALSE
      FILL-IN_SFORNAMN:HIDDEN = FALSE
      FILL-IN_SPERSONALKOD:HIDDEN = FALSE     
      BRW_PERS:HIDDEN = FALSE. 
      DISPLAY BRW_PERS WITH FRAME {&FRAME-NAME}.
      ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.
      IF AVAILABLE personaltemp THEN DO:  
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(personaltemp)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.
      ELSE DO:
         APPLY "HOME" TO BRW_PERS.
         status-ok = BRW_PERS:SELECT-FOCUSED-ROW() NO-ERROR.
      END.       
   END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SORT C-Win
ON VALUE-CHANGED OF RAD_SORT IN FRAME DEFAULT-FRAME
DO:
   RAD_SORT = INPUT RAD_SORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EJSUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EJSUM C-Win
ON VALUE-CHANGED OF TOG_EJSUM IN FRAME DEFAULT-FRAME /* Summera ej */
DO:
  TOG_EJSUM = INPUT TOG_EJSUM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANSV
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
   IF VALID-HANDLE(perurvalapph) THEN DELETE PROCEDURE perurvalapph.
   IF VALID-HANDLE(persapph) THEN DELETE PROCEDURE persapph.
   
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
   
   EMPTY TEMP-TABLE personaltemp NO-ERROR.
   EMPTY TEMP-TABLE valsoktemp NO-ERROR.    
   CREATE valsoktemp.
   ASSIGN
   valsoktemp.SOKVAL = 1
   valsoktemp.SOKINT[1]  =  Guru.Konstanter:globniv 
   valsoktemp.SOKCHAR[1] =  Guru.Konstanter:globanv.
   /*tidsek = true perssek = true*/
   RUN ladda_UI IN perurvalapph (INPUT TRUE,INPUT TABLE valsoktemp,OUTPUT TABLE personaltemp).
   FOR EACH personaltemp WHERE personaltemp.AKTIV = FALSE:
      DELETE personaltemp.
   END.   
   /*/*OAVSETT FÖRETAG  - man får bara se de personer man har tidbehörighet för lena 20180117*/
   RUN ptidsek_UI IN persapph (INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE personaltemp).
   */
   
   RUN omr_UI IN perurvalapph (OUTPUT TABLE omrtemp2). 
   BRW_OMR:TITLE = Guru.Konstanter:gomrl + "/Organisation".
   omrtemp2.OMRADE:LABEL IN BROWSE BRW_OMR = Guru.Konstanter:gomrk.
   status-ok = RAD_ALLVAL:DELETE(" ").   
   status-ok = RAD_ALLVAL:ADD-LAST("Enhet/Sign",4).   
   status-ok = RAD_ALLVAL:ADD-LAST(Guru.Konstanter:gomrk, 2).
   status-ok = RAD_ALLVAL:ADD-LAST("Alla", 3).   
   ASSIGN  
   status-ok = RAD_ALLTID:DELETE("")
   status-ok = RAD_ALLTID:ADD-LAST("Lönetillägg", 1)
   status-ok = RAD_ALLTID:ADD-LAST("Beredskap", 2) 
   status-ok = RAD_ALLTID:ADD-LAST("Traktamente", 3)  
   status-ok = RAD_ALLTID:ADD-LAST("Övertid",4 ).
   ASSIGN
   FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY))
   FILL-IN-STOPPDAT = TODAY
   RAD_ALLTID = 1
   RAD_ALLVAL = 4.
   EMPTY TEMP-TABLE koder NO-ERROR.    
   {&WINDOW-NAME}:TITLE = "Lista " + SEL_UPP + ". Välj personal / lönekod.".
   RUN enable_UI.   
   {FRMSIZE.I}  
   ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.  
   OPEN QUERY BRW_PERS 
   FOR EACH personaltemp USE-INDEX PERSONALKOD NO-LOCK INDEXED-REPOSITION.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KODSKAP2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT RAD_ALLTID ,OUTPUT TABLE koder).
   END.
   ELSE DO:
      RUN KODSKAP2.P 
      (INPUT RAD_ALLTID ,OUTPUT TABLE koder).                  
   END.
   OPEN QUERY BRW_TILL FOR EACH koder  WHERE koder.VALBAR = TRUE BY koder.VILART.  
   APPLY "VALUE-CHANGED" TO RAD_SORT.
   APPLY "VALUE-CHANGED" TO RAD_ALLVAL.   
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE alla_UI C-Win 
PROCEDURE alla_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*OPEN*/
   OPEN QUERY persq FOR EACH personaltemp USE-INDEX PERSONALKOD NO-LOCK.
   GET FIRST persq NO-LOCK.
   DO WHILE AVAILABLE(personaltemp):      
      persrec = RECID(personaltemp).      
      CREATE perstill.
      ASSIGN    
      perstill.PERSTILLREC = RECID(personaltemp)
      perstill.PERSONALKOD = personaltemp.PERSONALKOD
      perstill.EFTERNAMN = personaltemp.EFTERNAMN
      perstill.OMRADE = personaltemp.OMRADE
      perstill.ANSVARIGTIDR = personaltemp.ANSVARIGTIDR
      perstill.FORNAMN = personaltemp.FORNAMN.  
      musz = TRUE.
      GET NEXT persq NO-LOCK.
   END. 
   CLOSE QUERY persq.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANSV:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_TILL:HANDLE IN FRAME {&FRAME-NAME}).   

   IF Guru.Konstanter:appcon THEN DO:
      RUN PERURVALAPP.P PERSISTENT SET perurvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PERURVALAPP.P PERSISTENT SET perurvalapph.
   END.
   
   RUN anvglob_UI IN perurvalapph (INPUT Guru.Konstanter:globanv).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ansv_UI C-Win 
PROCEDURE ansv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  IF BRW_PERS:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 AND
   RAD_ALLVAL = 4 THEN DO: 
      MESSAGE "Inga personer är markerade." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
   ELSE DO:  
      IF BRW_TILL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO: 
         MESSAGE "Inget lönetillägg är markerat." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.         
      END. 
      ELSE DO:   
         {muswait.i}
         EMPTY TEMP-TABLE perstill NO-ERROR. 
         EMPTY TEMP-TABLE tillpers NO-ERROR.          
         RUN valda_UI.   
         RUN valda2_UI.
         ASSIGN 
         persrec = RECID(personaltemp)        
         tillrec = RECID(koder)
         skrivut = FALSE.   
         ASSIGN
         bdatum = FILL-IN-STARTDAT
         avdatum = FILL-IN-STOPPDAT.   
         musz = FALSE.
         {AVBGOM.I}
         RUN PERSUTFU.W (INPUT RAD_ALLTID, INPUT TOG_EJSUM).
         {AVBFRAM.I}
         {musarrow.i}         
      END.
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
  DISPLAY RAD_SORT TOG_EJSUM FILL-IN-STARTDAT FILL-IN-STOPPDAT RAD_ALLVAL 
          RAD_ALLTID FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN 
          FILL-IN-KOD FILL-IN-TEXT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-22 RECT-23 RAD_SORT TOG_EJSUM BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 RAD_ALLVAL RAD_ALLTID FBTN_VISA 
         BRW_PERS BRW_TILL FBTN_EX FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD 
         FILL-IN_SEFTERNAMN FILL-IN-KOD FILL-IN-TEXT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enhet_UI C-Win 
PROCEDURE enhet_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_valda = BRW_PERS:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      FIND FIRST personaltemp USE-INDEX PERSONALKOD NO-LOCK.      
      persrec = RECID(personaltemp).
      persrec2 = RECID(personaltemp).
      CREATE perstill.
      ASSIGN    
      perstill.PERSTILLREC = RECID(personaltemp)
      perstill.PERSONALKOD = personaltemp.PERSONALKOD
      perstill.EFTERNAMN = personaltemp.EFTERNAMN 
      perstill.OMRADE = personaltemp.OMRADE
      perstill.ANSVARIGTIDR = personaltemp.ANSVARIGTIDR
      perstill.FORNAMN = personaltemp.FORNAMN.  
      musz = TRUE.
   END.   
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda :
      status-ok = BRW_PERS:FETCH-SELECTED-ROW(antal_raknare).
      IF antal_raknare = 1 THEN DO:
         persrec = RECID(personaltemp).
         persrec2 = RECID(personaltemp).
      END.      
      CREATE perstill.
      ASSIGN     
      perstill.PERSTILLREC = RECID(personaltemp)
      perstill.PERSONALKOD = personaltemp.PERSONALKOD
      perstill.EFTERNAMN = personaltemp.EFTERNAMN
      perstill.OMRADE = personaltemp.OMRADE
      perstill.ANSVARIGTIDR = personaltemp.ANSVARIGTIDR
      perstill.FORNAMN = personaltemp.FORNAMN.  
      antal_raknare = antal_raknare + 1.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omr_UI C-Win 
PROCEDURE omr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   EMPTY TEMP-TABLE perstill NO-ERROR.    
   EMPTY TEMP-TABLE valsoktemp NO-ERROR.    
   CREATE valsoktemp.
   ASSIGN
   valsoktemp.SOKVAL = 1
   valsoktemp.SOKINT[1]  =  Guru.Konstanter:globniv 
   valsoktemp.SOKCHAR[1] =  Guru.Konstanter:globanv.
      
   antal_valda = BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda :
      status-ok = BRW_OMR:FETCH-SELECTED-ROW(antal_raknare). 
      RUN perskap_UI IN perurvalapph (INPUT TRUE,INPUT TABLE valsoktemp,INPUT RAD_ALLVAL, INPUT " ", INPUT omrtemp2.OMRADE, OUTPUT TABLE perstill APPEND).
      antal_raknare = antal_raknare + 1.      
   END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tillagg_UI C-Win 
PROCEDURE tillagg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda2_UI C-Win 
PROCEDURE valda2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_valda2 = BRW_TILL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda2 = 0 THEN DO:      
      FIND FIRST koder USE-INDEX LONTIL NO-LOCK.      
      tillrec = RECID(koder).      
      CREATE tillpers.
      ASSIGN    
      tillpers.TILLPERSREC = RECID(koder)
      tillpers.LONTILLAGG = koder.LONTILLAGG
      tillpers.VILART = koder.VILART
      tillpers.LONKODTEXT = koder.LONKODTEXT
      tillpers.ENHET = koder.ENHET.  
      musz = TRUE.
   END.   
   antal_raknare2 = 1.
   DO WHILE antal_raknare2 LE antal_valda2 :
      status-ok = BRW_TILL:FETCH-SELECTED-ROW(antal_raknare2).
      IF antal_raknare2 = 1 THEN DO:
         tillrec = RECID(koder).         
      END.
      CREATE tillpers.
      ASSIGN 
      tillpers.TILLPERSREC = RECID(koder)
      tillpers.LONTILLAGG = koder.LONTILLAGG
      tillpers.VILART = koder.VILART
      tillpers.LONKODTEXT = koder.LONKODTEXT
      tillpers.ENHET = koder.ENHET           
      antal_raknare2 = antal_raknare2 + 1.
   END.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI C-Win 
PROCEDURE valda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
  IF RAD_ALLVAL = 2 THEN DO:     
     RUN omr_UI.
  END.   
  IF RAD_ALLVAL = 3 THEN DO:
     EMPTY TEMP-TABLE valsoktemp NO-ERROR.    
     CREATE valsoktemp.
     ASSIGN
     valsoktemp.SOKVAL = 1
     valsoktemp.SOKINT[1]  =  Guru.Konstanter:globniv 
     valsoktemp.SOKCHAR[1] =  Guru.Konstanter:globanv.
     RUN perskap_UI IN perurvalapph (INPUT TRUE,INPUT TABLE valsoktemp,INPUT RAD_ALLVAL, INPUT " ", INPUT " ", OUTPUT TABLE perstill).
  END.   
  IF RAD_ALLVAL = 4 THEN DO:     
     RUN enhet_UI.
  END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

