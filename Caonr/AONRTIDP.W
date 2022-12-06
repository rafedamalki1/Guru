&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: AONRTIDP.W

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

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
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER avblog AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{SOKDEF.I}
&Scoped-define NEW NEW  
{PHMTA.I}
&Scoped-define NEW 
{OMRTEMPW.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{AVDTEMP.I}

{EXTRATAB.I}  
{PHMT.I}
{HOPPSEK2W.I}
{AONRTIDPERS.I}


DEFINE NEW SHARED VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE helpbrwrid AS ROWID NO-UNDO.
DEFINE VARIABLE vemanvandare AS CHARACTER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE gampers AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE medvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.                     /* PHMTAPP.P */
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE houtlook AS Com-handle NO-UNDO.
DEFINE VARIABLE hnamespace AS Com-handle NO-UNDO.
DEFINE VARIABLE hfolder AS Com-handle NO-UNDO.
DEFINE VARIABLE hitem AS Com-handle NO-UNDO.
DEFINE VARIABLE num AS integer NO-UNDO.
DEFINE VARIABLE my1hand AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_PERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES personaltemp aonrtidperstemp

/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.AKTIV personaltemp.FORNAMN personaltemp.EFTERNAMN ~
personaltemp.TELEFON personaltemp.MOBILTEL personaltemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS personaltemp
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for BROWSE BRW_TIDPAONR                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDPAONR aonrtidperstemp.PERSONALKOD ~
aonrtidperstemp.FORNAMN aonrtidperstemp.EFTERNAMN aonrtidperstemp.ANVANDARE ~
aonrtidperstemp.DATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDPAONR 
&Scoped-define QUERY-STRING-BRW_TIDPAONR FOR EACH aonrtidperstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TIDPAONR OPEN QUERY BRW_TIDPAONR FOR EACH aonrtidperstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TIDPAONR aonrtidperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDPAONR aonrtidperstemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-20 RECT-21 CMB_JURP CMB_AKTIV ~
CMB_SEMFOR CMB_DELTID CMB_AVD CMB_VECKO CMB_FLEX CMB_OMR CMB_BEFATTNING ~
CMB_TIDSGODK CMB_TRA FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST ~
BTN_HAMT BRW_PERS BRW_TIDPAONR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN ~
FILL-IN_EPERSONALKOD BTN_OK BTN_AVB FILL-IN-PERSONAL 
&Scoped-Define DISPLAYED-OBJECTS CMB_JURP CMB_AKTIV CMB_SEMFOR CMB_DELTID ~
CMB_AVD CMB_VECKO CMB_FLEX CMB_OMR CMB_BEFATTNING CMB_TIDSGODK CMB_TRA ~
FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN FILL-IN_EPERSONALKOD FILL-IN-PERSONAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda personer tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla personer i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla personer väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Hämta och visa urval utifrån sökkriterierna ovan".

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE VARIABLE CMB_AKTIV AS CHARACTER FORMAT "X(256)":U INITIAL "Aktiv eller Inaktiv" 
     LABEL "Pers. med tidskrivning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Aktiv eller Inaktiv","Aktiv","Inaktiv" 
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ANST AS CHARACTER FORMAT "X(256)":U 
     LABEL "Anställningsform" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEFATTNING AS CHARACTER FORMAT "X(256)":U 
     LABEL "Befattning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BER AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beredskapsalternativ" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DELTID AS CHARACTER FORMAT "X(256)":U INITIAL "Ja eller Nej" 
     LABEL "Deltid" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ja eller Nej","Ja","Nej" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FLEX AS CHARACTER FORMAT "X(256)":U INITIAL "Ja eller Nej" 
     LABEL "Flex" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ja eller Nej","Ja","Nej" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Visa personal för" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_SEMFOR AS CHARACTER FORMAT "X(256)":U INITIAL "Ja eller Nej" 
     LABEL "Förskottssem" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ja eller Nej","Ja","Nej" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TIDSGODK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Godkänner tidsedlar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TRA AS CHARACTER FORMAT "X(256)":U 
     LABEL "Traktamentsavtal" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VECKO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Veckoschema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PERSONAL AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av personal" 
      VIEW-AS TEXT 
     SIZE 23.5 BY 1.25
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EPERSONALKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83.

DEFINE VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4
     SIZE 1.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 69 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.13 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.

DEFINE QUERY BRW_TIDPAONR FOR 
      aonrtidperstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS WINDOW-1 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      personaltemp.AKTIV COLUMN-LABEL "Tid!skrivning" FORMAT "Aktiv/Inaktiv":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 15
      personaltemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "x(11)":U
      personaltemp.MOBILTEL COLUMN-LABEL "Mobiltele" FORMAT "x(11)":U
      personaltemp.OMRADE FORMAT "x(6)":U
  ENABLE
      personaltemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 46.13 BY 12.63
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_TIDPAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDPAONR WINDOW-1 _STRUCTURED
  QUERY BRW_TIDPAONR NO-LOCK DISPLAY
      aonrtidperstemp.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "x(5)":U
      aonrtidperstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 15
      aonrtidperstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 25
      aonrtidperstemp.ANVANDARE COLUMN-LABEL "Upplagd av" FORMAT "X(9)":U
      aonrtidperstemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 69 BY 12.63
         TITLE "Tidskrivande personal".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_ALLVAL AT ROW 1 COL 1.5 NO-LABEL
     CMB_JURP AT ROW 2.71 COL 12.5
     CMB_AKTIV AT ROW 2.71 COL 69.13 COLON-ALIGNED
     CMB_SEMFOR AT ROW 2.71 COL 107.5 COLON-ALIGNED
     CMB_DELTID AT ROW 3.92 COL 107.5 COLON-ALIGNED
     CMB_AVD AT ROW 3.96 COL 11.5
     CMB_VECKO AT ROW 4 COL 69.13 COLON-ALIGNED
     CMB_FLEX AT ROW 5.13 COL 107.5 COLON-ALIGNED
     CMB_OMR AT ROW 5.21 COL 20.5 COLON-ALIGNED
     CMB_BEFATTNING AT ROW 5.29 COL 69.13 COLON-ALIGNED
     CMB_TIDSGODK AT ROW 6.46 COL 20.5 COLON-ALIGNED
     CMB_TRA AT ROW 6.58 COL 69.13 COLON-ALIGNED
     FILL-IN_FORNAMN AT ROW 7.71 COL 20.5 COLON-ALIGNED
     CMB_BER AT ROW 7.88 COL 69.13 COLON-ALIGNED
     FILL-IN_EFTERNAMN AT ROW 9.08 COL 20.5 COLON-ALIGNED
     CMB_ANST AT ROW 9.08 COL 69.13 COLON-ALIGNED
     BTN_HAMT AT ROW 10.83 COL 12.38
     BRW_PERS AT ROW 11.88 COL 1.5
     BRW_TIDPAONR AT ROW 11.88 COL 55.5
     BTN_ALLOVER AT ROW 14.5 COL 49.38
     BTN_OVER AT ROW 16.71 COL 49.38
     BTN_BACK AT ROW 18.92 COL 49.38
     BTN_ALLBACK AT ROW 21.08 COL 49.38
     FILL-IN_SPERSONALKOD AT ROW 24.92 COL 28.5 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 26.04 COL 18.5 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 27.13 COL 18.5 COLON-ALIGNED
     FILL-IN_EPERSONALKOD AT ROW 27.13 COL 84 COLON-ALIGNED AUTO-RETURN 
     BTN_OK AT ROW 28.38 COL 95.5
     BTN_AVB AT ROW 28.38 COL 110.5
     FILL-IN-PERSONAL AT ROW 1.08 COL 1.5 NO-LABEL
     "Hämta person:" VIEW-AS TEXT
          SIZE 12.75 BY .83 AT ROW 27.13 COL 57.13
     "Sök i urval:" VIEW-AS TEXT
          SIZE 12 BY .83 AT ROW 24.67 COL 2
     RECT-20 AT ROW 26.96 COL 55.5
     RECT-21 AT ROW 24.54 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.38 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: aonrtidperstemp T "?" NO-UNDO temp-db aonrtidperstemp
      TABLE: otidbeordtemp T "?" NO-UNDO temp-db otidbeordtemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Personaladministration"
         HEIGHT             = 28.42
         WIDTH              = 124.63
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
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
/* BROWSE-TAB BRW_PERS BTN_HAMT FRAME-A */
/* BROWSE-TAB BRW_TIDPAONR BRW_PERS FRAME-A */
ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_PERS:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_TIDPAONR:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_TIDPAONR:MAX-DATA-GUESS IN FRAME FRAME-A         = 300
       BRW_TIDPAONR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_TIDPAONR:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-PERSONAL IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.AKTIV
"personaltemp.AKTIV" "Tid!skrivning" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.personaltemp.TELEFON
"personaltemp.TELEFON" "Telefon" ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.personaltemp.MOBILTEL
"personaltemp.MOBILTEL" "Mobiltele" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = Temp-Tables.personaltemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDPAONR
/* Query rebuild information for BROWSE BRW_TIDPAONR
     _TblList          = "Temp-Tables.aonrtidperstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aonrtidperstemp.PERSONALKOD
"aonrtidperstemp.PERSONALKOD" "Enhet!/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aonrtidperstemp.FORNAMN
"aonrtidperstemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aonrtidperstemp.EFTERNAMN
"aonrtidperstemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aonrtidperstemp.ANVANDARE
"aonrtidperstemp.ANVANDARE" "Upplagd av" "X(9)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aonrtidperstemp.DATUM
"aonrtidperstemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TIDPAONR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_PERS IN FRAME FRAME-A /* Urvalsresultat */
DO:
   /*ccc*/
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLBACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLBACK WINDOW-1
ON CHOOSE OF BTN_ALLBACK IN FRAME FRAME-A /* Alla personer i listan */
DO:
   GET FIRST BRW_TIDPAONR NO-LOCK.
   DO WHILE AVAILABLE(aonrtidperstemp):
      RUN bort_UI.
      GET NEXT BRW_TIDPAONR NO-LOCK.
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLOVER WINDOW-1
ON CHOOSE OF BTN_ALLOVER IN FRAME FRAME-A /* Alla personer i listan */
DO:
   GET FIRST BRW_PERS NO-LOCK.
   DO WHILE AVAILABLE(personaltemp):
      RUN ny_UI.
      GET NEXT BRW_PERS NO-LOCK.
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
   IF svar THEN DO:      
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      avblog = TRUE.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK WINDOW-1
ON CHOOSE OF BTN_BACK IN FRAME FRAME-A
DO:   
   ASSIGN
   antal_valda = BRW_TIDPAONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
   antal_raknare = 1
   musz = TRUE.
   DO WHILE antal_raknare LE antal_valda:      
      status-ok = BRW_TIDPAONR:FETCH-SELECTED-ROW(antal_raknare).               
      RUN bort_UI.     
      antal_raknare = antal_raknare + 1.              
   END.   
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT WINDOW-1
ON CHOOSE OF BTN_HAMT IN FRAME FRAME-A /* Hämta och visa urval */
DO:   
   BTN_HAMT:LOAD-IMAGE("BILDER\xbtn_hamt_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN utvald_UI.   
   RUN hamta_UI (INPUT 1,INPUT "").  
   BTN_HAMT:LOAD-IMAGE("BILDER\xbtn_hamt.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   /*Dummy */
   status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   status-ok = BRW_PERS:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:  
   /*Nya eller ändrade*/
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   FOR EACH extrakopptemp WHERE extrakopptemp.PROGRAM = "AOTIDPERS" AND extrakopptemp.SOKINT[1] = 1 NO-LOCK:
      CREATE inextrakopptemp.
      BUFFER-COPY extrakopptemp TO inextrakopptemp.
   END.
   FIND FIRST inextrakopptemp NO-LOCK NO-ERROR.
   IF AVAILABLE inextrakopptemp THEN RUN extraspar_UI IN edataapph (INPUT TABLE inextrakopptemp).      
   /*De som ska tas bort*/
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   FOR EACH extrakopptemp WHERE extrakopptemp.PROGRAM = "AOTIDPERS" AND extrakopptemp.SOKINT[1] = 2 NO-LOCK:
      CREATE inextrakopptemp.
      BUFFER-COPY extrakopptemp TO inextrakopptemp.      
   END.
   FIND FIRST inextrakopptemp NO-LOCK NO-ERROR.
   IF AVAILABLE inextrakopptemp THEN RUN exbort_UI IN edataapph (INPUT TABLE inextrakopptemp).      
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER WINDOW-1
ON CHOOSE OF BTN_OVER IN FRAME FRAME-A
DO:
   ASSIGN
   antal_valda = BRW_PERS:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
   antal_raknare = 1
   musz = TRUE.
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_PERS:FETCH-SELECTED-ROW(antal_raknare).         
      RUN ny_UI.
      IF AVAILABLE aonrtidperstemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(aonrtidperstemp)).                           
      END.
      antal_raknare = antal_raknare + 1.              
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AKTIV WINDOW-1
ON VALUE-CHANGED OF CMB_AKTIV IN FRAME FRAME-A /* Pers. med tidskrivning */
DO:
   {muswait.i}
   CMB_AKTIV = INPUT CMB_AKTIV.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ANST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ANST WINDOW-1
ON VALUE-CHANGED OF CMB_ANST IN FRAME FRAME-A /* Anställningsform */
DO:
   {muswait.i}
   CMB_ANST = INPUT CMB_ANST.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD WINDOW-1
ON VALUE-CHANGED OF CMB_AVD IN FRAME FRAME-A /* Avdelning */
DO:   
   CMB_AVD = INPUT CMB_AVD.         
   {CMB_AVDN1.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BEFATTNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BEFATTNING WINDOW-1
ON VALUE-CHANGED OF CMB_BEFATTNING IN FRAME FRAME-A /* Befattning */
DO:
   {muswait.i}
   CMB_BEFATTNING = INPUT CMB_BEFATTNING.            
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BER WINDOW-1
ON VALUE-CHANGED OF CMB_BER IN FRAME FRAME-A /* Beredskapsalternativ */
DO:
   {muswait.i}
   CMB_BER = INPUT CMB_BER.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DELTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DELTID WINDOW-1
ON VALUE-CHANGED OF CMB_DELTID IN FRAME FRAME-A /* Deltid */
DO:
   {muswait.i}
   CMB_DELTID = INPUT CMB_DELTID.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FLEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FLEX WINDOW-1
ON VALUE-CHANGED OF CMB_FLEX IN FRAME FRAME-A /* Flex */
DO:
   {muswait.i}
   CMB_FLEX = INPUT CMB_FLEX.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP WINDOW-1
ON VALUE-CHANGED OF CMB_JURP IN FRAME FRAME-A /* Juridisp */
DO:    
   CMB_JURP = INPUT CMB_JURP.        
   {CMB_JURP.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR WINDOW-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME FRAME-A /* Visa personal för */
DO:
   {muswait.i}
   CMB_OMR = INPUT CMB_OMR.            
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.        
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_SEMFOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_SEMFOR WINDOW-1
ON VALUE-CHANGED OF CMB_SEMFOR IN FRAME FRAME-A /* Förskottssem */
DO:
   {muswait.i}
   CMB_SEMFOR = INPUT CMB_SEMFOR.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TIDSGODK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TIDSGODK WINDOW-1
ON VALUE-CHANGED OF CMB_TIDSGODK IN FRAME FRAME-A /* Godkänner tidsedlar */
DO:
   {muswait.i}
   CMB_TIDSGODK = INPUT CMB_TIDSGODK.            
   FIND FIRST godkannartemp WHERE godkannartemp.NAMN = CMB_TIDSGODK
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TRA WINDOW-1
ON VALUE-CHANGED OF CMB_TRA IN FRAME FRAME-A /* Traktamentsavtal */
DO:
   CMB_TRA = INPUT CMB_TRA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VECKO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VECKO WINDOW-1
ON ENTRY OF CMB_VECKO IN FRAME FRAME-A /* Veckoschema */
DO:
   CMB_VECKO = INPUT CMB_VECKO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VECKO WINDOW-1
ON LEAVE OF CMB_VECKO IN FRAME FRAME-A /* Veckoschema */
DO:
   CMB_VECKO = INPUT CMB_VECKO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EFTERNAMN WINDOW-1
ON LEAVE OF FILL-IN_EFTERNAMN IN FRAME FRAME-A /* Efternamn */
DO:
   FILL-IN_EFTERNAMN = INPUT FILL-IN_EFTERNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EPERSONALKOD WINDOW-1
ON ANY-KEY OF FILL-IN_EPERSONALKOD IN FRAME FRAME-A /* Enhet/Sign */
DO:
  {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EPERSONALKOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EPERSONALKOD IN FRAME FRAME-A /* Enhet/Sign */
DO:
   ASSIGN
   SESSION:DATA-ENTRY-RETURN = TRUE.
   FILL-IN_EPERSONALKOD = INPUT FILL-IN_EPERSONALKOD.
   IF FILL-IN_EPERSONALKOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST aonrtidperstemp WHERE aonrtidperstemp.PERSONALKOD = FILL-IN_EPERSONALKOD     
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE aonrtidperstemp THEN DO:  
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN_EPERSONALKOD NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE personaltemp THEN DO:
         {muswait.i}
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN PHMTAEN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT FILL-IN_EPERSONALKOD ,OUTPUT TABLE personaltemp APPEND).
         END.
         ELSE DO:
            RUN PHMTAEN.P  
            (INPUT FILL-IN_EPERSONALKOD ,OUTPUT TABLE personaltemp APPEND).
         END.          
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN_EPERSONALKOD NO-LOCK NO-ERROR.   
         IF AVAILABLE personaltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].          
            APPLY "CHOOSE" TO BTN_OVER IN FRAME FRAME-A.
         END.
         ELSE DO:
            MESSAGE FILL-IN_EPERSONALKOD + " finns inte!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(personaltemp)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].          
         APPLY "CHOOSE" TO BTN_OVER IN FRAME FRAME-A.
      END.
      
   END.
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FORNAMN WINDOW-1
ON LEAVE OF FILL-IN_FORNAMN IN FRAME FRAME-A /* Förnamn */
DO:
   FILL-IN_FORNAMN = INPUT FILL-IN_FORNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD WINDOW-1
ON LEAVE OF FILL-IN_SPERSONALKOD IN FRAME FRAME-A /* Enhet/Sign */
DO:
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.    
   IF VALID-HANDLE(persapph) THEN DELETE PROCEDURE persapph. 
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2 NO-ERROR.
      nyttaoapph2 = ?.
   END.   
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
   EMPTY TEMP-TABLE personaltemp NO-ERROR. 
   medvar = FALSE.   
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      RUN godk_UI IN persapph (INPUT "",OUTPUT TABLE godkannartemp).
   END.
   {JURPAVDSTART2.I}   
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   status-ok = CMB_TIDSGODK:ADD-LAST("Alla").   
   FOR EACH godkannartemp:
      status-ok = CMB_TIDSGODK:ADD-LAST(godkannartemp.NAMN).
   END.
   status-ok = CMB_BEFATTNING:ADD-LAST("Alla").
   status-ok = CMB_BER:ADD-LAST("Alla").
   status-ok = CMB_TRA:ADD-LAST("Alla").
   status-ok = CMB_ANST:ADD-LAST("Alla").
   status-ok = CMB_VECKO:ADD-LAST("Alla").

   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN 
   CMB_OMR = omrtemp.NAMN
   CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.       
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallpers = TRUE THEN DO:
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla" 
      CMB_OMR = Guru.Konstanter:gomrk + " : alla".
   END.      
   ASSIGN
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL = Guru.Konstanter:gavdk
   CMB_TIDSGODK:SCREEN-VALUE = "Alla"
   CMB_BEFATTNING:SCREEN-VALUE = "Alla"          
   CMB_AKTIV:SCREEN-VALUE = "Aktiv"
   CMB_AKTIV = "Aktiv"
   CMB_ANST:SCREEN-VALUE = "Alla"
   CMB_TRA:SCREEN-VALUE = "Alla"
   CMB_BER:SCREEN-VALUE = "Alla"
   CMB_VECKO:SCREEN-VALUE = "Alla".
   DISPLAY CMB_OMR CMB_TIDSGODK CMB_BEFATTNING CMB_ANST CMB_AKTIV CMB_TRA
   CMB_BER CMB_VECKO
   WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      CMB_SEMFOR:LABEL = "Spärr arbförk".     
   END.
   CREATE uppvalPerstemp.
   ASSIGN 
   uppvalPerstemp.OMRADE = Guru.Konstanter:gomrk + " : alla"
   uppvalPerstemp.TIDSGODK = "ALLA"
   uppvalPerstemp.AKTIV = "AKTIV ELLER INAKTIV"
   uppvalPerstemp.VECKOSCHEMA = "ALLA"
   uppvalPerstemp.BEREDSKAPSAVTAL = "ALLA"
   uppvalPerstemp.EFTERNAMN = ""
   uppvalPerstemp.FORNAMN = ""
   uppvalPerstemp.BEFATTNING = "ALLA"
   uppvalPerstemp.ANSTALLNING = "ALLA"
   uppvalPerstemp.TRAAVTAL = "ALLA"
   uppvalPerstemp.SEMFOR = "Ja eller Nej"
   uppvalPerstemp.DTID = "Ja eller Nej"
   uppvalPerstemp.FLX = "Ja eller Nej".
   RUN enable_UI.
   {FRMSIZE.I}
  IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.
  IF avdraknare < 2 THEN CMB_AVD:HIDDEN = TRUE.

 /** *************** *********' FIXA SEKRETESS ********** **************** */

   IF Guru.Konstanter:varforetypval[2] = 1 THEN DO:
      ASSIGN
      personaltemp.AKTIV:VISIBLE IN BROWSE BRW_PERS = FALSE
      CMB_TIDSGODK:HIDDEN = TRUE 
      CMB_BEFATTNING:HIDDEN = TRUE 
      CMB_ANST:HIDDEN = TRUE 
      CMB_AKTIV:HIDDEN = TRUE 
      CMB_TRA:HIDDEN = TRUE
      CMB_BER:HIDDEN = TRUE
      CMB_VECKO:HIDDEN = TRUE      
      CMB_FLEX:HIDDEN = TRUE
      CMB_DELTID:HIDDEN = TRUE
      CMB_SEMFOR:HIDDEN = TRUE.
   END.                    
   RUN goma_UI.
   RUN aonrtidpersh IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE aonrtidperstemp).
   RUN openbdynspec_UI IN brwproc[2]. 
   RUN PlaceraKnapp_UI.
   {KRYSSBORT.I}
   {musarrow.i}  
   {WIN_M_SLUT.I}
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.   
   IF jid NE "" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
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
   ASSIGN
   personaltemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_PERS = TRUE.   
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_TIDPAONR:HANDLE IN FRAME {&FRAME-NAME}).

   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_SPERSONALKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT "PERSONALKOD"). 
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_SFORNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "FORNAMN"). 
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_SEFTERNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "EFTERNAMN"). 
   
   FIND FIRST valsoktemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
   FIND FIRST uppvalPerstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE uppvalPerstemp THEN CREATE uppvalPerstemp.
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN EXTRATABHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN EXTRATABHMT.P PERSISTENT SET edataapph.
   END. 
   /*phmt.p*/
   IF Guru.Konstanter:appcon THEN DO:      
      RUN PHMTAPP.P PERSISTENT SET persapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PHMTAPP.P PERSISTENT SET persapph.
   END.     
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST extrakopptemp WHERE extrakopptemp.PROGRAM = "AOTIDPERS" AND                  
   extrakopptemp.KOPPLACHAR1 = aonrvar AND extrakopptemp.KOPPLAINT1 = delnrvar AND
   extrakopptemp.KOPPLACHAR2 = aonrtidperstemp.PERSONALKOD
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE extrakopptemp THEN CREATE extrakopptemp.
   ASSIGN
   extrakopptemp.PROGRAM = "AOTIDPERS"
   extrakopptemp.KOPPLACHAR1 = aonrvar    
   extrakopptemp.KOPPLAINT1 = delnrvar 
   extrakopptemp.KOPPLACHAR2 = aonrtidperstemp.PERSONALKOD         
   extrakopptemp.KOPPLAINT2 =  ?
   extrakopptemp.SOKCHAR[1] = Guru.Konstanter:globanv
   extrakopptemp.SOKDAT[1] = TODAY
   extrakopptemp.SOKINT[1] = 2.
   IF AVAILABLE aonrtidperstemp THEN DO:         
      DELETE aonrtidperstemp.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[2].      
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
  DISPLAY CMB_JURP CMB_AKTIV CMB_SEMFOR CMB_DELTID CMB_AVD CMB_VECKO CMB_FLEX 
          CMB_OMR CMB_BEFATTNING CMB_TIDSGODK CMB_TRA FILL-IN_FORNAMN CMB_BER 
          FILL-IN_EFTERNAMN CMB_ANST FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN 
          FILL-IN_SEFTERNAMN FILL-IN_EPERSONALKOD FILL-IN-PERSONAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-20 RECT-21 CMB_JURP CMB_AKTIV CMB_SEMFOR CMB_DELTID CMB_AVD 
         CMB_VECKO CMB_FLEX CMB_OMR CMB_BEFATTNING CMB_TIDSGODK CMB_TRA 
         FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST BTN_HAMT BRW_PERS 
         BRW_TIDPAONR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK 
         FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
         FILL-IN_EPERSONALKOD BTN_OK BTN_AVB FILL-IN-PERSONAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI WINDOW-1 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   ASSIGN
   CMB_AKTIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
   CMB_VECKO:HIDDEN = TRUE
   CMB_BEFATTNING:HIDDEN = TRUE
   CMB_TRA:HIDDEN = TRUE
   CMB_BER:HIDDEN = TRUE
   CMB_ANST:HIDDEN = TRUE
   CMB_SEMFOR:HIDDEN = TRUE
   CMB_DELTID:HIDDEN = TRUE
   CMB_FLEX:HIDDEN = TRUE   
   BTN_BACK:HIDDEN = FALSE
   BTN_OVER:HIDDEN = FALSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WINDOW-1 
PROCEDURE hamta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO. 
   {muswait.i}
   EMPTY TEMP-TABLE valsoktemp NO-ERROR.
   EMPTY TEMP-TABLE personaltemp NO-ERROR. 
   CREATE valsoktemp.
   ASSIGN
   valsoktemp.SOKVAL = 1
   valsoktemp.SOKINT[1]  =  Guru.Konstanter:globniv 
   valsoktemp.SOKCHAR[1] =  Guru.Konstanter:globanv.  
   /*tidskrivning*/
   /*phmt.p*/         
   /*TIDSEK ,PERSSEK*/
   RUN phamta_UI IN persapph (INPUT FALSE,INPUT FALSE,INPUT TABLE uppvalPerstemp,OUTPUT TABLE personaltemp,INPUT TABLE valsoktemp).
   
   IF vem = "" THEN DO:
     RUN openbdyn_UI IN brwproc[1] (INPUT "").
     RUN fetchselrowid_UI IN brwproc[1].
   END.
   ELSE DO:
      RUN openbdyn_UI IN brwproc[2] (INPUT "").
      RUN fetchselrowid_UI IN brwproc[2].      
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST aonrtidperstemp WHERE aonrtidperstemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE aonrtidperstemp THEN DO:
      CREATE aonrtidperstemp.
      ASSIGN
      aonrtidperstemp.PERSONALKOD   = personaltemp.PERSONALKOD         
      aonrtidperstemp.FORNAMN       = personaltemp.FORNAMN             
      aonrtidperstemp.EFTERNAMN     = personaltemp.EFTERNAMN           
      aonrtidperstemp.DATUM         = TODAY         
      aonrtidperstemp.ANVANDARE     = Guru.Konstanter:globanv.                
   END.
   FIND FIRST extrakopptemp WHERE extrakopptemp.PROGRAM = "AOTIDPERS" AND                  
   extrakopptemp.KOPPLACHAR1 = aonrvar AND extrakopptemp.KOPPLAINT1 = delnrvar AND
   extrakopptemp.KOPPLACHAR2 = aonrtidperstemp.PERSONALKOD
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE extrakopptemp THEN CREATE extrakopptemp.
   ASSIGN
   extrakopptemp.PROGRAM = "AOTIDPERS"
   extrakopptemp.KOPPLACHAR1 = aonrvar    
   extrakopptemp.KOPPLAINT1 = delnrvar 
   extrakopptemp.KOPPLACHAR2 = aonrtidperstemp.PERSONALKOD         
   extrakopptemp.KOPPLAINT2 =  ?
   extrakopptemp.SOKCHAR[1] = Guru.Konstanter:globanv
   extrakopptemp.SOKDAT[1] = TODAY
   extrakopptemp.SOKINT[1] = 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceraKnapp_UI WINDOW-1 
PROCEDURE PlaceraKnapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:StartRadForKnappar = CMB_JURP:ROW IN FRAME FRAME-A. 
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_JURP:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_AVD:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_OMR:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_TIDSGODK:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_FORNAMN:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(FILL-IN_EFTERNAMN:HANDLE).
   Guru.GlobalaVariabler:StartRadForKnappar = CMB_JURP:ROW. 
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_AKTIV:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_VECKO:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_BEFATTNING:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_TRA:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_BER:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_ANST:HANDLE).
       
   Guru.GlobalaVariabler:StartRadForKnappar = CMB_JURP:ROW.
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_SEMFOR:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_DELTID:HANDLE).
   Guru.Konstanter:PlaceraKnapparLodratt(CMB_FLEX:HANDLE). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utvald_UI WINDOW-1 
PROCEDURE utvald_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN   
   CMB_AVD = INPUT FRAME {&FRAME-NAME}  CMB_AVD  
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR 
   CMB_TIDSGODK = INPUT FRAME {&FRAME-NAME}  CMB_TIDSGODK  
   CMB_AKTIV = INPUT FRAME {&FRAME-NAME} CMB_AKTIV  
   CMB_VECKO = INPUT FRAME {&FRAME-NAME} CMB_VECKO
   CMB_BER = INPUT FRAME {&FRAME-NAME} CMB_BER
   CMB_SEMFOR = INPUT FRAME {&FRAME-NAME} CMB_SEMFOR
   CMB_FLEX = INPUT FRAME {&FRAME-NAME} CMB_FLEX
   CMB_DELTID = INPUT FRAME {&FRAME-NAME} CMB_DELTID
   FILL-IN_EFTERNAMN = INPUT FILL-IN_EFTERNAMN
   FILL-IN_FORNAMN = INPUT FILL-IN_FORNAMN
   CMB_BEFATTNING = INPUT FRAME {&FRAME-NAME} CMB_BEFATTNING
   CMB_ANST = INPUT FRAME {&FRAME-NAME} CMB_ANST
   CMB_TRA = INPUT FRAME {&FRAME-NAME} CMB_TRA.
   IF jid = "" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   FIND FIRST uppvalPerstemp NO-ERROR.
   FIND FIRST godkannartemp WHERE godkannartemp.NAMN = CMB_TIDSGODK NO-LOCK NO-ERROR.   
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN uppvalPerstemp.OMRADE = omrtemp.OMRADE.
   ELSE uppvalPerstemp.OMRADE = "ALLA".
   FIND FIRST aavtaltemp WHERE aavtaltemp.AVTALNAMN = CMB_VECKO AND            
   aavtaltemp.TYP = "VEC" NO-LOCK NO-ERROR.
   IF AVAILABLE aavtaltemp THEN uppvalPerstemp.VECKOSCHEMA = aavtaltemp.AVTALTYP.   
   ELSE uppvalPerstemp.VECKOSCHEMA = "ALLA".
   FIND FIRST aavtaltemp WHERE aavtaltemp.AVTALNAMN = CMB_BER AND            
   aavtaltemp.TYP = "BER" NO-LOCK NO-ERROR.
   IF AVAILABLE aavtaltemp THEN uppvalPerstemp.BEREDSKAPSAVTAL = aavtaltemp.AVTALTYP.
   ELSE uppvalPerstemp.BEREDSKAPSAVTAL = "ALLA".
   FIND FIRST aavtaltemp WHERE aavtaltemp.AVTALNAMN = CMB_BEFATTNING AND            
   aavtaltemp.TYP = "BEF" NO-LOCK NO-ERROR.
   IF AVAILABLE aavtaltemp THEN uppvalPerstemp.BEFATTNING = aavtaltemp.AVTALTYP.
   ELSE uppvalPerstemp.BEFATTNING = "ALLA".
   FIND FIRST aavtaltemp WHERE aavtaltemp.AVTALNAMN = CMB_ANST AND            
   aavtaltemp.TYP = "ANS" NO-LOCK NO-ERROR.
   IF AVAILABLE aavtaltemp THEN uppvalPerstemp.ANSTALLNING = aavtaltemp.AVTALTYP.
   ELSE uppvalPerstemp.ANSTALLNING = "ALLA".
   FIND FIRST aavtaltemp WHERE aavtaltemp.AVTALNAMN = CMB_TRA AND            
   aavtaltemp.TYP = "TRA" NO-LOCK NO-ERROR.
   IF AVAILABLE aavtaltemp THEN uppvalPerstemp.TRAAVTAL = aavtaltemp.AVTALTYP.
   ELSE uppvalPerstemp.TRAAVTAL = "ALLA".
   IF CMB_TIDSGODK NE "Alla" THEN uppvalPerstemp.TIDSGODK = godkannartemp.PERSONALKOD.
   ELSE uppvalPerstemp.TIDSGODK = "Alla".
   uppvalPerstemp.JUDID = "ALLA".
   FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.        
   IF AVAILABLE jurperstemp THEN uppvalPerstemp.JUDID = jurperstemp.JUDID.   
   FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
   IF AVAILABLE avdtemp THEN uppvalPerstemp.AVDNR = STRING(avdtemp.AVDELNINGNR).
   ELSE uppvalPerstemp.AVDNR = "ALLA".
   ASSIGN 
   uppvalPerstemp.AKTIV = CMB_AKTIV
   uppvalPerstemp.EFTERNAMN = FILL-IN_EFTERNAMN
   uppvalPerstemp.FORNAMN = FILL-IN_FORNAMN
   uppvalPerstemp.SEMFOR = CMB_SEMFOR
   uppvalPerstemp.FLX = CMB_FLEX
   uppvalPerstemp.DTID = CMB_DELTID
   uppvalPerstemp.RULLID = "ALLA".   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

