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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{SOKDEF.I}
&Scoped-define NEW 
/* {UPPGHMT.I} */
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{ANVTEMPS.I}
{SEKXGURUTEMP.I}
{OTIDBEORD.I}
{EXTRADATA.I}

{RHMT.I}
{VHMT.I}
{PHMT.I}
{DHMT.I}
{PHMTA.I}
{HOPPSEK2W.I}
&Scoped-define NEW NEW
{OMRTEMPW.I}
{AVDTEMP.I}

DEFINE NEW SHARED VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE vemanvandare AS CHARACTER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE gampers AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE medvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE num AS integer NO-UNDO.
DEFINE VARIABLE my1hand AS HANDLE NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyotbeord AS LOGICAL NO-UNDO.
DEFINE VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.                     /* PHMTAPP.P */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_OTBRD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES otidbeordtemp personaltemp

/* Definitions for BROWSE BRW_OTBRD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_OTBRD otidbeordtemp.PERSONALKOD ~
otidbeordtemp.FORNAMN otidbeordtemp.EFTERNAMN otidbeordtemp.AKTIV ~
otidbeordtemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OTBRD otidbeordtemp.AKTIV 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OTBRD otidbeordtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OTBRD otidbeordtemp
&Scoped-define QUERY-STRING-BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OTBRD OPEN QUERY BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OTBRD otidbeordtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OTBRD otidbeordtemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.AKTIV personaltemp.FORNAMN personaltemp.EFTERNAMN ~
personaltemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS personaltemp
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-20 RECT-21 CMB_JURP CMB_AKTIV ~
CMB_SEMFOR CMB_DELTID CMB_AVD CMB_VECKO CMB_FLEX CMB_OMR CMB_BEFATTNING ~
CMB_TIDSGODK CMB_TRA FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST ~
BTN_HAMT BRW_PERS BRW_OTBRD BTN_OVER BTN_BACK FILL-IN_EPERSONALKOD ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN BTN_OK BTN_AVB ~
FILL-IN-PERSONAL 
&Scoped-Define DISPLAYED-OBJECTS CMB_JURP CMB_AKTIV CMB_SEMFOR CMB_DELTID ~
CMB_AVD CMB_VECKO CMB_FLEX CMB_OMR CMB_BEFATTNING CMB_TIDSGODK CMB_TRA ~
FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST FILL-IN_EPERSONALKOD ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN FILL-IN-PERSONAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_OTBRD 
       MENU-ITEM m_E-post_adress LABEL "E-post adress från Outlook".


/* Definitions of the field level widgets                               */
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

DEFINE {&NEW} SHARED VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
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

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4
     SIZE 1.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.88 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 44.25 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OTBRD FOR 
      otidbeordtemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OTBRD WINDOW-1 _STRUCTURED
  QUERY BRW_OTBRD NO-LOCK DISPLAY
      otidbeordtemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(6)":U
            WIDTH 7
      otidbeordtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      otidbeordtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 14
      otidbeordtemp.AKTIV COLUMN-LABEL "Beord" FORMAT "Aktiv/Inaktiv":U
      otidbeordtemp.OMRADE FORMAT "X(6)":U
  ENABLE
      otidbeordtemp.AKTIV
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 74.88 BY 12.63
         TITLE "Övertidsbeordrare".

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
      personaltemp.OMRADE FORMAT "x(6)":U
  ENABLE
      personaltemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 44.25 BY 12.63
         TITLE "Urvalsresultat".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_ALLVAL AT ROW 1 COL 1 NO-LABEL
     CMB_JURP AT ROW 2.88 COL 13.25
     CMB_AKTIV AT ROW 2.88 COL 69.88 COLON-ALIGNED
     CMB_SEMFOR AT ROW 2.88 COL 108.38 COLON-ALIGNED
     CMB_DELTID AT ROW 4.08 COL 108.38 COLON-ALIGNED
     CMB_AVD AT ROW 4.13 COL 12.25
     CMB_VECKO AT ROW 4.17 COL 69.88 COLON-ALIGNED
     CMB_FLEX AT ROW 5.29 COL 108.38 COLON-ALIGNED
     CMB_OMR AT ROW 5.38 COL 21.25 COLON-ALIGNED
     CMB_BEFATTNING AT ROW 5.46 COL 69.88 COLON-ALIGNED
     CMB_TIDSGODK AT ROW 6.63 COL 21.25 COLON-ALIGNED
     CMB_TRA AT ROW 6.75 COL 69.88 COLON-ALIGNED
     FILL-IN_FORNAMN AT ROW 7.88 COL 21.25 COLON-ALIGNED
     CMB_BER AT ROW 8.04 COL 69.88 COLON-ALIGNED
     FILL-IN_EFTERNAMN AT ROW 9.25 COL 21.25 COLON-ALIGNED
     CMB_ANST AT ROW 9.25 COL 69.88 COLON-ALIGNED
     BTN_HAMT AT ROW 10.42 COL 13.13
     BRW_PERS AT ROW 11.46 COL 1.5
     BRW_OTBRD AT ROW 11.46 COL 50.38
     BTN_OVER AT ROW 16.08 COL 46
     BTN_BACK AT ROW 18.33 COL 46
     FILL-IN_EPERSONALKOD AT ROW 24.5 COL 78 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_SPERSONALKOD AT ROW 24.58 COL 29.25 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 25.71 COL 19.25 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 26.88 COL 19.25 COLON-ALIGNED
     BTN_OK AT ROW 26.92 COL 96.38
     BTN_AVB AT ROW 26.92 COL 111.38
     FILL-IN-PERSONAL AT ROW 1.25 COL 1.5 NO-LABEL
     "Sök i urval:" VIEW-AS TEXT
          SIZE 12 BY .83 AT ROW 24.42 COL 2.38
     "Hämta person:" VIEW-AS TEXT
          SIZE 12.75 BY .83 AT ROW 24.58 COL 51.13
     RECT-20 AT ROW 24.29 COL 50.5
     RECT-21 AT ROW 24.29 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 27.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
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
         HEIGHT             = 27.25
         WIDTH              = 125
         MAX-HEIGHT         = 28.33
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.33
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
/* BROWSE-TAB BRW_OTBRD BRW_PERS FRAME-A */
ASSIGN 
       BRW_OTBRD:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_OTBRD:HANDLE
       BRW_OTBRD:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_OTBRD:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_PERS:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-PERSONAL IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OTBRD
/* Query rebuild information for BROWSE BRW_OTBRD
     _TblList          = "Temp-Tables.otidbeordtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.otidbeordtemp.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" "x(6)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.otidbeordtemp.FORNAMN
"FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.otidbeordtemp.EFTERNAMN
"EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.otidbeordtemp.AKTIV
"AKTIV" "Beord" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.otidbeordtemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OTBRD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.AKTIV
"AKTIV" "Tid!skrivning" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.personaltemp.FORNAMN
"FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.personaltemp.EFTERNAMN
"EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.personaltemp.OMRADE
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_OTBRD
&Scoped-define SELF-NAME BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OTBRD WINDOW-1
ON ROW-LEAVE OF BRW_OTBRD IN FRAME FRAME-A /* Övertidsbeordrare */
DO:
   status-ok = BRW_OTBRD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE otidbeordtemp THEN DO:
      /*DISPLAY  otidbeordtemp.EPOST WITH BROWSE BRW_OTBRD.
      otidbeordtemp.EPOST = INPUT BROWSE BRW_OTBRD otidbeordtemp.EPOST.
      DISPLAY  otidbeordtemp.EPOST WITH BROWSE BRW_OTBRD.*/
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OTBRD WINDOW-1
ON VALUE-CHANGED OF BRW_OTBRD IN FRAME FRAME-A /* Övertidsbeordrare */
DO:
   status-ok = BRW_OTBRD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE otidbeordtemp THEN DO:
/*      DISPLAY  otidbeordtemp.EPOST WITH BROWSE BRW_OTBRD.*/
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME otidbeordtemp.AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL otidbeordtemp.AKTIV BRW_OTBRD _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF otidbeordtemp.AKTIV IN BROWSE BRW_OTBRD /* Beord */
DO:
   IF otidbeordtemp.AKTIV = TRUE THEN otidbeordtemp.AKTIV = FALSE.
   ELSE IF otidbeordtemp.AKTIV = FALSE THEN otidbeordtemp.AKTIV = TRUE.
   DISPLAY otidbeordtemp.AKTIV WITH BROWSE BRW_OTBRD. 
   RUN ny_UI (INPUT otidbeordtemp.PERSONALKOD).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_PERS IN FRAME FRAME-A /* Urvalsresultat */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
   /*
   IF NOT AVAILABLE personaltemp THEN RETURN NO-APPLY.
   FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE otidbeordtemp THEN DO:
      CREATE otidbeordtemp.
      BUFFER-COPY personaltemp TO otidbeordtemp.
   END.
   RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(otidbeordtemp)).              
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[2].   
   RUN and_UI.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   MESSAGE "Vill du spara dina ändringar?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL. 
   IF svar THEN DO:
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK WINDOW-1
ON CHOOSE OF BTN_BACK IN FRAME FRAME-A
DO:
   /*För alla som är ändrade på nått sätt*/
   ASSIGN
   antal_valda = BRW_OTBRD:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_OTBRD:FETCH-SELECTED-ROW(antal_raknare).         
      /*Gör check om den kan tas bort*/
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      RUN otbortcheck_UI IN otbeordapph (INPUT otidbeordtemp.PERSONALKOD,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.
      END.
      ELSE DO:
         otidbeordtemp.ANDRAD = 2.
         FIND FIRST extradatatemp WHERE extradatatemp.PROGRAM = "OTBEORD" AND                  
         extradatatemp.HUVUDCH = otidbeordtemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE extradatatemp THEN CREATE extradatatemp.
         ASSIGN
         extradatatemp.PROGRAM = "OTBEORD"                   
         extradatatemp.HUVUDCH = otidbeordtemp.PERSONALKOD         
         extradatatemp.SOKCHAR[1] = otidbeordtemp.EPOST
         extradatatemp.SOKLOG[1] = otidbeordtemp.AKTIV
         extradatatemp.SOKINT[1] = otidbeordtemp.ANDRAD.
         DELETE otidbeordtemp.
         RUN selnextprevrow_UI IN brwproc[2].
         
      END.
      antal_raknare = antal_raknare + 1.              
   END.
   
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].
   RUN title_UI IN brwproc[2].
   
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
   RELEASE personaltemp NO-ERROR.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:  
   RUN rowleave_UI.
   IF musz = FALSE THEN DO:
      RUN btnok_UI IN otbeordapph (INPUT TABLE extradatatemp).
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.
   musz = FALSE.
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
   musz = TRUE
   nyotbeord = FALSE.
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_PERS:FETCH-SELECTED-ROW(antal_raknare).  
      RUN ny_UI (INPUT personaltemp.PERSONALKOD).
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2].
      FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE otidbeordtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(otidbeordtemp)).              
         RUN lastselectdyn_UI IN brwproc[2].   
/*         IF nyotbeord = TRUE THEN APPLY "ENTRY" TO otidbeordtemp.EPOST IN BROWSE BRW_OTBRD.*/            
         nyotbeord = FALSE.
      END.      
      antal_raknare = antal_raknare + 1.   
   END.
   RUN title_UI IN brwproc[2]. 
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
   {CMB_AVD1.I}  
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
  /* IF CMB_TIDSGODK NE "Alla" THEN
   CMB_TIDSGODK:TOOLTIP = godkannartemp.FORNAMN + " " + godkannartemp.EFTERNAMN.
   */
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
   FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = FILL-IN_EPERSONALKOD     
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE otidbeordtemp THEN DO:  
      RUN psek_UI (INPUT FILL-IN_EPERSONALKOD).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         medvar = FALSE.
         MESSAGE "Du har inte behörighet att göra ändringar på denna person" VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
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
   
   /*
   APPLY "LEAVE" TO FILL-IN_EPERSONALKOD.
   APPLY "ENTRY" TO otidbeordtemp.EPOST IN BROWSE BRW_OTBRD.            
   
   
   CHOOSE FIELD FILL-IN_sPERSONALKOD AUTO-RETURN WITH FRAME {&FRAME-NAME}. 
    APPLY "ENTRY" TO  FILL-IN_sPERSONALKOD IN FRAME {&FRAME-NAME}. 
   */
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


&Scoped-define SELF-NAME m_E-post_adress
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_E-post_adress WINDOW-1
ON CHOOSE OF MENU-ITEM m_E-post_adress /* E-post adress från Outlook */
DO:
   
   IF AVAILABLE otidbeordtemp THEN DO:
      {muswait.i}  
      {AVBGOM.I}
      RUN EPOSTVAL.W (INPUT otidbeordtemp.FORNAMN + " " + otidbeordtemp.EFTERNAMN,OUTPUT epostvar).
      {AVBFRAM.I}   
      {musarrow.i}
    /*  IF epostvar NE "" THEN DO:
         otidbeordtemp.EPOST = epostvar.
         DISPLAY  otidbeordtemp.EPOST WITH BROWSE BRW_OTBRD.      
      END.*/      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OTBRD
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
   RUN borthandle_UI IN otbeordapph.
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   IF VALID-HANDLE(persapph) THEN DELETE PROCEDURE persapph NO-ERROR.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
   IF svar = FALSE THEN DO:
      MESSAGE "Vill du spara dina ändringar?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL. 
      IF svar THEN DO:
         APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.      
   END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
   MESSAGE "Vill du spara dina ändringar?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL. 
   IF svar THEN DO:
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
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
   EMPTY TEMP-TABLE avdtemp NO-ERROR. 
   EMPTY TEMP-TABLE omrtemp NO-ERROR. 
   medvar = FALSE.
   DEFINE VARIABLE juidraknare AS INTEGER NO-UNDO.  
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (OUTPUT TABLE avdtemp).
   END.
   ELSE DO:
      RUN AVSKAP.P
      (OUTPUT TABLE avdtemp).
   END.
   {OMRHMT.I}     
   CMB_OMR:LIST-ITEMS = "".
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
   END.
   CMB_JURP:LIST-ITEMS = "". 
   status-ok = CMB_JURP:ADD-FIRST("Alla").
   juidraknare = 0.
   FOR EACH jurperstemp:
     status-ok = CMB_JURP:ADD-LAST(jurperstemp.NAMN).
     juidraknare = juidraknare + 1.
   END.         
   IF Guru.Konstanter:globjid = "" THEN DO:
      CMB_JURP = "Alla".
      CMB_JURP:SCREEN-VALUE = "Alla".   
   END.    
   ELSE DO:  
      FIND FIRST jurperstemp WHERE jurperstemp.JUDID = Guru.Konstanter:globjid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE jurperstemp THEN DO:
         FIND FIRST jurperstemp NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE jurperstemp THEN DO:
         CMB_JURP = jurperstemp.NAMN.
         CMB_JURP:SCREEN-VALUE = jurperstemp.NAMN.
      END.    
      ELSE DO:
         CMB_JURP = "".
         CMB_JURP:SCREEN-VALUE = "".     
      END.
   END.   
   IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.
   CMB_AVD:LIST-ITEMS = "".
   status-ok = CMB_AVD:ADD-FIRST("Alla").
   FOR EACH judavdtemp:
      status-ok = CMB_AVD:ADD-LAST(judavdtemp.AVDELNINGNAMN).
   END.
   CMB_AVD:SCREEN-VALUE = "Alla".
   
   status-ok = CMB_TIDSGODK:ADD-LAST("Alla").   
   FOR EACH godkannartemp:
      status-ok = CMB_TIDSGODK:ADD-LAST(godkannartemp.NAMN).
   END.
   
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.       
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallpers = TRUE THEN DO:
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
   END.      
   
   ASSIGN
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL= Guru.Konstanter:gavdk
   CMB_TIDSGODK:SCREEN-VALUE = "Alla".
   DISPLAY CMB_OMR CMB_TIDSGODK CMB_AVD WITH FRAME {&FRAME-NAME}.

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
   CMB_AKTIV:SCREEN-VALUE = "Aktiv".
   IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.
   OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
   OPEN QUERY BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK.
   RUN title_UI IN brwproc[2].   
   {FRMSIZE.I}
  
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
/*   RELEASE otidbeordtemp NO-ERROR.
*/
   status-ok = BRW_OTBRD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   {KRYSSBORT.I}
   {musarrow.i}   
   {WIN_M_SLUT.I}
   
   my1hand = FILL-IN_EPERSONALKOD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_OTBRD:MOVE-AFTER-TAB-ITEM(my1hand).
   
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
      (INPUT BRW_OTBRD:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_SPERSONALKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT "PERSONALKOD"). 
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_SFORNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "FORNAMN"). 
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_SEFTERNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "EFTERNAMN"). 
   
   FIND FIRST valsoktemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
   FIND FIRST uppvalPerstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE uppvalPerstemp THEN CREATE uppvalPerstemp.
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.
   END.
   /*phmt.p*/         
   IF Guru.Konstanter:appcon THEN DO:      
      RUN PHMTAPP.P PERSISTENT SET persapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PHMTAPP.P PERSISTENT SET persapph.
   END.           
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
   END. 
   RUN othmt_UI IN otbeordapph (OUTPUT TABLE otidbeordtemp).
   
/*    CREATE "outlook.application" houtlook NO-ERROR. */
/*    IF ERROR-STATUS:ERROR THEN DO:                  */
/*    END.                                            */
/*    ELSE DO:                                        */
/*       hnamespace = houtlook:getnamespace("MAPI").  */
/*       hfolder = hnamespace:addresslists:ITEM(1).   */
/*    END.                                            */
/*                                                    */
    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE and_UI WINDOW-1 
PROCEDURE and_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /* status-ok = BRW_OTBRD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   pkod = otidbeordtemp.PERSONALKOD.
   IF musz =  TRUE THEN musz = FALSE.
   ELSE DO:     
      {muswait.i}
      IF musz = TRUE THEN DO:
         musz = FALSE.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.           
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      {musarrow.i} 
      
   END.*/   
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
   status-ok = BRW_OTBRD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_OTBRD.
   IF NOT AVAILABLE otidbeordtemp THEN DO:
      RETURN.
   END.
   ELSE DO:
      /*Ta bort i extradata*/
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
          FILL-IN_EFTERNAMN CMB_ANST FILL-IN_EPERSONALKOD FILL-IN_SPERSONALKOD 
          FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN FILL-IN-PERSONAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-20 RECT-21 CMB_JURP CMB_AKTIV CMB_SEMFOR CMB_DELTID CMB_AVD 
         CMB_VECKO CMB_FLEX CMB_OMR CMB_BEFATTNING CMB_TIDSGODK CMB_TRA 
         FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST BTN_HAMT BRW_PERS 
         BRW_OTBRD BTN_OVER BTN_BACK FILL-IN_EPERSONALKOD FILL-IN_SPERSONALKOD 
         FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN BTN_OK BTN_AVB FILL-IN-PERSONAL 
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
   /*DISPLAY TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA WITH FRAME {&FRAME-NAME}.*/
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
   /*phmt.p*/         
   /*TIDSEK ,PERSSEK*/
   RUN phamta_UI IN persapph (INPUT FALSE,INPUT TRUE,INPUT TABLE uppvalPerstemp,OUTPUT TABLE personaltemp,INPUT TABLE valsoktemp).
   
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
 
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEBUGGER:SET-BREAK().
   nyotbeord = FALSE.
   FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE otidbeordtemp THEN DO:
      nyotbeord = TRUE.
      CREATE otidbeordtemp.
      ASSIGN
      otidbeordtemp.PERSONALKOD = personaltemp.PERSONALKOD
      otidbeordtemp.FORNAMN = personaltemp.FORNAMN
      otidbeordtemp.EFTERNAMN = personaltemp.EFTERNAMN
      otidbeordtemp.EPOST = SUBSTRING(personaltemp.PERSONSOK,20)
      otidbeordtemp.AKTIV = TRUE
      otidbeordtemp.OMRADE = personaltemp.OMRADE
      otidbeordtemp.ANDRAD = 1. 
      FIND FIRST extradatatemp WHERE extradatatemp.PROGRAM = "OTBEORD" AND                  
      extradatatemp.HUVUDCH = otidbeordtemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN CREATE extradatatemp.
      ASSIGN
      extradatatemp.PROGRAM = "OTBEORD"                   
      extradatatemp.HUVUDCH = otidbeordtemp.PERSONALKOD         
      extradatatemp.SOKCHAR[1] = otidbeordtemp.EPOST
      extradatatemp.SOKLOG[1] = otidbeordtemp.AKTIV
      extradatatemp.SOKINT[1] = otidbeordtemp.ANDRAD.
      
   END.
   ELSE DO:
      otidbeordtemp.ANDRAD = 1.
      FIND FIRST extradatatemp WHERE  extradatatemp.PROGRAM = "OTBEORD" AND                  
      extradatatemp.HUVUDCH = otidbeordtemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN CREATE extradatatemp.
      ASSIGN
      extradatatemp.PROGRAM = "OTBEORD"                   
      extradatatemp.HUVUDCH = otidbeordtemp.PERSONALKOD         
      extradatatemp.SOKCHAR[1] = otidbeordtemp.EPOST
      extradatatemp.SOKLOG[1] = otidbeordtemp.AKTIV
      extradatatemp.SOKINT[1] = otidbeordtemp.ANDRAD.
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE psek_UI WINDOW-1 
PROCEDURE psek_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER pkodvar AS CHARACTER NO-UNDO.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 60
   soktemp.SOKINT[1] = Guru.Konstanter:globniv
   soktemp.SOKCHAR[1] = pkodvar
   soktemp.SOKCHAR[2] = Guru.Konstanter:globanv
   soktemp.SOKLOG[1] = FALSE
   soktemp.SOKLOG[2] = FALSE.
   {SOKANROP.I}      
   ASSIGN
   musz   = soktemp.SOKLOG[1]
   medvar = soktemp.SOKLOG[2].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI WINDOW-1 
PROCEDURE rowleave_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
  IF entryepost = TRUE THEN DO:
   /*  IF AVAILABLE otidbeordtemp THEN DO:   
        APPLY "LEAVE" TO otidbeordtemp.EPOST IN BROWSE BRW_OTBRD.
     END.*/
  END.  
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
   CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP
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
   uppvalPerstemp.RULLID = "ALLA"   .   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

