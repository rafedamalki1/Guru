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
{FAKTTYPDEF.I}
{DIRDEF.I}
{BRWSOK.I}
{OMRTEMPW.I}
{ANSPROJBER.I}
{SOKDEF.I}
{ARBATE.I}
{PHMT.I}
{VISUPPTMP.I}
{SKVSUM.I}

{TIDDETTEMP.I}

DEFINE TEMP-TABLE delNRaonr NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER 
   INDEX AONR IS PRIMARY AONR DELNR.
&Scoped-define NEW 
&Scoped-define SHARED  
{BESTKUNDALLT.I}
{AVDTEMP.I}
DEFINE  TEMP-TABLE avtaltemp NO-UNDO
   FIELD AVTALNAMN AS CHARACTER
   FIELD AVTALTYP AS CHARACTER
   FIELD TYP AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX TYP IS PRIMARY TYP ORDNING.

DEFINE TEMP-TABLE upvaltemp2  NO-UNDO
   FIELD JUDID AS CHARACTER
   FIELD AVDNR AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD TIDSGODK AS CHARACTER
   FIELD AKTIV AS CHARACTER 
   FIELD VECKOSCHEMA AS CHARACTER
   FIELD BEREDSKAPSAVTAL AS CHARACTER
   FIELD EFTERNAMN  AS CHARACTER   
   FIELD FORNAMN AS CHARACTER  
   FIELD BEFATTNING AS CHARACTER
   FIELD ANSTALLNING AS CHARACTER
   FIELD TRAAVTAL AS CHARACTER
   FIELD SEMFOR AS CHARACTER
   FIELD FLX AS CHARACTER
   FIELD DTID AS CHARACTER 
   FIELD RULLID AS CHARACTER. /*ccc */

{TIDUTTT.I}
DEFINE TEMP-TABLE vpers           
   FIELD PERSONALKOD AS CHARACTER
   FIELD TOTTIM AS DECIMAL
   FIELD INVEST AS DECIMAL
   FIELD DRIFT AS DECIMAL
   FIELD RAKN AS INTEGER.
   
DEFINE INPUT-OUTPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR omrtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR personaltemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valperstemp.

/* Local Variable Definitions ---                                       */
&Scoped-define NEW

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.                     /* PHMTAPP.P */
DEFINE VARIABLE valdarec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE VARIABLE akivinexh AS HANDLE NO-UNDO.
DEFINE VARIABLE dirutfapph AS HANDLE NO-UNDO.
DEFINE VARIABLE rakn AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnz AS CHARACTER NO-UNDO.
DEFINE VARIABLE medvar AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_FRAO

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdaao personaltemp valperstemp

/* Definitions for BROWSE BRW_FRAO                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FRAO valdaao.AONR valdaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRAO 
&Scoped-define QUERY-STRING-BRW_FRAO FOR EACH valdaao NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FRAO OPEN QUERY BRW_FRAO FOR EACH valdaao NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FRAO valdaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRAO valdaao


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.AKTIV personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH personaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for BROWSE BRW_VPERS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VPERS valperstemp.PERSONALKOD ~
valperstemp.FORNAMN valperstemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VPERS 
&Scoped-define QUERY-STRING-BRW_VPERS FOR EACH valperstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VPERS OPEN QUERY BRW_VPERS FOR EACH valperstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VPERS valperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VPERS valperstemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-49 RECT-SOK CMB_ARTAL CMB_UPP BTN_NVE ~
BTN_NVE-2 RAD_PERIOD FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 ~
TOG_GOD TOG_ARBF CMB_JURP CMB_AKTIV CMB_SEMFOR FBTN_VISA CMB_AVD CMB_VECKO ~
CMB_DELTID FBTN_EX CMB_OMR CMB_BEFATTNING CMB_FLEX CMB_TIDSGODK CMB_TRA ~
CMB_RULL FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST BTN_HAMT ~
BRW_PERS BRW_VPERS BRW_FRAO BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK ~
FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN ~
FILL-IN_EPERSONALKOD BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS CMB_ARTAL CMB_UPP RAD_PERIOD ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT TOG_GOD TOG_ARBF CMB_JURP CMB_AKTIV ~
CMB_SEMFOR CMB_AVD CMB_VECKO CMB_DELTID CMB_OMR CMB_BEFATTNING CMB_FLEX ~
CMB_TIDSGODK CMB_TRA CMB_RULL FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN ~
CMB_ANST FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN ~
FILL-IN_EPERSONALKOD FILL-IN-AOTEXT FILL-IN-SOK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 21.5 BY 1.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

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

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

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

DEFINE VARIABLE CMB_RULL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rullande schema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

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

DEFINE VARIABLE CMB_UPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Val av lista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VECKO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Veckoschema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av personal" 
      VIEW-AS TEXT 
     SIZE 24.5 BY 1.04
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK AS CHARACTER FORMAT "X(256)":U INITIAL "Sök på:" 
      VIEW-AS TEXT 
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

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

DEFINE VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa allt", 3,
"Visning per år", 1,
"Visning per period", 2
     SIZE 65 BY 1 TOOLTIP "Vising av kostnader och intäkter." NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52.5 BY 1.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51.5 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE TOG_ARBF AS LOGICAL INITIAL no 
     LABEL "Endast arbetstidsförkortning" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.5 BY .83 NO-UNDO.

DEFINE VARIABLE TOG_GOD AS LOGICAL INITIAL no 
     LABEL "Endast ekonomi och lönekörda uppgifter" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FRAO FOR 
      valdaao SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.

DEFINE QUERY BRW_VPERS FOR 
      valperstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FRAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRAO C-Win _STRUCTURED
  QUERY BRW_FRAO NO-LOCK DISPLAY
      valdaao.AONR COLUMN-LABEL "aonr" FORMAT "X(6)":U
      valdaao.ORT COLUMN-LABEL "Frånvaro" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 20.5 BY 10
         TITLE "Frånvaro".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      personaltemp.AKTIV COLUMN-LABEL "Tid!skrivning" FORMAT "Aktiv/Inaktiv":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 50 BY 10
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VPERS C-Win _STRUCTURED
  QUERY BRW_VPERS NO-LOCK DISPLAY
      valperstemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      valperstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      valperstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 45.63 BY 10
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_ARTAL AT ROW 2.54 COL 82
     CMB_UPP AT ROW 2.75 COL 13.25 COLON-ALIGNED
     BTN_NVE AT ROW 3.67 COL 87
     BTN_NVE-2 AT ROW 3.67 COL 106
     RAD_PERIOD AT ROW 3.88 COL 1.5 NO-LABEL
     FILL-IN-STARTDAT AT ROW 3.88 COL 75 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 3.88 COL 94 COLON-ALIGNED
     BTN_FVE AT ROW 4.5 COL 87
     BTN_FVE-2 AT ROW 4.5 COL 106
     TOG_GOD AT ROW 5.17 COL 1.5
     TOG_ARBF AT ROW 5.17 COL 49.5
     CMB_JURP AT ROW 6.42 COL 12
     CMB_AKTIV AT ROW 6.42 COL 58.38 COLON-ALIGNED
     CMB_SEMFOR AT ROW 6.42 COL 93.25 COLON-ALIGNED
     FBTN_VISA AT ROW 6.92 COL 111
     CMB_AVD AT ROW 7.54 COL 11
     CMB_VECKO AT ROW 7.58 COL 58.38 COLON-ALIGNED
     CMB_DELTID AT ROW 7.58 COL 93.25 COLON-ALIGNED
     FBTN_EX AT ROW 8 COL 111
     CMB_OMR AT ROW 8.67 COL 20 COLON-ALIGNED
     CMB_BEFATTNING AT ROW 8.75 COL 58.38 COLON-ALIGNED
     CMB_FLEX AT ROW 8.75 COL 93.25 COLON-ALIGNED
     CMB_TIDSGODK AT ROW 9.79 COL 20 COLON-ALIGNED
     CMB_TRA AT ROW 9.92 COL 58.38 COLON-ALIGNED
     CMB_RULL AT ROW 9.92 COL 93.25 COLON-ALIGNED
     FILL-IN_FORNAMN AT ROW 10.92 COL 20 COLON-ALIGNED
     CMB_BER AT ROW 11.08 COL 58.38 COLON-ALIGNED
     FILL-IN_EFTERNAMN AT ROW 12.17 COL 20 COLON-ALIGNED
     CMB_ANST AT ROW 12.17 COL 58.38 COLON-ALIGNED
     BTN_HAMT AT ROW 13.46 COL 20.5
     BRW_PERS AT ROW 14.75 COL 1.5
     BRW_VPERS AT ROW 14.75 COL 56.38
     BRW_FRAO AT ROW 14.75 COL 103.5
     BTN_ALLOVER AT ROW 16.88 COL 52.13
     BTN_OVER AT ROW 18.88 COL 52.13
     BTN_BACK AT ROW 20.88 COL 52.13
     FILL-IN-AR AT ROW 22.46 COL 90.25 COLON-ALIGNED NO-LABEL
     BTN_ALLBACK AT ROW 22.88 COL 52.13
     FILL-IN_SFORNAMN AT ROW 25.25 COL 30.13 COLON-ALIGNED
     FILL-IN_SPERSONALKOD AT ROW 26.33 COL 11 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 26.33 COL 30.13 COLON-ALIGNED
     FILL-IN_EPERSONALKOD AT ROW 26.38 COL 86.25 COLON-ALIGNED
     BTN_AVSL AT ROW 26.46 COL 111
     FILL-IN-AOTEXT AT ROW 1.5 COL 1.5 NO-LABEL
     FILL-IN-SOK AT ROW 25.25 COL 2.88 NO-LABEL
     "Hämta person:" VIEW-AS TEXT
          SIZE 12.75 BY .83 AT ROW 26.38 COL 59.75
     RECT-49 AT ROW 26.21 COL 58.13
     RECT-SOK AT ROW 25.04 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.25 BY 26.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
      TABLE: valperstemp T "?" NO-UNDO temp-db valperstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 26.79
         WIDTH              = 124.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.38
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
/* BROWSE-TAB BRW_PERS BTN_HAMT DEFAULT-FRAME */
/* BROWSE-TAB BRW_VPERS BRW_PERS DEFAULT-FRAME */
/* BROWSE-TAB BRW_FRAO BRW_VPERS DEFAULT-FRAME */
ASSIGN 
       BRW_FRAO:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VPERS:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRAO
/* Query rebuild information for BROWSE BRW_FRAO
     _TblList          = "Temp-Tables.valdaao"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.valdaao.AONR
"valdaao.AONR" "aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.valdaao.ORT
"valdaao.ORT" "Frånvaro" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRAO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.personaltemp.AKTIV
"personaltemp.AKTIV" "Tid!skrivning" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VPERS
/* Query rebuild information for BROWSE BRW_VPERS
     _TblList          = "Temp-Tables.valperstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valperstemp.PERSONALKOD
"valperstemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.valperstemp.FORNAMN
"valperstemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.valperstemp.EFTERNAMN
"valperstemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VPERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL C-Win
ON CHOOSE OF BTN_AVSL IN FRAME DEFAULT-FRAME /* Avsluta */
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


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT C-Win
ON CHOOSE OF BTN_HAMT IN FRAME DEFAULT-FRAME /* Hämta och visa urval */
DO:   
   RUN utvald_UI. 
   RUN hamta_UI (INPUT 3,INPUT "").     
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


&Scoped-define SELF-NAME CMB_AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AKTIV C-Win
ON VALUE-CHANGED OF CMB_AKTIV IN FRAME DEFAULT-FRAME /* Pers. med tidskrivning */
DO:
   {muswait.i}
   CMB_AKTIV = INPUT CMB_AKTIV.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ANST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ANST C-Win
ON VALUE-CHANGED OF CMB_ANST IN FRAME DEFAULT-FRAME /* Anställningsform */
DO:
   {muswait.i}
   CMB_ANST = INPUT CMB_ANST.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:    
   ASSIGN                        
   CMB_ARTAL = INPUT CMB_ARTAL
   FILL-IN-AR = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   FILL-IN-AR = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME DEFAULT-FRAME /* Avdelning */
DO:   
   CMB_AVD = INPUT CMB_AVD.         
   {CMB_AVDN1.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BEFATTNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BEFATTNING C-Win
ON VALUE-CHANGED OF CMB_BEFATTNING IN FRAME DEFAULT-FRAME /* Befattning */
DO:
   {muswait.i}
   CMB_BEFATTNING = INPUT CMB_BEFATTNING.            
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BER C-Win
ON VALUE-CHANGED OF CMB_BER IN FRAME DEFAULT-FRAME /* Beredskapsalternativ */
DO:
   {muswait.i}
   CMB_BER = INPUT CMB_BER.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DELTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DELTID C-Win
ON VALUE-CHANGED OF CMB_DELTID IN FRAME DEFAULT-FRAME /* Deltid */
DO:
   {muswait.i}
   CMB_DELTID = INPUT CMB_DELTID.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FLEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FLEX C-Win
ON VALUE-CHANGED OF CMB_FLEX IN FRAME DEFAULT-FRAME /* Flex */
DO:
   {muswait.i}
   CMB_FLEX = INPUT CMB_FLEX.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP C-Win
ON VALUE-CHANGED OF CMB_JURP IN FRAME DEFAULT-FRAME /* Juridisp */
DO:    
   CMB_JURP = INPUT CMB_JURP.        
   {CMB_JURP.I}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR C-Win
ON VALUE-CHANGED OF CMB_OMR IN FRAME DEFAULT-FRAME /* Visa personal för */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_SEMFOR C-Win
ON VALUE-CHANGED OF CMB_SEMFOR IN FRAME DEFAULT-FRAME /* Förskottssem */
DO:
   {muswait.i}
   CMB_SEMFOR = INPUT CMB_SEMFOR.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TIDSGODK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TIDSGODK C-Win
ON VALUE-CHANGED OF CMB_TIDSGODK IN FRAME DEFAULT-FRAME /* Godkänner tidsedlar */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TRA C-Win
ON VALUE-CHANGED OF CMB_TRA IN FRAME DEFAULT-FRAME /* Traktamentsavtal */
DO:
   CMB_TRA = INPUT CMB_TRA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_UPP C-Win
ON VALUE-CHANGED OF CMB_UPP IN FRAME DEFAULT-FRAME /* Val av lista */
DO:
   CMB_UPP = INPUT CMB_UPP.   
   FIND FIRST visaupp WHERE visaupp.UT = CMB_UPP NO-ERROR.
   IF visaupp.UPPFOLJVAL = 40 THEN FBTN_VISA:LABEL = "Skapa CSV-fil".
   ELSE FBTN_VISA:LABEL = "Visa".        
   IF visaupp.VISEX = 1 THEN FBTN_EX:HIDDEN = FALSE.
   ELSE  FBTN_EX:HIDDEN = TRUE.
   IF visaupp.VISAP = 1 THEN FBTN_VISA:HIDDEN = FALSE.
   ELSE FBTN_VISA:HIDDEN = TRUE.
   IF visaupp.UPPFOLJVAL = 46 OR visaupp.UPPFOLJVAL = 53 THEN DO:
      RAD_PERIOD = 2.
      TOG_GOD = FALSE. 
      RAD_PERIOD:HIDDEN = TRUE.
      TOG_GOD:HIDDEN = TRUE. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VECKO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VECKO C-Win
ON ENTRY OF CMB_VECKO IN FRAME DEFAULT-FRAME /* Veckoschema */
DO:
   CMB_VECKO = INPUT CMB_VECKO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VECKO C-Win
ON LEAVE OF CMB_VECKO IN FRAME DEFAULT-FRAME /* Veckoschema */
DO:
   CMB_VECKO = INPUT CMB_VECKO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX C-Win
ON CHOOSE OF FBTN_EX IN FRAME DEFAULT-FRAME /* Visa I EXCEL */
DO: 
   visvalvar = 2.
   {AMERICANEUROPEAN.I}
   RUN btnok_UI.
   {EUROPEANAMERICAN.I}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO: 
   visvalvar = 1.
   {AMERICANEUROPEAN.I}
   RUN btnok_UI. 
   {EUROPEANAMERICAN.I}       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   FIND FIRST uppvaltemp NO-ERROR.
   {AVBGOM.I} 
   IF vallista = 22  THEN DO:
      excellista = 7.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POMEAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, 
         OUTPUT TABLE tidut). /*,OUTPUT str, OUTPUT str2, OUTPUT str3)*/
      END.
      ELSE DO:         
         RUN POMEAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, 
         OUTPUT TABLE tidut). /*,OUTPUT str, OUTPUT str2, OUTPUT str3)*/         
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.
   ELSE IF  vallista = 27  THEN DO:
      excellista = 11.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POARBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, 
         OUTPUT TABLE tidut). /*,OUTPUT str, OUTPUT str2, OUTPUT str3)*/
      END.
      ELSE DO:         
         RUN POARBAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   ELSE IF  vallista = 48  THEN DO:
      excellista = 39.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POSAMARBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN POSAMARBAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp,OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   
   ELSE IF  vallista = 45  THEN DO:
      excellista = 24.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POVSUAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN POVSUAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, 
         OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.
   
   ELSE IF  vallista = 47  THEN DO:
      excellista = 24.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN PUTBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp,OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN PUTBAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   ELSE IF  vallista = 30  THEN DO:
      excellista = 11.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POOVAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN POOVAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   ELSE IF  vallista = 32  THEN DO:
      excellista = 12.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POOVBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).
      END.
      ELSE DO:         
         RUN POOVBAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   ELSE IF  vallista = 33  THEN DO:
      excellista = 13.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POKOMPEAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN POKOMPEAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   ELSE IF vallista = 35  THEN DO:     
      excellista = 14.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN POMESUAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut,OUTPUT TABLE vpers). 
      END.
      ELSE DO:         
         RUN POMESUAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut,OUTPUT TABLE vpers).          
      END.      
      
      RUN prognosexcel_UI IN akivinexh (INPUT TABLE tidut,INPUT TABLE vpers).      
   END.
   ELSE IF  vallista = 36 THEN DO:
      excellista = 15.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN PODEBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN PODEBAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END. 
   ELSE IF  vallista = 39  THEN DO:
      excellista = 20.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN MIARBAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN MIARBAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.   
   ELSE IF vallista = 40  THEN DO:
      excellista = 7.               
      IF Guru.Konstanter:appcon THEN DO:
         RUN CSPOMEAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE skvsum). 
      END.
      ELSE DO:         
         RUN CSPOMEAPP.P 
         (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, 
         OUTPUT TABLE skvsum).          
      END.
      RUN PROJPESKV.P (INPUT TABLE skvsum,OUTPUT filnamnz).       
      MESSAGE filnamnz VIEW-AS ALERT-BOX.
   END.
   ELSE IF vallista = 44  THEN DO:       
      EMPTY TEMP-TABLE evaldaao NO-ERROR. 
      antal_valda = BRW_FRAO:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_FRAO:FETCH-SELECTED-ROW(antal_raknare).
         CREATE evaldaao.
         BUFFER-COPY valdaao TO evaldaao.
         antal_raknare = antal_raknare + 1.
      END.       
           
      excellista = 23.
      IF Guru.Konstanter:appcon THEN DO:
         RUN POFRANAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT TABLE evaldaao, INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut). 
      END.
      ELSE DO:         
         RUN POFRANAPP.P 
         (INPUT TABLE evaldaao, INPUT TABLE uppvaltemp, INPUT TABLE valperstemp, OUTPUT TABLE tidut).          
      END.                     
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
   END.
   ELSE IF vallista = 46  THEN DO:      
      RUN anvglob_UI IN dirutfapph (INPUT Guru.Konstanter:globanv).
      RUN tiddet_UI IN dirutfapph (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp,OUTPUT TABLE tiddettemp).      
      RUN TIDDETVIS2.W ( INPUT TABLE tiddettemp).                                   
   END.
   ELSE IF vallista = 53  THEN DO:      
      RUN anvglob_UI IN dirutfapph (INPUT Guru.Konstanter:globanv).
      RUN tiddsum_UI IN dirutfapph (INPUT TABLE uppvaltemp, INPUT TABLE valperstemp,OUTPUT TABLE tiddettemp).      
      RUN TIDSUMVIS.W ( INPUT TABLE tiddettemp).                                   
   END.
   ELSE DO:            
      status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
      RUN DIRUTFUC.W (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE valdaao).                
   END.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AR C-Win
ON LEAVE OF FILL-IN-AR IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AR = INPUT FILL-IN-AR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
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
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
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


&Scoped-define SELF-NAME FILL-IN_EFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EFTERNAMN C-Win
ON LEAVE OF FILL-IN_EFTERNAMN IN FRAME DEFAULT-FRAME /* Efternamn */
DO:
   FILL-IN_EFTERNAMN = INPUT FILL-IN_EFTERNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EPERSONALKOD C-Win
ON ANY-KEY OF FILL-IN_EPERSONALKOD IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
  {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EPERSONALKOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EPERSONALKOD IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   
   ASSIGN
   FILL-IN_EPERSONALKOD = INPUT FILL-IN_EPERSONALKOD.
   IF FILL-IN_EPERSONALKOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST valperstemp WHERE valperstemp.PERSONALKOD = FILL-IN_EPERSONALKOD     
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE valperstemp THEN DO:  
      RUN psek_UI (INPUT FILL-IN_EPERSONALKOD).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         medvar = FALSE.
         MESSAGE "Du har inte behörighet att göra ändringar på denna person" VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
      {muswait.i}
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN PHMTAEN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT FILL-IN_EPERSONALKOD ,OUTPUT TABLE valperstemp APPEND).
      END.
      ELSE DO:
         RUN PHMTAEN.P  
         (INPUT FILL-IN_EPERSONALKOD ,OUTPUT TABLE valperstemp APPEND).
      END.   
      RUN kollbolagen_UI IN persapph (INPUT Guru.Konstanter:globanv,INPUT FILL-IN_EPERSONALKOD,INPUT-OUTPUT TABLE valperstemp).  
      FIND FIRST valperstemp WHERE valperstemp.PERSONALKOD = FILL-IN_EPERSONALKOD     
      NO-LOCK NO-ERROR.
                    
   END.
   IF NOT AVAILABLE valperstemp THEN DO:
      MESSAGE FILL-IN_EPERSONALKOD + " finns inte!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valperstemp)).              
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].     
   RUN goma_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FORNAMN C-Win
ON LEAVE OF FILL-IN_FORNAMN IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
   FILL-IN_FORNAMN = INPUT FILL-IN_FORNAMN.
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
   RUN sok_UI (INPUT 3).
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
   RUN sok_UI (INPUT 2).
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
ON LEAVE OF FILL-IN_SPERSONALKOD IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DEFAULT-FRAME /* Enhet/Sign */
DO:
   RUN sok_UI (INPUT 1).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD C-Win
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME DEFAULT-FRAME
DO:
   RAD_PERIOD = INPUT RAD_PERIOD.
   RUN goma_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ARBF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ARBF C-Win
ON VALUE-CHANGED OF TOG_ARBF IN FRAME DEFAULT-FRAME /* Endast arbetstidsförkortning */
DO:
   TOG_ARBF = INPUT TOG_ARBF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FRAO
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
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch.   
   IF VALID-HANDLE(persapph) THEN DELETE PROCEDURE persapph.
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2 NO-ERROR.
      nyttaoapph2 = ?.
   END.  
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.      
   IF VALID-HANDLE(akivinexh) THEN DELETE PROCEDURE akivinexh NO-ERROR.         
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
   {JURPAVDSTART2.I}   
   DEBUGGER:SET-BREAK().
   EMPTY TEMP-TABLE valperstemp NO-ERROR. 
   FIND FIRST uppvaltemp NO-ERROR.   
   CMB_UPP:ADD-LAST(uppvaltemp.VALDLISTA).
   CMB_UPP:SCREEN-VALUE = uppvaltemp.VALDLISTA.
   CMB_AKTIV = "Aktiv".
   CMB_AKTIV:SCREEN-VALUE = "Aktiv".
   ASSIGN C-Win:TITLE = uppvaltemp.VALDLISTA.
   /*phmt.p*/   
   RUN godk_UI IN persapph (INPUT "",OUTPUT TABLE godkannartemp).   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN AVTAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE avtaltemp).
   END.
   ELSE DO:
      RUN AVTAPP.P
      (OUTPUT TABLE avtaltemp).
   END.   
   status-ok = CMB_OMR:ADD-FIRST("Alla").   
   ASSIGN
   CMB_OMR = uppvaltemp.OMRNAMN.
   musz = FALSE.      
   ASSIGN
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL = Guru.Konstanter:gavdk
   CMB_OMR:LABEL = Guru.Konstanter:gomrk.   
   status-ok = CMB_TIDSGODK:ADD-LAST("Alla").
   FOR EACH godkannartemp:
      status-ok = CMB_TIDSGODK:ADD-LAST(godkannartemp.NAMN).
   END.
   status-ok = CMB_BEFATTNING:ADD-LAST("Alla").
   status-ok = CMB_BER:ADD-LAST("Alla").
   status-ok = CMB_TRA:ADD-LAST("Alla").
   status-ok = CMB_ANST:ADD-LAST("Alla").
   status-ok = CMB_VECKO:ADD-LAST("Alla").
   status-ok = CMB_RULL:ADD-LAST("Alla").
   FOR EACH avtaltemp :
      IF avtaltemp.TYP = "BEF" THEN DO:
         status-ok = CMB_BEFATTNING:ADD-LAST(avtaltemp.AVTALNAMN).      
      END.
      IF avtaltemp.TYP = "BER" THEN DO:
         status-ok = CMB_BER:ADD-LAST(avtaltemp.AVTALNAMN).      
      END.
      IF avtaltemp.TYP = "TRA" THEN DO:
         status-ok = CMB_TRA:ADD-LAST(avtaltemp.AVTALNAMN).
      END.
      IF avtaltemp.TYP = "ANS" THEN DO:
         status-ok = CMB_ANST:ADD-LAST(avtaltemp.AVTALNAMN).         
      END.
      IF avtaltemp.TYP = "VEC" THEN DO:
         status-ok = CMB_VECKO:ADD-LAST(avtaltemp.AVTALNAMN).      
      END.
      IF avtaltemp.TYP = "RUL" THEN DO:
         status-ok = CMB_RULL:ADD-LAST(avtaltemp.AVTALNAMN).      
      END.
   END.   
   ASSIGN
   CMB_TIDSGODK:SCREEN-VALUE = "Alla"
   CMB_BEFATTNING:SCREEN-VALUE = "Alla"          
   CMB_ANST:SCREEN-VALUE = "Alla"
   CMB_TRA:SCREEN-VALUE = "Alla"
   CMB_BER:SCREEN-VALUE = "Alla"
   CMB_VECKO:SCREEN-VALUE = "Alla".
   CMB_RULL:SCREEN-VALUE = "Alla".
   DISPLAY CMB_OMR CMB_TIDSGODK CMB_BEFATTNING CMB_ANST CMB_AKTIV CMB_TRA
   CMB_BER CMB_VECKO CMB_RULL
   WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      CMB_SEMFOR:LABEL = "Spärr arbförk".     
   END.   
   IF vallista = 40 THEN FBTN_VISA:LABEL = "Skapa CSV-fil".
   ELSE FBTN_VISA:LABEL = "Visa".
   EMPTY TEMP-TABLE valdaao NO-ERROR.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:  
      IF vallista = 32 THEN TOG_ARBF:LABEL = "Visa all övertid".     
   END.
   RUN aonrfran IN aonrapph (OUTPUT TABLE valdaao).
   FIND FIRST valdaao NO-LOCK NO-ERROR.
   RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(valdaao)).              
   RUN openbdynspec_UI IN brwproc[5].
   RUN lastselectdyn_UI IN brwproc[5].     
   RUN enable_UI. 
   IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.   
   IF avdraknare < 2 THEN CMB_AVD:HIDDEN = TRUE.   
   {FRMSIZE.I}   
   RUN ladda_UI.    
   IF  vallista = 35 OR vallista = 36 THEN DO:
      TOG_ARBF:HIDDEN = TRUE.
   END.   
   BRW_FRAO:HIDDEN = TRUE.
   IF vallista = 44  THEN DO:
      BRW_FRAO:HIDDEN = FALSE.      
   END.   
   APPLY "VALUE-CHANGED" TO CMB_UPP.   
   {musarrow.i}   
   {ARTALBORT.I}
   {WIN_M_SLUT.I}
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.   
   IF vallista = 45 OR vallista = 46 OR vallista = 53 THEN DO:      
      RAD_PERIOD:HIDDEN = TRUE.
   END.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VPERS:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_PERS:HANDLE, INPUT BRW_VPERS:HANDLE,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).        
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_FRAO:HANDLE IN FRAME {&FRAME-NAME}).
   FIND FIRST valsoktemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
   FIND FIRST upvaltemp2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE upvaltemp2 THEN CREATE upvaltemp2.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN DIRUTFAPPC.P PERSISTENT SET dirutfapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.   
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
      RUN DIRUTFAPPC.P PERSISTENT SET dirutfapph.         
   END.  
   /*phmt.p*/
   IF Guru.Konstanter:appcon THEN DO:      
      RUN PHMTAPP.P PERSISTENT SET persapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PHMTAPP.P PERSISTENT SET persapph.
   END.  
   RUN AKIVINEX2.P PERSISTENT SET akivinexh.                                         
   RUN recidset_UI IN persapph (INPUT-OUTPUT TABLE valperstemp).
   
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
   FIND FIRST valperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE valperstemp THEN DO:
      MESSAGE "Du måste välja någon person ".
      RETURN NO-APPLY.
   END.        
   RUN utvald_UI.   

   APPLY "GO" TO FBTN_VISA IN FRAME {&FRAME-NAME}.  
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
  DISPLAY CMB_ARTAL CMB_UPP RAD_PERIOD FILL-IN-STARTDAT FILL-IN-STOPPDAT TOG_GOD 
          TOG_ARBF CMB_JURP CMB_AKTIV CMB_SEMFOR CMB_AVD CMB_VECKO CMB_DELTID 
          CMB_OMR CMB_BEFATTNING CMB_FLEX CMB_TIDSGODK CMB_TRA CMB_RULL 
          FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST FILL-IN_SFORNAMN 
          FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN FILL-IN_EPERSONALKOD 
          FILL-IN-AOTEXT FILL-IN-SOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-49 RECT-SOK CMB_ARTAL CMB_UPP BTN_NVE BTN_NVE-2 RAD_PERIOD 
         FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 TOG_GOD TOG_ARBF 
         CMB_JURP CMB_AKTIV CMB_SEMFOR FBTN_VISA CMB_AVD CMB_VECKO CMB_DELTID 
         FBTN_EX CMB_OMR CMB_BEFATTNING CMB_FLEX CMB_TIDSGODK CMB_TRA CMB_RULL 
         FILL-IN_FORNAMN CMB_BER FILL-IN_EFTERNAMN CMB_ANST BTN_HAMT BRW_PERS 
         BRW_VPERS BRW_FRAO BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK 
         FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD FILL-IN_SEFTERNAMN 
         FILL-IN_EPERSONALKOD BTN_AVSL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST personaltemp NO-LOCK NO-ERROR.
   IF AVAILABLE personaltemp THEN DO:
      ASSIGN                      
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME}  = FALSE 
      BTN_ALLOVER:HIDDEN = FALSE 
      BTN_OVER:HIDDEN = FALSE
      BTN_BACK:HIDDEN = FALSE
      BRW_PERS:HIDDEN = FALSE.
   END.    
   FIND FIRST uppvaltemp NO-ERROR.
   FIND FIRST valperstemp NO-ERROR.
   IF AVAILABLE valperstemp THEN DO:
      ASSIGN
      BTN_ALLBACK:HIDDEN = FALSE 
      BTN_BACK:HIDDEN = FALSE.
   END.   
   
   FIND FIRST uppvaltemp NO-ERROR.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      CMB_ARTAL:HIDDEN = FALSE
      BTN_FVE:HIDDEN = TRUE 
      BTN_FVE-2:HIDDEN = TRUE 
      BTN_NVE:HIDDEN = TRUE 
      BTN_NVE-2:HIDDEN = TRUE         
      FILL-IN-STARTDAT:HIDDEN = TRUE 
      FILL-IN-STOPPDAT:HIDDEN = TRUE.
   END.
   ELSE IF RAD_PERIOD = 2 THEN DO:
      ASSIGN
      CMB_ARTAL:HIDDEN = FALSE
      BTN_FVE:HIDDEN = FALSE 
      BTN_FVE-2:HIDDEN = FALSE 
      BTN_NVE:HIDDEN = FALSE 
      BTN_NVE-2:HIDDEN = FALSE
      FILL-IN-STARTDAT:HIDDEN = FALSE 
      FILL-IN-STOPPDAT:HIDDEN = FALSE.
   END.           
   ELSE IF RAD_PERIOD = 3 THEN DO:
      ASSIGN
      CMB_ARTAL:HIDDEN = TRUE
      BTN_FVE:HIDDEN = TRUE  
      BTN_FVE-2:HIDDEN = TRUE 
      BTN_NVE:HIDDEN = TRUE 
      BTN_NVE-2:HIDDEN = TRUE
      FILL-IN-STARTDAT:HIDDEN = TRUE 
      FILL-IN-STOPPDAT:HIDDEN = TRUE.
   END.
   DISPLAY RAD_PERIOD WITH FRAME {&FRAME-NAME}.

            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI C-Win 
PROCEDURE hamta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO. 
   DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO. 
   {muswait.i}
   /**/
   EMPTY TEMP-TABLE personaltemp NO-ERROR.
   EMPTY TEMP-TABLE valsoktemp NO-ERROR.    
   CREATE valsoktemp.
   ASSIGN
   valsoktemp.SOKVAL = 1
   valsoktemp.SOKINT[1]  =  Guru.Konstanter:globniv 
   valsoktemp.SOKCHAR[1] =  Guru.Konstanter:globanv.
   /*phmt.p*/         
   /*TIDSEK ,PERSSEK*/
   RUN phamta_UI IN persapph (INPUT TRUE,INPUT FALSE,INPUT TABLE upvaltemp2,OUTPUT TABLE personaltemp,INPUT TABLE valsoktemp).     
   /*IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "cgkal" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
      
      /*Luleå vill att man bara får se de personer man har tidbehörighet för i uppföljning*/
      /*mittsverige tillagt 20170217  även godkännare skall kunna se sina personer*/
      /*även kalmar 20131203*/
      /*flyttat till phamta_ui första input tidsek = true andra persek = true*/
      RUN ptidsek_UI IN persapph (INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE personaltemp).
   END.*/
   RUN kollbolag_UI IN persapph (INPUT Guru.Konstanter:globanv,INPUT-OUTPUT TABLE personaltemp).
   IF vem = "" THEN DO:
     RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
     RUN fetchselrowid_UI IN brwproc[{&LEFT-BROWSE}].
     RUN title_UI IN brwproc[{&LEFT-BROWSE}].
   END.
   ELSE DO:
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      RUN fetchselrowid_UI IN brwproc[{&RIGHT-BROWSE}].      
      RUN title_UI IN brwproc[{&RIGHT-BROWSE}].
   END.
   RUN goma_UI.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI C-Win 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CMB_ARTAL = YEAR(uppvaltemp.STARTDATUM).
   ASSIGN   
   FILL-IN-STARTDAT = uppvaltemp.STARTDATUM
   FILL-IN-STOPPDAT = uppvaltemp.SLUTDATUM
   TOG_GOD = uppvaltemp.VISGODKANDA.
   IF uppvaltemp.VISPERAR = TRUE THEN DO:
      RAD_PERIOD = 1.
   END.
   ELSE IF uppvaltemp.VISPERAR = FALSE THEN DO:
      RAD_PERIOD = 2.
   END.
   ELSE DO:
      RAD_PERIOD = 3.
   END.   
   DISPLAY  FILL-IN-STARTDAT FILL-IN-STOPPDAT  
   RAD_PERIOD TOG_GOD WITH FRAME {&FRAME-NAME}.
   RUN goma_UI.    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE psek_UI C-Win 
PROCEDURE psek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pskapa_UI C-Win 
PROCEDURE pskapa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   IF vadgora = 1 THEN DO:
      FIND FIRST valperstemp WHERE valperstemp.PERSONALKOD = personaltemp.PERSONALKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valperstemp THEN DO:
         CREATE valperstemp.
         BUFFER-COPY personaltemp TO valperstemp.         
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sok_UI C-Win 
PROCEDURE sok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER sokvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   IF sokvar = 1 THEN DO:
      FILL-IN_SPERSONALKOD = INPUT FRAME {&FRAME-NAME} FILL-IN_SPERSONALKOD.
      RUN sokurvaldyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "PERSONALKOD", INPUT FILL-IN_EPERSONALKOD).      
   END.
   IF sokvar = 2 THEN DO:
      FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
      RUN sokurvaldyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "FORNAMN", INPUT FILL-IN_SFORNAMN).
      
   END.
   IF sokvar = 3 THEN DO:      
      FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
      RUN sokurvaldyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "EFTERNAMN", INPUT FILL-IN_SEFTERNAMN).
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utvald_UI C-Win 
PROCEDURE utvald_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR 
   CMB_TIDSGODK = INPUT FRAME {&FRAME-NAME}  CMB_TIDSGODK  
   CMB_AKTIV = INPUT FRAME {&FRAME-NAME} CMB_AKTIV  
   CMB_VECKO = INPUT FRAME {&FRAME-NAME} CMB_VECKO
   CMB_BER = INPUT FRAME {&FRAME-NAME} CMB_BER
   FILL-IN_EFTERNAMN = INPUT FILL-IN_EFTERNAMN
   FILL-IN_FORNAMN = INPUT FILL-IN_FORNAMN
   CMB_BEFATTNING = INPUT FRAME {&FRAME-NAME} CMB_BEFATTNING
   CMB_ANST = INPUT FRAME {&FRAME-NAME} CMB_ANST
   CMB_TRA = INPUT FRAME {&FRAME-NAME} CMB_TRA
   CMB_SEMFOR = INPUT FRAME {&FRAME-NAME} CMB_SEMFOR
   CMB_DELTID = INPUT FRAME {&FRAME-NAME} CMB_DELTID
   CMB_FLEX = INPUT FRAME {&FRAME-NAME} CMB_FLEX.
   CMB_RULL = INPUT FRAME {&FRAME-NAME} CMB_RULL.
   IF jid = "" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   IF Guru.Konstanter:globforetag = "cLULE" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   FIND FIRST upvaltemp2 NO-ERROR.
   FIND FIRST godkannartemp WHERE godkannartemp.NAMN = CMB_TIDSGODK NO-LOCK NO-ERROR.   
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN upvaltemp2.OMRADE = omrtemp.OMRADE.
   ELSE upvaltemp2.OMRADE = "ALLA".
   FIND FIRST avtaltemp WHERE avtaltemp.AVTALNAMN = CMB_VECKO AND            
   avtaltemp.TYP = "VEC" NO-LOCK NO-ERROR.
   IF AVAILABLE avtaltemp THEN upvaltemp2.VECKOSCHEMA = avtaltemp.AVTALTYP.
   ELSE upvaltemp2.VECKOSCHEMA = "ALLA".
   FIND FIRST avtaltemp WHERE avtaltemp.AVTALNAMN = CMB_BER AND            
   avtaltemp.TYP = "BER" NO-LOCK NO-ERROR.
   IF AVAILABLE avtaltemp THEN upvaltemp2.BEREDSKAPSAVTAL = avtaltemp.AVTALTYP.
   ELSE upvaltemp2.BEREDSKAPSAVTAL = "ALLA".
   FIND FIRST avtaltemp WHERE avtaltemp.AVTALNAMN = CMB_BEFATTNING AND            
   avtaltemp.TYP = "BEF" NO-LOCK NO-ERROR.
   IF AVAILABLE avtaltemp THEN upvaltemp2.BEFATTNING = avtaltemp.AVTALTYP.
   ELSE upvaltemp2.BEFATTNING = "ALLA".
   FIND FIRST avtaltemp WHERE avtaltemp.AVTALNAMN = CMB_ANST AND            
   avtaltemp.TYP = "ANS" NO-LOCK NO-ERROR.
   IF AVAILABLE avtaltemp THEN upvaltemp2.ANSTALLNING = avtaltemp.AVTALTYP.
   ELSE upvaltemp2.ANSTALLNING = "ALLA".
   FIND FIRST avtaltemp WHERE avtaltemp.AVTALNAMN = CMB_TRA AND            
   avtaltemp.TYP = "TRA" NO-LOCK NO-ERROR.
   IF AVAILABLE avtaltemp THEN upvaltemp2.TRAAVTAL = avtaltemp.AVTALTYP.
   ELSE upvaltemp2.TRAAVTAL = "ALLA".
   IF CMB_TIDSGODK NE "Alla" THEN upvaltemp2.TIDSGODK = godkannartemp.PERSONALKOD.
   ELSE upvaltemp2.TIDSGODK = "Alla".
   upvaltemp2.JUDID = "ALLA".

   FIND FIRST avtaltemp WHERE avtaltemp.AVTALNAMN = CMB_RULL:SCREEN-VALUE AND            
   avtaltemp.TYP = "RUL" NO-LOCK NO-ERROR.
   IF AVAILABLE avtaltemp THEN upvaltemp2.RULLID = avtaltemp.AVTALTYP.   
   ELSE upvaltemp2.RULLID = "ALLA".
   FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.        
   IF AVAILABLE jurperstemp THEN upvaltemp2.JUDID = jurperstemp.JUDID.   
   FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
   IF AVAILABLE avdtemp THEN upvaltemp2.AVDNR = STRING(avdtemp.AVDELNINGNR).
   ELSE upvaltemp2.AVDNR = "ALLA".
   FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
   IF AVAILABLE avdtemp THEN uppvaltemp.AVDNR = STRING(avdtemp.AVDELNINGNR).
   ELSE uppvaltemp.AVDNR = "ALLA".
   ASSIGN 
   upvaltemp2.AKTIV = CMB_AKTIV
   upvaltemp2.EFTERNAMN = FILL-IN_EFTERNAMN
   upvaltemp2.FORNAMN = FILL-IN_FORNAMN
   upvaltemp2.SEMFOR = CMB_SEMFOR
   upvaltemp2.DTID = CMB_DELTID
   upvaltemp2.FLX = CMB_FLEX.    
   TOG_GOD = INPUT FRAME {&FRAME-NAME} TOG_GOD.
   TOG_ARBF = INPUT FRAME {&FRAME-NAME} TOG_ARBF.
   ASSIGN     
   uppvaltemp.VISPERAR = FALSE
   uppvaltemp.STARTDATUM = FILL-IN-STARTDAT
   uppvaltemp.SLUTDATUM  = FILL-IN-STOPPDAT
   uppvaltemp.VISGODKANDA = TOG_GOD.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
   IF AVAILABLE omrtemp THEN uppvaltemp.OMRADE = omrtemp.OMRADE.
   ELSE uppvaltemp.OMRADE = "Alla".
   uppvaltemp.OMRNAMN = CMB_OMR.
   uppvaltemp.ENDBEST = FALSE.
   IF TOG_ARBF = TRUE THEN DO:
      uppvaltemp.ENDBEST = TRUE.
   END.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
   IF AVAILABLE omrtemp THEN uppvaltemp.OMRADE = omrtemp.OMRADE.
   ELSE uppvaltemp.OMRADE = "Alla".
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN         
      uppvaltemp.VISPERAR = TRUE
      uppvaltemp.STARTDATUM = DATE(01,01,CMB_ARTAL).
   END.
   ELSE IF RAD_PERIOD = 3 THEN DO:
      ASSIGN         
      uppvaltemp.VISPERAR = ?.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

