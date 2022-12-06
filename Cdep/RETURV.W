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
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{DEPATEMP.I}
{SPECMTRLTEMP.I}
{SOKDEF.I}
{HOPPSEK2W.I}
{LAGERTRUM.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}

/*DEFINE TEMP-TABLE lptemp NO-UNDO
   FIELD ENR AS CHARACTER 
   FIELD TRUMMANR AS INTEGER
   FIELD LEVKOD AS CHARACTER
   FIELD RMETER AS INTEGER 
   FIELD KMETER AS INTEGER
   FIELD METER AS INTEGER  
   INDEX ENR ENR. 

DEFINE TEMP-TABLE lptemp2 NO-UNDO
   FIELD ENR AS CHARACTER 
   FIELD TRUMMANR AS INTEGER   
   INDEX ENR ENR.*/ 


DEFINE NEW SHARED VARIABLE dep AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE invdat AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valnamn AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE akval AS INTEGER NO-UNDO.  
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE valknrvis AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE nytt_bestnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE summa AS INTEGER NO-UNDO.
DEFINE VARIABLE dbenamning AS CHARACTER NO-UNDO.  
DEFINE VARIABLE bnummer AS INTEGER NO-UNDO. 
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
DEFINE VARIABLE val_rec AS ROWID NO-UNDO.
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE enrval AS LOGICAL NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE sistaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.*/
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE spenr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE kontroll AS LOGICAL NO-UNDO.
DEFINE VARIABLE lplats AS INTEGER NO-UNDO.
DEFINE VARIABLE kvarmeter AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE spec_mtrl2 NO-UNDO LIKE spec_mtrl.

DEFINE NEW SHARED TEMP-TABLE aspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD NY AS LOGICAL
   FIELD ANDRAD AS LOGICAL
   FIELD BORT AS LOGICAL.   


{DEFSOK.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_DEPA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrldeptemp mtrltemp spec_mtrl

/* Definitions for BROWSE BRW_DEPA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DEPA mtrldeptemp.FACKID mtrldeptemp.ENR ~
mtrldeptemp.BENAMNING mtrldeptemp.ENHET mtrldeptemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH mtrldeptemp ~
      WHERE mtrldeptemp.DEPNR = vald_depa NO-LOCK ~
    BY mtrldeptemp.ENR
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH mtrldeptemp ~
      WHERE mtrldeptemp.DEPNR = vald_depa NO-LOCK ~
    BY mtrldeptemp.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA mtrldeptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA mtrldeptemp


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.Enr spec_mtrl.SALDO ~
spec_mtrl.NPRIS spec_mtrl.BENAMNING spec_mtrl.Enhet 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.SALDO ~
spec_mtrl.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK ~
    BY spec_mtrl.Enr
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK ~
    BY spec_mtrl.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-SOKALT RAD_DEPA FBTN_VISA FBTN_SKRIV ~
btn_over FBTN_BEST btn_back BTN_AVB BTN_LEV CMB_LEV BTN_SPEC BTN_SATS ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 ~
FILL-IN-ANTAL BTN_UP FILL-IN-FACK BTN_MIN RAD_SOK BTN_TRUMMA RECT-2 ~
BRW_DEPA RECT-4 BRW_HLEV IMAGE-5 BRW_MTRL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-SOKALT RAD_DEPA CMB_LEV ~
FILL-IN-ENR FILL-IN-BEN FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 ~
FILL-IN-ANTAL FILL-IN-FACK RAD_SOK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_LEV 
     LABEL "Huvudleverantör" 
     SIZE 16 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_SATS 
     LABEL "Satsinformation":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SPEC 
     LABEL "Spec.mtrl" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_TRUMMA DEFAULT 
     LABEL "Trumma" 
     SIZE 14 BY 1 TOOLTIP "Lägg returen för markerat enr på trumma"
     BGCOLOR 8 .

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>>9":U INITIAL 1 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 11.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACK AS CHARACTER FORMAT "X(8)" 
     LABEL "Sök på Fack" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(6)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-5
     FILENAME "BILDER\sokpa.gif":U
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_DEPA AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Depå-mtrl", 1,
"Total-mtrl", 2
     SIZE 31.75 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 31.13 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 4.83
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55.75 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEPA FOR 
      mtrldeptemp SCROLLING.

DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEPA WINDOW-1 _STRUCTURED
  QUERY BRW_DEPA NO-LOCK DISPLAY
      mtrldeptemp.FACKID FORMAT "X(8)":U
      mtrldeptemp.ENR FORMAT "X(11)":U
      mtrldeptemp.BENAMNING FORMAT "x(40)":U
      mtrldeptemp.ENHET FORMAT "x(5)":U
      mtrldeptemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 47 BY 19
         TITLE "Materiellista för depå. ".

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 47 BY 19
         TITLE "Materiellista huvudleverantör ".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.Enr FORMAT "X(11)":U
      spec_mtrl.SALDO COLUMN-LABEL "Antal" FORMAT ">>>,>>9":U
      spec_mtrl.NPRIS FORMAT ">>>>99.99":U
      spec_mtrl.BENAMNING FORMAT "x(40)":U WIDTH 35
      spec_mtrl.Enhet FORMAT "x(5)":U
  ENABLE
      spec_mtrl.SALDO
      spec_mtrl.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 55.75 BY 19
         TITLE "Returnerat materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-SOKALT AT ROW 28 COL 2.5 NO-LABEL
     RAD_DEPA AT ROW 1.25 COL 1.5 NO-LABEL
     FBTN_VISA AT ROW 8 COL 111.5
     FBTN_SKRIV AT ROW 9.25 COL 111.5
     btn_over AT ROW 7.54 COL 49.13
     FBTN_BEST AT ROW 26.88 COL 111.5
     btn_back AT ROW 11.04 COL 49.13
     BTN_AVB AT ROW 27.96 COL 111.5
     BTN_LEV AT ROW 22.38 COL 15.38
     CMB_LEV AT ROW 22.38 COL 13.25 COLON-ALIGNED
     BTN_SPEC AT ROW 24.13 COL 75.5
     BTN_SATS AT ROW 22.38 COL 34
     FILL-IN-ENR AT ROW 24.46 COL 21 COLON-ALIGNED
     FILL-IN-BEN AT ROW 25.63 COL 21 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 26.92 COL 21 COLON-ALIGNED
     FILL-IN-FACKID2 AT ROW 26.92 COL 37.25 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 25.63 COL 68.63 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 27.54 COL 69.5 COLON-ALIGNED
     BTN_UP AT ROW 27.17 COL 84.5
     FILL-IN-FACK AT ROW 26.92 COL 21 COLON-ALIGNED
     BTN_MIN AT ROW 28.17 COL 84.5
     RAD_SOK AT ROW 27.96 COL 16.88 NO-LABEL
     BTN_TRUMMA AT ROW 10.5 COL 111.5 WIDGET-ID 2
     BRW_DEPA AT ROW 2.67 COL 1.5
     BRW_HLEV AT ROW 2.67 COL 1.5
     BRW_MTRL AT ROW 2.67 COL 55.25
     "Hämta:" VIEW-AS TEXT
          SIZE 7 BY .83 AT ROW 25.63 COL 56.5
     RECT-2 AT ROW 24.13 COL 1.5
     RECT-4 AT ROW 25.33 COL 55.25
     IMAGE-5 AT ROW 24.5 COL 2.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrldeptemp T "?" NO-UNDO temp-db mtrldeptemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: ? T "?" NO-UNDO temp-db sok_mtrl
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = no
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BRW_DEPA RECT-2 FRAME-A */
/* BROWSE-TAB BRW_HLEV RECT-4 FRAME-A */
/* BROWSE-TAB BRW_MTRL IMAGE-5 FRAME-A */
ASSIGN 
       BRW_DEPA:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_DEPA:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_DEPA:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BTN_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_MIN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_UP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ANTAL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACK:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID1:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

ASSIGN 
       RECT-4:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEPA
/* Query rebuild information for BROWSE BRW_DEPA
     _TblList          = "Temp-Tables.mtrldeptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrldeptemp.ENR|yes"
     _Where[1]         = "Temp-Tables.mtrldeptemp.DEPNR = vald_depa"
     _FldNameList[1]   = Temp-Tables.mtrldeptemp.FACKID
     _FldNameList[2]   = Temp-Tables.mtrldeptemp.ENR
     _FldNameList[3]   = Temp-Tables.mtrldeptemp.BENAMNING
     _FldNameList[4]   = Temp-Tables.mtrldeptemp.ENHET
     _FldNameList[5]   = Temp-Tables.mtrldeptemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_kundlev AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   = Temp-Tables.mtrltemp.Benamning
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.spec_mtrl.Enr|yes"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.spec_mtrl.SALDO
"spec_mtrl.SALDO" "Antal" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.NPRIS
"spec_mtrl.NPRIS" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.spec_mtrl.BENAMNING
"spec_mtrl.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.spec_mtrl.Enhet
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DEPA
&Scoped-define SELF-NAME BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON ANY-KEY OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå.  */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå.  */
DO:                                                                   
   /* RUN infoES_UI (INPUT 1).   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå.  */
DO:
  APPLY "CHOOSE" TO BTN_OVER. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON ANY-KEY OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:                                                                   
  /* RUN infoES_UI (INPUT 3).   
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:
   APPLY "CHOOSE" TO BTN_OVER. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON ANY-KEY OF BRW_MTRL IN FRAME FRAME-A /* Returnerat materiel */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("DELETE-CHARACTER") THEN DO:
      APPLY "CHOOSE" TO BTN_BACK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_MTRL IN FRAME FRAME-A /* Returnerat materiel */
DO:                                                                   
   RUN infoES_UI (INPUT 2).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME FRAME-A /* Returnerat materiel */
DO:            
  
   /*{muswait.i}
   {AVBGOM.I}
   RUN RETANT.W (INPUT ROWID(spec_mtrl)).  
   status-ok = BRW_MTRL:REFRESH().
   {AVBFRAM.I}      

   
   {musarrow.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO: 
   IF depatitta = FALSE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:      
        
         APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:
         
         FOR EACH lptemp2 :
            FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = lptemp2.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
            IF AVAILABLE lagertrum THEN DELETE lagertrum.
                     
         END.
         
         RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).
         EMPTY TEMP-TABLE lptemp2 NO-ERROR. 
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                    
   END.
   ELSE DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.
   
      

   /* ändrat för att man alltid svarar fel /Lena 050202


   MESSAGE "Är det korrekt att du vill avbryta rutinen för returer?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Avbryt?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:  
      APPLY "CLOSE":U TO THIS-PROCEDURE.       
   END.
   ELSE DO:       
      musz = musz.
   END.*/              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO: 
   RUN btnback_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LEV WINDOW-1
ON CHOOSE OF BTN_LEV IN FRAME FRAME-A /* Huvudleverantör */
DO:
   {muswait.i}
   vald_lev = vald_kundlev.         
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + huvudlev).         
   /*BRW_HLEV:TITLE = "Materiellista huvudleverantör: " + huvudlev.   */
   ASSIGN 
   FILL-IN-BEN:HIDDEN = FALSE 
   FILL-IN-ENR:HIDDEN = FALSE    
   BTN_LEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = FALSE
   BRW_HLEV:HIDDEN = FALSE 
   CMB_LEV:SCREEN-VALUE = huvudlev
   vald_lev = vald_kundlev.   
   {musarrow.i}
   IF posok NE " " THEN DO:
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN WINDOW-1
ON CHOOSE OF BTN_MIN IN FRAME FRAME-A /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 2 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 1!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
   APPLY "ENTRY" TO FILL-IN-ANTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A
DO: 
   RUN btnover_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SATS WINDOW-1
ON CHOOSE OF BTN_SATS IN FRAME FRAME-A /* Satsinformation */
DO:
  {muswait.i}
   satsinn = FALSE.
   IF RAD_DEPA = 1 THEN DO:
      status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      
      RUN SATSDEPV.W (INPUT mtrldeptemp.ENR, INPUT mtrldeptemp.LEVKOD, INPUT-OUTPUT satsinn).      
   END.
   ELSE DO:      
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      
      RUN SATSDEPV.W (INPUT mtrltemp.ENR, INPUT mtrltemp.LEVKOD, INPUT-OUTPUT satsinn).
      
   END.
   IF satsinn = TRUE THEN DO:
      satsinn = FALSE.
      RUN openbdynspec_UI IN brwproc[5]. 
      RUN title_UI IN brwproc[5].
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPEC WINDOW-1
ON CHOOSE OF BTN_SPEC IN FRAME FRAME-A /* Spec.mtrl */
DO:           
   {muswait.i}
   {AVBGOM.I}
   RUN DEPSPECANV2.W (INPUT 3).
   
   {AVBFRAM.I}   

   IF musz = FALSE THEN DO:      
      RUN openbdynspec_UI IN brwproc[5].
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:      
         RUN setlastrowid_UI IN brwproc[5] (INPUT mtrl_rowid).
         RUN lastselectdyn_UI IN brwproc[5].   
      END.      
      RUN title_UI IN brwproc[5].
   END.
   ELSE musz = FALSE.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TRUMMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TRUMMA WINDOW-1
ON CHOOSE OF BTN_TRUMMA IN FRAME FRAME-A /* Trumma */
DO:
   {muswait.i}
   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(spec_mtrl)).
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).      
      
      
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      CREATE elagertrum.
      ASSIGN 
      elagertrum.BENAMNING = spec_mtrl.BENAMNING
      elagertrum.ENHET = spec_mtrl.ENHET
      elagertrum.ENR = spec_mtrl.ENR
      elagertrum.STMETER = spec_mtrl.SALDO
      elagertrum.LAGMETER = spec_mtrl.SALDO
      elagertrum.PRIS = spec_mtrl.NPRIS
      elagertrum.DATUM = TODAY
      elagertrum.ANVANDARE = Guru.Konstanter:globanv.
      kvarmeter = spec_mtrl.SALDO.
      /*vilka enr som läggs på trummor, check om samma enr fördelas på två trummor*/
      FIND FIRST lptemp WHERE lptemp.ENR = elagertrum.ENR NO-ERROR.
      IF AVAILABLE lptemp THEN DO:
         kvarmeter = lptemp.KMETER. 
      END.
      IF kvarmeter > 0 THEN DO:         
         RUN TRUMREG.W (INPUT-OUTPUT TABLE elagertrum,INPUT kvarmeter, OUTPUT lplats).
         IF lplats > 0 THEN DO:
            FIND FIRST elagertrum NO-ERROR.
            IF AVAILABLE elagertrum THEN DO:            
               FIND FIRST lptemp WHERE lptemp.ENR = elagertrum.ENR NO-ERROR.
               IF NOT AVAILABLE lptemp  THEN DO:
                  CREATE lptemp.
                  lptemp.TRUMMANR = lplats.
                  lptemp.METER = spec_mtrl.SALDO.
               END.
               ASSIGN
               lptemp.ENR = elagertrum.ENR
               lptemp.RMETER = lptemp.RMETER + elagertrum.STMETER
               lptemp.KMETER = lptemp.METER - lptemp.RMETER.
               /*vilka trummor som lagts upp för att kunna rensa vid avbryt*/
               FIND FIRST lptemp2 WHERE lptemp2.TRUMMANR = lplats NO-ERROR.
               IF NOT AVAILABLE lptemp2  THEN DO:
                  CREATE lptemp2.
                  ASSIGN 
                  lptemp2.ENR = lptemp.ENR
                  lptemp2.TRUMMANR = lplats.
               END.
               
            END.
         END.
      END.
      ELSE DO:
         MESSAGE "För " +  LC(Guru.Konstanter:genk) + " - " + spec_mtrl.ENR + "är redan hela länden registrerad på trumma" VIEW-AS ALERT-BOX.
      END.
      EMPTY TEMP-TABLE elagertrum NO-ERROR. 
      antal_raknare = antal_raknare + 1.   
   END.      
   musz = FALSE.
   status-ok = BRW_MTRL:DESELECT-ROWS().   
   RUN lastselectdyn_UI IN brwproc[5].   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP WINDOW-1
ON CHOOSE OF BTN_UP IN FRAME FRAME-A /* + */
DO: 
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
   APPLY "ENTRY" TO FILL-IN-ANTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:        
   RUN vccmblev_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BEST WINDOW-1
ON CHOOSE OF FBTN_BEST IN FRAME FRAME-A /* Ok */
DO:
   /*{muswait.i}  */
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      
      FOR EACH spec_mtrl NO-LOCK:
         FIND FIRST lagertrum  WHERE lagertrum.ENR = spec_mtrl.ENR AND lagertrum.TRUMMA = "S" /*AND lagertrum.LAGMETER > 0*/ NO-LOCK NO-ERROR.
         IF AVAILABLE lagertrum THEN DO:
            /*om inga trummor upplagda via retur skall frågan fås.*/
            FIND FIRST lptemp2 WHERE lptemp2.ENR = spec_mtrl.ENR  NO-ERROR.
            IF NOT AVAILABLE lptemp2 THEN DO:
               RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(spec_mtrl)).
               status-ok = BRW_MTRL:DESELECT-ROWS() NO-ERROR.               
               RUN lastselectdyn_UI IN brwproc[5].                         
               MESSAGE "Enr " + spec_mtrl.ENR + " finns lagd på trumma. Vill du lägga in dessa meter på en ny trumma ?" 
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Ny trumma?"
               UPDATE svar.         
               IF svar THEN DO:
                  APPLY "CHOOSE" TO BTN_TRUMMA IN FRAME FRAME-A.
                  RETURN.                
               END.               
               ELSE IF NOT svar THEN DO:
               END.
               ELSE DO:
                  RETURN   NO-APPLY.
               END.
            END.        
         END.                    
      END.
   END.
   
   
   RUN btnbest_UI.
   APPLY "GO" TO FRAME {&FRAME-NAME}.  
   APPLY "CLOSE":U TO THIS-PROCEDURE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO: 
   RUN btnskriv_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:            
   RUN btnvisa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ANTAL WINDOW-1
ON ANY-KEY OF FILL-IN-ANTAL IN FRAME FRAME-A /* Antal */
DO:
   ASSIGN  
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.   
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
      IF SUBSTRING(FILL-IN-ENR2,1,1) NE "E" THEN FILL-IN-ENR2 = "E" + FILL-IN-ENR2.                                       
   END. 
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN enrval_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:   
   {muswait.i}
   ASSIGN
   FILL-IN-BEN = INPUT FILL-IN-BEN. 
   IF INDEX(FILL-IN-BEN,"+") NE 0 THEN DO:
      FILL-IN-BEN = TRIM(FILL-IN-BEN,"+").
      DISP FILL-IN-BEN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_DEPA = 1 THEN DO:     
      IF AVAILABLE mtrldeptemp THEN mtrl_rowid = ROWID(mtrldeptemp).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-BEN + '*'. 
      status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.       
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = mtrl_rowid NO-LOCK NO-ERROR.      
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.BENAMNING MATCHES aosok AND 
      mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.IBDATUM = ?
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.BENAMNING MATCHES aosok AND 
         mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.IBDATUM = ?
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO: 
            MESSAGE "Det finns inget på sökbegreppet i depån. Välj total materiel och utför ny sökning."
            VIEW-AS ALERT-BOX TITLE "Meddelande". 
         END.
      END.            
      IF AVAILABLE mtrldeptemp THEN DO:                        
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
         RUN lastselectdyn_UI IN brwproc[1].        
         
      END.      
   END. 
   IF RAD_DEPA = 2 THEN DO:
      {BENHMT2.I}           
      RUN initsok_UI (INPUT 1,INPUT aosok).      
   END.       
   {musarrow.i}
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
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:    
   {muswait.i}
   ASSIGN
   FILL-IN-ENR = INPUT FILL-IN-ENR.   
   IF INDEX(FILL-IN-ENR,"+") NE 0 THEN DO:
      FILL-IN-ENR = TRIM(FILL-IN-ENR,"+").
      DISP FILL-IN-ENR WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_DEPA = 1 THEN DO:      
      IF AVAILABLE mtrldeptemp THEN mtrl_rowid = ROWID(mtrldeptemp).
      IF FILL-IN-ENR = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      posok = '*' + FILL-IN-ENR + '*'. 
      status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.      
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = mtrl_rowid NO-LOCK NO-ERROR.                  
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.ENR MATCHES posok AND mtrldeptemp.DEPNR = vald_depa
      AND mtrldeptemp.IBDATUM = ? USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR MATCHES posok AND 
         mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.IBDATUM = ? USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO: 
            MESSAGE "Det finns inget på sökbegreppet i depån. Välj total materiel och utför ny sökning."
            VIEW-AS ALERT-BOX TITLE "Meddelande". 
         END.
      END.    
      IF AVAILABLE mtrldeptemp THEN DO:                                 
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
         RUN lastselectdyn_UI IN brwproc[1].        
      END.      
   END.
   IF RAD_DEPA = 2 THEN DO:
      {ENRHMT2.I}
      RUN initsok_UI (INPUT 2,INPUT posok).      
   END.       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON ANY-KEY OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO:
   ASSIGN  
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
      IF SUBSTRING(FILL-IN-ENR2,1,1) NE "E" THEN FILL-IN-ENR2 = "E" + FILL-IN-ENR2.                                       
   END. 
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN enrval_UI.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON ANY-KEY OF FILL-IN-FACK IN FRAME FRAME-A /* Sök på Fack */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACK IN FRAME FRAME-A /* Sök på Fack */
DO:
   IF INDEX(FILL-IN-FACKID1,"+") NE 0 THEN DO:
      FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1,"+").
      DISP FILL-IN-FACKID1 WITH FRAME {&FRAME-NAME}.
   END.
   IF INDEX(FILL-IN-FACKID2,"+") NE 0 THEN DO:
      FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2,"+").
      FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2).
      DISP FILL-IN-FACKID2 WITH FRAME {&FRAME-NAME}.
   END.
   FILL-IN-FACKID1 = FILL-IN-FACKID1 + FILL(" ",2 - LENGTH(FILL-IN-FACKID1)).          
   FILL-IN-FACKID2 = FILL(" ",6 - LENGTH(FILL-IN-FACKID2)) + FILL-IN-FACKID2.          
   ASSIGN
   FILL-IN-FACK = FILL-IN-FACKID1 + FILL-IN-FACKID2.     
   /*ASSIGN
   FILL-IN-FACK = var1 + var2.     */
   IF AVAILABLE mtrldeptemp THEN mtrl_rowid = ROWID(mtrldeptemp).
   IF FILL-IN-FACK = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-FACK IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   aosok = '*' + FILL-IN-FACK + '*'.
   status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.            
   FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = mtrl_rowid NO-LOCK NO-ERROR.                  
   FIND NEXT mtrldeptemp WHERE mtrldeptemp.FACKID MATCHES aosok AND mtrldeptemp.DEPNR = vald_depa
   AND mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE mtrldeptemp THEN DO:      
      FIND FIRST mtrldeptemp WHERE mtrldeptemp.FACKID MATCHES aosok AND mtrldeptemp.DEPNR = vald_depa
      AND mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE mtrldeptemp THEN DO: 
         MESSAGE "Det finns inget på sökbegreppet i depån."
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
      END.
   END.    
   IF AVAILABLE mtrldeptemp THEN DO:                     
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
      RUN lastselectdyn_UI IN brwproc[1].        
   END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
      IF INDEX(FILL-IN-FACKID1,"+") NE 0 THEN DO:
         FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1,"+").
         DISP FILL-IN-FACKID1 WITH FRAME {&FRAME-NAME}.
      END.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   ASSIGN
   FILL-IN-FACKID1 = INPUT FILL-IN-FACKID1  
   FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1).
   /*ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:
   ASSIGN
   FILL-IN-FACKID1 = INPUT FILL-IN-FACKID1  
   FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1).
   APPLY "LEAVE" TO FILL-IN-FACKID2 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
   /*ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.                         
   APPLY "LEAVE" TO FILL-IN-FACKID2 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID2 IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO: 
   ASSIGN
   FILL-IN-FACKID2 = INPUT FILL-IN-FACKID2  
   FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2).
   /*ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   ASSIGN
   FILL-IN-FACKID2 = INPUT FILL-IN-FACKID2.
   FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2).
   APPLY "LEAVE" TO FILL-IN-FACKID1 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
   /*
   ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      
   APPLY "LEAVE" TO FILL-IN-FACKID1 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_DEPA WINDOW-1
ON VALUE-CHANGED OF RAD_DEPA IN FRAME FRAME-A
DO:
   {muswait.i}                                
   RAD_DEPA = INPUT RAD_DEPA.
   IF RAD_DEPA = 1 THEN DO:   
      ASSIGN
      BRW_DEPA:HIDDEN = FALSE
      BRW_HLEV:HIDDEN = TRUE      
      CMB_LEV:HIDDEN = TRUE
      BTN_LEV:HIDDEN = TRUE      
      FILL-IN-ENR:HIDDEN = FALSE
      FILL-IN-BEN:HIDDEN = FALSE
      FILL-IN-FACKID1:HIDDEN = FALSE  
      FILL-IN-FACKID2:HIDDEN = FALSE.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa) + "  AND mtrldeptemp.IBDATUM = ?").         
      RUN openbdynspec_UI IN brwproc[1]. 
      RUN title_UI IN brwproc[1].
   END.
   IF RAD_DEPA = 2 THEN DO:

      RUN changebrwh_UI IN brwproc[6] 
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}, INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
      ASSIGN
      BRW_DEPA:HIDDEN = TRUE
      BRW_HLEV:HIDDEN = FALSE            
      CMB_LEV:HIDDEN = FALSE
      BTN_LEV:HIDDEN = TRUE      
      FILL-IN-ENR:HIDDEN = FALSE
      FILL-IN-BEN:HIDDEN = FALSE 
      FILL-IN-FACKID1:HIDDEN = TRUE 
      FILL-IN-FACKID2:HIDDEN = TRUE       
      CMB_LEV:SCREEN-VALUE = huvudlev
      vald_lev = vald_kundlev.      
      EMPTY TEMP-TABLE sok_mtrl NO-ERROR.          
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "mtrltemp.KALKNR = 0 AND  mtrltemp.LEVKOD = '" + STRING(vald_kundlev) + "'").
      RUN openbdynspec_UI IN brwproc[2].               
   END.            
   {musarrow.i}                                
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DEPA
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
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.

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

   RUN mtrlbhmt_UI IN mtrlbapph (INPUT vald_depa,OUTPUT TABLE mtrldeptemp,OUTPUT vald_kundlev, OUTPUT dbenamning,OUTPUT bnummer).             
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa)).
   RUN openbdynspec_UI IN brwproc[1]. 
   RUN title_UI IN brwproc[1].
   ASSIGN vald_lev = vald_kundlev.
  
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN.
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + huvudlev).         
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      /*Niklas personlig spec_mtrl*/
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(Guru.Konstanter:globanv + " " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         IF Guru.Konstanter:globforetag = "LULE" THEN DO:
            IF levtemp.LEVKOD NE "41"  THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "bode" THEN DO:
            IF levtemp.LEVKOD NE "6" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "KRAF" THEN DO:
            IF levtemp.LEVKOD NE "6" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         /*kalmar depåändring abc nu*/
         ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
            IF levtemp.LEVKOD NE "20" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "snat" THEN DO:
            IF levtemp.LEVKOD = "20" THEN.
            ELSE IF levtemp.LEVKOD = "21" THEN.
            ELSE  status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE DO:
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.  
      END.              
   END.
   ASSIGN             
   CMB_LEV = INPUT CMB_LEV   
   vald_lev = vald_kundlev
   bestant = FALSE   
   musz = FALSE
   RAD_DEPA = 1.
   /*RAD_SORT  = 2.*/
   
   ASSIGN
   mtrldeptemp.ENR:LABEL IN BROWSE BRW_DEPA = Guru.Konstanter:genk
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk   
   spec_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk   
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.
            
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa USE-INDEX DEP-NR NO-LOCK NO-ERROR.          
   IF akval = 1 THEN   
   {&WINDOW-NAME}:TITLE = "Returer från " + CAPS(Guru.Konstanter:gaol) + ": " + STRING(valaonr) + " DELNR: " + 
   STRING(valdelnr,Guru.Konstanter:varforetypchar[1]) + CAPS(Guru.Konstanter:gaonamnk) + ": " + valort 
   + " till depå - " + depatemp.BENAMNING.                
   ELSE {&WINDOW-NAME}:TITLE = "Returer från kund med Id: " + STRING(valknrvis) + " Namn: " + 
   STRING(valnamn) + " till depå - " + depatemp.BENAMNING.                     
   /*BRW_HLEV:TITLE = "Materiellista huvudleverantör: " + huvudlev.*/
   FILL-IN-ANTAL = 1.
   RUN enable_UI.   
   {FRMSIZE.I}      
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = FALSE.
   END.
   ELSE BTN_TRUMMA:HIDDEN IN FRAME FRAME-A           = TRUE. 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa) + "  AND mtrldeptemp.IBDATUM = ?").   
   RUN openbdynspec_UI IN brwproc[1]. 
   RUN title_UI IN brwproc[1].
   ASSIGN
   BRW_HLEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = TRUE
   BTN_LEV:HIDDEN = TRUE
   FILL-IN-FACK:HIDDEN = TRUE. 
   IF depatitta = TRUE THEN DO:
      DISABLE FBTN_BEST WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_SPEC WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_OVER WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BACK WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_TRUMMA WITH FRAME {&FRAME-NAME}.
      
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN mtrldeptemp.NPRIS:VISIBLE IN BROWSE BRW_DEPA = FALSE.
      ASSIGN mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE.
      ASSIGN spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.      
   END.
   {musarrow.i}   
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
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
      (INPUT BRW_DEPA:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   /*RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).*/
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).          
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[5] (INPUT THIS-PROCEDURE).            
   RUN DYNARROW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_DEPA:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ?,
       INPUT ? , INPUT ?).
   RUN setcolindex_UI IN brwproc[5] (INPUT "ENR").
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.      
   /*RUN setcolindex_UI IN brwproc[3] (INPUT "sok_mtrl.ENR BY sok_mtrl.LEV ").*/
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 2).
   RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnback_UI WINDOW-1 
PROCEDURE btnback_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).   
      FIND FIRST lptemp2  WHERE lptemp2.ENR = spec_mtrl.ENR NO-LOCK NO-ERROR.    
      IF AVAILABLE lptemp2 THEN DO:
         MESSAGE "Du har lagt upp trummor för detta enr för denna retur. Om du pilar bort enr från returen så tas trummorna bort. Vill du det?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Borttag av upplagda trummor?" 
         UPDATE svar.         
         IF svar THEN DO:                           
            /*om man pilar bort ett enr som man just har lagt upp trummor för och sedan trycker Ok måste dessa trummor bort Lena 20220529*/
            FOR EACH lptemp2 WHERE lptemp2.ENR = spec_mtrl.ENR:
               FIND FIRST lagertrum WHERE lagertrum.TRUMMANR = lptemp2.TRUMMANR AND lagertrum.TRUMMA = "S" NO-LOCK NO-ERROR.
               IF AVAILABLE lagertrum THEN DELETE lagertrum.
               DELETE lptemp2.
            END.            
         END.
         ELSE DO:
            RETURN.
         END.   
      END.      
      DELETE spec_mtrl.
      RUN valrattright_UI IN brwproc[5] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(spec_mtrl)).
      antal_raknare = antal_raknare + 1.   
   END.  
   RUN SparafaltTrumma_UI IN  Guru.GlobalaVariabler:ClienttdSetapph (INPUT TABLE lagertrum,INPUT TABLE aotrum).            
   RUN refreshbrw_UI IN brwproc[5].
   RUN lastselectdyn_UI IN brwproc[5].
   RUN title_UI IN brwproc[5].
          
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnbest_UI WINDOW-1 
PROCEDURE btnbest_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rekvar AS INTEGER NO-UNDO.      
   EMPTY TEMP-TABLE spec_mtrl2 NO-ERROR.
      
       
   FIND FIRST spec_mtrl WHERE NO-LOCK NO-ERROR.  
   IF AVAILABLE spec_mtrl THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         RUN ARTKODD.W (OUTPUT rekvar).
      END.    
   END.  
   RUN okretur_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa , INPUT valkund ,INPUT rekvar,INPUT Guru.Konstanter:globanv,
                                OUTPUT invdat, INPUT-OUTPUT TABLE spec_mtrl, OUTPUT TABLE spec_mtrl2).                                        
   IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "GKAL" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BODE" THEN DO:      
      FIND FIRST spec_mtrl2 NO-ERROR.
      IF AVAILABLE spec_mtrl2 THEN DO:
         MESSAGE "Det finns materiel som ej är lagerförd. Vill du börja lagerföra materielet?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ej lagerfört materiel!" 
         UPDATE svar.         
         IF svar THEN DO:
            RUN RETURLAG2V.W( INPUT TABLE spec_mtrl2).
            
         END.   
         ELSE DO:
            RUN ejlager_UI IN mtrlbapph (INPUT vald_depa, INPUT invdat, INPUT TABLE spec_mtrl2).       
         END.
      END.
   END.      
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnover_UI WINDOW-1 
PROCEDURE btnover_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RAD_DEPA = INPUT FRAME {&FRAME-NAME} RAD_DEPA.
   IF RAD_DEPA = 1 THEN DO: 
      antal_valda = BRW_DEPA:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_DEPA:FETCH-SELECTED-ROW(antal_raknare).
         ASSIGN antalvar = 1
         enrval = FALSE.
         RUN over3_UI.
      END.       
      RUN openbdynspec_UI IN brwproc[5].            
      status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.      
      RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
      RUN lastselectdyn_UI IN brwproc[5].
      RUN title_UI IN brwproc[5].
   END.
   IF RAD_DEPA = 2 THEN DO:
      antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
         ASSIGN antalvar = 1
         enrval = FALSE.
         RUN over_UI.
      END.             
      RUN openbdynspec_UI IN brwproc[5].                  
      status-ok = BRW_HLEV:DESELECT-ROWS() NO-ERROR.            
      RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
      RUN lastselectdyn_UI IN brwproc[5].
      RUN title_UI IN brwproc[5].
   END.                                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnskriv_UI WINDOW-1 
PROCEDURE btnskriv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      {muswait.i}             
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISARETV.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvisa_UI WINDOW-1 
PROCEDURE btnvisa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   {muswait.i}
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   {AVBGOM.I}   
   RUN VISARETV.W.
   {AVBFRAM.I}
   {musarrow.i}         
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
  DISPLAY FILL-IN-SOKALT RAD_DEPA CMB_LEV FILL-IN-ENR FILL-IN-BEN 
          FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL 
          FILL-IN-FACK RAD_SOK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE FILL-IN-SOKALT RAD_DEPA FBTN_VISA FBTN_SKRIV btn_over FBTN_BEST 
         btn_back BTN_AVB BTN_LEV CMB_LEV BTN_SPEC BTN_SATS FILL-IN-ENR 
         FILL-IN-BEN FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL 
         BTN_UP FILL-IN-FACK BTN_MIN RAD_SOK BTN_TRUMMA RECT-2 BRW_DEPA RECT-4 
         BRW_HLEV IMAGE-5 BRW_MTRL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrval_UI WINDOW-1 
PROCEDURE enrval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR = FILL-IN-ENR2 AND mtrldeptemp.DEPNR = vald_depa AND
  mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR.
  IF AVAILABLE mtrldeptemp THEN DO:
     ASSIGN
     antalvar = FILL-IN-ANTAL
     enrval = TRUE.
     RUN over3_UI.
  END.
  ELSE DO:  
     FIND FIRST mtrltemp WHERE mtrltemp.ENR = FILL-IN-ENR2 AND mtrltemp.LEVKOD = vald_lev
     NO-LOCK NO-ERROR.
     IF AVAILABLE mtrltemp THEN DO:
        ASSIGN
        antalvar = FILL-IN-ANTAL
        enrval = TRUE.
        RUN over_UI.
     END.  
     ELSE DO:     
        EMPTY TEMP-TABLE emtrltemp NO-ERROR.     
        RUN hmtskap_UI IN mtrlbapph (INPUT FILL-IN-ENR2,INPUT vald_lev,OUTPUT TABLE emtrltemp).      
        FIND FIRST emtrltemp NO-LOCK NO-ERROR.
        IF AVAILABLE emtrltemp THEN DO:
           ASSIGN
           antalvar = FILL-IN-ANTAL
           enrval = TRUE.
           RUN over4_UI.        
         END.
         ELSE DO:      
            FIND FIRST levtemp WHERE levtemp.LEVKOD =  vald_lev NO-LOCK NO-ERROR.
            MESSAGE "Det finns ingen materiel med E-nummer:" + FILL-IN-ENR2 + "i depå eller hos leverantör:" + STRING(levtemp.LEVNAMN) VIEW-AS ALERT-BOX
            TITLE "Meddelande".    
            RETURN.
         END.
     END.
  END.
  RUN openbdynspec_UI IN brwproc[5].  
  RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
  RUN lastselectdyn_UI IN brwproc[5].
  RUN title_UI IN brwproc[5].
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
   IF brwname = "BRW_DEPA" THEN valbrw = 1.  
   IF brwname = "BRW_MTRL" THEN valbrw = 2. 
   IF brwname = "BRW_HLEV" THEN valbrw = 3. 
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/      
   IF valbrw = 1 THEN DO:   
      status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      valenr = mtrldeptemp.ENR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrldeptemp.LEVKOD
      NO-LOCK NO-ERROR.      
   END.
   ELSE IF valbrw = 2 THEN DO:
      
      valenr = spec_mtrl.ENR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
      NO-LOCK NO-ERROR.      
   END.   
   ELSE IF valbrw = 3 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      valenr = mtrltemp.ENR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
      NO-LOCK NO-ERROR.      
   END.   
   IF status-ok THEN DO:   
      levnamnvar = levtemp.LEVNAMN .
      {LEVLANK.I}
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-1 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV ".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV ".
     tth = TEMP-TABLE mtrltemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over3_UI WINDOW-1 
PROCEDURE over3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrldeptemp.ENR AND 
   spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE spec_mtrl THEN DO:      
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         /*gör en kontroll om det som returneras finns uttaget på projektet om inte ge en varning om att så ej är fallet*/
         RUN returkontroll_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa , 
                                            INPUT valkund ,INPUT mtrldeptemp.ENR,OUTPUT kontroll).                                        
         IF kontroll = TRUE THEN DO:
            CREATE spec_mtrl. 
            ASSIGN
            spec_mtrl.ENR = mtrldeptemp.ENR
            spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
            spec_mtrl.ENHET = mtrldeptemp.ENHET
            spec_mtrl.SALDO = antalvar
            spec_mtrl.NPRIS = mtrldeptemp.NPRIS          
            spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD
            val_rec = ROWID(spec_mtrl).
         END.
         ELSE DO:
            MESSAGE "Det finns inget uttag gjort med " Guru.Konstanter:genk + ":" +  mtrldeptemp.ENR + " mot " +
            Guru.Konstanter:gaok + ":" + valaonr + " delnr:" + STRING(valdelnr) + ". Vill Ni registrera returen ändå?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Retur?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:      
               CREATE spec_mtrl. 
               ASSIGN
               spec_mtrl.ENR = mtrldeptemp.ENR
               spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
               spec_mtrl.ENHET = mtrldeptemp.ENHET
               spec_mtrl.SALDO = antalvar
               spec_mtrl.NPRIS = mtrldeptemp.NPRIS          
               spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD
               val_rec = ROWID(spec_mtrl). 
            END.      
         END.
      END.
      ELSE DO:
         CREATE spec_mtrl. 
         ASSIGN
         spec_mtrl.ENR = mtrldeptemp.ENR
         spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
         spec_mtrl.ENHET = mtrldeptemp.ENHET
         spec_mtrl.SALDO = antalvar
         spec_mtrl.NPRIS = mtrldeptemp.NPRIS          
         spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD
         val_rec = ROWID(spec_mtrl).
      END.      
   END.  
   ELSE DO:
      IF enrval = TRUE THEN DO:
         ASSIGN
         spec_mtrl.SALDO = antalvar
         val_rec = ROWID(spec_mtrl).               
      END.      
   END.
   antal_raknare = antal_raknare + 1.
                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over4_UI WINDOW-1 
PROCEDURE over4_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
      /*Niklas personlig spec_mtrl*/
   IF emtrltemp.LEVKOD BEGINS "99" THEN DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = emtrltemp.ENR AND spec_mtrl.LEVKOD = "99" 
      NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = emtrltemp.ENR AND spec_mtrl.LEVKOD = emtrltemp.LEVKOD 
      NO-LOCK NO-ERROR. 
   END.       
   IF NOT AVAILABLE spec_mtrl THEN DO: 
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         /*gör en kontroll om det som returneras finns uttaget på projektet om inte ge en varning om att så ej är fallet*/
         RUN returkontroll_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa , 
                                            INPUT valkund ,INPUT emtrltemp.ENR,OUTPUT kontroll).                                        
         IF kontroll = TRUE THEN DO:
            CREATE spec_mtrl. 
            ASSIGN
            spec_mtrl.ENR = emtrltemp.ENR
            spec_mtrl.BENAMNING = emtrltemp.BENAMNING
            spec_mtrl.ENHET = emtrltemp.ENHET
            spec_mtrl.SALDO = antalvar
            spec_mtrl.NPRIS = emtrltemp.NPRIS                
            val_rec = ROWID(spec_mtrl).
            /*Niklas personlig spec_mtrl*/
            IF emtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
            ELSE spec_mtrl.LEVKOD = emtrltemp.LEVKOD. 
         END.
         ELSE DO:
            MESSAGE "Det finns inget uttag gjort med " Guru.Konstanter:genk + ":" +  emtrltemp.ENR + " mot " +
            Guru.Konstanter:gaok + ":" + valaonr + " delnr:" + STRING(valdelnr) + ". Vill Ni registrera returen ändå?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Retur?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:      
               CREATE spec_mtrl. 
               ASSIGN
               spec_mtrl.ENR = emtrltemp.ENR
               spec_mtrl.BENAMNING = emtrltemp.BENAMNING
               spec_mtrl.ENHET = emtrltemp.ENHET
               spec_mtrl.SALDO = antalvar
               spec_mtrl.NPRIS = emtrltemp.NPRIS                
               val_rec = ROWID(spec_mtrl).
               /*Niklas personlig spec_mtrl*/
               IF emtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
               ELSE spec_mtrl.LEVKOD = emtrltemp.LEVKOD. 
            END.      
         END.
      END.
      ELSE DO:
         CREATE spec_mtrl. 
         ASSIGN
         spec_mtrl.ENR = emtrltemp.ENR
         spec_mtrl.BENAMNING = emtrltemp.BENAMNING
         spec_mtrl.ENHET = emtrltemp.ENHET
         spec_mtrl.SALDO = antalvar
         spec_mtrl.NPRIS = emtrltemp.NPRIS                
         val_rec = ROWID(spec_mtrl).
         /*Niklas personlig spec_mtrl*/
         IF emtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
         ELSE spec_mtrl.LEVKOD = emtrltemp.LEVKOD. 
      END.
   END. 
   ELSE DO:
      IF enrval = TRUE THEN DO:
         ASSIGN
         spec_mtrl.SALDO = antalvar
         val_rec = ROWID(spec_mtrl).               
      END.      
   END.
   antal_raknare = antal_raknare + 1.                     
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
   /*Niklas personlig spec_mtrl*/
   IF mtrltemp.LEVKOD BEGINS "99" THEN DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND spec_mtrl.LEVKOD = "99" 
      NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND spec_mtrl.LEVKOD = mtrltemp.LEVKOD 
      NO-LOCK NO-ERROR. 
   END.      
   IF NOT AVAILABLE spec_mtrl THEN DO: 
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         /*gör en kontroll om det som returneras finns uttaget på projektet om inte ge en varning om att så ej är fallet*/
         RUN returkontroll_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa , 
                                            INPUT valkund ,INPUT mtrltemp.ENR,OUTPUT kontroll).                                        
         IF kontroll = TRUE THEN DO:
            CREATE spec_mtrl. 
            ASSIGN
            spec_mtrl.ENR = mtrltemp.ENR
            spec_mtrl.BENAMNING = mtrltemp.BENAMNING
            spec_mtrl.ENHET = mtrltemp.ENHET
            spec_mtrl.SALDO = antalvar
            spec_mtrl.NPRIS = mtrltemp.NPRIS          
            val_rec = ROWID(spec_mtrl).
            /*Niklas personlig spec_mtrl*/
            IF mtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
            ELSE spec_mtrl.LEVKOD = mtrltemp.LEVKOD. 
         END.
         ELSE DO:
            MESSAGE "Det finns inget uttag gjort med " Guru.Konstanter:genk + ":" +  mtrldeptemp.ENR + " mot " +
            Guru.Konstanter:gaok + ":" + valaonr + " delnr:" + STRING(valdelnr) + ". Vill Ni registrera returen ändå?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Retur?" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:      
               CREATE spec_mtrl. 
               ASSIGN
               spec_mtrl.ENR = mtrltemp.ENR
               spec_mtrl.BENAMNING = mtrltemp.BENAMNING
               spec_mtrl.ENHET = mtrltemp.ENHET
               spec_mtrl.SALDO = antalvar
               spec_mtrl.NPRIS = mtrltemp.NPRIS          
               val_rec = ROWID(spec_mtrl).
               /*Niklas personlig spec_mtrl*/
               IF mtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
               ELSE spec_mtrl.LEVKOD = mtrltemp.LEVKOD. 
            END.      
         END.
      END.
      ELSE DO:
         CREATE spec_mtrl. 
         ASSIGN
         spec_mtrl.ENR = mtrltemp.ENR
         spec_mtrl.BENAMNING = mtrltemp.BENAMNING
         spec_mtrl.ENHET = mtrltemp.ENHET
         spec_mtrl.SALDO = antalvar
         spec_mtrl.NPRIS = mtrltemp.NPRIS          
         val_rec = ROWID(spec_mtrl).
         /*Niklas personlig spec_mtrl*/
         IF mtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
         ELSE spec_mtrl.LEVKOD = mtrltemp.LEVKOD. 
      END.
   END. 
   ELSE DO:   
      IF enrval = TRUE THEN DO:
         ASSIGN
         spec_mtrl.SALDO = antalvar
         val_rec = ROWID(spec_mtrl).               
      END.      
   
   END.
   antal_raknare = antal_raknare + 1.                     
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
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28].
            
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vccmblev_UI WINDOW-1 
PROCEDURE vccmblev_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   {muswait.i}                                
   lev = INPUT FRAME {&FRAME-NAME} CMB_LEV.
   IF lev NE huvudlev THEN DO:
      ASSIGN                            
      CMB_LEV:HIDDEN = TRUE
      BTN_LEV:HIDDEN = FALSE.  
      /*Niklas personlig spec_mtrl*/
      IF lev BEGINS Guru.Konstanter:globanv THEN DO:            
         vald_lev = "99" + Guru.Konstanter:globanv. 
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev 
         USE-INDEX LEV NO-LOCK NO-ERROR.
         vald_lev = levtemp.LEVKOD. 
      END.      
      IF vald_lev BEGINS "99" THEN DO:        
         RUN initsok_UI (INPUT 2,INPUT "").
      END.           
      RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + lev).              
   END.
   ELSE DO:
      vald_lev = vald_kundlev.
   END.
   {musarrow.i}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

