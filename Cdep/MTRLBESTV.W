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
DEFINE INPUT-OUTPUT PARAMETER input_bestnr AS INTEGER NO-UNDO.
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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
{MTTEMP.I}   
&Scoped-define NEW 

DEFINE NEW SHARED VARIABLE dep AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE avb AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytt_bestnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bestalld AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
  
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev  AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE val_rec AS ROWID NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE dbenamning AS CHARACTER NO-UNDO.  
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
DEFINE VARIABLE bnummer AS INTEGER NO-UNDO. 
DEFINE VARIABLE hleverantorkod AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lnamn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.   
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE enrval AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE enrvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE sistaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollbt AS LOGICAL NO-UNDO.
/*DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.*/

DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlevnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytlev AS LOGICAL NO-UNDO.

DEFINE VARIABLE vlev AS CHARACTER NO-UNDO.


DEFINE NEW SHARED TEMP-TABLE aspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD NY AS LOGICAL
   FIELD ANDRAD AS LOGICAL
   FIELD BORT AS LOGICAL.   
&Scoped-define NEW   
&Scoped-define SHARED SHARED 
{bestnrtab.I}
/*DEFINE SHARED TEMP-TABLE best_nr_tab NO-UNDO
    FIELD bestnr AS INTEGER FORMAT ">>>>>9" LABEL "Bestnr"
    FIELD bestdatum AS DATE FORMAT "99/99/99" LABEL "Best-datum"
    FIELD bestallare AS CHARACTER FORMAT "x(12)" LABEL "Beställare"
    FIELD aonr AS CHARACTER
    FIELD delnr AS INTEGER
    INDEX NR bestnr DESCENDING.*/

    
DEFINE QUERY sok FOR mtrltemp.

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
mtrldeptemp.BENAMNING mtrldeptemp.ENHET mtrldeptemp.LEVKOD ~
mtrldeptemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH mtrldeptemp ~
      WHERE mtrldeptemp.LEVKOD NO-LOCK ~
    BY mtrldeptemp.ENR
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH mtrldeptemp ~
      WHERE mtrldeptemp.LEVKOD NO-LOCK ~
    BY mtrldeptemp.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA mtrldeptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA mtrldeptemp


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.LEVKOD mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.Enr spec_mtrl.BERKVANT ~
spec_mtrl.NPRIS spec_mtrl.Enhet spec_mtrl.Benamning spec_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.BERKVANT ~
spec_mtrl.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_DEPA BRW_HLEV FBTN_SKAPA FBTN_BYT ~
btn_over FBTN_VISA FBTN_SKRIV btn_back FBTN_BEST BTN_AVB BTN_LEV CMB_LEV ~
BTN_SPEC FILL-IN-ENR FILL-IN-BEN FILL-IN-FACK FILL-IN-FACKID1 ~
FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL BTN_UP BTN_MIN RAD_SOK ~
FBTN_HBESTPU FBTN_IEXC FILL-IN-SALDO CMB_LEV-2 RECT-2 BRW_DEPA RECT-4 ~
BRW_MTRL IMAGE-3 
&Scoped-Define DISPLAYED-OBJECTS RAD_DEPA CMB_LEV FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-FACK FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL ~
RAD_SOK FILL-IN-SALDO CMB_LEV-2 

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
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_LEV 
     LABEL "Huvudleverantör" 
     SIZE 18 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_SPEC 
     LABEL "Spec.mtrl" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_BYT 
     LABEL "Byt lev." 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_HBESTPU 
     LABEL "Hämta best.punkt" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_IEXC 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Enr första kolumnen, antal i andra (blank = 1), lev i tredje (blank = vald leverantör depå)".

DEFINE BUTTON FBTN_SKAPA 
     LABEL "Leverans/Beställning" 
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

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV-2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>>9":U INITIAL 1 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 8.38 BY .83 NO-UNDO.

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
     SIZE 4.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(6)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SALDO AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Saldo senast markerad" 
      VIEW-AS TEXT 
     SIZE 7.63 BY .67 NO-UNDO.

DEFINE IMAGE IMAGE-3
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_DEPA AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Depå-mtrl", 1,
"Total-mtrl", 2
     SIZE 31.75 BY .75 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 34.88 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.5 BY 4.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55.38 BY 3.63
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
      mtrldeptemp.BENAMNING FORMAT "x(256)":U WIDTH 18
      mtrldeptemp.ENHET FORMAT "x(5)":U
      mtrldeptemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "X(4)":U
      mtrldeptemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 19.5
         TITLE "Materiellista för depå.".

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 20
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.LEVKOD COLUMN-LABEL "Lev" FORMAT "X(4)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 19.5
         TITLE "Materiellista huvudleverantör ".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.Enr FORMAT "X(11)":U
      spec_mtrl.BERKVANT FORMAT ">>>>>9":U
      spec_mtrl.NPRIS FORMAT ">>>>>9.99":U
      spec_mtrl.Enhet FORMAT "x(5)":U
      spec_mtrl.Benamning FORMAT "x(256)":U WIDTH 23
      spec_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U
  ENABLE
      spec_mtrl.BERKVANT
      spec_mtrl.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 55.38 BY 19.5
         TITLE "Beställd materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_DEPA AT ROW 1.33 COL 1.5 NO-LABEL
     BRW_HLEV AT ROW 2.29 COL 1.5
     FBTN_SKAPA AT ROW 9.08 COL 111.5
     FBTN_BYT AT ROW 10.21 COL 111.5
     btn_over AT ROW 10.29 COL 51.13
     FBTN_VISA AT ROW 11.29 COL 111.5
     FBTN_SKRIV AT ROW 12.42 COL 111.5
     btn_back AT ROW 13.04 COL 51.13
     FBTN_BEST AT ROW 26.83 COL 111.5
     BTN_AVB AT ROW 27.92 COL 111.5
     BTN_LEV AT ROW 22.29 COL 24.25
     CMB_LEV AT ROW 22.25 COL 22.25 COLON-ALIGNED
     BTN_SPEC AT ROW 24.08 COL 78.88
     FILL-IN-ENR AT ROW 24.92 COL 21.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 25.92 COL 21.88 COLON-ALIGNED
     FILL-IN-FACK AT ROW 26.83 COL 21.88 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 26.83 COL 21.88 COLON-ALIGNED
     FILL-IN-FACKID2 AT ROW 26.83 COL 39.25 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 25.83 COL 69 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 27.29 COL 69 COLON-ALIGNED
     BTN_UP AT ROW 27.04 COL 80.25
     BTN_MIN AT ROW 28.04 COL 80.25
     RAD_SOK AT ROW 27.92 COL 14.13 NO-LABEL
     FBTN_HBESTPU AT ROW 8 COL 111.5
     FBTN_IEXC AT ROW 13.54 COL 111.5
     FILL-IN-SALDO AT ROW 23.54 COL 5.75 WIDGET-ID 2
     CMB_LEV-2 AT ROW 1.25 COL 104 COLON-ALIGNED WIDGET-ID 4
     BRW_DEPA AT ROW 2.38 COL 1.5
     BRW_MTRL AT ROW 2.29 COL 55.5
     "Hämta:" VIEW-AS TEXT
          SIZE 7.88 BY .83 AT ROW 25.83 COL 55.88
     "Sökalternativ:" VIEW-AS TEXT
          SIZE 10.63 BY .83 AT ROW 27.75 COL 2.38
     RECT-2 AT ROW 24.5 COL 1.5
     RECT-4 AT ROW 25.5 COL 54.63
     IMAGE-3 AT ROW 24.83 COL 2
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
/* BROWSE-TAB BRW_HLEV RAD_DEPA FRAME-A */
/* BROWSE-TAB BRW_DEPA RECT-2 FRAME-A */
/* BROWSE-TAB BRW_MTRL RECT-4 FRAME-A */
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
       BTN_UP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV-2:HIDDEN IN FRAME FRAME-A           = TRUE.

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

/* SETTINGS FOR FILL-IN FILL-IN-SALDO IN FRAME FRAME-A
   ALIGN-L                                                              */
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
     _Where[1]         = "Temp-Tables.mtrldeptemp.LEVKOD"
     _FldNameList[1]   = Temp-Tables.mtrldeptemp.FACKID
     _FldNameList[2]   = Temp-Tables.mtrldeptemp.ENR
     _FldNameList[3]   > Temp-Tables.mtrldeptemp.BENAMNING
"mtrldeptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.mtrldeptemp.ENHET
     _FldNameList[5]   > Temp-Tables.mtrldeptemp.LEVKOD
"mtrldeptemp.LEVKOD" "Lev" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = Temp-Tables.mtrldeptemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_kundlev
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   > Temp-Tables.mtrltemp.LEVKOD
"mtrltemp.LEVKOD" "Lev" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.spec_mtrl.BERKVANT
"spec_mtrl.BERKVANT" ? ">>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.NPRIS
"spec_mtrl.NPRIS" ? ">>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.spec_mtrl.Enhet
     _FldNameList[5]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.spec_mtrl.LEVKOD
"spec_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DEPA
&Scoped-define SELF-NAME BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå. */
DO:
   APPLY "CHOOSE"  TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON VALUE-CHANGED OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå. */
DO:
   status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   antal_valda = BRW_DEPA:NUM-SELECTED-ROWS NO-ERROR.  
   IF antal_valda = 0 THEN FILL-IN-SALDO = 0.
   ELSE FILL-IN-SALDO = mtrldeptemp.SALDO.
   DISPLAY FILL-IN-SALDO WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:
   APPLY "CHOOSE"  TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON VALUE-CHANGED OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:
   status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON VALUE-CHANGED OF BRW_MTRL IN FRAME FRAME-A /* Beställd materiel */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.BERKVANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BERKVANT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF spec_mtrl.BERKVANT IN BROWSE BRW_MTRL /* antal */
DO:
   APPLY "ENTRY" TO spec_mtrl.BERKVANT IN BROWSE BRW_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO: 
   IF bestalld = TRUE THEN DO:
      MESSAGE "Ni har gjort en beställning och uppgifterna kommer att lagras."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:   
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO: 
         musz = TRUE.
         IF input_bestnr = 0 THEN DO:
            RUN bortbest_UI IN mtrlbapph (INPUT vald_depa,INPUT input_bestnr,INPUT nytt_bestnr).                                     
         END.
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                    
      ELSE DO:
         musz = musz.
      END.     
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   
   RUN bort_UI.                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LEV WINDOW-1
ON CHOOSE OF BTN_LEV IN FRAME FRAME-A /* Huvudleverantör */
DO:   
   {muswait.i}
   vald_lev = vald_kundlev.         
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + Guru.SharedVariable:ValdmtrlLeverantorName).         
   ASSIGN 
   FILL-IN-BEN:HIDDEN = FALSE 
   FILL-IN-ENR:HIDDEN = FALSE    
   BTN_LEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = FALSE   
   CMB_LEV:SCREEN-VALUE = Guru.SharedVariable:ValdmtrlLeverantorName
   vald_lev = vald_kundlev.   
   /*BRW_HLEV:TITLE = "Materiellista huvudleverantör: " + huvudlev.*/
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
   RAD_DEPA = INPUT RAD_DEPA.
   RUN btnover_UI.
                                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPEC WINDOW-1
ON CHOOSE OF BTN_SPEC IN FRAME FRAME-A /* Spec.mtrl */
DO:           
   {muswait.i}
   {AVBGOM.I}
   RUN DEPSPECANV2.W (INPUT 1).
   {AVBFRAM.I}   
   RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
   RUN openbdynspec_UI IN brwproc[5]. 
   RUN title_UI IN brwproc[5].
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
   {muswait.i}                                
   lev = INPUT CMB_LEV.   
   IF lev NE Guru.SharedVariable:ValdmtrlLeverantorName THEN DO:                 
      RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + lev). 
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
      ASSIGN                            
      CMB_LEV:HIDDEN = TRUE      
      BTN_LEV:HIDDEN = FALSE.      
      
   END.
   ELSE DO:
      vald_lev = vald_kundlev.
   END.
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV-2 WINDOW-1
ON VALUE-CHANGED OF CMB_LEV-2 IN FRAME FRAME-A /* Leverantörer */
DO: 
   {muswait.i}                                
    CMB_LEV-2 = INPUT CMB_LEV-2.   
   
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BEST WINDOW-1
ON CHOOSE OF FBTN_BEST IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}
   DEFINE VARIABLE varning AS LOGICAL NO-UNDO.
   /*kalmar depåändring abc nu*/
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      varning = FALSE.
      FOR EACH spec_mtrl:
         EMPTY TEMP-TABLE emtrltemp NO-ERROR.          
         RUN hmtskap_UI IN mtrlbapph (INPUT spec_mtrl.ENR,INPUT spec_mtrl.LEVKOD,OUTPUT TABLE emtrltemp). 
         
         FIND FIRST emtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE emtrltemp THEN DO:
            IF spec_mtrl.NPRIS NE emtrltemp.NPRIS THEN DO:
               varning = TRUE.
            END.            
         END.
         /*kalmar depåändring abc nu*/
         IF Guru.Konstanter:globforetag = "GKAL" AND varning = TRUE THEN DO:                        
            FIND FIRST mtrldeptemp WHERE  mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.Enr = spec_mtrl.ENR AND mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR.
            IF AVAILABLE mtrldeptemp  THEN DO:
               IF mtrldeptemp.LEVKOD = "10" THEN varning = FALSE.
            END.   
         END.    
      END.
       
      IF varning = TRUE THEN DO:
         MESSAGE "Ni har ändrat något pris mot vad prislistan säger. Är detta korrekt?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Uppdatera Priser?" UPDATE svar.         
         IF NOT svar THEN DO:           
            RETURN NO-APPLY.
         END.
      END.
   END.
   RUN ok_UI.   
   APPLY "GO" TO FRAME {&FRAME-NAME}.
   APPLY "CLOSE":U TO THIS-PROCEDURE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BYT WINDOW-1
ON CHOOSE OF FBTN_BYT IN FRAME FRAME-A /* Byt lev. */
DO:            
   RUN bytlev_UI.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_HBESTPU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_HBESTPU WINDOW-1
ON CHOOSE OF FBTN_HBESTPU IN FRAME FRAME-A /* Hämta best.punkt */
DO:            
   RUN hbestpunkt_UI.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IEXC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IEXC WINDOW-1
ON CHOOSE OF FBTN_IEXC IN FRAME FRAME-A /* Import Exel */
DO:   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE mttemp NO-ERROR. 
   EMPTY TEMP-TABLE felmex NO-ERROR. 
   fildir = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den excelfil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                      
      {muswait.i}   
       
      RUN MTRLEXELIN.P (INPUT fildir,INPUT vald_lev, OUTPUT TABLE mttemp ).         
      RUN enrvalexc_UI.
      
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning adm*/                     
         RUN adnettomark_UI IN nettoh (INPUT TABLE spec_mtrl,INPUT-OUTPUT TABLE ikmtrltemp ).                  
      END.   
      /*IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         RUN linettomark2_UI IN nettoh (INPUT TABLE spec_mtrl, OUTPUT TABLE kmtrltemp APPEND).            
      END.*/
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
      RUN openbdynspec_UI IN brwproc[5].  
      RUN title_UI IN brwproc[5].
      FIND FIRST felmex NO-ERROR.
      IF AVAILABLE felmex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).         
         PUT "Dessa enr blev ej inlästa från excel . Var god kontrollera!" AT 6.         
         PUT SKIP.         
         FOR EACH felmex:           
            PUT UNFORMATTED felmex.ENR AT 6 felmex.ANTAL AT 19 SUBSTRING(felmex.LEVKOD,1,3) AT 32 SUBSTRING(felmex.LEVNAMN,1,20) AT 42 SUBSTRING(felmex.BENAMNING,1,20) AT 65.  
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


&Scoped-define SELF-NAME FBTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKAPA WINDOW-1
ON CHOOSE OF FBTN_SKAPA IN FRAME FRAME-A /* Leverans/Beställning */
DO:  
   
   {AMERICANEUROPEAN.I}
   RUN leverans_UI.
   {EUROPEANAMERICAN.I}
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO: 
   
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      {muswait.i}             
      skrivut = TRUE.
      {AVBGOM.I}
      {AMERICANEUROPEAN.I}
      RUN VISDBESTV.W.
      {EUROPEANAMERICAN.I}
      
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
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
   
   {muswait.i}
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   {AVBGOM.I}
   {AMERICANEUROPEAN.I}
   RUN VISDBESTV.W.
   {EUROPEANAMERICAN.I}   
   
   {AVBFRAM.I}
   {musarrow.i}   
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
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:   
   {muswait.i}
   ASSIGN
   FILL-IN-BEN = INPUT FILL-IN-BEN
   RAD_DEPA = INPUT RAD_DEPA.
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
     /* ELSE DO:        
         IF CMB_LEV = huvudlev THEN DO:
            RUN openbdyn_UI IN brwproc[2] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" + STRING(vald_kundlev) + "'").
            FIND FIRST mtrltemp NO-LOCK NO-ERROR.
            IF AVAILABLE mtrltemp THEN DO:
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
               RUN lastselectdyn_UI IN brwproc[2].
            END.
         END.
         ELSE DO:
            RUN openbdyn_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' AND mtrltemp.KALKNR = 0 ").
            FIND FIRST mtrltemp NO-LOCK NO-ERROR.
            IF AVAILABLE mtrltemp THEN DO:   
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
               RUN lastselectdyn_UI IN brwproc[2].
            END.      
         END.               
      END. */
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:   
   {muswait.i} 
   ASSIGN
   FILL-IN-ENR = INPUT FILL-IN-ENR
   RAD_DEPA = INPUT RAD_DEPA.
   

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
      /*ELSE DO:
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX ENR ").
         RUN openbdynspec_UI IN brwproc[2].
         FIND FIRST mtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:      
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.      
      END.*/
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACK IN FRAME FRAME-A /* Sök på Fack */
DO:
   {muswait.i}
   ASSIGN
   FILL-IN-FACK = var1 + var2.    
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
   {musarrow.i}
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   ASSIGN
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:   
   ASSIGN
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
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO: 
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
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
      FILL-IN-FACKID2:HIDDEN = FALSE
      RAD_SOK:HIDDEN = TRUE.      
      FILL-IN-SALDO:HIDDEN = FALSE.      
   END.
   IF RAD_DEPA = 2 THEN DO:  
      RUN changebrwh_UI IN brwproc[6] 
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}, INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
      /*RUN openbdynspec_UI IN brwproc[2].               */
      vald_lev = vald_kundlev.      
      ASSIGN
      BRW_DEPA:HIDDEN = TRUE
      BRW_HLEV:HIDDEN = FALSE            
      CMB_LEV:HIDDEN = FALSE
      BTN_LEV:HIDDEN = TRUE      
      FILL-IN-ENR:HIDDEN = FALSE
      FILL-IN-BEN:HIDDEN = FALSE 
      FILL-IN-FACKID1:HIDDEN = TRUE 
      FILL-IN-FACKID2:HIDDEN = TRUE       
      CMB_LEV:SCREEN-VALUE = Guru.SharedVariable:ValdmtrlLeverantorName
      RAD_SOK:HIDDEN = FALSE.      
      FILL-IN-SALDO:HIDDEN = TRUE.      
            
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" +   STRING(vald_lev) + "'").
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
ON CLOSE OF THIS-PROCEDURE 
DO:
   
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:   
   IF musz = FALSE THEN DO:
      IF bestalld = FALSE THEN DO:
         MESSAGE "OBS! Vill du spara dina ändringar?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
         IF svar THEN DO:
            APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF NOT svar THEN DO:             
            RUN bortbest_UI IN mtrlbapph (INPUT vald_depa,INPUT input_bestnr,INPUT nytt_bestnr).                         
            APPLY "CLOSE":U TO THIS-PROCEDURE.   
         END.                    
         ELSE DO:
            musz = musz.
            RETURN NO-APPLY.
         END.  
      END.
      ELSE DO:
         MESSAGE "Ni har gjort en beställning och uppgifterna kommer att lagras."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
      END.   
   END.
   ELSE DO:
      musz = FALSE.   
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
   END.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:  
  IF musz = FALSE THEN DO:
      IF bestalld = FALSE THEN DO:
         MESSAGE "OBS! Vill du spara dina ändringar?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
         IF svar THEN DO:
            APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF NOT svar THEN DO TRANSACTION: 
            RUN bortbest_UI IN mtrlbapph (INPUT vald_depa,INPUT input_bestnr,INPUT nytt_bestnr).                         
            APPLY "CLOSE":U TO THIS-PROCEDURE.   
         END.                    
         ELSE DO:
            musz = musz.
            RETURN NO-APPLY.
         END.  
      END.
      ELSE DO:
         MESSAGE "Ni har gjort en beställning och uppgifterna kommer att lagras."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
      END.   
   END.
   ELSE DO:
      musz = FALSE.   
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN NO-APPLY.
   END.
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
   EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
   RUN mtrlbhmt_UI IN mtrlbapph (INPUT vald_depa,OUTPUT TABLE mtrldeptemp,OUTPUT vald_kundlev, OUTPUT dbenamning,OUTPUT bnummer).
             
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning adm*/                     
      RUN depnettomark_UI IN nettoh (INPUT TABLE mtrldeptemp,INPUT-OUTPUT TABLE ikmtrltemp ).                  
   END.   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa) + "  AND mtrldeptemp.IBDATUM = ? AND mtrldeptemp.LAGER = TRUE ").   
   RUN openbdynspec_UI IN brwproc[1].         
   ASSIGN vald_lev = vald_kundlev.   
   RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
   RUN openbdynspec_UI IN brwproc[5].
   RUN title_UI IN brwproc[5].
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.    
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.  
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.    
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN.     
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + Guru.SharedVariable:ValdmtrlLeverantorName).
   
   status-ok = CMB_LEV-2:DELETE("0"). 
   status-ok = CMB_LEV-2:ADD-LAST("Alla")IN FRAME {&FRAME-NAME}.
   CMB_LEV-2:SCREEN-VALUE = "Alla".
            
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0"  AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      /*Niklas personlig spec_mtrl*/
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(Guru.Konstanter:globanv + " " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.         
      END.
      ELSE DO:
         /*kalmar depåändring abc nu*/
         IF Guru.Konstanter:globforetag = "LULE" THEN DO:
            IF levtemp.LEVKOD NE "41" THEN DO:
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
               status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
            END.   
         END.
         ELSE IF Guru.Konstanter:globforetag = "BODE" THEN DO:
            IF levtemp.LEVKOD NE "6" THEN DO:
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
               status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
            END.   
         END.
         ELSE IF Guru.Konstanter:globforetag = "KRAF" THEN DO:
            IF levtemp.LEVKOD NE "6" THEN DO:
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
               status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
            END.   
         END.
         ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
            IF levtemp.LEVKOD NE "20" THEN DO:
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
               status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
            END.   
         END.
         ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            IF levtemp.LEVKOD = "20" THEN.
            ELSE IF  levtemp.LEVKOD = "21" THEN.
            ELSE DO:
               status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
               status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
            END.   
         END.
         ELSE DO:
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
            status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.  
      END.      
   END.   
                           
   ASSIGN             
   CMB_LEV = INPUT CMB_LEV
   CMB_LEV-2 = INPUT CMB_LEV-2   
   vald_lev = vald_kundlev
   bestant = FALSE   
   musz = FALSE
   RAD_DEPA = 1.   
   IF input_bestnr = 0 THEN DO:  
      nytt_bestnr = bnummer .
      IF valaonr = "" THEN
      {&WINDOW-NAME}:TITLE = "Beställning nr " + STRING(nytt_bestnr) 
         + " för depå - " + dbenamning.      
      ELSE {&WINDOW-NAME}:TITLE = "Beställning nr " + string (nytt_bestnr) 
         + " på " + Guru.Konstanter:gaok + ": " + valaonr + " Delnr: " + STRING(valdelnr,Guru.Konstanter:varforetypchar[1]) + " för depå - " + dbenamning.      
      RUN bdepskap_UI IN mtrlbapph (INPUT vald_depa,INPUT nytt_bestnr).                   
   END.
   ELSE DO:
      ASSIGN
      nytt_bestnr = input_bestnr.
      IF valaonr = "" THEN
      {&WINDOW-NAME}:TITLE = "Ändring av beställning nr " + string (nytt_bestnr) + " för depå - " + dbenamning.              
      ELSE {&WINDOW-NAME}:TITLE = "Ändring av beställning nr " + string (nytt_bestnr) + " på " + Guru.Konstanter:gaok + ": " + valaonr + 
      " Delnr: " + STRING(valdelnr) + " för depå - " + dbenamning.
   END.       
   /*BRW_HLEV:TITLE = "Materiellista huvudleverantör: " + huvudlev.*/

   RUN enable_UI.      
   ASSIGN
   RECT-4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN-ENR2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN-ANTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   BTN_UP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   BTN_MIN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   FBTN_IEXC:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   /*IF Guru.Konstanter:globforetag = "elpa" THEN FBTN_IEXC:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.*/
   {FRMSIZE.I}          
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa) + "  AND mtrldeptemp.IBDATUM = ? AND mtrldeptemp.LAGER = TRUE ").
   RUN openbdynspec_UI IN brwproc[1].      
   
   IF input_bestnr NE 0 THEN DO:    
      RUN bdepandra_UI IN mtrlbapph (INPUT vald_depa,INPUT input_bestnr,INPUT TABLE mtrldeptemp, OUTPUT TABLE spec_mtrl ).                               
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning adm*/                     
         RUN adnettomark_UI IN nettoh (INPUT TABLE spec_mtrl,INPUT-OUTPUT TABLE ikmtrltemp ).                  
      END.   
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
      RUN openbdynspec_UI IN brwproc[5]. 
      RUN title_UI IN brwproc[5].
   END.     
   
   
   /*kalmar depåändring abc nu*/
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:
      musz = musz.
   END.
   ELSE DO:      
      spec_mtrl.NPRIS:READ-ONLY IN BROWSE BRW_MTRL = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "cELPA" {GLOBVES.I} THEN DO:
      FBTN_SKAPA:HIDDEN = TRUE.
   END.   
   IF Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      FBTN_HBESTPU:HIDDEN = TRUE.
      CMB_LEV-2:HIDDEN = TRUE.
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN mtrldeptemp.NPRIS:VISIBLE IN BROWSE BRW_DEPA = FALSE.
      ASSIGN mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE.
      ASSIGN spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.
   END.
   ASSIGN
   BRW_HLEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = TRUE
   BTN_LEV:HIDDEN = TRUE
   RAD_SOK:HIDDEN = TRUE               
   FILL-IN-FACK:HIDDEN = TRUE. 
   FILL-IN-SALDO:HIDDEN = FALSE.
   Guru.GlobalaVariabler:collefth = ?.      
   IF FBTN_HBESTPU:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_HBESTPU:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF FBTN_SKAPA:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_SKAPA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_BYT:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_BYT:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_VISA:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF FBTN_SKRIV:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   
   IF FBTN_IEXC:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_IEXC:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   FBTN_IEXC:LABEL = "Import Excel".
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
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).             
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[5] (INPUT THIS-PROCEDURE).    
   RUN dynprogextra IN brwproc[5] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[5] (INPUT TRUE).   
   RUN DYNARROW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_DEPA:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ?,
       INPUT ? , INPUT ?).
   RUN setcolindex_UI IN brwproc[5] (INPUT "spec_mtrl.ENR").   
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 2).
   RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").   
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avbryt_UI WINDOW-1 
PROCEDURE avbryt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   
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
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                      
      DELETE spec_mtrl.
      RUN valrattright_UI IN brwproc[5] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(spec_mtrl)).
      antal_raknare = antal_raknare + 1.   
   END.
   RUN refreshbrw_UI IN brwproc[5].
   RUN lastselectdyn_UI IN brwproc[5].
   RUN title_UI IN brwproc[5].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnover_UI WINDOW-1 
PROCEDURE btnover_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF RAD_DEPA = 1 THEN DO: 
      antal_valda = BRW_DEPA:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_DEPA:FETCH-SELECTED-ROW(antal_raknare).
         ASSIGN antalvar = 1
         enrval = FALSE.
         RUN over3_UI.
      END.       
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
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
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
      RUN openbdynspec_UI IN brwproc[5].       
      status-ok = BRW_HLEV:DESELECT-ROWS() NO-ERROR.
      RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
      RUN lastselectdyn_UI IN brwproc[5].                     
      RUN title_UI IN brwproc[5].
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytlev_UI WINDOW-1 
PROCEDURE bytlev_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   {muswait.i}  
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" 
   OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:
      MESSAGE "OBS! Vill Ni byta levarantör på alla artiklar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Byte av leverantör?" UPDATE svar.         
      IF svar THEN DO:         
         {AVBGOM.I}                               
         RUN LEVBYTE2V.W (INPUT ?). 
         {AVBFRAM.I}         
      END.
      ELSE IF NOT svar THEN DO:             
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                  
            {AVBGOM.I}                               
            RUN LEVBYTE2V.W (INPUT ROWID(spec_mtrl)). 
            {AVBFRAM.I}            
            antal_raknare = antal_raknare + 1.   
         END.   
      END.
      ELSE musz = musz.
   END.
   ELSE DO:   
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).            
         {AVBGOM.I}                               
         RUN LEVBYTE2V.W (INPUT ROWID(spec_mtrl)). 
         {AVBFRAM.I}      
         antal_raknare = antal_raknare + 1.   
      END.   
   END.
   {musarrow.i}      
   RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
   RUN openbdynspec_UI IN brwproc[5]. 
   RUN title_UI IN brwproc[5].
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
  DISPLAY RAD_DEPA CMB_LEV FILL-IN-ENR FILL-IN-BEN FILL-IN-FACK FILL-IN-FACKID1 
          FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL RAD_SOK FILL-IN-SALDO 
          CMB_LEV-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_DEPA BRW_HLEV FBTN_SKAPA FBTN_BYT btn_over FBTN_VISA FBTN_SKRIV 
         btn_back FBTN_BEST BTN_AVB BTN_LEV CMB_LEV BTN_SPEC FILL-IN-ENR 
         FILL-IN-BEN FILL-IN-FACK FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 
         FILL-IN-ANTAL BTN_UP BTN_MIN RAD_SOK FBTN_HBESTPU FBTN_IEXC 
         FILL-IN-SALDO CMB_LEV-2 RECT-2 BRW_DEPA RECT-4 BRW_MTRL IMAGE-3 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrvalexc_UI WINDOW-1 
PROCEDURE enrvalexc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN kollev_UI IN mtrlbapph (INPUT vald_lev,OUTPUT vlevnamn). 
   FOR EACH mttemp:        
      RUN kollev_UI IN mtrlbapph (INPUT mttemp.LEVKOD,OUTPUT lnamn).       
      IF lnamn = "" THEN DO:
         ASSIGN
          vlev = vald_lev
          mttemp.LEVKOD = vald_lev          
          mttemp.LEVNAMN = vlevnamn.
      END.
      ELSE DO: 
         ASSIGN
         vlev = mttemp.LEVKOD.
         mttemp.LEVNAMN = lnamn.
      END.

      FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR = mttemp.ENR AND mtrldeptemp.DEPNR = vald_depa AND 
      mtrldeptemp.IBDATUM = ? AND mtrldeptemp.LAGER = TRUE 
      NO-LOCK NO-ERROR.
      IF AVAILABLE mtrldeptemp THEN DO:                
         antalvar = INTEGER(mttemp.ANTAL).         
         ASSIGN enrval = FALSE.
         RUN over3_UI.
      END.  
      ELSE DO:  
         FIND FIRST mtrltemp WHERE mtrltemp.ENR = mttemp.ENR AND mtrltemp.LEVKOD = vlev
         NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:
            antalvar = INTEGER(mttemp.ANTAL).         
            enrval = TRUE.
            RUN over_UI.
         END.  
         ELSE DO:
            EMPTY TEMP-TABLE emtrltemp NO-ERROR.          
            RUN hmtskap_UI IN mtrlbapph (INPUT mttemp.ENR,INPUT vlev,OUTPUT TABLE emtrltemp). 
            FIND FIRST emtrltemp NO-LOCK NO-ERROR.
            IF AVAILABLE emtrltemp THEN DO:
               ASSIGN
               antalvar = INTEGER(mttemp.ANTAL).    
               enrval = TRUE.
               RUN over4_UI.        
             END.
             ELSE DO:      
                CREATE felmex.       
                ASSIGN 
                felmex.ENR = mttemp.ENR
                felmex.ANTAL = mttemp.ANTAL
                felmex.LEVKOD = mttemp.LEVKOD
                felmex.LEVNAMN = mttemp.LEVNAMN                 
                felmex.BENAMNING = mttemp.BENAMNING.                          
             END.
          END.
      END.      
   END.
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
  mtrldeptemp.IBDATUM = ? AND mtrldeptemp.LAGER = TRUE 
  NO-LOCK NO-ERROR.
  IF AVAILABLE mtrldeptemp THEN DO:     
     
     /*antalvar = 1*/
     IF FILL-IN-ANTAL > 1 THEN ASSIGN  antalvar = FILL-IN-ANTAL.
     ELSE ASSIGN antalvar = 1.
     ASSIGN enrval = FALSE.
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
            MESSAGE "Det finns ingen materiel med E-nummer:" + FILL-IN-ENR2 + "hos leverantör:" + STRING(levtemp.LEVNAMN) VIEW-AS ALERT-BOX
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hbestpunkt_UI WINDOW-1 
PROCEDURE hbestpunkt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/            
   IF CMB_LEV-2 = "Alla" THEN DO:
      OPEN QUERY utq FOR EACH mtrldeptemp WHERE mtrldeptemp.DEPNR = vald_depa AND
      mtrldeptemp.LAGER = TRUE AND mtrldeptemp.IBDATUM = ? USE-INDEX ENR NO-LOCK.
   END.   
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV-2  USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.
      OPEN QUERY utq FOR EACH mtrldeptemp WHERE mtrldeptemp.DEPNR = vald_depa AND
      mtrldeptemp.LAGER = TRUE AND mtrldeptemp.IBDATUM = ? AND mtrldeptemp.LEVKOD = levtemp.LEVKOD USE-INDEX ENR NO-LOCK.
   END.      
   GET FIRST utq NO-LOCK.
   DO WHILE AVAILABLE(mtrldeptemp): 
      IF mtrldeptemp.SALDO < mtrldeptemp.BESTPUNKT THEN DO:                      
         ASSIGN antalvar = 1.
         ASSIGN enrval = FALSE.
         kollbt = FALSE.
         RUN kollbest_UI IN mtrlbapph (INPUT mtrldeptemp.ENR,INPUT vald_depa ,INPUT mtrldeptemp.LEVKOD,OUTPUT kollbt). 
         IF kollbt = FALSE THEN DO:         
            RUN over3_UI.          
         END.
      END.   
     GET NEXT utq NO-LOCK.
   END.                    
   CLOSE QUERY utq.  
   RUN openbdynspec_UI IN brwproc[5]. 
   RUN title_UI IN brwproc[5].

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoes_UI WINDOW-1 
PROCEDURE infoes_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_HLEV" THEN valbrw = 1.  
   IF brwname = "BRW_MTRL" THEN valbrw = 2. 
   IF brwname = "BRW_DEPA" THEN valbrw = 3. 
   IF valbrw = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 2 THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = spec_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 3 THEN DO:
      status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrldeptemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrldeptemp.LEVKOD
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leverans_UI WINDOW-1 
PROCEDURE leverans_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   /*IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
      FOR EACH spec_mtrl:
         IF spec_mtrl.LEVKOD = "11" THEN DO:
            spec_mtrl.LEVKOD = "1".
         END.
      END.
   END.*/
   IF Guru.Konstanter:globforetag NE "GKAL" THEN DO:         
      RUN hlev_UI IN mtrlbapph (OUTPUT hleverantorkod).                                     
      /*FIND FIRST HUVUDLEV WHERE HUVUDLEV.DEP-NR = 999 NO-LOCK NO-ERROR. 
      FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = HUVUDLEV.LEVKOD
      NO-LOCK NO-ERROR. */
      FIND FIRST levtemp WHERE levtemp.LEVKOD = hleverantorkod NO-LOCK NO-ERROR. 
      FIND FIRST spec_mtrl WHERE (spec_mtrl.LEVKOD NE hleverantorkod AND spec_mtrl.LEVKOD NE "0")
      NO-LOCK NO-ERROR. 
      IF AVAILABLE spec_mtrl THEN DO:      
         MESSAGE "OBS! Det finns materiel  som ej kommer att beställas från " +
         levtemp.LEVNAMN + ". Ni kan byta leverantör genom att markera artikel och klicka på knappen byt leverantör. Vill Ni fortsätta?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Byte av leverantör." UPDATE svar.         
         IF NOT svar THEN DO: 
            musz = musz.
            RETURN NO-APPLY.   
         END.                    
      END.
   END.
   {muswait.i}
   {AVBGOM.I}
   {AMERICANEUROPEAN.I}
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      bytlev = FALSE.     
      FOR EACH spec_mtrl  NO-LOCK:
         IF spec_mtrl.LEVKOD NE vald_kundlev THEN bytlev = TRUE. 
      END.                     
     
      IF bytlev = TRUE THEN RUN LEVBYTE2V.W (INPUT ?).                
   END.
   RUN DEPTRPV.W. 
   {EUROPEANAMERICAN.I}  
   {AVBFRAM.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok_UI WINDOW-1 
PROCEDURE ok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   RUN ok_UI IN mtrlbapph (INPUT vald_depa,INPUT nytt_bestnr,INPUT input_bestnr,INPUT Guru.Konstanter:globanv,
                           INPUT TABLE spec_mtrl, INPUT valaonr, INPUT valdelnr).                                        
   IF input_bestnr = 0 THEN DO:
      FIND FIRST spec_mtrl NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:    
         CREATE best_nr_tab.
         ASSIGN 
         best_nr_tab.bestnr = nytt_bestnr
         best_nr_tab.bestdatum = TODAY
         best_nr_tab.bestallare = Guru.Konstanter:globanv.                    
         IF valaonr NE "" THEN DO:
            ASSIGN
            best_nr_tab.AONR = valaonr
            best_nr_tab.DELNR = valdelnr.
         END.
      END.   
   END.
   ELSE DO:
      FIND FIRST best_nr_tab WHERE best_nr_tab.bestnr = nytt_bestnr
      EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN
      best_nr_tab.bestdatum = TODAY.    
   END.   
   input_bestnr = nytt_bestnr.   
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "SOLE" 
     /*OR Guru.Konstanter:globforetag = "LULE"*/  THEN DO:
      MESSAGE "Vill du uppdatera priser för lagerförd materiel?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Uppdatera Priser?" UPDATE svar.         
      IF svar THEN DO:           
         EMPTY TEMP-TABLE emtrldeptemp NO-ERROR.
         FOR EACH spec_mtrl:            
            OPEN QUERY uq FOR EACH mtrldeptemp WHERE mtrldeptemp.DEPNR = vald_depa AND
            mtrldeptemp.ENR = spec_mtrl.ENR AND mtrldeptemp.IBDATUM = ? AND 
            mtrldeptemp.LAGER = TRUE USE-INDEX DEPNR NO-LOCK.
            DO TRANSACTION:
               GET FIRST uq EXCLUSIVE-LOCK.
               IF AVAILABLE mtrldeptemp THEN DO: 
                  IF Guru.Konstanter:globforetag = "cLULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                     ASSIGN
                     mtrldeptemp.NPRIS = spec_mtrl.NPRIS
                     mtrldeptemp.BPRIS = spec_mtrl.NPRIS.
                  END.
                  ELSE mtrldeptemp.NPRIS = spec_mtrl.NPRIS.
               END.
               CREATE emtrldeptemp.
               BUFFER-COPY mtrldeptemp TO emtrldeptemp.
            END.
            REPEAT:
               DO TRANSACTION:
                  GET NEXT uq EXCLUSIVE-LOCK.
                  IF AVAILABLE mtrldeptemp THEN DO: 
                     IF Guru.Konstanter:globforetag = "cLULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                        ASSIGN
                        mtrldeptemp.NPRIS = spec_mtrl.NPRIS
                        mtrldeptemp.BPRIS = spec_mtrl.NPRIS.
                     END.
                     ELSE mtrldeptemp.NPRIS = spec_mtrl.NPRIS.
                  END.
                  ELSE LEAVE.
                  CREATE emtrldeptemp.
                  BUFFER-COPY mtrldeptemp TO emtrldeptemp.
               END.
            END.            

         END.                 
         RUN uppmtrldeppris_UI IN mtrlbapph (INPUT TABLE emtrldeptemp).                                        
         EMPTY TEMP-TABLE emtrldeptemp NO-ERROR.
      END.   
      ELSE DO:       
         musz = musz.
      END.     
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
   FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrldeptemp.LEVKOD 
   NO-LOCK NO-ERROR.
   IF levtemp.BORTTAG = FALSE THEN DO:     
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrldeptemp.ENR AND 
      spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD NO-LOCK NO-ERROR. 
      IF AVAILABLE spec_mtrl THEN DO:
         ASSIGN
         val_rec = ROWID(spec_mtrl).         
      END.      
      ELSE DO:         
         CREATE spec_mtrl. 
         ASSIGN
         spec_mtrl.ENR = mtrldeptemp.ENR
         spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
         spec_mtrl.ENHET = mtrldeptemp.ENHET
         spec_mtrl.BERKVANT = mtrldeptemp.BESTKVANT
         spec_mtrl.NPRIS = mtrldeptemp.NPRIS 
         spec_mtrl.BPRIS = mtrldeptemp.BPRIS
         spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD         
         spec_mtrl.FACKID = mtrldeptemp.FACKID
         val_rec = ROWID(spec_mtrl).
         IF antalvar > 1 THEN spec_mtrl.BERKVANT = antalvar.
         /*kalmar depåändring abc nu*/          
         
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "KRAF" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
            EMPTY TEMP-TABLE emtrltemp NO-ERROR.          
            RUN hmtskap_UI IN mtrlbapph (INPUT mtrldeptemp.ENR,INPUT mtrldeptemp.LEVKOD,OUTPUT TABLE emtrltemp). 
            FIND FIRST emtrltemp NO-LOCK NO-ERROR.
            IF AVAILABLE emtrltemp THEN DO:
               IF emtrltemp.NPRIS > 0 THEN DO:
                  ASSIGN 
                  spec_mtrl.NPRIS = emtrltemp.NPRIS.
                  spec_mtrl.BPRIS = emtrltemp.NPRIS.
               END.          
            END.            
         END.
      END. 
   END.
   ELSE DO:      
      MESSAGE "Leverantör " + levtemp.LEVNAM + " är borttagen. Det går ej att göra beställning av E-nr " + mtrldeptemp.ENR + " med benämning " + mtrldeptemp.BENAMNING
      VIEW-AS ALERT-BOX TITLE "Meddelande".   
   END.   
   antal_raknare = antal_raknare + 1.               
   /*IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ikmtrltemp THEN DO:                        
         CREATE ikmtrltemp.
         BUFFER-COPY mtrltemp TO ikmtrltemp.                  
      END.
   END.*/

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
   IF AVAILABLE spec_mtrl THEN DO:
      ASSIGN
      val_rec = ROWID(spec_mtrl).         
   END.      
   ELSE DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.ENR = emtrltemp.ENR
      spec_mtrl.BENAMNING = emtrltemp.BENAMNING
      spec_mtrl.ENHET = emtrltemp.ENHET
      spec_mtrl.BERKVANT = antalvar
      spec_mtrl.NPRIS = emtrltemp.NPRIS 
      spec_mtrl.BPRIS = emtrltemp.BPRIS      
      val_rec = ROWID(spec_mtrl).
      /*Niklas personlig spec_mtrl*/
      IF emtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
      ELSE spec_mtrl.LEVKOD = emtrltemp.LEVKOD. 
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
   IF AVAILABLE spec_mtrl THEN DO:
      ASSIGN
      val_rec = ROWID(spec_mtrl).         
   END.      
   ELSE DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.ENR = mtrltemp.ENR
      spec_mtrl.BENAMNING = mtrltemp.BENAMNING
      spec_mtrl.ENHET = mtrltemp.ENHET
      spec_mtrl.BERKVANT = antalvar
      spec_mtrl.NPRIS = mtrltemp.NPRIS 
      spec_mtrl.BPRIS = mtrltemp.BPRIS      
      val_rec = ROWID(spec_mtrl).
      /*Niklas personlig spec_mtrl*/
      IF mtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
      ELSE spec_mtrl.LEVKOD = mtrltemp.LEVKOD. 
   END. 
   antal_raknare = antal_raknare + 1.                     
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ikmtrltemp THEN DO:                        
         CREATE ikmtrltemp.
         BUFFER-COPY mtrltemp TO ikmtrltemp.                  
      END.
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
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 2).    
   IF brwh:NAME = "BRW_DEPA" THEN RUN rowdispmtrl_UI (INPUT 3).    
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
   IF vad = 2  THEN DO:
      IF AVAILABLE spec_mtrl THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = spec_mtrl.LEVKOD AND ikmtrltemp.ENR = spec_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].            
         END.
      END.

   END.
   IF vad = 3  THEN DO:
      IF AVAILABLE mtrldeptemp THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrldeptemp.LEVKOD AND ikmtrltemp.ENR = mtrldeptemp.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            mtrldeptemp.ENR:BGCOLOR IN BROWSE BRW_DEPA = Guru.Konstanter:varforetypval[28].            
         END.
      END.

   END.
   /*IF vad = 2 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
         IF mtrltemp.KUND = TRUE THEN mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEVSM = 11.
      END.
   END.
   IF vad = 3 THEN DO: 
      IF AVAILABLE list_mtrl THEN DO:         
         FIND FIRST berstolptemp WHERE berstolptemp.LEVKOD = list_mtrl.LEVKOD AND berstolptemp.ENR = list_mtrl.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE berstolptemp THEN DO:            
            list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRL = 14.
            list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = 14.
         END.
      END.
   END.   
   IF vad = 4 THEN DO: 
      IF AVAILABLE list_mtrl THEN DO:         
         FIND FIRST berstolptemp WHERE berstolptemp.LEVKOD = list_mtrl.LEVKOD AND berstolptemp.ENR = list_mtrl.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE berstolptemp THEN DO:            
            list_mtrl.BENAMNING:BGCOLOR IN BROWSE BRW_MTRLSM = 14.
            list_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRLSM = 14.
         END.
      END.
   END.   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

