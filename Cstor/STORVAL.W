&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win




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
DEBUGGER:SET-BREAK().
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
DEFINE NEW SHARED TEMP-TABLE vavdelningtemp    
   FIELD AVDELNINGNR AS INTEGER  
   FIELD AVDELNINGNAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR ASCENDING.
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE treminuter AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE utomr AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE alla2 AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE allaspann AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE forerec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE uttyp AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valfore AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE omr AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE franar AS INTEGER NO-UNDO.
DEFINE VARIABLE brwomr2h AS HANDLE NO-UNDO.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 

{STORTEMP.I}
{AVDELNINGTEMP.I}
 
DEFINE NEW SHARED TEMP-TABLE visaupp NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   FIELD VISEX AS INTEGER
   FIELD VISAP AS INTEGER
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.
DEFINE NEW SHARED TEMP-TABLE omr_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY AVDELNINGNR DISTRIKTID.

DEFINE NEW SHARED TEMP-TABLE avd_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR.

DEFINE TEMP-TABLE spann_temp    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER.
   
DEFINE NEW SHARED TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID.

DEFINE TEMP-TABLE spar_temp
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID.   


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-KALLE
&Scoped-define BROWSE-NAME BRW_AVD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avdelningtemp vavdelningtemp stordistemp ~
spann_temp visaupp

/* Definitions for BROWSE BRW_AVD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_AVD avdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVD 
&Scoped-define QUERY-STRING-BRW_AVD FOR EACH avdelningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVD OPEN QUERY BRW_AVD FOR EACH avdelningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVD avdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVD avdelningtemp


/* Definitions for BROWSE BRW_AVD-2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVD-2 vavdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVD-2 
&Scoped-define QUERY-STRING-BRW_AVD-2 FOR EACH vavdelningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVD-2 OPEN QUERY BRW_AVD-2 FOR EACH vavdelningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVD-2 vavdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVD-2 vavdelningtemp


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR stordistemp.AVDELNINGNR ~
stordistemp.VIDISTRIKT stordistemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR stordistemp.AVDELNINGNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OMR stordistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OMR stordistemp
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH stordistemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH stordistemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OMR stordistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR stordistemp


/* Definitions for BROWSE BRW_OMR2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR2 avdelningtemp.AVDELNINGNR ~
avdelningtemp.AVDELNINGNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR2 avdelningtemp.AVDELNINGNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OMR2 avdelningtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OMR2 avdelningtemp
&Scoped-define QUERY-STRING-BRW_OMR2 FOR EACH avdelningtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OMR2 OPEN QUERY BRW_OMR2 FOR EACH avdelningtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OMR2 avdelningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR2 avdelningtemp


/* Definitions for BROWSE BRW_SPAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SPAN spann_temp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SPAN 
&Scoped-define QUERY-STRING-BRW_SPAN FOR EACH spann_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SPAN OPEN QUERY BRW_SPAN FOR EACH spann_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SPAN spann_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SPAN spann_temp


/* Definitions for BROWSE BRW_UPP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UPP visaupp.UT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPP 
&Scoped-define QUERY-STRING-BRW_UPP FOR EACH visaupp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPP OPEN QUERY BRW_UPP FOR EACH visaupp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPP visaupp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPP visaupp


/* Definitions for FRAME FRAME-KALLE                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-KALLE ~
    ~{&OPEN-QUERY-BRW_UPP}

/* Definitions for FRAME FRAME_FORETAG                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_PERIOD CMB_ARTAL BTN_NVE BTN_NVE-2 ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 TOG_PLAN TOG_DRIFT ~
BTN_HAOF FILL-IN-FAVOlista TOG_FORE TOG_FORE2 BTN_AOF FBTN_VISA CMB_FOR ~
FBTN_EX TOG_ALLA TOG_ALLA2 BRW_OMR2 BRW_OMR BRW_SPAN BRW_UPP BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT TOG_PLAN TOG_DRIFT FILL-IN-FAVOlista TOG_FORE TOG_FORE2 ~
CMB_FOR TOG_ALLA TOG_ALLA2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AOF 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_HAOF 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

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

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_FOR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 28.75 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAVOlista AS CHARACTER FORMAT "X(256)":U 
     LABEL "Från/Till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Sätt ett namn på din lista! Sätter du ett befintlig skrivs den över!" NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "9999/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "9999/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.42 NO-UNDO.

DEFINE VARIABLE TOG_ALLA AS LOGICAL INITIAL no 
     LABEL "Alla distrikt" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.63 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_ALLA2 AS LOGICAL INITIAL no 
     LABEL "Alla spänningsnivåer" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.63 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_DRIFT AS LOGICAL INITIAL no 
     LABEL "Driftstörningar" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_FORE AS LOGICAL INITIAL no 
     LABEL "Alla företag" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.63 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_FORE2 AS LOGICAL INITIAL no 
     LABEL "Välja företag" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.63 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_PLAN AS LOGICAL INITIAL no 
     LABEL "Planerade avbrott" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 NO-UNDO.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVD FOR 
      avdelningtemp SCROLLING.

DEFINE QUERY BRW_AVD-2 FOR 
      vavdelningtemp SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      stordistemp SCROLLING.

DEFINE QUERY BRW_OMR2 FOR 
      avdelningtemp SCROLLING.

DEFINE QUERY BRW_SPAN FOR 
      spann_temp SCROLLING.

DEFINE QUERY BRW_UPP FOR 
      visaupp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVD C-Win _STRUCTURED
  QUERY BRW_AVD NO-LOCK DISPLAY
      avdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Företag" FORMAT "x(256)":U
            WIDTH 18.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 21 BY 20
         TITLE "Företag" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_AVD-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVD-2 C-Win _STRUCTURED
  QUERY BRW_AVD-2 NO-LOCK DISPLAY
      vavdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Företag" FORMAT "x(256)":U
            WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 21 BY 20
         TITLE "Valda Företag" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      stordistemp.AVDELNINGNR COLUMN-LABEL "Företag" FORMAT ">>>":U
      stordistemp.VIDISTRIKT COLUMN-LABEL "Distrikt" FORMAT "X(8)":U
      stordistemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U WIDTH 20
  ENABLE
      stordistemp.AVDELNINGNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 39.25 BY 21.

DEFINE BROWSE BRW_OMR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR2 C-Win _STRUCTURED
  QUERY BRW_OMR2 NO-LOCK DISPLAY
      avdelningtemp.AVDELNINGNR COLUMN-LABEL "Företag" FORMAT ">>>>>>9":U
      avdelningtemp.AVDELNINGNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U
            WIDTH 28
  ENABLE
      avdelningtemp.AVDELNINGNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 39.25 BY 21.

DEFINE BROWSE BRW_SPAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SPAN C-Win _STRUCTURED
  QUERY BRW_SPAN DISPLAY
      spann_temp.NAMN COLUMN-LABEL "Spänningsnivå" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 23.25 BY 21.

DEFINE BROWSE BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPP C-Win _STRUCTURED
  QUERY BRW_UPP NO-LOCK DISPLAY
      visaupp.UT COLUMN-LABEL "Lista" FORMAT "X(256)":U WIDTH 32.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 36.63 BY 21.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-KALLE
     RAD_PERIOD AT ROW 1.08 COL 1.5 NO-LABEL
     CMB_ARTAL AT ROW 1.17 COL 26.63
     BTN_NVE AT ROW 2.21 COL 47
     BTN_NVE-2 AT ROW 2.21 COL 67.75
     FILL-IN-STARTDAT AT ROW 2.5 COL 31.63 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 2.5 COL 53.63 COLON-ALIGNED
     BTN_FVE AT ROW 3.04 COL 47
     BTN_FVE-2 AT ROW 3.04 COL 67.75
     TOG_PLAN AT ROW 3.92 COL 1.5
     TOG_DRIFT AT ROW 3.92 COL 24
     BTN_HAOF AT ROW 4.17 COL 74.5 WIDGET-ID 4
     FILL-IN-FAVOlista AT ROW 4.83 COL 94 COLON-ALIGNED WIDGET-ID 8
     TOG_FORE AT ROW 5.04 COL 1.5
     TOG_FORE2 AT ROW 5.04 COL 24
     BTN_AOF AT ROW 5.58 COL 74.5 WIDGET-ID 6
     FBTN_VISA AT ROW 5.79 COL 111.25
     CMB_FOR AT ROW 6 COL 8.63 COLON-ALIGNED
     FBTN_EX AT ROW 6.88 COL 111.25
     TOG_ALLA AT ROW 7.08 COL 1.5
     TOG_ALLA2 AT ROW 7.08 COL 24
     BRW_OMR2 AT ROW 8.04 COL 1.5
     BRW_OMR AT ROW 8.04 COL 1.5
     BRW_SPAN AT ROW 8.04 COL 50.38
     BRW_UPP AT ROW 8.04 COL 74.38
     BTN_AVB AT ROW 26.79 COL 111.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME_FORETAG
     BRW_AVD AT ROW 1.17 COL 1.5 WIDGET-ID 200
     BRW_AVD-2 AT ROW 1.17 COL 27.5 WIDGET-ID 300
     BTN_OVER AT ROW 8.63 COL 23 WIDGET-ID 4
     BTN_BACK AT ROW 10.58 COL 23 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 8.04
         SIZE 48.5 BY 21 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: avdelningtemp T "?" NO-UNDO TEMP-DB avdelningtemp
      TABLE: stordistemp T "?" NO-UNDO TEMP-DB stordistemp
      TABLE: vavdelningtemp T "?" NO-UNDO temp-db vavdelningtemp
      TABLE: visaupp T "?" NO-UNDO temp-db visaupp
      TABLE: ? T "?" NO-UNDO temp-db spann_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
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
ASSIGN FRAME FRAME_FORETAG:FRAME = FRAME FRAME-KALLE:HANDLE.

/* SETTINGS FOR FRAME FRAME-KALLE
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME_FORETAG:MOVE-AFTER-TAB-ITEM (BRW_OMR:HANDLE IN FRAME FRAME-KALLE)
       XXTABVALXX = FRAME FRAME_FORETAG:MOVE-BEFORE-TAB-ITEM (BRW_SPAN:HANDLE IN FRAME FRAME-KALLE)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BRW_OMR2 TOG_ALLA2 FRAME-KALLE */
/* BROWSE-TAB BRW_OMR BRW_OMR2 FRAME-KALLE */
/* BROWSE-TAB BRW_SPAN FRAME_FORETAG FRAME-KALLE */
/* BROWSE-TAB BRW_UPP BRW_SPAN FRAME-KALLE */
ASSIGN 
       BRW_OMR2:HIDDEN  IN FRAME FRAME-KALLE                = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME FRAME-KALLE           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME FRAME-KALLE           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME FRAME-KALLE           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME FRAME-KALLE           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME FRAME-KALLE
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME FRAME-KALLE           = TRUE.

ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME FRAME-KALLE           = TRUE.

/* SETTINGS FOR FRAME FRAME_FORETAG
                                                                        */
/* BROWSE-TAB BRW_AVD 1 FRAME_FORETAG */
/* BROWSE-TAB BRW_AVD-2 BRW_AVD FRAME_FORETAG */
ASSIGN 
       FRAME FRAME_FORETAG:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVD
/* Query rebuild information for BROWSE BRW_AVD
     _TblList          = "Temp-Tables.avdelningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avdelningtemp.AVDELNINGNAMN
"avdelningtemp.AVDELNINGNAMN" "Företag" "x(256)" "character" ? ? ? ? ? ? no ? no no "18.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVD-2
/* Query rebuild information for BROWSE BRW_AVD-2
     _TblList          = "Temp-Tables.vavdelningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.vavdelningtemp.AVDELNINGNAMN
"vavdelningtemp.AVDELNINGNAMN" "Företag" "x(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVD-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.stordistemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.stordistemp.AVDELNINGNR
"stordistemp.AVDELNINGNR" "Företag" ">>>" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.stordistemp.VIDISTRIKT
"stordistemp.VIDISTRIKT" "Distrikt" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.stordistemp.NAMN
"stordistemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR2
/* Query rebuild information for BROWSE BRW_OMR2
     _TblList          = "Temp-Tables.avdelningtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.avdelningtemp.AVDELNINGNR
"avdelningtemp.AVDELNINGNR" "Företag" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.avdelningtemp.AVDELNINGNAMN
"avdelningtemp.AVDELNINGNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMR2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SPAN
/* Query rebuild information for BROWSE BRW_SPAN
     _TblList          = "Temp-Tables.spann_temp"
     _FldNameList[1]   > Temp-Tables.spann_temp.NAMN
"spann_temp.NAMN" "Spänningsnivå" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SPAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPP
/* Query rebuild information for BROWSE BRW_UPP
     _TblList          = "Temp-Tables.visaupp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.visaupp.UT
"visaupp.UT" "Lista" "X(256)" "character" ? ? ? ? ? ? no ? no no "32.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_UPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME_FORETAG
/* Query rebuild information for FRAME FRAME_FORETAG
     _Query            is NOT OPENED
*/  /* FRAME FRAME_FORETAG */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define BROWSE-NAME BRW_UPP
&Scoped-define SELF-NAME BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_UPP IN FRAME FRAME-KALLE
DO:
   status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   visvalvar = 1.
   RUN btnok_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP C-Win
ON VALUE-CHANGED OF BRW_UPP IN FRAME FRAME-KALLE
DO:
   status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE visaupp THEN DO:
      IF visaupp.ORDNING = 22 THEN DO:
         ASSIGN
         TOG_ALLA = TRUE
         TOG_ALLA2 = TRUE.
         DISPLAY TOG_ALLA TOG_ALLA2 WITH FRAME {&FRAME-NAME}.
         APPLY "VALUE-CHANGED" TO TOG_ALLA IN FRAME {&FRAME-NAME}.
         APPLY "VALUE-CHANGED" TO TOG_ALLA2 IN FRAME {&FRAME-NAME}.
         DISABLE TOG_ALLA TOG_ALLA2 WITH FRAME {&FRAME-NAME}.
      END.
      ELSE ENABLE TOG_ALLA TOG_ALLA2 WITH FRAME {&FRAME-NAME}.
   END.
   /*
   IF visaupp.ORDNING = 20 THEN FRAME FRAME_FORETAG:HIDDEN = FALSE.
   ELSE FRAME FRAME_FORETAG:HIDDEN = TRUE.
    */
   IF visaupp.VISEX = 0  THEN FBTN_EX:HIDDEN = TRUE.
   ELSE FBTN_EX:HIDDEN = FALSE.
   IF visaupp.VISAP = 0  THEN FBTN_VISA:HIDDEN = TRUE.
   ELSE FBTN_VISA:HIDDEN = FALSE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AOF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AOF C-Win
ON CHOOSE OF BTN_AOF IN FRAME FRAME-KALLE /* spara favorit */
DO:
   RUN sparaaofavoriter_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME FRAME-KALLE /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE C-Win
ON CHOOSE OF BTN_FVE IN FRAME FRAME-KALLE /* - */
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
ON CHOOSE OF BTN_FVE-2 IN FRAME FRAME-KALLE /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAOF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAOF C-Win
ON CHOOSE OF BTN_HAOF IN FRAME FRAME-KALLE /* HÄMTA favorit */
DO:
   RUN hmtfavoriter_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE C-Win
ON CHOOSE OF BTN_NVE IN FRAME FRAME-KALLE /* + */
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
ON CHOOSE OF BTN_NVE-2 IN FRAME FRAME-KALLE /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME FRAME-KALLE /* Årtal */
DO:                            
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME FRAME-KALLE /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.   
   FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_FOR NO-LOCK NO-ERROR.    
   ASSIGN 
   franar = CMB_ARTAL
   forvar = avdelningtemp.AVDELNINGNR.
   RUN changequery_UI.
   IF RAD_PERIOD = 2 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = DATE(01,01,CMB_ARTAL)
      FILL-IN-STOPPDAT = DATE(12,31,CMB_ARTAL).
      DISPLAY FILL-IN-STARTDAT FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FOR C-Win
ON VALUE-CHANGED OF CMB_FOR IN FRAME FRAME-KALLE /* Företag */
DO:
   CMB_FOR = INPUT CMB_FOR.   
   FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_FOR NO-LOCK NO-ERROR. 
   ASSIGN
   forerec = RECID(avdelningtemp)
   forvar = avdelningtemp.AVDELNINGNR.
   RUN changequery_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX C-Win
ON CHOOSE OF FBTN_EX IN FRAME FRAME-KALLE /* Visa I EXCEL */
DO:
   visvalvar = 2.
   RUN btnok_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-KALLE /* Visa */
DO: 
   visvalvar = 1.
   RUN btnok_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FAVOlista
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FAVOlista C-Win
ON LEAVE OF FILL-IN-FAVOlista IN FRAME FRAME-KALLE /* Från/Till */
DO:
   FILL-IN-FAVOlista = INPUT FILL-IN-FAVOlista.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME FRAME-KALLE /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME FRAME-KALLE /* Från */
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
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME FRAME-KALLE /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME FRAME-KALLE /* Till */
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


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD C-Win
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME FRAME-KALLE
DO:
   RAD_PERIOD = INPUT RAD_PERIOD.
   RUN hide_UI.
   IF RAD_PERIOD = 2 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = DATE(01,01,CMB_ARTAL)
      FILL-IN-STOPPDAT = DATE(12,31,CMB_ARTAL).
      DISPLAY FILL-IN-STARTDAT FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA C-Win
ON VALUE-CHANGED OF TOG_ALLA IN FRAME FRAME-KALLE /* Alla distrikt */
DO:
  TOG_ALLA = INPUT TOG_ALLA.
  IF TOG_ALLA = TRUE THEN DO:
     BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
  END.
  ELSE DO:
     BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.    
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA2 C-Win
ON VALUE-CHANGED OF TOG_ALLA2 IN FRAME FRAME-KALLE /* Alla spänningsnivåer */
DO:
  TOG_ALLA2 = INPUT TOG_ALLA2.
  IF TOG_ALLA2 = TRUE THEN DO:
     BRW_SPAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
  END.
  ELSE DO:
     BRW_SPAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.    
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DRIFT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DRIFT C-Win
ON VALUE-CHANGED OF TOG_DRIFT IN FRAME FRAME-KALLE /* Driftstörningar */
DO:
   TOG_DRIFT = INPUT TOG_DRIFT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FORE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FORE C-Win
ON VALUE-CHANGED OF TOG_FORE IN FRAME FRAME-KALLE /* Alla företag */
DO:
  TOG_FORE = INPUT TOG_FORE.
  IF TOG_FORE = TRUE THEN DO:
     ASSIGN
     CMB_FOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     TOG_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
  END.
  ELSE DO:
     ASSIGN
     CMB_FOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
     TOG_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     APPLY "VALUE-CHANGED" TO TOG_ALLA IN FRAME {&FRAME-NAME}.
  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FORE2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FORE2 C-Win
ON VALUE-CHANGED OF TOG_FORE2 IN FRAME FRAME-KALLE /* Välja företag */
DO:
  TOG_FORE2 = INPUT TOG_FORE2.
  IF TOG_FORE2 = TRUE THEN DO:
     ASSIGN
     CMB_FOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     TOG_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     TOG_FORE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     BRW_OMR2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE     
     BTN_AOF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
     BTN_HAOF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
     FILL-IN-FAVOlista:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
        
  END.
  ELSE DO:
     ASSIGN
/*      CMB_FOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE  */
/*      TOG_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE */
     TOG_FORE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
     BRW_OMR2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     APPLY "VALUE-CHANGED" TO TOG_FORE IN FRAME {&FRAME-NAME}.
     ASSIGN

     BTN_AOF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     BTN_HAOF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
     FILL-IN-FAVOlista:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

  END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_PLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PLAN C-Win
ON VALUE-CHANGED OF TOG_PLAN IN FRAME FRAME-KALLE /* Planerade avbrott */
DO:
   TOG_PLAN = INPUT TOG_PLAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVD
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
   musz = TRUE.
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
   RUN rapp_UI.
   CMB_FOR:DELIMITER = "$".
   OPEN QUERY aq FOR EACH avdelningtemp USE-INDEX AVDELNINGNR NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(avdelningtemp):
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF avdelningtemp.AVDELNINGNR >= 64 THEN DO:
            
            status-ok = CMB_FOR:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
         END.
      END.
      ELSE DO:
             
         status-ok = CMB_FOR:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
      END.     
      GET NEXT aq NO-LOCK.
   END.
   CLOSE QUERY aq.
   RUN openbdynspec_UI IN brwproc[5].
   RUN openbdynspec_UI IN brwproc[6].
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR >= 64 
      USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   END.
   ELSE DO:   
      FIND FIRST avdelningtemp USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   END.     
   ASSIGN
   forerec = RECID(avdelningtemp)   
   forvar = avdelningtemp.AVDELNINGNR
   CMB_FOR:SCREEN-VALUE = avdelningtemp.AVDELNINGNAMN.
   CMB_FOR = avdelningtemp.AVDELNINGNAMN.   
   EMPTY TEMP-TABLE spann_temp NO-ERROR.    
   {&WINDOW-NAME}:TITLE = "Urval för avbrott/störnings rapporter".
   OPEN QUERY sysq FOR EACH inlastabtemp WHERE inlastabtemp.INKOD = "D" NO-LOCK.
   GET FIRST sysq NO-LOCK.
   DO WHILE AVAILABLE(inlastabtemp):
      IF inlastabtemp.INKODPOSCH NE "" THEN DO:
         IF inlastabtemp.INKODTYP = "1" THEN DO:
            FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.INKODID = 
            inlastabtemp.INKODID NO-LOCK NO-ERROR.
            IF AVAILABLE spanningsnivtemp THEN DO:
               CREATE spann_temp.
               ASSIGN
               spann_temp.NAMN = spanningsnivtemp.NAMN
               spann_temp.SPANID = spanningsnivtemp.SPANID.                              
            END.
         END.         
      END.
      GET NEXT sysq NO-LOCK.         
   END.   
   
   CLOSE QUERY sysq.
   OPEN QUERY BRW_SPAN FOR EACH spann_temp NO-LOCK.
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0").
   FIND FIRST stordistemp USE-INDEX ARTAL NO-LOCK NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:      
      ASSIGN  
      uppar = stordistemp.ARTAL.
      FIND LAST stordistemp 
      USE-INDEX ARTAL NO-LOCK NO-ERROR.
      ASSIGN
      slutar = stordistemp.ARTAL.
   END.   
   ELSE DO:
      ASSIGN
      uppar = YEAR(TODAY)
      slutar = YEAR(TODAY).
   END.  
   IF uppar = ? THEN   uppar = YEAR(TODAY).    
   IF slutar = ? THEN  slutar = YEAR(TODAY).  
   /*LADDAR ÅR I CMB_ARTAL*/
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")  
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(slutar,"9999")   
   CMB_ARTAL = INPUT CMB_ARTAL
   RAD_PERIOD = 1
   TOG_PLAN = TRUE
   TOG_DRIFT = TRUE
   TOG_FORE = FALSE
   TOG_FORE2 = FALSE
   franar = slutar.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = DATE(01,01,slutar)
      FILL-IN-STOPPDAT = DATE(12,31,slutar).
   END.
   RUN changequery_UI.
   RUN openbdynspec_UI IN brwproc[2].
   RUN openbdynspec_UI IN brwproc[4].
   RUN enable_UI.   
   {FRMSIZE.I}  
   FRAME FRAME_FORETAG:HIDDEN = TRUE.
   ASSIGN
   BTN_AOF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_HAOF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-FAVOlista:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

   BRW_OMR2:HIDDEN = TRUE.
/*    RAD_PERIOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */
   RUN hide_UI.
   APPLY "VALUE-CHANGED" TO BROWSE BRW_UPP.
   {musarrow.i}
   {WIN_M_SLUT.I}

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
   ASSIGN
   stordistemp.AVDELNINGNR:READ-ONLY IN BROWSE BRW_OMR = TRUE
   avdelningtemp.AVDELNINGNR:READ-ONLY IN BROWSE BRW_OMR2 = TRUE.
   
   brwomr2h = BRW_OMR2:HANDLE IN FRAME {&FRAME-NAME}.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_OMR2:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_SPAN:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_UPP:HANDLE IN FRAME {&FRAME-NAME}).  
   
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_AVD:HANDLE IN FRAME FRAME_FORETAG).   
   RUN DYNBRW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_AVD-2:HANDLE IN FRAME FRAME_FORETAG).    
   RUN DYNARROW.P PERSISTENT SET brwproc[7] (INPUT BRW_AVD:HANDLE, INPUT BRW_AVD-2:HANDLE,
       INPUT BTN_OVER:HANDLE, ?,INPUT ?, INPUT BTN_BACK:HANDLE).     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   IF RAD_PERIOD = 2 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = INPUT FRAME {&FRAME-NAME} FILL-IN-STARTDAT
      FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
      IF YEAR(FILL-IN-STARTDAT) NE CMB_ARTAL THEN DO:
         MESSAGE "Perioden måste vara inom det årtal Ni valt"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-STARTDAT IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      IF YEAR(FILL-IN-STOPPDAT) NE CMB_ARTAL THEN DO:
         MESSAGE "Perioden måste vara inom det årtal Ni valt"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-STOPPDAT IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   
   IF TOG_PLAN = FALSE AND TOG_DRIFT = FALSE THEN DO:
      MESSAGE "Ni måste antingen välja att visa driftstörningar, planerade avbrott eller både och."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   IF TOG_FORE2 = TRUE THEN DO:
      IF BRW_OMR2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO:
         MESSAGE "Inget företag är markerat." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO BRW_OMR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY. 
      END.
   END.
   ELSE DO:
      IF TOG_FORE = FALSE THEN DO:  
         IF TOG_ALLA = FALSE THEN DO:
            IF BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO:
               MESSAGE "Inget distrikt är markerat." VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO BRW_OMR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY. 
            END.
         END.      
      END.   
   END.
   IF TOG_ALLA2 = FALSE THEN DO:
      IF BRW_SPAN:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO:
         MESSAGE "Ingen spänningsnivå är markerad." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO BRW_SPAN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY. 
      END.
   END.
   EMPTY TEMP-TABLE omr_temp NO-ERROR. 
   EMPTY TEMP-TABLE avd_temp NO-ERROR. 
   EMPTY TEMP-TABLE spann_temp2 NO-ERROR.    
   RUN val_UI.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      bdatum = DATE(01,01,CMB_ARTAL)
      avdatum = DATE(12,31,CMB_ARTAL)
      period = 1.
   END.
   ELSE DO:
      ASSIGN
      bdatum = FILL-IN-STARTDAT
      avdatum = FILL-IN-STOPPDAT
      period = 2.
   END.    
   skrivut = FALSE. 
   IF visaupp.TYP = "STORFEL.W" OR visaupp.TYP = "STORAND.W" OR visaupp.TYP = "STORFELSAIDI.W" THEN DO:
      IF uttyp = 2 THEN DO:
         MESSAGE "Obs! Ni har valt att visa både planerade avbrott och driftstörningar. Vid denna lista berörs ej planerade avbrott även om Ni så valt."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE IF uttyp = 3 THEN DO:
         MESSAGE "Obs! Ni har valt att visa planerade avbrott. Vid denna lista berörs ej planerade avbrott även om Ni så valt. Utfallet kommer att visa driftstörningar."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.  
   END.
   IF visaupp.TYP = "RELAFEL.W" THEN DO:
      IF uttyp = 2 THEN DO:
         MESSAGE "Obs! Ni har valt att visa både planerade avbrott och driftstörningar. Vid denna lista berörs ej planerade avbrott även om Ni så valt."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE IF UTTYP = 3 THEN DO:
         MESSAGE "Obs! Ni har valt att visa planerade avbrott. Vid denna lista berörs ej planerade avbrott även om Ni så valt. Utfallet kommer att visa driftstörningar."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      IF allaspann = FALSE THEN DO:
         MESSAGE "Obs! Listan kommer att visa alla spänningsnivåer."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      EMPTY TEMP-TABLE spann_temp2 NO-ERROR.       
      FOR EACH spar_temp:
         CREATE spann_temp2.
         ASSIGN
         spann_temp2.SPANID = spar_temp.SPANID
         spann_temp2.NAMN = spar_temp.NAMN.
      END.
   END.
   {AVBGOM.I}
   
   vallista = visaupp.UPPFOLJVAL.
   IF visaupp.TYP = "TOTSAIFI.W" OR visaupp.TYP = "STORAND.W" OR visaupp.TYP = "STORFEL.W" OR visaupp.TYP = "STORFELSAIDI.W" THEN DO:
      IF visaupp.ORDNING = 16 OR visaupp.ORDNING = 23 OR visaupp.ORDNING = 24 THEN treminuter = TRUE.
      ELSE treminuter = FALSE.
   END.
   
   vallista = visaupp.UPPFOLJVAL.
   
   
   IF visaupp.TYP = "STORNINGEX.P (INPUT 1)"  THEN  RUN STORNINGEX.P (INPUT 1).
   ELSE IF visaupp.TYP = "STORNINGEX.P (INPUT 2)"  THEN  RUN STORNINGEX.P (INPUT 2).
   ELSE IF visaupp.TYP = "STORFTGUT.W" AND visvalvar = 2 THEN RUN STORFTGUT.W (INPUT TRUE, INPUT forvar, INPUT franar,INPUT TABLE spann_temp).
   ELSE IF visaupp.TYP = "STORFTGUT.W" THEN RUN STORFTGUT.W (INPUT FALSE, INPUT forvar, INPUT franar, INPUT TABLE spann_temp).
   ELSE RUN VALUE(visaupp.TYP).
   {AVBFRAM.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changequery_UI C-Win 
PROCEDURE changequery_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   kommandoquery = "stordistemp.AVDELNINGNR = " + STRING(forvar) + 
      " AND stordistemp.ARTAL = " + STRING(franar).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).
   RUN openbdynspec_UI IN brwproc[1].   
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
  DISPLAY RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT FILL-IN-STOPPDAT TOG_PLAN 
          TOG_DRIFT FILL-IN-FAVOlista TOG_FORE TOG_FORE2 CMB_FOR TOG_ALLA 
          TOG_ALLA2 
      WITH FRAME FRAME-KALLE IN WINDOW C-Win.
  ENABLE RAD_PERIOD CMB_ARTAL BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 TOG_PLAN TOG_DRIFT BTN_HAOF 
         FILL-IN-FAVOlista TOG_FORE TOG_FORE2 BTN_AOF FBTN_VISA CMB_FOR FBTN_EX 
         TOG_ALLA TOG_ALLA2 BRW_OMR2 BRW_OMR BRW_SPAN BRW_UPP BTN_AVB 
      WITH FRAME FRAME-KALLE IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KALLE}
  ENABLE BRW_AVD BRW_AVD-2 BTN_OVER BTN_BACK 
      WITH FRAME FRAME_FORETAG IN WINDOW C-Win.
  VIEW FRAME FRAME_FORETAG IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME_FORETAG}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide_UI C-Win 
PROCEDURE hide_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_PERIOD = 1 THEN DO:            
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.     
   END.
   ELSE DO:                                             
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
/*       CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtfavoriter_UI C-Win 
PROCEDURE hmtfavoriter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ttbuffh AS HANDLE NO-UNDO.
   tthandle = TEMP-TABLE vavdelningtemp:HANDLE.
   IF FILL-IN-FAVOlista = "" THEN DO:
      MESSAGE "Favoritlistan måste ha ett namn!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.  
      
   /*
   ttbuffh = tthandle:DEFAULT-BUFFER-HANDLE.
   RUN ttcopy_UI IN huvprogh (INPUT TABLE-HANDLE tthandle).
   ttbuffh:EMPTY-TEMP-TABLE() NO-ERROR.
   */
   FIND FIRST sparaladdatemp NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
   ASSIGN
   sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
   sparaladdatemp.BENAMNING = "STOR" + FILL-IN-FAVOlista /*Benämnings sufix, i detta fall ELPAO$STOR*/
   sparaladdatemp.TABVAL = "AVDELNING" /*Tabellnamn*/
   /*
   sparaladdatemp.FALTVALAO = "AVDELNINGNR " /*CHARACTER field*/
   */
   sparaladdatemp.FALTVALDEL = "AVDELNINGNR " /*Integer field*/
   sparaladdatemp.FALTVALDATE = "".   /*DATE field*/
   EMPTY TEMP-TABLE vavdelningtemp NO-ERROR. 
   RUN laddabrw_UI IN brwproc[6]
      (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp). 
/*MARKERA*/   
   RUN openbdyn_UI IN brwproc[6] (INPUT ""). 
   DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE rid AS ROWID NO-UNDO.
   
   FOR EACH vavdelningtemp:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = vavdelningtemp.AVDELNINGNR NO-LOCK NO-ERROR.
      IF AVAILABLE avdelningtemp THEN DO:
         rid = ROWID(avdelningtemp).
         RUN multiselectin_UI IN brwproc[2] (ROWID(avdelningtemp)).
         /* 
         dynok = brwomr2h:REPOSITION-TO-ROWID(rid) .  /* Positionerar queryn */
         IF dynok = TRUE THEN brwomr2h:SELECT-FOCUSED-ROW() NO-ERROR.    /* Markerar postitionerad post */
         */
      END.   
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rapp_UI C-Win 
PROCEDURE rapp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "OVERSIKT.W"
   visaupp.ORDNING = 1
   visaupp.UT = "Översikt över fördel. av planerade avbrott och drifts."
   visaupp.VISEX = 0
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "KUNDKONS.W"
   visaupp.ORDNING = 2
   visaupp.UT = "Kundkonsekvenser"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN      
   visaupp.TYP = "STORFEL.W"
   visaupp.ORDNING = 3
   visaupp.UT = "Driftstörningar fördelade på felorsak"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   
   CREATE visaupp.
   ASSIGN      
   visaupp.TYP = "STORFELSAIDI.W"
   visaupp.ORDNING = 3
   visaupp.UT = "Driftstörningar fördelade på felorsak med SAIDI"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   visaupp.UPPFOLJVAL = 12.
   
   
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "STORVARA.W"
   visaupp.ORDNING = 4
   visaupp.UT = "Driftstörningars varaktighetsfördelning"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   /*slå ihop spänning*/
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "STORVARAK.W"
   visaupp.ORDNING = 5
   visaupp.UT = "Antal drabbade kunder per tidsintervall"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   
   ASSIGN
   visaupp.TYP = "RELAFEL.W"
   visaupp.ORDNING = 6
   visaupp.UT = "Driftstörning fördelade på utlösande relä/felyttring"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   /*
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 7
   visaupp.UPPFOLJVAL = 1
   visaupp.UT = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder'"  
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 8
   visaupp.UPPFOLJVAL = 2
   visaupp.UT = "SAIDI - Medelavbrottstid för 'alla kunder'"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 9
   visaupp.UPPFOLJVAL = 3
   visaupp.UT = "CAIDI - Medelavbrottstid för 'berörda kunder'"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 10
   visaupp.UPPFOLJVAL = 4
   visaupp.UT = "ASAI - tillgängligheten i nätet"   
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 12
   visaupp.UPPFOLJVAL = 5
   visaupp.UT = "SAIFI - Medelavbrottsfrekvensen för 'alla kunder' > 3 min"  
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 13
   visaupp.UPPFOLJVAL = 6
   visaupp.UT = "SAIDI - Medelavbrottstid för 'alla kunder' > 3 min"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 14
   visaupp.UPPFOLJVAL = 7
   visaupp.UT = "CAIDI - Medelavbrottstid för 'berörda kunder' > 3 min"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "SAIFI.W"
   visaupp.ORDNING = 15
   visaupp.UPPFOLJVAL = 8
   visaupp.UT = "ASAI - tillgängligheten i nätet > 3 min"   
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   */
   /*
   CREATE visaupp.
   ASSIGN
   /*
   visaupp.TYP = "SAIFI.W"
   */
   visaupp.TYP = "TOTSAIFI.W"
   visaupp.ORDNING = 11
   visaupp.UT = "SAIFI - SAIDI - CAIDI - ASAI"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   */
  
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "TOTSAIFI.W"
   visaupp.ORDNING = 16
   visaupp.UT = "SAIFI - SAIDI - CAIDI - ASAI > 3 min"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "ATERARB.W"
   visaupp.ORDNING = 17
   visaupp.UPPFOLJVAL = 1
   visaupp.UT = "Lista återstående arbete"
   visaupp.VISEX = 0
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "ATERARB.W"
   visaupp.ORDNING = 18
   visaupp.UPPFOLJVAL = 1
   visaupp.UT = "Detaljerad lista återstående arbete"
   visaupp.VISEX = 0
   visaupp.VISAP = 1.
   /*
   CREATE visaupp.
   ASSIGN
   visaupp.TYP = "STORNINGEX.P (INPUT 1)"
   visaupp.ORDNING = 19
   visaupp.UT = "Störningar i EXCEL"
   visaupp.VISEX = 1
   visaupp.VISAP = 0.
   /*
   status-ok = SEL_UPP:ADD-LAST("Störningar i EXCEL (snabb)").
   */
   */
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:         
      CREATE visaupp.
      ASSIGN
      visaupp.TYP = "STORNINGEX.P (INPUT 2)"
      visaupp.ORDNING = 20
      visaupp.UT = "Störningar i EXCEL (snabb)"
      visaupp.VISEX = 1
      visaupp.VISAP = 0.
      CREATE visaupp.
      ASSIGN
      visaupp.TYP = "SUMLED.W"
      visaupp.ORDNING = 21
      visaupp.UT = "Summering av ledningsdata"
      visaupp.VISEX = 1
      visaupp.VISAP = 1.
      CREATE visaupp. /*my*/
      ASSIGN
      visaupp.TYP = "STORFTGUT.W" /*forvar = foretagsid, franar = årtal*/
      visaupp.ORDNING = 22
      visaupp.UT = "Kunddata / Företag"
      visaupp.VISEX = 1
      visaupp.VISAP = 0. 
     
      
   END.
   CREATE visaupp.
   ASSIGN      
   visaupp.TYP = "STORAND.W"
   visaupp.ORDNING = 3
   visaupp.UT = "Driftstörningar fördelade på anläggning"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   CREATE visaupp.
   ASSIGN      
   visaupp.TYP = "STORAND.W"
   visaupp.ORDNING = 23
   visaupp.UT = "Driftstörningar fördelade på anläggning > 3 min"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
  CREATE visaupp. 
    ASSIGN      
   visaupp.TYP = "STORFEL.W"
   visaupp.ORDNING = 24
   visaupp.UT = "Driftstörningar fördelade på felorsak > 3 min"
   visaupp.VISEX = 1
   visaupp.VISAP = 1.
   
   FOR EACH spann_temp2:
      CREATE spar_temp.
      ASSIGN
      spar_temp.SPANID = spann_temp2.SPANID
      spar_temp.NAMN = spann_temp2.NAMN.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparaaofavoriter_UI C-Win 
PROCEDURE sparaaofavoriter_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*HMTA MARKERADE*/
   IF FILL-IN-FAVOlista = "" THEN DO:
      MESSAGE "Favoritlistan måste ha ett namn!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.  
   EMPTY TEMP-TABLE vavdelningtemp NO-ERROR. 
   antal_valda = BRW_OMR2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_OMR2:FETCH-SELECTED-ROW(antal_raknare).
      CREATE vavdelningtemp.
      BUFFER-COPY avdelningtemp TO vavdelningtemp.
      antal_raknare = antal_raknare + 1.      
   END.   
   tthandle = TEMP-TABLE vavdelningtemp:HANDLE.
   RUN openbdyn_UI IN brwproc[6] (INPUT ""). 
   FIND FIRST sparaladdatemp NO-ERROR.
   IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
   ASSIGN
   sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
   sparaladdatemp.BENAMNING = "STOR" + FILL-IN-FAVOlista /*Benämnings sufix, i detta fall ELPAO$STOR*/
   sparaladdatemp.TABVAL = "AVDELNING" /*Tabellnamn*/
   /*
   sparaladdatemp.FALTVALAO = "AVDELNINGNR " /*CHARACTER field*/
   */
   sparaladdatemp.FALTVALDEL = "AVDELNINGNR " /*Integer field*/
   sparaladdatemp.FALTVALDATE = "".   /*DATE field*/
   
   RUN sparabrw_UI IN brwproc[6] 
      (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).
    tthandle = TEMP-TABLE vavdelningtemp:HANDLE.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI C-Win 
PROCEDURE val_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_PLAN = FALSE AND TOG_DRIFT = TRUE THEN uttyp = 1.
   ELSE IF TOG_PLAN = TRUE AND TOG_DRIFT = FALSE THEN uttyp = 2.
   ELSE uttyp = 3.
   IF TOG_FORE2 = TRUE THEN DO:
      ASSIGN
      valfore = TRUE
      antal_valda = BRW_OMR2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_OMR2:FETCH-SELECTED-ROW(antal_raknare).
         CREATE avd_temp.
         ASSIGN
         avd_temp.AVDELNINGNR = avdelningtemp.AVDELNINGNR         
         avd_temp.NAMN = avdelningtemp.AVDELNINGNAMN.         
         antal_raknare = antal_raknare + 1.
      END.
   END.
   ELSE DO:   
      valfore = FALSE.
      IF TOG_FORE = FALSE THEN DO:
         alla = FALSE.
         IF TOG_ALLA = FALSE THEN DO:
            ASSIGN
            alla2 = FALSE
            antal_valda = BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
            antal_raknare = 1.
            DO WHILE antal_raknare LE antal_valda:
               status-ok = BRW_OMR:FETCH-SELECTED-ROW(antal_raknare).
               CREATE omr_temp.
               ASSIGN
               omr_temp.AVDELNINGNR = stordistemp.AVDELNINGNR
               omr_temp.DISTRIKTID = stordistemp.DISTRIKTID
               omr_temp.NAMN = stordistemp.NAMN.         
               antal_raknare = antal_raknare + 1.
            END.   
         END.
         ELSE DO:      
            alla2 = TRUE.       
         END.   
      END.   
      ELSE DO:
         alla = TRUE.
      END.
   END.
   IF TOG_ALLA2 = FALSE THEN DO:      
      ASSIGN
      allaspann = FALSE
      antal_valda = BRW_SPAN:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_SPAN:FETCH-SELECTED-ROW(antal_raknare).
         CREATE spann_temp2.
         ASSIGN
         spann_temp2.SPANID = spann_temp.SPANID
         spann_temp2.NAMN = spann_temp.NAMN.
         /*slå ihop spänning*/
         IF spann_temp.SPANID = 21 THEN DO:
            FIND FIRST utgspanningsnivtemp WHERE utgspanningsnivtemp.SPANID = 20 NO-LOCK NO-ERROR.
            IF AVAILABLE utgspanningsnivtemp THEN DO:
               CREATE spann_temp2.
               ASSIGN
               spann_temp2.SPANID = utgspanningsnivtemp.SPANID
               spann_temp2.NAMN = utgspanningsnivtemp.NAMN.
            END.
         END.
         antal_raknare = antal_raknare + 1.
      END.
   END.  
   ELSE DO:
      FOR EACH spann_temp:
         CREATE spann_temp2.
         ASSIGN
         spann_temp2.SPANID = spann_temp.SPANID
         spann_temp2.NAMN = spann_temp.NAMN.
         /*slå ihop spänning*/
         IF spann_temp.SPANID = 21 THEN DO:
            FIND FIRST utgspanningsnivtemp WHERE utgspanningsnivtemp.SPANID = 20 NO-LOCK NO-ERROR.
            IF AVAILABLE utgspanningsnivtemp THEN DO:
               CREATE spann_temp2.
               ASSIGN
               spann_temp2.SPANID = utgspanningsnivtemp.SPANID
               spann_temp2.NAMN = utgspanningsnivtemp.NAMN.
            END.
         END.
      END.
      allaspann = TRUE.      
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

