&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
{DIRDEF.I}

{OMRTEMPW.I}
{JURPERST.I}
DEFINE SHARED VARIABLE utomr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE omr AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE aonummer AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE delnummer AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valmanad AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE valar AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE valaonrrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE valdelnrlog AS LOGICAL NO-UNDO. 
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE aoomvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.

DEFINE SHARED TEMP-TABLE aoval NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD AONRREC AS RECID
   INDEX AONR IS PRIMARY AONR DELNR. 

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdaao

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR valdaao.OMRADE valdaao.AONR ~
valdaao.DELNR valdaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR 
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH valdaao NO-LOCK ~
    BY valdaao.OMRADE ~
       BY valdaao.AONR ~
        BY valdaao.DELNR INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH valdaao NO-LOCK ~
    BY valdaao.OMRADE ~
       BY valdaao.AONR ~
        BY valdaao.DELNR INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_AONR valdaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR valdaao


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 RAD_PERIOD CMB_ARTAL BTN_NVE ~
BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 BTN_NVE-3 ~
BTN_NVE-4 FILL-IN-STARTD FILL-IN-SLUTD TOG_AVS BTN_FVE-3 BTN_FVE-4 ~
FBTN_VISA CMB_OMR RAD_FAST SEL_OMR FILL-IN-AR FILL-IN_AONR FILL-IN_ORT ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT FILL-IN-RTEXT FILL-IN-MELL FILL-IN-STARTD FILL-IN-OCH ~
FILL-IN-SLUTD TOG_AVS FILL-IN-VALJ FILL-IN-VISA CMB_OMR RAD_FAST SEL_OMR ~
FILL-IN-AR FILL-IN_AONR FILL-IN_ORT 

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

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 20.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Vid urval av arbetsordrar" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33.5 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VISA AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 36.13 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.92 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.5 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE SEL_OMR AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 28.5 BY 10.67 NO-UNDO.

DEFINE VARIABLE TOG_AVS AS LOGICAL INITIAL no 
     LABEL "Avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 12.13 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      valdaao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR C-Win _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      valdaao.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      valdaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(256)":U
            WIDTH 36
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 60.5 BY 10.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_PERIOD AT ROW 1.5 COL 1.5 NO-LABEL
     CMB_ARTAL AT ROW 1.63 COL 35.63
     BTN_NVE AT ROW 2.79 COL 43
     BTN_NVE-2 AT ROW 2.79 COL 62
     FILL-IN-STARTDAT AT ROW 3.21 COL 31 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 3.21 COL 50 COLON-ALIGNED
     BTN_FVE AT ROW 3.67 COL 43
     BTN_FVE-2 AT ROW 3.67 COL 62
     FILL-IN-RTEXT AT ROW 5.25 COL 24.13 COLON-ALIGNED NO-LABEL
     BTN_NVE-3 AT ROW 6.42 COL 55.63
     BTN_NVE-4 AT ROW 6.42 COL 73.5
     FILL-IN-MELL AT ROW 6.71 COL 35.75 COLON-ALIGNED NO-LABEL
     FILL-IN-STARTD AT ROW 6.71 COL 43.5 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 6.71 COL 56.88 COLON-ALIGNED NO-LABEL
     FILL-IN-SLUTD AT ROW 6.71 COL 61.5 COLON-ALIGNED NO-LABEL
     TOG_AVS AT ROW 6.75 COL 25.63
     BTN_FVE-3 AT ROW 7.29 COL 55.63
     BTN_FVE-4 AT ROW 7.29 COL 73.5
     FBTN_VISA AT ROW 8 COL 95.75
     FILL-IN-VALJ AT ROW 8.5 COL 1.5 NO-LABEL
     FILL-IN-VISA AT ROW 8.5 COL 60.38 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 9.79 COL 60.25 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 9.88 COL 25.75 NO-LABEL
     SEL_OMR AT ROW 11.21 COL 1.5 NO-LABEL
     BRW_AONR AT ROW 11.25 COL 33.38
     FILL-IN-AR AT ROW 22.13 COL 21 COLON-ALIGNED NO-LABEL
     FILL-IN_AONR AT ROW 22.21 COL 52.25 COLON-ALIGNED
     FILL-IN_ORT AT ROW 22.21 COL 73.5 COLON-ALIGNED
     BTN_AVB AT ROW 22.25 COL 95.5
     "Sök på:" VIEW-AS TEXT
          SIZE 7.88 BY .67 AT ROW 22.42 COL 37.75
     RECT-22 AT ROW 22.04 COL 33.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.25 BY 22.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 22.42
         WIDTH              = 109.25
         MAX-HEIGHT         = 27.92
         MAX-WIDTH          = 109.25
         VIRTUAL-HEIGHT     = 27.92
         VIRTUAL-WIDTH      = 109.25
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
                                                                        */
/* BROWSE-TAB BRW_AONR SEL_OMR DEFAULT-FRAME */
/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_AONR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-MELL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-RTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-SLUTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-STARTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-VISA IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DEFAULT-FRAME
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME DEFAULT-FRAME
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.valdaao"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.valdaao.OMRADE|yes,Temp-Tables.valdaao.AONR|yes,Temp-Tables.valdaao.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.valdaao.OMRADE
"valdaao.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdaao.AONR
"valdaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdaao.DELNR
"valdaao.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valdaao.ORT
"valdaao.ORT" "Ort/Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "36" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
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


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR C-Win
ON ENTRY OF BRW_AONR IN FRAME DEFAULT-FRAME
DO:
  musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR C-Win
ON VALUE-CHANGED OF BRW_AONR IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   aonummer = valdaao.AONR
   delnummer = valdaao.DELNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.
   IF VALID-HANDLE(aoomvalapph) THEN DELETE PROCEDURE aoomvalapph.
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


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 C-Win
ON CHOOSE OF BTN_FVE-3 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTD = INPUT FILL-IN-STARTD.   
   FILL-IN-STARTD = FILL-IN-STARTD - 1.      
   DISPLAY FILL-IN-STARTD WITH FRAME {&FRAME-NAME}. 
   RUN allaao_UI.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 C-Win
ON CHOOSE OF BTN_FVE-4 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD.   
   FILL-IN-SLUTD = FILL-IN-SLUTD - 1.      
   DISPLAY FILL-IN-SLUTD WITH FRAME {&FRAME-NAME}.
   RUN allaao_UI.
   {musarrow.i}     
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


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 C-Win
ON CHOOSE OF BTN_NVE-3 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STARTD = INPUT FILL-IN-STARTD.   
   FILL-IN-STARTD = FILL-IN-STARTD + 1.        
   DISPLAY FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
   RUN allaao_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 C-Win
ON CHOOSE OF BTN_NVE-4 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD.   
   FILL-IN-SLUTD = FILL-IN-SLUTD + 1.        
   DISPLAY FILL-IN-SLUTD WITH FRAME {&FRAME-NAME}.
   RUN allaao_UI.
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


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR C-Win
ON VALUE-CHANGED OF CMB_OMR IN FRAME DEFAULT-FRAME
DO:
   CMB_OMR = INPUT CMB_OMR.  
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR. 
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN omrvar = omrtemp.OMRADE.
   END.
  
   RUN allaao_UI.    
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO: 
   EMPTY TEMP-TABLE aoval NO-ERROR.    
   RUN valda_UI. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   skrivut = FALSE.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      bdatum = DATE(01,01,FILL-IN-AR)
      avdatum = DATE(01,01,FILL-IN-AR).
   END.
   ELSE DO:
      ASSIGN
      bdatum = FILL-IN-STARTDAT
      avdatum = FILL-IN-STOPPDAT.
      
   END.        
   APPLY "GO" TO FBTN_VISA IN FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   {AVBGOM.I} 
   RUN AOOMUTFU.W.
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


&Scoped-define SELF-NAME FILL-IN-SLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTD C-Win
ON LEAVE OF FILL-IN-SLUTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD.
   IF FILL-IN-SLUTD < FILL-IN-STARTD THEN DO:
      FILL-IN-STARTD = FILL-IN-SLUTD.
      DISPLAY FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
   END.
   RUN allaao_UI.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTD IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTD.
   RUN AlmanBtn.w.
   FILL-IN-SLUTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-SLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTD C-Win
ON LEAVE OF FILL-IN-STARTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-STARTD = INPUT FILL-IN-STARTD.
   IF FILL-IN-SLUTD < FILL-IN-STARTD THEN DO:
      FILL-IN-SLUTD = FILL-IN-STARTD.
      DISPLAY FILL-IN-SLUTD WITH FRAME {&FRAME-NAME}.
   END.
   RUN allaao_UI.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTD IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
   FILL-IN-STARTD = INPUT FILL-IN-STARTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTD.
   RUN AlmanBtn.w.
   FILL-IN-STARTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME FILL-IN_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR C-Win
ON ANY-KEY OF FILL-IN_AONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR C-Win
ON LEAVE OF FILL-IN_AONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   FILL-IN_AONR = INPUT FILL-IN_AONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   FILL-IN_AONR = INPUT FILL-IN_AONR.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(valdaao).
   IF FILL-IN_AONR = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   {muswait.i}
   aosok = '*' + FILL-IN_AONR + '*'.
   IF TOG_AVS = TRUE THEN DO:
      FIND valdaao WHERE RECID(valdaao) = aonrrec NO-LOCK NO-ERROR.
      IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:          
         FIND NEXT valdaao WHERE valdaao.AONR MATCHES aosok AND
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:         
         FIND NEXT valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
         valdaao.AONR MATCHES aosok AND
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE valdaao THEN DO:
         IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:          
            FIND FIRST valdaao WHERE valdaao.AONR MATCHES aosok AND
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         ELSE DO:         
            FIND FIRST valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
            valdaao.AONR MATCHES aosok AND
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE valdaao THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
   END.
   ELSE DO:
      FIND valdaao WHERE RECID(valdaao) = aonrrec NO-LOCK NO-ERROR.
      IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:          
         FIND NEXT valdaao WHERE valdaao.AONR MATCHES aosok AND
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:         
         FIND NEXT valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
         valdaao.AONR MATCHES aosok AND
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE valdaao THEN DO:
         IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:          
            FIND FIRST valdaao WHERE valdaao.AONR MATCHES aosok AND
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM = 01/01/1991 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         ELSE DO:         
            FIND FIRST valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
            valdaao.AONR MATCHES aosok AND
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM = 01/01/1991 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE valdaao THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
   END.
   IF AVAILABLE valdaao THEN DO:      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT C-Win
ON ANY-KEY OF FILL-IN_ORT IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORT IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT C-Win
ON LEAVE OF FILL-IN_ORT IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   FILL-IN_ORT = INPUT FILL-IN_ORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORT IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   FILL-IN_ORT = INPUT FILL-IN_ORT.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   aonrrec = RECID(valdaao).
   IF FILL-IN_ORT = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ORT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.     
   {muswait.i} 
   ortssok = '*' + FILL-IN_ORT + '*'.
   IF TOG_AVS = TRUE THEN DO:
      FIND valdaao WHERE RECID(valdaao) = aonrrec NO-LOCK NO-ERROR.
      IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:       
         FIND NEXT valdaao WHERE valdaao.ORT MATCHES ortssok AND 
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND NEXT valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
         valdaao.ORT MATCHES ortssok AND
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE valdaao THEN DO:
         IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:       
            FIND FIRST valdaao WHERE valdaao.ORT MATCHES ortssok AND 
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         ELSE DO:      
            FIND FIRST valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
            valdaao.ORT MATCHES ortssok AND
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE valdaao THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_ORT IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
   END.
   ELSE DO:
      FIND valdaao WHERE RECID(valdaao) = aonrrec NO-LOCK NO-ERROR.
      IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:       
         FIND NEXT valdaao WHERE valdaao.ORT MATCHES ortssok AND 
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/91 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:      
         FIND NEXT valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
         valdaao.ORT MATCHES ortssok AND
         valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE valdaao THEN DO:
         IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:       
            FIND FIRST valdaao WHERE valdaao.ORT MATCHES ortssok AND 
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM = 01/01/91 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         ELSE DO:      
            FIND FIRST valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND 
            valdaao.ORT MATCHES ortssok AND
            valdaao.FASTAAONR = RAD_FAST AND 
            valdaao.AONRAVDATUM = 01/01/1991 
            USE-INDEX OMRADE NO-LOCK NO-ERROR.
         END.
         IF NOT AVAILABLE valdaao THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_ORT IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
   END.
   IF AVAILABLE valdaao THEN DO:      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST C-Win
ON VALUE-CHANGED OF RAD_FAST IN FRAME DEFAULT-FRAME
DO:
   RAD_FAST = INPUT RAD_FAST. 
   RUN goma_UI.
   RUN allaao_UI.
   {musarrow.i}
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


&Scoped-define SELF-NAME SEL_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_OMR C-Win
ON VALUE-CHANGED OF SEL_OMR IN FRAME DEFAULT-FRAME
DO:
   SEL_OMR = INPUT SEL_OMR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AVS C-Win
ON VALUE-CHANGED OF TOG_AVS IN FRAME DEFAULT-FRAME /* Avslutade */
DO:
   TOG_AVS = INPUT TOG_AVS.
   RUN goma_UI.
   RUN allaao_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   
   RUN jurp_UI IN aonrapph (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
     
    
   /*visa bara eget bolags områden*/  
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   FOR EACH judavdtemp,         
   EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      status-ok = SEL_OMR:ADD-LAST(omrtemp.NAMN).                        
   END.
    
   ASSIGN
   FILL-IN-RTEXT = "Vid urval av " + LC(Guru.Konstanter:gaol)
   FILL-IN-VISA = "Visa " + LC(Guru.Konstanter:gaok) + " för:"
   FILL-IN-VALJ = "Välj " + LC(Guru.Konstanter:gomrl)
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok
   valdaao.OMRADE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gomrk
   valdaao.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok.   
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN valdaao.AONR
   &Scoped-define BROWSE-NAME BRW_AONR
   {AOFORMAT1.I}
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   ASSIGN
   RAD_FAST = FALSE
   TOG_AVS = FALSE
   FILL-IN-MELL = "mellan" 
   FILL-IN-OCH = "och"    
   FILL-IN-SLUTD = TODAY
   FILL-IN-STARTD = DATE(01,01,YEAR(TODAY)).
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN      
      FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY))
      FILL-IN-STOPPDAT = TODAY.
   END.
   IF FILL-IN-STARTDAT = ? THEN FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY)).
   IF FILL-IN-STOPPDAT = ? THEN FILL-IN-STOPPDAT = TODAY.
   RUN enable_UI.       
   ASSIGN C-win:TITLE = "Lista " + SEL_UPP + ". Välj " + LC(Guru.Konstanter:gomrk) + " och " + LC(Guru.Konstanter:gaol) + ".".
   {FRMSIZE.I}
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}. 
   ASSIGN CMB_OMR:SCREEN-VALUE = CMB_OMR.
   CMB_OMR = INPUT CMB_OMR.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN
      omrvar = omrtemp.OMRADE.
   END.
   ELSE CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".   
   FILL-IN-AR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.       
   RUN goma_UI.
   RUN allaao_UI. 
   IF valaonrrec = ? THEN valaonrrec = valaonrrec.
   ELSE DO:
      FIND valdaao WHERE RECID(valdaao) = valaonrrec NO-LOCK NO-ERROR.
      IF AVAILABLE valdaao THEN DO:
         IF valdaao.AONRAVDATUM = 01/01/91 AND valdaao.FASTAAONR = RAD_FAST THEN DO:                             
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(valdaao)).
            RUN lastselectdyn_UI IN brwproc[1].               
         END.
      END.    
   END.     
   {musarrow.i}
   {ARTALBORT.I}
   {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaao_UI C-Win 
PROCEDURE allaao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   CMB_OMR = INPUT FRAME  {&FRAME-NAME} CMB_OMR.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}. 
   EMPTY TEMP-TABLE valdaao NO-ERROR.    
   RUN aohmt_UI IN aoomvalapph (INPUT TOG_AVS,INPUT CMB_OMR,INPUT RAD_FAST,
                             INPUT FILL-IN-STARTD,INPUT FILL-IN-SLUTD,
                             INPUT omrvar,INPUT-OUTPUT TABLE valdaao).
   IF TOG_AVS = FALSE  THEN DO: 
      IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH valdaao WHERE valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST valdaao WHERE valdaao.FASTAAONR = RAD_FAST AND
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM = 01/01/1991 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
   END.
   ELSE DO:
      IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO: 
         OPEN QUERY BRW_AONR
         FOR EACH valdaao WHERE valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST valdaao WHERE valdaao.FASTAAONR = RAD_FAST AND
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         OPEN QUERY BRW_AONR 
         FOR EACH valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
         USE-INDEX OMRADE NO-LOCK INDEXED-REPOSITION.
         FIND FIRST valdaao WHERE valdaao.OMRADE = omrtemp.OMRADE AND valdaao.FASTAAONR = RAD_FAST AND 
         valdaao.AONRAVDATUM >= FILL-IN-STARTD AND valdaao.AONRAVDATUM <= FILL-IN-SLUTD 
         USE-INDEX OMRADE NO-LOCK NO-ERROR.
      END.  
   END.
   IF AVAILABLE valdaao THEN DO:  
      BRW_AONR:HIDDEN = FALSE.
      ENABLE FBTN_VISA FILL-IN_AONR FILL-IN_ORT
      WITH FRAME {&FRAME-NAME}. 
   END.
   ELSE  DO:
      BRW_AONR:HIDDEN = TRUE.
      DISABLE FILL-IN_AONR FILL-IN_ORT
      WITH FRAME {&FRAME-NAME}.  
   END.       
          
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
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).  
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN AOOMVALAPP.P PERSISTENT SET aoomvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN AOOMVALAPP.P PERSISTENT SET aoomvalapph.
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
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
  DISPLAY RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT FILL-IN-STOPPDAT FILL-IN-RTEXT 
          FILL-IN-MELL FILL-IN-STARTD FILL-IN-OCH FILL-IN-SLUTD TOG_AVS 
          FILL-IN-VALJ FILL-IN-VISA CMB_OMR RAD_FAST SEL_OMR FILL-IN-AR 
          FILL-IN_AONR FILL-IN_ORT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-22 RAD_PERIOD CMB_ARTAL BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 BTN_NVE-3 BTN_NVE-4 FILL-IN-STARTD 
         FILL-IN-SLUTD TOG_AVS BTN_FVE-3 BTN_FVE-4 FBTN_VISA CMB_OMR RAD_FAST 
         SEL_OMR FILL-IN-AR FILL-IN_AONR FILL-IN_ORT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Param eters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF TOG_AVS = FALSE THEN DO:            
      ASSIGN
      FILL-IN-MELL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-OCH:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      FILL-IN-SLUTD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      FILL-IN-STARTD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:                                             
      ASSIGN
      FILL-IN-MELL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-OCH:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-SLUTD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-STARTD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE-3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE-4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE-3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE-4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
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
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE placera_UI C-Win 
PROCEDURE placera_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    IF aonrrec NE 0 THEN DO:
      FIND valdaao WHERE RECID(valdaao) = aonrrec NO-LOCK NO-ERROR.
      IF valdaao.FASTAAONR = RAD_FAST AND valdaao.AONRAVDATUM = 01/01/91 THEN DO:                        
         IF AVAILABLE valdaao THEN DO:      
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(valdaao)).
            RUN lastselectdyn_UI IN brwproc[1].      
         END.
      END.
      ELSE DO:
         APPLY "HOME" TO BRW_AONR IN FRAME {&FRAME-NAME}.
      END.      
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
   antal_valda = {&BROWSE-NAME}:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF SEL_OMR = ? OR SEL_OMR = "" THEN DO:
      MESSAGE "Välj ett " + LC(Guru.Konstanter:gomrl) + "! "VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN.
   END.   
   FIND FIRST omrtemp WHERE omrtemp.NAMN = SEL_OMR USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   utomr = omrtemp.OMRADE.   
   IF antal_valda = 0 THEN DO:            
      musz = musz.
      RETURN.                
   END.
   ELSE DO:   
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :
         status-ok = {&BROWSE-NAME}:FETCH-SELECTED-ROW(antal_raknare).      
         CREATE aoval.
         ASSIGN 
         aoval.AONR = valdaao.AONR
         aoval.DELNR = valdaao.DELNR      
         antal_raknare = antal_raknare + 1.
      END.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

