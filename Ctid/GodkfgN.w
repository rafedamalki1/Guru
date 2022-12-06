&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
&Scoped-define NEW
&Scoped-define SHARED SHARED
{TIDPERS.I}
{PHMT.I}
{GODTEMP.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
{OMRTEMPW.I}
DEFINE NEW SHARED VARIABLE korda AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE regmnrspar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE regarspar AS INTEGER FORMAT "99" NO-UNDO.
DEFINE VARIABLE sppkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE spman AS INTEGER NO-UNDO.
DEFINE VARIABLE spar AS INTEGER NO-UNDO.
DEFINE VARIABLE hjpkod AS CHARACTER NO-UNDO.
  /*FÖR TIDSEDEL*/
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
&Scoped-define NEW
&Scoped-define SHARED SHARED
DEFINE {&NEW} SHARED VARIABLE CMB_TAR AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     SIZE 12.5 BY 1
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_TMANAD AS CHARACTER FORMAT "X(9)":U INITIAL "januari" 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "januari","februari","mars","april","maj","juni","juli","augusti",
     "september","oktober","november","december","hela året" 
     SIZE 15 BY 1
     BGCOLOR 8 FGCOLOR 0  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TID

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES godmarkpers vgodmarkpers

/* Definitions for BROWSE BRW_TID                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TID godmarkpers.PERSONALKOD ~
godmarkpers.FORNAMN godmarkpers.EFTERNAMN godmarkpers.MANAD ~
godmarkpers.DATUM godmarkpers.TIDSGODK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TID godmarkpers.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TID godmarkpers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TID godmarkpers
&Scoped-define QUERY-STRING-BRW_TID FOR EACH godmarkpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TID OPEN QUERY BRW_TID FOR EACH godmarkpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TID godmarkpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TID godmarkpers


/* Definitions for BROWSE BRW_VTID                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VTID vgodmarkpers.PERSONALKOD ~
vgodmarkpers.FORNAMN vgodmarkpers.EFTERNAMN vgodmarkpers.MANAD ~
vgodmarkpers.DATUM vgodmarkpers.GODKAND 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VTID vgodmarkpers.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VTID vgodmarkpers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VTID vgodmarkpers
&Scoped-define QUERY-STRING-BRW_VTID FOR EACH vgodmarkpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VTID OPEN QUERY BRW_VTID FOR EACH vgodmarkpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VTID vgodmarkpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VTID vgodmarkpers


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_OVER BTN_BACK FBTN_VISA FBTN_VISAM ~
BTN_GODM BTN_AVB TOG_FARDIG 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS TOG_FARDIG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_GODM 
     LABEL "Godkänn tidsedel":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa tidsedel":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_VISAM 
     LABEL "Visa månadsamm.":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(13)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(14)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Godkänner tidsedel", 6,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 65.5 BY 1.17 NO-UNDO.

DEFINE VARIABLE TOG_FARDIG AS LOGICAL INITIAL yes 
     LABEL "Visa enbart färdigrapporterad ej godkänd tid i tidsedel" 
     VIEW-AS TOGGLE-BOX
     SIZE 58 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TID FOR 
      godmarkpers SCROLLING.

DEFINE QUERY BRW_VTID FOR 
      vgodmarkpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TID C-Win _STRUCTURED
  QUERY BRW_TID NO-LOCK DISPLAY
      godmarkpers.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "X(256)":U
            WIDTH 6
      godmarkpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 10
      godmarkpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 15
      godmarkpers.MANAD COLUMN-LABEL "Månad" FORMAT "X(256)":U
            WIDTH 9
      godmarkpers.DATUM COLUMN-LABEL "Färdig!till" FORMAT "99/99/99":U
      godmarkpers.TIDSGODK COLUMN-LABEL "God-!kännare" FORMAT "x(256)":U
            WIDTH 6
  ENABLE
      godmarkpers.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 53.5 BY 20
         TITLE "Gamla färdigrapporterade tidsedlar".

DEFINE BROWSE BRW_VTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VTID C-Win _STRUCTURED
  QUERY BRW_VTID NO-LOCK DISPLAY
      vgodmarkpers.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "X(256)":U
            WIDTH 6
      vgodmarkpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 10
      vgodmarkpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 18
      vgodmarkpers.MANAD COLUMN-LABEL "Månad" FORMAT "X(256)":U
            WIDTH 9
      vgodmarkpers.DATUM COLUMN-LABEL "Färdig!till" FORMAT "99/99/99":U
      vgodmarkpers.GODKAND COLUMN-LABEL "Godkänd" FORMAT "X(4)":U
  ENABLE
      vgodmarkpers.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 63.88 BY 20
         TITLE "Godkänn gammal tidsedel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN_EFTERNAMN AT ROW 1.88 COL 39.13 COLON-ALIGNED NO-LABEL
     FILL-IN-REGIS AT ROW 1.96 COL 2.5 NO-LABEL
     FILL-IN_NAMN AT ROW 1.96 COL 21 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_FORNAMN AT ROW 1.96 COL 21 COLON-ALIGNED NO-LABEL
     BRW_TID AT ROW 3.88 COL 1.5
     BRW_VTID AT ROW 3.88 COL 61.5
     BTN_OVER AT ROW 12.38 COL 55.38
     BTN_BACK AT ROW 14.58 COL 55.38
     FBTN_VISA AT ROW 24.75 COL 6.75
     FBTN_VISAM AT ROW 24.75 COL 26
     BTN_GODM AT ROW 24.75 COL 82.5
     BTN_AVB AT ROW 24.75 COL 111.38
     RAD_ALLVAL AT ROW 26.17 COL 54.5 NO-LABEL
     TOG_FARDIG AT ROW 26.42 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 26.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: vgodmarkpers T "?" NO-UNDO temp-db vgodmarkpers
      TABLE: ? T "?" NO-UNDO temp-db godmarkpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Godkänna gamla tidsedlar"
         HEIGHT             = 26.75
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_TID FILL-IN_FORNAMN DEFAULT-FRAME */
/* BROWSE-TAB BRW_VTID BRW_TID DEFAULT-FRAME */
/* SETTINGS FOR BROWSE BRW_TID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TID:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_TID:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300.

/* SETTINGS FOR BROWSE BRW_VTID IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_VTID:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_VTID:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300.

/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_EFTERNAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TID
/* Query rebuild information for BROWSE BRW_TID
     _TblList          = "Temp-Tables.godmarkpers"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.godmarkpers.PERSONALKOD
"godmarkpers.PERSONALKOD" "Enhet!/Sign" "X(256)" "character" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.godmarkpers.FORNAMN
"godmarkpers.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.godmarkpers.EFTERNAMN
"godmarkpers.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.godmarkpers.MANAD
"godmarkpers.MANAD" "Månad" "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.godmarkpers.DATUM
"godmarkpers.DATUM" "Färdig!till" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.godmarkpers.TIDSGODK
"godmarkpers.TIDSGODK" "God-!kännare" "x(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VTID
/* Query rebuild information for BROWSE BRW_VTID
     _TblList          = "Temp-Tables.vgodmarkpers"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.vgodmarkpers.PERSONALKOD
"vgodmarkpers.PERSONALKOD" "Enhet!/Sign" "X(256)" "character" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.vgodmarkpers.FORNAMN
"vgodmarkpers.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.vgodmarkpers.EFTERNAMN
"vgodmarkpers.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.vgodmarkpers.MANAD
"vgodmarkpers.MANAD" "Månad" "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.vgodmarkpers.DATUM
"vgodmarkpers.DATUM" "Färdig!till" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.vgodmarkpers.GODKAND
"vgodmarkpers.GODKAND" "Godkänd" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VTID */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Godkänna gamla tidsedlar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Godkänna gamla tidsedlar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TID
&Scoped-define SELF-NAME BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TID C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_TID IN FRAME DEFAULT-FRAME /* Gamla färdigrapporterade tidsedlar */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   FIND FIRST vgodmarkpers WHERE vgodmarkpers.GODKAND = "" NO-ERROR.
   IF AVAILABLE vgodmarkpers THEN DO:
      MESSAGE "Vill du avsluta fast du inte har godkänt alla personer du har flyttat över?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL 
      UPDATE valet AS LOGICAL.
      CASE valet:
         WHEN TRUE THEN DO:         
            regmnr = regmnrspar.
            regar = regarspar.
            APPLY "CLOSE":U TO THIS-PROCEDURE.
         END.
      END.
   END.
   ELSE DO: 
      regmnr = regmnrspar.
      regar = regarspar.
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME
DO:
  antal_valda = BRW_VTID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.         
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon" VIEW-AS ALERT-BOX.
      RETURN.
   END.  
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_VTID:FETCH-SELECTED-ROW(antal_raknare).      
      IF vgodmarkpers.GODKAND NE "" THEN DO:
         MESSAGE "Enhet/Sign " vgodmarkpers.PERSONALKOD " är redan godkänd och kan inte tas bort"  VIEW-AS ALERT-BOX.
      END.
      ELSE DO:      
         DELETE vgodmarkpers.
      END.
      antal_raknare = antal_raknare + 1.        
   END.
   RUN refreshbrw_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODM C-Win
ON CHOOSE OF BTN_GODM IN FRAME DEFAULT-FRAME /* Godkänn tidsedel */
DO:   
   {muswait.i}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 16.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:
      MESSAGE "Det går inte att godkänna tidsedlar nu." Skip
      "För det pågår en ekonomi- och lönesammanställning."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN NO-APPLY.    
   END.
   RUN godmt2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER C-Win
ON CHOOSE OF BTN_OVER IN FRAME DEFAULT-FRAME
DO:
  antal_valda = BRW_TID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.         
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon" VIEW-AS ALERT-BOX.
      RETURN.
   END.      
   antal_raknare = 1.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
      DO WHILE antal_raknare LE antal_valda:    
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).      
         IF AVAILABLE godmarkpers THEN DO:
            FIND FIRST vgodmarkpers WHERE vgodmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD NO-LOCK NO-ERROR.
            IF NOT AVAILABLE vgodmarkpers  THEN DO:               
               CREATE vgodmarkpers.
               BUFFER-COPY godmarkpers TO vgodmarkpers.
               CREATE evgodmarkpers.
               BUFFER-COPY godmarkpers TO evgodmarkpers.
            END.            
         END.         
         antal_raknare = antal_raknare + 1.        
      END.           
      FOR EACH evgodmarkpers BY evgodmarkpers.PERSONALKOD:
         hjpkod = evgodmarkpers.PERSONALKOD.        
         FIND FIRST godmarkpers WHERE godmarkpers.PERSONALKOD = evgodmarkpers.PERSONALKOD NO-LOCK NO-ERROR.
         APPLY "CHOOSE" TO FBTN_VISA.               
      END.
      hjpkod = "".
   END.
   ELSE DO:      
      IF antal_valda > 1 THEN DO:
         MESSAGE "Du kan bara skicka över 1 person åt gången" VIEW-AS ALERT-BOX.
         RETURN.
      END.      
      status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).      
      ASSIGN
      sppkod = godmarkpers.PERSONALKOD
      spman = godmarkpers.MANADNR
      spar = godmarkpers.AR.
      APPLY "CHOOSE" TO FBTN_VISA.     
      IF AVAILABLE godmarkpers THEN DO:
         IF sppkod = godmarkpers.PERSONALKOD AND spman = godmarkpers.MANADNR AND spar = godmarkpers.AR THEN DO:         
            CREATE vgodmarkpers.
            BUFFER-COPY godmarkpers TO vgodmarkpers.
         END.
      END.
      antal_raknare = antal_raknare + 1.        
   END.
   
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").
   RUN openbdynspec_UI IN brwproc[2].      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa tidsedel */
DO:
   IF AVAILABLE godmarkpers THEN DO:
      {muswait.i}   
      ASSIGN    
      persrec = godmarkpers.TIDPERSREC
      vaxla = TRUE.
      IF TOG_FARDIG = TRUE THEN DO:      
         ASSIGN RAD_TIDSVAL = 2
         korda = 4.
      END.
      ELSE DO:      
         ASSIGN RAD_TIDSVAL = 1.
         korda = 0.
      END.
      /*RAD_TIDSVAL = 1.                
      Korda = 3.*/
      ASSIGN
      bdatum = DATE(godmarkpers.MANADNR,01,godmarkpers.AR).      
      IF godmarkpers.MANADNR = 12 THEN avdatum = DATE(12,31,godmarkpers.AR).
      ELSE avdatum = DATE(godmarkpers.MANADNR + 1,01,godmarkpers.AR) - 1.   
      {AVBGOM.I}
      {AMERICANEUROPEAN.I}
      RUN TIDSEDL.W.
      {EUROPEANAMERICAN.I}
      {AVBFRAM.I}   
      IF hjpkod NE "" THEN DO:
         FIND FIRST godmarkpers WHERE godmarkpers.PERSONALKOD = hjpkod NO-LOCK NO-ERROR.
         FIND FIRST vgodmarkpers WHERE vgodmarkpers.PERSONALKOD = hjpkod NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST vgodmarkpers WHERE vgodmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD NO-ERROR.
      END.
      regdatum = DATE(godmarkpers.MANADNR,01,godmarkpers.AR).
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT godmarkpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN GODKOLLA.P  
         (INPUT godmarkpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
      END.
      IF tillochmeddatum NE ? THEN DO:
         ASSIGN godmarkpers.DATUM = tillochmeddatum.
         /* om färdigrapportering borttagen och enbart godkända registreringar kvar*/
         IF godmarkpers.KOLLDATUM = godmarkpers.DATUM THEN DELETE godmarkpers.        /*???????*/
         IF AVAILABLE vgodmarkpers THEN DO:
            BUFFER-COPY godmarkpers TO vgodmarkpers.
            IF vgodmarkpers.KOLLDATUM = vgodmarkpers.DATUM THEN DELETE vgodmarkpers.        
            RUN refreshbrw_UI IN brwproc[2].
         END.
         FIND FIRST felmeddtemp NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            DELETE felmeddtemp.               
         END.
      END.
      ELSE DO:
         FIND FIRST vgodmarkpers WHERE vgodmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD AND
         vgodmarkpers.MANADNR = godmarkpers.MANADNR AND vgodmarkpers.AR = godmarkpers.AR  NO-ERROR.
         IF AVAILABLE vgodmarkpers THEN DO:
            DELETE vgodmarkpers.
            RUN refreshbrw_UI IN brwproc[2].
         END.
         DELETE godmarkpers.        
      END.
      IF AVAILABLE godmarkpers THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(godmarkpers)).
      END.
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
      vaxla = FALSE.      
      {musarrow.i} 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa tidsedel */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISAM C-Win
ON CHOOSE OF FBTN_VISAM IN FRAME DEFAULT-FRAME /* Visa månadsamm. */
DO:
   IF AVAILABLE godmarkpers THEN DO:
      {muswait.i}   
      ASSIGN    
      persrec = godmarkpers.TIDPERSREC
      vaxla = TRUE
      RAD_TIDSVAL = 1.                      
      CMB_TAR = godmarkpers.AR.
      IF godmarkpers.MANADNR = 1 THEN CMB_TMANAD = STRING("januari").
      ELSE IF godmarkpers.MANADNR = 2 THEN  CMB_TMANAD = STRING("februari").      
      ELSE IF godmarkpers.MANADNR = 3 THEN CMB_TMANAD = STRING("mars").      
      ELSE IF godmarkpers.MANADNR = 4 THEN CMB_TMANAD = STRING("april"). 
      ELSE IF godmarkpers.MANADNR = 5 THEN CMB_TMANAD = STRING("maj").
      ELSE IF godmarkpers.MANADNR = 6 THEN CMB_TMANAD = STRING("juni").
      ELSE IF godmarkpers.MANADNR = 7 THEN CMB_TMANAD = STRING("juli").
      ELSE IF godmarkpers.MANADNR = 8 THEN CMB_TMANAD = STRING("augusti").
      ELSE IF godmarkpers.MANADNR = 9 THEN  CMB_TMANAD = STRING("september").
      ELSE IF godmarkpers.MANADNR = 10 THEN CMB_TMANAD = STRING("oktober").
      ELSE IF godmarkpers.MANADNR = 11 THEN CMB_TMANAD = STRING("november").
      ELSE IF godmarkpers.MANADNR = 12 THEN CMB_TMANAD = STRING("december").      
      {AVBGOM.I}
      RUN MANADSL.W.
      {AVBFRAM.I}   
      regdatum = DATE(godmarkpers.MANADNR,01,godmarkpers.AR).
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT godmarkpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN GODKOLLA.P  
         (INPUT godmarkpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
      END.
      IF tillochmeddatum NE ? THEN DO:
         ASSIGN godmarkpers.DATUM = tillochmeddatum.
         FIND FIRST felmeddtemp NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            DELETE felmeddtemp.               
         END.
      END.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(godmarkpers)).
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].
      vaxla = FALSE.      
      {musarrow.i} 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISAM C-Win
ON GO OF FBTN_VISAM IN FRAME DEFAULT-FRAME /* Visa månadsamm. */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FARDIG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FARDIG C-Win
ON VALUE-CHANGED OF TOG_FARDIG IN FRAME DEFAULT-FRAME /* Visa enbart färdigrapporterad ej godkänd tid i tidsedel */
DO:
  TOG_FARDIG = INPUT TOG_FARDIG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
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
   {ALLSTARTDYN.I}
   {muswait.i}
   EMPTY TEMP-TABLE godmarkpers NO-ERROR. 
   ASSIGN
   regmnrspar = regmnr
   regarspar = regar
   regdatum = TODAY - 7.
   RUN REGVEC.P.            
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT Guru.Konstanter:globanv,INPUT "F",
       INPUT-OUTPUT regar,INPUT-OUTPUT regmnr,INPUT-OUTPUT regdatum,
       INPUT TABLE tidpers, INPUT-OUTPUT TABLE godmarkpers).
   END.
   ELSE DO:
      RUN GODAPP.P 
      (INPUT 2,INPUT Guru.Konstanter:globanv,INPUT "F",
       INPUT-OUTPUT regar,INPUT-OUTPUT regmnr,INPUT-OUTPUT regdatum,
       INPUT TABLE tidpers, INPUT-OUTPUT TABLE godmarkpers).
   END.
   
   
   FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN LEAVE MAIN-BLOCK.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD
   NO-LOCK NO-ERROR.
   IF RAD_ALLVAL = 1 THEN DO:
      FILL-IN-REGIS = "Ansvarig:".
      FIND FIRST ansvarigtemp WHERE ansvarigtemp.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,1,5) NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_EFTERNAMN = ansvarigtemp.EFTERNAMN
      FILL-IN_FORNAMN = ansvarigtemp.FORNAMN
      FILL-IN_EFTERNAMN:HIDDEN = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 2 THEN DO:
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_NAMN:HIDDEN = FALSE
      FILL-IN_NAMN = omrtemp.NAMN.
      DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      FILL-IN-REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      FILL-IN-REGIS = "".
   END.
   IF RAD_ALLVAL = 5 THEN FILL-IN-REGIS = "Markerade enheter".
   IF RAD_ALLVAL = 6 THEN DO:
      FILL-IN-REGIS = "Godkänner tidsedel:".
      FIND FIRST godkannartemp WHERE godkannartemp.PERSONALKOD = personaltemp.TIDSGODK NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_EFTERNAMN = godkannartemp.EFTERNAMN
      FILL-IN_FORNAMN = godkannartemp.FORNAMN
      FILL-IN_EFTERNAMN:HIDDEN = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}  
   FIND FIRST godmarkpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE godmarkpers THEN DO:
      LEAVE MAIN-BLOCK.
   END.   
   ELSE DO:    
      RUN openbdyn_UI IN brwproc[1] (INPUT "").    
      ENABLE BRW_TID BRW_VTID WITH FRAME {&FRAME-NAME}.
      BRW_TID:HIDDEN = FALSE.
      BRW_VTID:HIDDEN = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
       TOG_FARDIG = FALSE.
       DISPLAY TOG_FARDIG WITH FRAME {&FRAME-NAME}.       
   END.
   status-ok = RAD_ALLVAL:DELETE("Ansvarig tidredovisare").
   status-ok = RAD_ALLVAL:DELETE("Godkänner tidsedel").
   status-ok = RAD_ALLVAL:DELETE("Område").
   status-ok = RAD_ALLVAL:DELETE("Alla").
   status-ok = RAD_ALLVAL:DELETE("Enhet/Sign").
   status-ok = RAD_ALLVAL:DELETE("Markerade").
   RAD_ALLVAL:ADD-LAST("Ansvarig tidredovisare", 1).
   RAD_ALLVAL:ADD-LAST("Godkänner tidsedel", 2).
   RAD_ALLVAL:ADD-LAST(Guru.Konstanter:gomrk, 3).
   RAD_ALLVAL:ADD-LAST("Alla", 4).
   RAD_ALLVAL:ADD-LAST("Enhet/Sign", 5).
   RAD_ALLVAL:ADD-LAST("Markerade", 6).
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
   godmarkpers.PERSONALKOD:READ-ONLY IN BROWSE BRW_TID = TRUE.
   vgodmarkpers.PERSONALKOD:READ-ONLY IN BROWSE BRW_VTID = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TID:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_VTID:HANDLE IN FRAME {&FRAME-NAME}).    
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
  DISPLAY FILL-IN-REGIS TOG_FARDIG 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BTN_OVER BTN_BACK FBTN_VISA FBTN_VISAM BTN_GODM BTN_AVB TOG_FARDIG 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE godmt2_UI C-Win 
PROCEDURE godmt2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}    
   antal_valda = BRW_VTID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:        
      ASSIGN
      antal_raknare = 1
      regdatum = TODAY.      
      RUN REGVEC.P.    
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_VTID:FETCH-SELECTED-ROW(antal_raknare).
         IF vgodmarkpers.MANADNR = 12 THEN regdatum = DATE(12,31,vgodmarkpers.AR). 
         ELSE regdatum = DATE((vgodmarkpers.MANADNR + 1),01,vgodmarkpers.AR) - 1.               
         ASSIGN
         regar = YEAR(regdatum)
         regmnr = MONTH(regdatum).
         tidtabrec = RECID(vgodmarkpers).         
         IF vgodmarkpers.GODKAND = "" THEN DO:         
            CREATE appmarkpers.
            ASSIGN appmarkpers.PERSONALKOD = vgodmarkpers.PERSONALKOD
            appmarkpers.DATUM = vgodmarkpers.DATUM
            appmarkpers.VECKONUMMER = vgodmarkpers.VECKONUMMER.            
            ASSIGN         
            vgodmarkpers.GODKAND = "G" + STRING(regvnr, "999").      
            FIND FIRST godmarkpers WHERE godmarkpers.PERSONALKOD = vgodmarkpers.PERSONALKOD
            AND godmarkpers.DATUM = vgodmarkpers.DATUM  NO-ERROR.
            IF AVAILABLE godmarkpers THEN DELETE godmarkpers.
         END.
         antal_raknare = antal_raknare + 1.         
      END.               
      {GODKAFIN.I}                              
      EMPTY TEMP-TABLE appmarkpers NO-ERROR.       
      RUN refreshbrw_UI IN brwproc[1].
      RUN refreshbrw_UI IN brwproc[2].
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

