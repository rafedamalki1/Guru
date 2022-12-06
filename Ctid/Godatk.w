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
DEFINE INPUT  PARAMETER valmeny AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{EXTRADATA.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{TIDPERS.I}
{PHMT.I}
{GODATK.I}
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
DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE epost AS CHARACTER NO-UNDO.
DEFINE VARIABLE visartal AS INTEGER NO-UNDO.
DEFINE VARIABLE suppdat AS DATE NO-UNDO.
DEFINE VARIABLE finnstid AS LOGICAL NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE exdatahmth AS HANDLE NO-UNDO.
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.

{TIDUTTTNEW.I}
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW-EJREG

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES godmarkpers godatk

/* Definitions for BROWSE BRW-EJREG                                     */
&Scoped-define FIELDS-IN-QUERY-BRW-EJREG godmarkpers.PERSONALKOD ~
godmarkpers.FORNAMN godmarkpers.EFTERNAMN godmarkpers.TIDSGODK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW-EJREG 
&Scoped-define QUERY-STRING-BRW-EJREG FOR EACH godmarkpers NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW-EJREG OPEN QUERY BRW-EJREG FOR EACH godmarkpers NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW-EJREG godmarkpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW-EJREG godmarkpers


/* Definitions for BROWSE BRW_GATK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_GATK godatk.PERSONALKOD godatk.FORNAMN ~
godatk.EFTERNAMN godatk.atk godatk.lonvatk godatk.frisk godatk.DATUM ~
godatk.GODKAND godatk.gdatum godatk.TIDSGODK godatk.ARSUPP godatk.ARSDATUM ~
godatk.PERSONNUMMER godatk.JUDNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GATK 
&Scoped-define QUERY-STRING-BRW_GATK FOR EACH godatk NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_GATK OPEN QUERY BRW_GATK FOR EACH godatk NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_GATK godatk
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GATK godatk


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW-EJREG}~
    ~{&OPEN-QUERY-BRW_GATK}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_GATK BRW-EJREG BTN_GODATK BTN_BORTG ~
BTN_UPPDAT BTN_AVB FILL-IN-RUBRIK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS FILL-IN-RUBRIK 

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

DEFINE BUTTON BTN_BORTG 
     LABEL "Ta bort G" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_GODATK 
     LABEL "Godkänn" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_UPPDAT 
     LABEL "Årsuppdatering" 
     SIZE 15 BY 1.13 TOOLTIP "Kör årsuppdatering för alla godkända ATK och friskvårdsval för nytt år".

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RUBRIK AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 45 BY 1
     FONT 17 NO-UNDO.

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
     SIZE 76.25 BY 1.17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW-EJREG FOR 
      godmarkpers SCROLLING.

DEFINE QUERY BRW_GATK FOR 
      godatk SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW-EJREG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW-EJREG C-Win _STRUCTURED
  QUERY BRW-EJREG NO-LOCK DISPLAY
      godmarkpers.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(5)":U
            WIDTH 7
      godmarkpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U
            WIDTH 10
      godmarkpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 9.13
      godmarkpers.TIDSGODK COLUMN-LABEL "God-!kännare" FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 38.5 BY 19.25
         TITLE "Ej registrerat ATK friskvård nästa år".

DEFINE BROWSE BRW_GATK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GATK C-Win _STRUCTURED
  QUERY BRW_GATK NO-LOCK DISPLAY
      godatk.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(6)":U
            WIDTH 7
      godatk.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(256)":U WIDTH 10
      godatk.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(256)":U
            WIDTH 15
      godatk.atk COLUMN-LABEL "Atk" FORMAT "Ja/Nej":U
      godatk.lonvatk COLUMN-LABEL "Löneväxla! ATK" FORMAT "Ja/Nej":U
      godatk.frisk COLUMN-LABEL "Frisk" FORMAT "Ja/Nej":U
      godatk.DATUM COLUMN-LABEL "Reg.datum" FORMAT "99/99/99":U
      godatk.GODKAND COLUMN-LABEL "Godkänd" FORMAT "Ja/Nej":U
      godatk.gdatum COLUMN-LABEL "Godkänd! datum" FORMAT "99/99/99":U
      godatk.TIDSGODK COLUMN-LABEL "God-!kännare" FORMAT "x(8)":U
      godatk.ARSUPP COLUMN-LABEL "Årsupp!daterad" FORMAT "Ja/Nej":U
      godatk.ARSDATUM COLUMN-LABEL "Årsuppdaterad!datum" FORMAT "99/99/99":U
      godatk.PERSONNUMMER COLUMN-LABEL "Personnummer" FORMAT "999999-9999":U
      godatk.JUDNAMN COLUMN-LABEL "Bolag" FORMAT "x(256)":U WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 82.5 BY 19.25
         TITLE "Godkänn val av atk/ friskvård för nästa år".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-REGIS AT ROW 3.25 COL 5 COLON-ALIGNED NO-LABEL
     FILL-IN_FORNAMN AT ROW 3.25 COL 24.38 COLON-ALIGNED NO-LABEL
     FILL-IN_NAMN AT ROW 3.25 COL 24.5 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 3.25 COL 46 COLON-ALIGNED NO-LABEL
     BRW_GATK AT ROW 5 COL 3 WIDGET-ID 100
     BRW-EJREG AT ROW 5 COL 87 WIDGET-ID 300
     RAD_ALLVAL AT ROW 13.25 COL 1 NO-LABEL
     BTN_GODATK AT ROW 25.5 COL 14 WIDGET-ID 2
     BTN_BORTG AT ROW 25.5 COL 38 WIDGET-ID 4
     BTN_UPPDAT AT ROW 27.5 COL 27 WIDGET-ID 6
     BTN_AVB AT ROW 28 COL 111.5
     FILL-IN-RUBRIK AT ROW 1.5 COL 5 COLON-ALIGNED NO-LABEL WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: godatk T "?" NO-UNDO temp-db godatk
      TABLE: godmarkpers T "?" NO-UNDO temp-db godmarkpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Godkänn val av atk coh friskvård för nästa år"
         HEIGHT             = 28.54
         WIDTH              = 125.63
         MAX-HEIGHT         = 30.79
         MAX-WIDTH          = 127.13
         VIRTUAL-HEIGHT     = 30.79
         VIRTUAL-WIDTH      = 127.13
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
/* BROWSE-TAB BRW_GATK FILL-IN_EFTERNAMN DEFAULT-FRAME */
/* BROWSE-TAB BRW-EJREG BRW_GATK DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW-EJREG
/* Query rebuild information for BROWSE BRW-EJREG
     _TblList          = "Temp-Tables.godmarkpers"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.godmarkpers.PERSONALKOD
"godmarkpers.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.godmarkpers.FORNAMN
"godmarkpers.FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.godmarkpers.EFTERNAMN
"godmarkpers.EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "9.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.godmarkpers.TIDSGODK
"godmarkpers.TIDSGODK" "God-!kännare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW-EJREG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GATK
/* Query rebuild information for BROWSE BRW_GATK
     _TblList          = "Temp-Tables.godatk"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.godatk.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" "X(6)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.godatk.FORNAMN
"FORNAMN" "Förnamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.godatk.EFTERNAMN
"EFTERNAMN" "Efternamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.godatk.atk
"atk" "Atk" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.godatk.lonvatk
"lonvatk" "Löneväxla! ATK" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.godatk.frisk
"frisk" "Frisk" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.godatk.DATUM
"DATUM" "Reg.datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.godatk.GODKAND
"GODKAND" "Godkänd" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.godatk.gdatum
"gdatum" "Godkänd! datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.godatk.TIDSGODK
"TIDSGODK" "God-!kännare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.godatk.ARSUPP
"ARSUPP" "Årsupp!daterad" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.godatk.ARSDATUM
"ARSDATUM" "Årsuppdaterad!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.godatk.PERSONNUMMER
"PERSONNUMMER" "Personnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.godatk.JUDNAMN
"JUDNAMN" "Bolag" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_GATK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Godkänn val av atk coh friskvård för nästa år */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Godkänn val av atk coh friskvård för nästa år */
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
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTG C-Win
ON CHOOSE OF BTN_BORTG IN FRAME DEFAULT-FRAME /* Ta bort G */
DO:
  /*ccc*/
  {muswait.i}    
   antal_valda = BRW_GATK:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:        
      ASSIGN
      antal_raknare = 1
      regdatum = TODAY.      
      RUN REGVEC.P.          
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_GATK:FETCH-SELECTED-ROW(antal_raknare).
         tidtabrec = RECID(godatk).         
         IF godatk.GODKAND = TRUE THEN DO:
            ASSIGN
            godatk.GDATUM = ?
            godatk.GODKAND = FALSE.
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "ATKFRISK"                   
            inextradatatemp.HUVUDCH = godatk.PERSONALKOD.                    
            inextradatatemp.HUVUDINT = visartal.      
            inextradatatemp.SOKLOG[1] = godatk.ATK .
            IF Guru.Konstanter:globforetag = "gkal"   OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
               inextradatatemp.SOKLOG[2] = godatk.FRISK .
               inextradatatemp.SOKLOG[5] = godatk.LONVATK .
            END.
            IF Guru.Konstanter:globforetag = "snat" OR Guru.Konstanter:globforetag = "sUND" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:               
               inextradatatemp.SOKLOG[5] = godatk.LONVATK .
            END.   
            inextradatatemp.SOKDAT[1] = godatk.DATUM.
            inextradatatemp.SOKDAT[2] = godatk.GDATUM.              
            inextradatatemp.SOKLOG[3] = godatk.GODKAND.
            inextradatatemp.SOKCHAR[1] = Guru.Konstanter:globanv.
            RUN extraspar_UI IN exdatahmth (INPUT TABLE inextradatatemp).                     
         END.
         antal_raknare = antal_raknare + 1. 
      END.
      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(godatk)).        
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].            
   END.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODATK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODATK C-Win
ON CHOOSE OF BTN_GODATK IN FRAME DEFAULT-FRAME /* Godkänn */
DO:  
  {muswait.i}    
   antal_valda = BRW_GATK:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:        
      ASSIGN
      antal_raknare = 1
      regdatum = TODAY.      
      RUN REGVEC.P.          
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_GATK:FETCH-SELECTED-ROW(antal_raknare).
         tidtabrec = RECID(godatk).         
         IF godatk.GODKAND = FALSE THEN DO:
            ASSIGN
            godatk.GDATUM = TODAY
            godatk.GODKAND = TRUE.
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "ATKFRISK"                   
            inextradatatemp.HUVUDCH = godatk.PERSONALKOD.                    
            inextradatatemp.HUVUDINT = visartal.      
            inextradatatemp.SOKLOG[1] = godatk.ATK .
            IF Guru.Konstanter:globforetag = "gkal"   OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
               inextradatatemp.SOKLOG[2] = godatk.FRISK .
               inextradatatemp.SOKLOG[5] = godatk.LONVATK .
            END.
            IF Guru.Konstanter:globforetag = "snat" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:               
               inextradatatemp.SOKLOG[5] = godatk.LONVATK .
            END.   
            inextradatatemp.SOKDAT[1] = godatk.DATUM.
            inextradatatemp.SOKDAT[2] = godatk.GDATUM.              
            inextradatatemp.SOKLOG[3] = godatk.GODKAND.
            inextradatatemp.SOKCHAR[1] = Guru.Konstanter:globanv.
            RUN extraspar_UI IN exdatahmth (INPUT TABLE inextradatatemp).                     
         END.
         antal_raknare = antal_raknare + 1. 
      END.      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(godatk)).        
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].               
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPDAT C-Win
ON CHOOSE OF BTN_UPPDAT IN FRAME DEFAULT-FRAME /* Årsuppdatering */
DO:  
  {muswait.i}        
   IF suppdat NE ? THEN DO:
      MESSAGE "Det är redan gjord en årsuppdatering för " + STRING(visartal) + ". Uppdateringen gjordes " + STRING(suppdat) + ". Vill du göra ytterligare en uppdatering?"
      VIEW-AS ALERT-BOX   QUESTION BUTTONS YES-NO TITLE "Ny årsuppdatering?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         RUN arsuppdat_UI IN otbeordapph(INPUT visartal,INPUT-OUTPUT TABLE godatk ).
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
      END.              
   END.   
   ELSE DO:
      MESSAGE "Vill du starta en årsuppdatering för " + STRING(visartal) + " nu?"
      VIEW-AS ALERT-BOX   QUESTION BUTTONS YES-NO TITLE "Ny årsuppdatering?" UPDATE svar2 AS LOGICAL.         
      IF svar2 THEN DO:
         RUN arsuppdat_UI IN otbeordapph(INPUT visartal,INPUT-OUTPUT TABLE godatk ).
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
      END. 
   END.      
  {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW-EJREG
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(exdatahmth) THEN DELETE PROCEDURE exdatahmth NO-ERROR.   
   IF VALID-HANDLE(otbeordapph) THEN DELETE PROCEDURE otbeordapph NO-ERROR.
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
   EMPTY TEMP-TABLE godatk NO-ERROR. 
   EMPTY TEMP-TABLE godmarkpers NO-ERROR.
   IF valmeny = 1 THEN visartal = YEAR(today) + 1.
   IF valmeny = 2 THEN DO:
      IF Guru.Konstanter:globforetag = "gkal" THEN DO:
         IF MONTH(TODAY) = 10 OR MONTH(TODAY) = 11 OR MONTH(TODAY) = 12 THEN visartal = YEAR(today) + 1. 
         ELSE visartal = YEAR(today).
      END.
      IF Guru.Konstanter:globforetag = "sund" THEN DO:
         IF MONTH(TODAY) = 9 or MONTH(TODAY) = 10 OR MONTH(TODAY) = 11 OR MONTH(TODAY) = 12 THEN visartal = YEAR(today) + 1. 
         ELSE visartal = YEAR(today).
      END.
      IF Guru.Konstanter:globforetag = "misv" OR Guru.Konstanter:globforetag = "snat" THEN DO:
         IF  MONTH(TODAY) = 11 OR MONTH(TODAY) = 12 THEN visartal = YEAR(today) + 1. 
         ELSE visartal = YEAR(today).
      END.            
   END.   
   IF Guru.Konstanter:globforetag = "gkal" THEN DO:
     CURRENT-WINDOW:TITLE =    "Godkänn val av atk och friskvård för nästa år".
     FILL-IN-RUBRIK = "Godkänn val av atk och friskvård för nästa år".
     BRW_GATK:TITLE = "Godkänn val av atk/ friskvård för : " + STRING(visartal).
     BRW-EJREG:TITLE = "Ej registrerat ATK friskvård nästa år".
   END.  
   IF Guru.Konstanter:globforetag = "sund"  OR Guru.Konstanter:globforetag = "snat"  OR Guru.Konstanter:globforetag = "misv" THEN DO:
      CURRENT-WINDOW:TITLE =    "Godkänn val av atk för nästa år".
      FILL-IN-RUBRIK = "Godkänn val av atk för nästa år".
      BRW_GATK:TITLE = "Godkänn val av atk för : " + STRING(visartal).
      BRW-EJREG:TITLE = "Ej registrerat ATK nästa år".
   END. 
   IF valmeny = 1 THEN DO:           
      FOR EACH tidpers USE-INDEX PERSONALKOD:
         /*godkännare ska bara få upp atkval för dem som färdigrapporterat hela månaden 20171207 Lena*/         
         RUN kollsistafardig_UI IN otbeordapph (INPUT tidpers.PERSONALKOD,INPUT avdatum,OUTPUT klar).
         IF klar = TRUE THEN DO:
            FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD  NO-LOCK NO-ERROR.            
            IF personaltemp.befattning = "INHYRD PERSONAL" OR personaltemp.befattning = "Timanställd" OR personaltemp.ANSTALLNING = "ENTREP.AVTAL"  THEN .            
            ELSE DO:
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
               EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "ATKFRISK"                   
               inextradatatemp.HUVUDCH = tidpers.PERSONALKOD.                    
               inextradatatemp.HUVUDINT =  visartal.                    
               RUN etabhamt_UI IN exdatahmth (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp APPEND).
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
               FIND FIRST extradatatemp NO-ERROR.
               IF AVAILABLE extradatatemp  THEN DO:      
                  CREATE godatk.
                  ASSIGN
                  godatk.EFTERNAMN = tidpers.EFTERNAMN
                  godatk.FORNAMN = tidpers.FORNAMN
                  godatk.PERSONALKOD = tidpers.PERSONALKOD
                  godatk.PERSONNUMMER = personaltemp.PERSONNUMMER
                  godatk.OMRADE = tidpers.OMRADE.
                  godatk.ATK = extradatatemp.SOKLOG[1].
                  IF Guru.Konstanter:globforetag = "gkal"   THEN DO:
                     godatk.LONVATK = extradatatemp.SOKLOG[5].
                     godatk.FRISKVARD = extradatatemp.SOKLOG[2].
                  END.
                  IF Guru.Konstanter:globforetag = "snat" OR  Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
                     godatk.LONVATK = extradatatemp.SOKLOG[5].                  
                  END.   
                  godatk.DATUM = extradatatemp.SOKDAT[1].
                  godatk.GDATUM = extradatatemp.SOKDAT[2].
                  godatk.godkand = extradatatemp.SOKLOG[3].
                  godatk.ANVR = extradatatemp.SOKCHAR[1].
                  godatk.ANVG = extradatatemp.SOKCHAR[2].            
                  godatk.ARTAL = extradatatemp.HUVUDINT.
                  godatk.arsupp = extradatatemp.SOKLOG[4].
               END.
               ELSE DO:
                  CREATE godmarkpers.
                  ASSIGN
                  godmarkpers.EFTERNAMN = tidpers.EFTERNAMN
                  godmarkpers.FORNAMN = tidpers.FORNAMN
                  godmarkpers.PERSONALKOD = tidpers.PERSONALKOD.                                 
               END.
                        
            END.
         END.   
      END.   
   END.
   IF valmeny = 2 THEN DO:
      suppdat = ?.
      RUN hamtpers_UI IN otbeordapph (INPUT visartal,OUTPUT TABLE godatk,OUTPUT TABLE godmarkpers,INPUT-OUTPUT suppdat).
   END.            
   RUN hamttidgod_UI IN otbeordapph (INPUT-OUTPUT TABLE godatk,INPUT-OUTPUT TABLE godmarkpers).
   
   FIND FIRST godatk USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE godatk THEN LEAVE MAIN-BLOCK.
   RUN godatkjur_UI IN otbeordapph(INPUT-OUTPUT TABLE godatk ).
   
   
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = godatk.PERSONALKOD  NO-LOCK NO-ERROR.   
   IF RAD_ALLVAL = 2 THEN DO:
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = godatk.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
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
   IF valmeny = 1 THEN DO:             
      godatk.ARSUPP:VISIBLE IN BROWSE BRW_GATK = FALSE.
      godatk.ARSDATUM:VISIBLE IN BROWSE BRW_GATK = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "CSUND" OR  Guru.Konstanter:globforetag = "Cmisv"   THEN DO:             
      godatk.FRISKVARD:VISIBLE IN BROWSE BRW_GATK = FALSE.
      godatk.LONVATK:VISIBLE IN BROWSE BRW_GATK = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:                   
      godatk.FRISKVARD:VISIBLE IN BROWSE BRW_GATK = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "MISV" THEN DO:                   
      godatk.FRISKVARD:VISIBLE IN BROWSE BRW_GATK = FALSE.
      godatk.ATK:LABEL IN BROWSE BRW_GATK = "Atk i! Tid".
      godatk.LONVATK:LABEL IN BROWSE BRW_GATK = "Pensions!avsättning".      
   END.
   RUN enable_UI.
   IF valmeny = 1 THEN DO:       
      BTN_UPPDAT:HIDDEN = TRUE.      
   END.   
   IF valmeny = 2 THEN DO:      
      BTN_UPPDAT:HIDDEN = FALSE.
   END.   
   {FRMSIZE.I}  
   FIND FIRST godatk USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE godatk THEN DO:   
   END.   
   ELSE DO:    
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      ENABLE BRW_GATK WITH FRAME {&FRAME-NAME}.
      BRW_GATK:HIDDEN = FALSE.
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
   /*godmarkpers.PERSONALKOD:READ-ONLY IN BROWSE BRW_TID-2 = TRUE.*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_GATK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW-EJREG:HANDLE IN FRAME {&FRAME-NAME}).    
   IF Guru.Konstanter:appcon THEN DO:      
      RUN EXTRADATAHMT.P PERSISTENT SET exdatahmth ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:      
      RUN EXTRADATAHMT.P PERSISTENT SET exdatahmth.
   END.   
    IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
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
  DISPLAY FILL-IN-REGIS FILL-IN-RUBRIK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_GATK BRW-EJREG BTN_GODATK BTN_BORTG BTN_UPPDAT BTN_AVB 
         FILL-IN-RUBRIK 
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
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

