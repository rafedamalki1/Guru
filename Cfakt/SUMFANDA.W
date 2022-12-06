&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW NEW                           
{BEFTEMP.I}
&Scoped-define NEW                           
{FAKTTEMP.I}
{GLOBVAR2DEL1.I}

{ORGPRIS.I}
DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vartyp AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER FILL-IN_SLUTFAKT AS LOGICAL NO-UNDO.  
DEFINE INPUT PARAMETER brwrec AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR faktaonrtemp.
DEFINE INPUT PARAMETER TABLE FOR kundregeltemp.
DEFINE OUTPUT PARAMETER TABLE FOR bytpers.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
DEFINE SHARED VARIABLE faktrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE musand AS LOGICAL NO-UNDO.
DEFINE VARIABLE rundavar AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktaonrtemp befattningstemp sumpers ~
sumtidtemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR faktaonrtemp.AONR ~
faktaonrtemp.DELNR faktaonrtemp.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR 
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH faktaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH faktaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONR faktaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR faktaonrtemp


/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF befattningstemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF 
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH befattningstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH befattningstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEF befattningstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF befattningstemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS sumpers.PERSONALKOD sumpers.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH sumpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH sumpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS sumpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS sumpers


/* Definitions for BROWSE BRW_TID                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TID sumtidtemp.PERSBEF sumtidtemp.DATUM ~
sumtidtemp.START sumtidtemp.SLUT sumtidtemp.TIMMAR sumtidtemp.OTIMMAR ~
sumtidtemp.VITRAKT sumtidtemp.TRAKTANTAL sumtidtemp.VILART ~
sumtidtemp.LONTILLANTAL sumtidtemp.RESTIM sumtidtemp.AONR sumtidtemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TID 
&Scoped-define QUERY-STRING-BRW_TID FOR EACH sumtidtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TID OPEN QUERY BRW_TID FOR EACH sumtidtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TID sumtidtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TID sumtidtemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_BEF}~
    ~{&OPEN-QUERY-BRW_TID}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RAD_VAD BRW_PERS BRW_AONR BRW_BEF ~
BRW_TID BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD runda Dialog-Frame 
FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_DELA 
     LABEL "Dela upp" 
     SIZE 12 BY 1.

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

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_STOPP 
     LABEL "Stoppa tid" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE RAD_VAD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Stoppa tid mellan två datum ", 1,
"Byta befattning på en person", 2
     SIZE 47 BY 2.33 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 40.75 BY 4.92
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      faktaonrtemp SCROLLING.

DEFINE QUERY BRW_BEF FOR 
      befattningstemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      sumpers SCROLLING.

DEFINE QUERY BRW_TID FOR 
      sumtidtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR Dialog-Frame _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      faktaonrtemp.AONR FORMAT "X(6)":U
      faktaonrtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      faktaonrtemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 47.75 BY 6.96.

DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF Dialog-Frame _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      befattningstemp.NAMN FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 30 BY 6.96.

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS Dialog-Frame _STRUCTURED
  QUERY BRW_PERS DISPLAY
      sumpers.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      sumpers.NAMN COLUMN-LABEL "Namn" FORMAT "X(15)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 30.25 BY 6.96.

DEFINE BROWSE BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TID Dialog-Frame _STRUCTURED
  QUERY BRW_TID NO-LOCK DISPLAY
      sumtidtemp.PERSBEF COLUMN-LABEL "Enhet/Sign!Befattning" FORMAT "X(17)":U
      sumtidtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      sumtidtemp.START COLUMN-LABEL "Start!tid" FORMAT ">>9.99":U
      sumtidtemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT ">>9.99":U
      sumtidtemp.TIMMAR COLUMN-LABEL "Timmar" FORMAT "->9.9<":U
      sumtidtemp.OTIMMAR COLUMN-LABEL "Över.!timmar" FORMAT "->9.9<":U
      sumtidtemp.VITRAKT COLUMN-LABEL "Lart" FORMAT "X(4)":U
      sumtidtemp.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "->9.9":U
      sumtidtemp.VILART COLUMN-LABEL "Lart" FORMAT "X(4)":U
      sumtidtemp.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>9.9<":U
      sumtidtemp.RESTIM COLUMN-LABEL "Res.!timmar" FORMAT "->9.9<":U
      sumtidtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(7)":U
      sumtidtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 94.75 BY 9.08.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_VAD AT ROW 2.88 COL 1.5 NO-LABEL
     BRW_PERS AT ROW 5.63 COL 1.5
     BRW_AONR AT ROW 5.63 COL 34.25
     BRW_BEF AT ROW 5.63 COL 34.38
     BRW_TID AT ROW 12.83 COL 1.5
     BTN_NVE AT ROW 15.17 COL 38.75
     BTN_NVE-2 AT ROW 15.17 COL 57.75
     FILL-IN-STARTDAT AT ROW 15.58 COL 26.75 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 15.58 COL 45.75 COLON-ALIGNED
     BTN_FVE AT ROW 16 COL 38.75
     BTN_FVE-2 AT ROW 16 COL 57.75
     BTN_STOPP AT ROW 17.33 COL 33.75
     BTN_DELA AT ROW 22.29 COL 43.88
     BTN_OK AT ROW 22.29 COL 67
     BTN_AVB AT ROW 22.29 COL 82
     "Vad vill du göra ?" VIEW-AS TEXT
          SIZE 33 BY 1.08 AT ROW 1.75 COL 1.5
          FONT 17
     RECT-2 AT ROW 14.17 COL 21.38
     SPACE(35.36) SKIP(4.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra tidposter"
         DEFAULT-BUTTON BTN_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: befattningstemp T "?" NO-UNDO temp-db befattningstemp
      TABLE: faktaonrtemp T "?" NO-UNDO temp-db faktaonrtemp
      TABLE: ? T "?" NO-UNDO temp-db sumtidtemp
      TABLE: ? T "?" NO-UNDO temp-db sumpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_PERS RAD_VAD Dialog-Frame */
/* BROWSE-TAB BRW_AONR BRW_PERS Dialog-Frame */
/* BROWSE-TAB BRW_BEF BRW_AONR Dialog-Frame */
/* BROWSE-TAB BRW_TID BRW_BEF Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

ASSIGN 
       BRW_AONR:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_BEF:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_TID:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR BUTTON BTN_DELA IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_DELA:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_STOPP IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_STOPP:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-STOPPDAT IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.faktaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.faktaonrtemp.AONR
     _FldNameList[2]   > Temp-Tables.faktaonrtemp.DELNR
"faktaonrtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.faktaonrtemp.ORT
"faktaonrtemp.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.befattningstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.befattningstemp.NAMN
"befattningstemp.NAMN" ? ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.sumpers"
     _FldNameList[1]   > Temp-Tables.sumpers.PERSONALKOD
"sumpers.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.sumpers.NAMN
"sumpers.NAMN" "Namn" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TID
/* Query rebuild information for BROWSE BRW_TID
     _TblList          = "Temp-db.sumtidtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-db.sumtidtemp.PERSBEF
"sumtidtemp.PERSBEF" "Enhet/Sign!Befattning" "X(17)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-db.sumtidtemp.DATUM
"sumtidtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-db.sumtidtemp.START
"sumtidtemp.START" "Start!tid" ">>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-db.sumtidtemp.SLUT
"sumtidtemp.SLUT" "Slut!tid" ">>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-db.sumtidtemp.TIMMAR
"sumtidtemp.TIMMAR" "Timmar" "->9.9<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-db.sumtidtemp.OTIMMAR
"sumtidtemp.OTIMMAR" "Över.!timmar" "->9.9<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-db.sumtidtemp.VITRAKT
"sumtidtemp.VITRAKT" "Lart" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-db.sumtidtemp.TRAKTANTAL
"sumtidtemp.TRAKTANTAL" "Antal" "->9.9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-db.sumtidtemp.VILART
"sumtidtemp.VILART" "Lart" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-db.sumtidtemp.LONTILLANTAL
"sumtidtemp.LONTILLANTAL" "Antal" ">>>9.9<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-db.sumtidtemp.RESTIM
"sumtidtemp.RESTIM" "Res.!timmar" "->9.9<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-db.sumtidtemp.AONR
"sumtidtemp.AONR" "Aonr" "X(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-db.sumtidtemp.DELNR
"sumtidtemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_TID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ändra tidposter */
DO:
   IF musand = TRUE THEN DO:
      MESSAGE "Du måste avsluta med Ok-knappen." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR Dialog-Frame
ON VALUE-CHANGED OF BRW_AONR IN FRAME Dialog-Frame
DO:
   /*dddddd*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS Dialog-Frame
ON VALUE-CHANGED OF BRW_PERS IN FRAME Dialog-Frame
DO:
   /*status-ok = BRW_PERS:SELECT-FOCUSED-ROW().   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TID
&Scoped-define SELF-NAME BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TID Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_TID IN FRAME Dialog-Frame
DO:
  /*cccc*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   IF musand = TRUE THEN DO:
      MESSAGE "Du måste avsluta med Ok-knappen." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {BORTBRWPROC.I}
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DELA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DELA Dialog-Frame
ON CHOOSE OF BTN_DELA IN FRAME Dialog-Frame /* Dela upp */
DO:   
   musand = TRUE.
   antal_valda = BRW_TID:NUM-SELECTED-ROWS NO-ERROR.
   antal_raknare = 1.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du valde ingen tidpost!"
         VIEW-AS ALERT-BOX TITLE "Dela upp tidposter".
         APPLY "CHOOSE" TO BTN_AVB IN FRAME {&FRAME-NAME}.      
   END.
   ELSE DO:         
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
         brwrec = ROWID(sumtidtemp).
         IF sumtidtemp.START = sumtidtemp.SLUT THEN musz = musz.
         ELSE RUN FADELTIDA.W (INPUT ROWID(sumtidtemp),INPUT-OUTPUT TABLE bytpers).
         antal_raknare = antal_raknare + 1.            
      END.
   END.
   RUN openbdyn_UI IN brwproc[4] (INPUT "").
   RUN setlastrowid_UI IN brwproc[4] (INPUT brwrec).              
   RUN lastselectdyn_UI IN brwproc[4].              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE Dialog-Frame
ON CHOOSE OF BTN_FVE IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 Dialog-Frame
ON CHOOSE OF BTN_FVE-2 IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE Dialog-Frame
ON CHOOSE OF BTN_NVE IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 Dialog-Frame
ON CHOOSE OF BTN_NVE-2 IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   
   IF RAD_VAD = 1 THEN musz = musz.
   ELSE IF RAD_VAD = 2 THEN DO:
      status-ok = BRW_BEF:SELECT-FOCUSED-ROW() NO-ERROR.
      antal_valda = BRW_TID:NUM-SELECTED-ROWS NO-ERROR.
      IF antal_valda = 0 THEN DO:
         MESSAGE "Du valde ingen tid!"
         VIEW-AS ALERT-BOX TITLE "Byta befattning".
         APPLY "CHOOSE" TO BTN_AVB IN FRAME {&FRAME-NAME}.      
      END.
      ELSE DO:         
         MESSAGE "Vill du verkligen byta befattning på markerade poster ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Byta befattning"
         UPDATE answer1 AS LOGICAL.
         IF answer1 THEN DO:
            antal_raknare = 1.
            DO WHILE antal_raknare LE antal_valda:                                   
               status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
               FIND FIRST bytpers WHERE 
               bytpers.PERSONALKOD = sumtidtemp.PERSONALKOD AND  
               bytpers.AONR = sumtidtemp.AONR AND
               bytpers.DELNR = sumtidtemp.DELNR AND  
               bytpers.DATUM = sumtidtemp.DATUM AND   
               bytpers.START = sumtidtemp.START AND
               bytpers.SLUT = sumtidtemp.SLUT NO-ERROR.
               IF NOT AVAILABLE bytpers THEN CREATE bytpers.
               ASSIGN
               bytpers.PERSONALKOD = sumtidtemp.PERSONALKOD   
               bytpers.AONR = sumtidtemp.AONR
               bytpers.DELNR = sumtidtemp.DELNR   
               bytpers.DATUM = sumtidtemp.DATUM
               bytpers.BEFATTNING = befattningstemp.BEFATTNING
               bytpers.START = sumtidtemp.START
               bytpers.SLUT = sumtidtemp.SLUT
               bytpers.GSTART = sumtidtemp.GSTART
               bytpers.GSLUT = sumtidtemp.GSLUT
               bytpers.BYT = TRUE.       
               antal_raknare = antal_raknare + 1.
            END.   
         END.      
         ELSE musz = TRUE.
      END.
   END.
   ELSE IF RAD_VAD = 3 THEN DO:
      musz = musz.
   END.
   {BORTBRWPROC.I}
   {muswait.i}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_STOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_STOPP Dialog-Frame
ON CHOOSE OF BTN_STOPP IN FRAME Dialog-Frame /* Stoppa tid */
DO:   
   DEFINE VARIABLE antalp AS INTEGER NO-UNDO.
   DEFINE VARIABLE raknarp AS INTEGER NO-UNDO. 
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS NO-ERROR.
   IF antal_valda = 0 THEN DO:     
      MESSAGE "Vill du verkligen ta bort all tid mellan" FILL-IN-STARTDAT " och " FILL-IN-STOPPDAT " för alla valda personer"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Stoppa tid"
      UPDATE answer AS LOGICAL.
      IF answer THEN DO:
         antal_valda = BRW_PERS:NUM-SELECTED-ROWS NO-ERROR.
         IF antal_valda > 0 THEN DO:
            antal_raknare = 1.
            DO WHILE antal_raknare LE antal_valda:                                   
               status-ok = BRW_PERS:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
               FOR EACH sumtidtemp WHERE 
               sumtidtemp.PERSONALKOD = sumpers.PERSONALKOD AND            
               sumtidtemp.DATUM >= FILL-IN-STARTDAT AND sumtidtemp.DATUM <= FILL-IN-STOPPDAT:
                  IF sumtidtemp.MED = TRUE THEN DO:               
                     RUN bertidkost_UI (INPUT FALSE).
                     ASSIGN sumtidtemp.MED = FALSE.
                  END.
               END.
               antal_raknare = antal_raknare + 1.   
            END.            
         END.           
      END.     
   END.
   ELSE DO:
      MESSAGE "Vill du verkligen ta bort all tid mellan" FILL-IN-STARTDAT " och " FILL-IN-STOPPDAT " för valda " Guru.Konstanter:gaok " och valda personer"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Stoppa tid"
      UPDATE answer1 AS LOGICAL.
      IF answer1 THEN DO:      
         antal_valda = BRW_PERS:NUM-SELECTED-ROWS NO-ERROR.
         IF antal_valda = 0 THEN DO:            
            antal_valda = BRW_AONR:NUM-SELECTED-ROWS NO-ERROR.
            antal_raknare = 1.            
            DO WHILE antal_raknare LE antal_valda:                                   
               status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
               FOR EACH sumtidtemp WHERE                
               sumtidtemp.AONR = faktaonrtemp.AONR AND sumtidtemp.DELNR = faktaonrtemp.DELNR AND
               sumtidtemp.DATUM >= FILL-IN-STARTDAT AND sumtidtemp.DATUM <= FILL-IN-STOPPDAT:
                  IF sumtidtemp.MED = TRUE THEN DO:               
                     RUN bertidkost_UI (INPUT FALSE).
                     ASSIGN sumtidtemp.MED = FALSE.
                  END.   
               END.
               antal_raknare = antal_raknare + 1. 
            END.                        
         END.
         ELSE DO:         
            antalp = BRW_PERS:NUM-SELECTED-ROWS NO-ERROR.
            antal_valda = BRW_AONR:NUM-SELECTED-ROWS NO-ERROR.
            antal_raknare = 1.
            raknarp = 1.
            DO WHILE raknarp LE antalp:                                   
               status-ok = BRW_PERS:FETCH-SELECTED-ROW(raknarp) NO-ERROR.              
               DO WHILE antal_raknare LE antal_valda:
                  status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
                  FOR EACH sumtidtemp WHERE                
                  sumtidtemp.PERSONALKOD = sumpers.PERSONALKOD AND
                  sumtidtemp.AONR = faktaonrtemp.AONR AND sumtidtemp.DELNR = faktaonrtemp.DELNR AND
                  sumtidtemp.DATUM >= FILL-IN-STARTDAT AND sumtidtemp.DATUM <= FILL-IN-STOPPDAT:
                     IF sumtidtemp.MED = TRUE THEN DO:               
                        RUN bertidkost_UI (INPUT FALSE).
                        ASSIGN sumtidtemp.MED = FALSE.
                     END.   
                  END.
                  antal_raknare = antal_raknare + 1.
               END.
               antal_raknare = 1.            
               raknarp = raknarp + 1.
            END.
         END.
      END.      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON LEAVE OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME Dialog-Frame /* Från */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT Dialog-Frame
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME Dialog-Frame /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME Dialog-Frame /* Till */
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


&Scoped-define SELF-NAME RAD_VAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAD Dialog-Frame
ON VALUE-CHANGED OF RAD_VAD IN FRAME Dialog-Frame
DO:
   RAD_VAD = INPUT RAD_VAD.

   DISPLAY RAD_VAD WITH FRAME {&FRAME-NAME}.   
   BTN_DELA:HIDDEN IN FRAME {&FRAME-NAME}= TRUE.
   IF RAD_VAD = 1 THEN DO:
      RUN stoppa_UI.
   END.
   ELSE IF RAD_VAD = 2 THEN DO:
      RUN bef_UI. 
   END.
   ELSE IF RAD_VAD = 3 THEN DO:
      RUN delaupp_UI. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   IF Guru.Konstanter:appcon THEN DO: 
      RUN BEFHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE befattningstemp).
   END.
   ELSE DO:
      RUN BEFHMT.P
      (OUTPUT TABLE befattningstemp).
   END.
   RAD_VAD = vad.
   status-ok = RAD_VAD:ADD-LAST("Dela upp post",3).     
   FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr 
   NO-LOCK NO-ERROR.
   RUN enable_UI.       
   {FRMSIZED.I} 
   RUN stoppa_UI.
   APPLY "VALUE-CHANGED" TO RAD_VAD.
   {musarrow.i}   
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_BEF:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_TID:HANDLE IN FRAME {&FRAME-NAME}).
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bef_UI Dialog-Frame 
PROCEDURE bef_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   BRW_TID:TOOLTIP IN FRAME {&FRAME-NAME} = "Markera poster som skall byta befattning.".
   ASSIGN         
   RECT-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
   BRW_BEF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE  
   BTN_STOPP:HIDDEN IN FRAME {&FRAME-NAME}= TRUE
   BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
   BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
   BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
   BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
   FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
   FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME}= TRUE.
   ENABLE BRW_PERS BRW_BEF BRW_TID WITH FRAME {&FRAME-NAME}. 
   FIND FIRST befattningstemp WHERE befattningstemp.BEFATTNING = sumpers.BEFATTNING NO-LOCK NO-ERROR.  
   RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(befattningstemp)).              
   RUN lastselectdyn_UI IN brwproc[2].            
   RUN openbdyn_UI IN brwproc[4] (INPUT "").
   RUN setlastrowid_UI IN brwproc[4] (INPUT brwrec).              
   RUN lastselectdyn_UI IN brwproc[4].            
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bertidkost_UI Dialog-Frame 
PROCEDURE bertidkost_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER medvar AS LOGICAL NO-UNDO.
   {muswait.i}
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF medvar = ? OR medvar = FALSE THEN DO:    
      IF kundregeltemp.TIMRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST - sumtidtemp.BELOPP.                    
      END.       
      IF kundregeltemp.OVERTIDRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP - sumtidtemp.OBELOPP.                               
      END. 
      IF kundregeltemp.TIMRGL = "KUNDENS EGNA" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST - sumtidtemp.BELOPP.                    
      END.  
      IF kundregeltemp.OVERTIDRGL = "EGNA REGLER" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP - sumtidtemp.OBELOPP.                               
      END.         
      IF kundregeltemp.TRAKTRGL = "TIDREDOVISNING" OR kundregeltemp.TRAKTRGL = "EGET PRIS" THEN DO:         
         gfaktemp.TRAKT = gfaktemp.TRAKT - sumtidtemp.TBELOPP.                            
      END.
      IF kundregeltemp.LONRGL = "TIDREDOVISNING" OR kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
         gfaktemp.LONTILL = gfaktemp.LONTILL - sumtidtemp.LONKOST.                     
      END.          
      gfaktemp.RES = gfaktemp.RES - sumtidtemp.RESKOSTDEC.    
   END.       
   ELSE DO:
     IF kundregeltemp.TIMRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                    
      END.       
      IF kundregeltemp.OVERTIDRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
      END. 
      IF kundregeltemp.TIMRGL = "KUNDENS EGNA" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                    
      END.  
      IF kundregeltemp.OVERTIDRGL = "EGNA REGLER" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
      END.         
      IF kundregeltemp.TRAKTRGL = "TIDREDOVISNING" OR kundregeltemp.TRAKTRGL = "EGET PRIS" THEN DO:         
         gfaktemp.TRAKT = gfaktemp.TRAKT + sumtidtemp.TBELOPP.                            
      END.
      IF kundregeltemp.LONRGL = "TIDREDOVISNING" OR kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
         gfaktemp.LONTILL = gfaktemp.LONTILL + sumtidtemp.LONKOST.                     
      END.          
      gfaktemp.RES = gfaktemp.RES + sumtidtemp.RESKOSTDEC.    
   END. 
   
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT)
      gfaktemp.RES = runda(gfaktemp.RES) 
      gfaktemp.ARBKOST = runda(gfaktemp.ARBKOST) 
      gfaktemp.LONTILL = runda(gfaktemp.LONTILL) 
      gfaktemp.MTRL = runda(gfaktemp.MTRL) 
      gfaktemp.OBELOPP = runda(gfaktemp.OBELOPP) 
      gfaktemp.OVRIG = runda(gfaktemp.OVRIG) 
      gfaktemp.TRAKT = runda(gfaktemp.TRAKT)       
      gfaktemp.KBELOPP = runda(gfaktemp.KBELOPP).
   END.                               
   IF (vartyp = 3 AND FILL-IN_SLUTFAKT = FALSE) OR 
   vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN DO:
      gfaktemp.TOTALT = gfaktemp.TOTALT + 
                        gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END.
   ELSE DO:
      gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delaupp_UI Dialog-Frame 
PROCEDURE delaupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  BRW_TID:TOOLTIP IN FRAME {&FRAME-NAME} = "Markera de poster som skall delas upp.".
  ASSIGN         
  RECT-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
  BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
  BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
  BRW_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
  BTN_STOPP:HIDDEN IN FRAME {&FRAME-NAME}= TRUE
  BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
  BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
  BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
  BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
  FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME}= TRUE 
  FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME}= TRUE
  BTN_DELA:HIDDEN IN FRAME {&FRAME-NAME}= FALSE.
  ENABLE BTN_DELA BRW_TID WITH FRAME {&FRAME-NAME}. 
  RUN openbdyn_UI IN brwproc[4] (INPUT "").
  RUN setlastrowid_UI IN brwproc[4] (INPUT brwrec).              
  RUN lastselectdyn_UI IN brwproc[4].              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAD 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-2 RAD_VAD BRW_PERS BRW_AONR BRW_BEF BRW_TID BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stoppa_UI Dialog-Frame 
PROCEDURE stoppa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   FIND FIRST sumpers NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sumpers THEN DO:
      FOR EACH sumtidtemp BREAK BY sumtidtemp.PERSONALKOD:         
         IF LAST-OF(sumtidtemp.PERSONALKOD) THEN DO TRANSACTION:
            CREATE sumpers.
            ASSIGN 
            sumpers.PERSONALKOD = sumtidtemp.PERSONALKOD
            sumpers.NAMN = sumtidtemp.NAMN
            sumpers.BEFATTNING = sumtidtemp.BEFATTNING.  
         END.
      END.      
      RUN openbdyn_UI IN brwproc[1] (INPUT "").      
      FIND FIRST sumtidtemp USE-INDEX DATUM NO-LOCK NO-ERROR.
      IF AVAILABLE sumtidtemp THEN DO: 
         ASSIGN 
         FILL-IN-STARTDAT = sumtidtemp.DATUM
         FILL-IN-STOPPDAT = FILL-IN-TOMDAT.
      END.         
      ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.      
      FIND sumtidtemp WHERE ROWID(sumtidtemp) = brwrec NO-LOCK NO-ERROR.
      FIND FIRST sumpers WHERE sumpers.PERSONALKOD = sumtidtemp.PERSONALKOD 
      NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(sumpers)).              
      RUN lastselectdyn_UI IN brwproc[1].        
      ASSIGN
      BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN openbdyn_UI IN brwproc[3] (INPUT "").      
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   END.
   ASSIGN         
   RECT-2:HIDDEN IN FRAME {&FRAME-NAME}= FALSE
   BRW_TID:HIDDEN IN FRAME {&FRAME-NAME}= TRUE
   BRW_AONR:HIDDEN = FALSE
   BRW_BEF:HIDDEN = TRUE.    
   ENABLE BTN_STOPP BRW_PERS BTN_FVE BTN_FVE-2 BTN_NVE BTN_NVE-2 
   FILL-IN-STARTDAT 
   FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}. 
   DISPLAY FILL-IN-STARTDAT FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sumut_UI Dialog-Frame 
PROCEDURE sumut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
musz = musz.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION runda Dialog-Frame 
FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN ROUND(varedin,0).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

