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

  Author: ej nya kalk i ber

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
DEFINE INPUT-OUTPUT PARAMETER arbkodvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER lopnrvar AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kon_rowid AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED

{SCADMIN.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
/*{LOP.I}  */
{LOPTEMP.I}
{PTEMP.I}
{KALKTEMP2.I}
DEFINE NEW SHARED VARIABLE prow AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE loprow AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE valrow AS ROWID NO-UNDO.
 

DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.  

DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE ortsok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE bortrow AS ROWID NO-UNDO.
DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE tval AS INTEGER NO-UNDO.
DEFINE VARIABLE entrymtrlantal AS LOGICAL NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttnr AS INTEGER NO-UNDO.
DEFINE VARIABLE extrkodapph AS HANDLE NO-UNDO.
DEFINE VARIABLE extrbefbapph AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE ehdkalktemp NO-UNDO LIKE hdkalktemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_KALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES hdkalktemp loptemp ptemp hdschakttemp

/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK hdkalktemp.ARBKOD hdkalktemp.LOPNR ~
hdkalktemp.ANTAL hdkalktemp.ENHET hdkalktemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK hdkalktemp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KALK hdkalktemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KALK hdkalktemp
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH hdkalktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH hdkalktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK hdkalktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK hdkalktemp


/* Definitions for BROWSE BRW_LOP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP loptemp.LOPNR loptemp.ENHET ~
loptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP 
&Scoped-define QUERY-STRING-BRW_LOP FOR EACH loptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LOP OPEN QUERY BRW_LOP FOR EACH loptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LOP loptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP loptemp


/* Definitions for BROWSE BRW_P                                         */
&Scoped-define FIELDS-IN-QUERY-BRW_P ptemp.ARBKOD ptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P 
&Scoped-define QUERY-STRING-BRW_P FOR EACH ptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_P OPEN QUERY BRW_P FOR EACH ptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_P ptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P ptemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL hdschakttemp.SID ~
hdschakttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH hdschakttemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH hdschakttemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VAL hdschakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL hdschakttemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VAL BRW_LOP BRW_P BRW_KALK BTN_OK ~
FILL-IN-KOD FILL-IN-BEN BTN_OVER BTN_BACK BTN_ANT FILL-IN-VKOD FILL-IN-VLOP ~
FILL-IN-ANTAL BTN_UP BTN_MIN BTN_EGEN BTN_ANDEGEN RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KOD FILL-IN-BEN FILL-IN-VKOD ~
FILL-IN-VLOP FILL-IN-ANTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDEGEN 
     LABEL "Ändra EGEN" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANT 
     LABEL "Antal/Anm." 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "Ta bort" 
     SIZE 5.88 BY 1.96.

DEFINE BUTTON BTN_EGEN 
     LABEL "Ny EGEN" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "Skapa" 
     SIZE 5.88 BY 1.96.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE VARIABLE FILL-IN-ANTAL AS DECIMAL FORMAT ">>>>9.999":U INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 11.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 25.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Arbetskod" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Arbetskod" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VLOP AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Löpnr" 
     VIEW-AS FILL-IN 
     SIZE 5.13 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.25 BY 1.46
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KALK FOR 
      hdkalktemp SCROLLING.

DEFINE QUERY BRW_LOP FOR 
      loptemp SCROLLING.

DEFINE QUERY BRW_P FOR 
      ptemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      hdschakttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK C-Win _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      hdkalktemp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U
      hdkalktemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      hdkalktemp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>>>9.999":U
      hdkalktemp.ENHET COLUMN-LABEL "Enhet" FORMAT "X(3)":U
      hdkalktemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(40)":U
            WIDTH 28
  ENABLE
      hdkalktemp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 63.38 BY 12.75
         TITLE "Valda koder".

DEFINE BROWSE BRW_LOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP C-Win _STRUCTURED
  QUERY BRW_LOP NO-LOCK DISPLAY
      loptemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      loptemp.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 12.75
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P C-Win _STRUCTURED
  QUERY BRW_P NO-LOCK DISPLAY
      ptemp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U
      ptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 10
         TITLE "Arbetskoder".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      hdschakttemp.SID COLUMN-LABEL "Nr" FORMAT ">>>9":U
      hdschakttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 44.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING NO-SCROLLBAR-VERTICAL SIZE 51.5 BY 10
         TITLE "Schakt" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_VAL AT ROW 1.25 COL 70
     BRW_LOP AT ROW 13.25 COL 1.5
     BRW_P AT ROW 1.25 COL 1.5
     BRW_KALK AT ROW 13.25 COL 62.13
     BTN_OK AT ROW 28 COL 105.63
     FILL-IN-KOD AT ROW 11.79 COL 19.38 COLON-ALIGNED
     FILL-IN-BEN AT ROW 11.79 COL 37.75 COLON-ALIGNED
     BTN_OVER AT ROW 16.75 COL 55.5
     BTN_BACK AT ROW 20.63 COL 55.5
     BTN_ANT AT ROW 26.25 COL 102.38
     FILL-IN-VKOD AT ROW 26.54 COL 10 COLON-ALIGNED
     FILL-IN-VLOP AT ROW 26.54 COL 23.63 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 26.54 COL 37 COLON-ALIGNED
     BTN_UP AT ROW 26.17 COL 51.88
     BTN_MIN AT ROW 27.17 COL 51.88
     BTN_EGEN AT ROW 26.25 COL 71
     BTN_ANDEGEN AT ROW 26.25 COL 86.75
     "Sök på:" VIEW-AS TEXT
          SIZE 7.88 BY .67 AT ROW 11.71 COL 2.5
     RECT-22 AT ROW 11.54 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: hdkalktemp T "?" NO-UNDO temp-db hdkalktemp
      TABLE: hdschakttemp T "?" NO-UNDO temp-db hdschakttemp
      TABLE: loptemp T "?" NO-UNDO temp-db loptemp
      TABLE: ptemp T "?" NO-UNDO temp-db ptemp
      TABLE: temphandkopp T "?" NO-UNDO temp-db temphandkopp
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BRW_VAL 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_LOP BRW_VAL DEFAULT-FRAME */
/* BROWSE-TAB BRW_P BRW_LOP DEFAULT-FRAME */
/* BROWSE-TAB BRW_KALK BRW_P DEFAULT-FRAME */
ASSIGN 
       BRW_KALK:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000.

ASSIGN 
       BRW_LOP:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_LOP:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_LOP:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_P:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_P:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BTN_MIN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_UP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-ANTAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-VKOD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-VLOP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.hdkalktemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.hdkalktemp.ARBKOD
"hdkalktemp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.hdkalktemp.LOPNR
"hdkalktemp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.hdkalktemp.ANTAL
"hdkalktemp.ANTAL" "Antal" "->>>>>>9.999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.hdkalktemp.ENHET
"hdkalktemp.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.hdkalktemp.BENAMNING
"hdkalktemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP
/* Query rebuild information for BROWSE BRW_LOP
     _TblList          = "Temp-Tables.loptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.loptemp.LOPNR
"loptemp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.loptemp.ENHET
"loptemp.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.loptemp.BENAMNING
"loptemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P
/* Query rebuild information for BROWSE BRW_P
     _TblList          = "Temp-Tables.ptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ptemp.ARBKOD
"ptemp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ptemp.BENAMNING
"ptemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.hdschakttemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.hdschakttemp.SID
"hdschakttemp.SID" "Nr" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.hdschakttemp.BENAMNING
"hdschakttemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "44.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
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


&Scoped-define BROWSE-NAME BRW_KALK
&Scoped-define SELF-NAME BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KALK C-Win
ON ROW-LEAVE OF BRW_KALK IN FRAME DEFAULT-FRAME /* Valda koder */
DO:
  RUN entryantal_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KALK C-Win
ON VALUE-CHANGED OF BRW_KALK IN FRAME DEFAULT-FRAME /* Valda koder */
DO:
    status-ok = BRW_KALK:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE hdkalktemp THEN DO:
      ASSIGN
      
      arbkodvar = hdkalktemp.ARBKOD
      lopnrvar  = hdkalktemp.LOPNR.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME hdkalktemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hdkalktemp.ANTAL BRW_KALK _BROWSE-COLUMN C-Win
ON ANY-KEY OF hdkalktemp.ANTAL IN BROWSE BRW_KALK /* Antal */
DO:
  entrymtrlantal = TRUE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hdkalktemp.ANTAL BRW_KALK _BROWSE-COLUMN C-Win
ON ENTRY OF hdkalktemp.ANTAL IN BROWSE BRW_KALK /* Antal */
DO:
  entrymtrlantal = TRUE.
   IF AVAILABLE hdkalktemp THEN
   DISPLAY hdkalktemp.ANTAL WITH BROWSE BRW_KALK. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL hdkalktemp.ANTAL BRW_KALK _BROWSE-COLUMN C-Win
ON LEAVE OF hdkalktemp.ANTAL IN BROWSE BRW_KALK /* Antal */
DO:
   hdkalktemp.ANDRAD = TRUE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP
&Scoped-define SELF-NAME BRW_LOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP C-Win
ON MOUSE-MENU-CLICK OF BRW_LOP IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   RUN entryantal_UI.
   BRW_LOP:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   RUN entryantal_UI.
   RUN btnskapa_UI.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP C-Win
ON VALUE-CHANGED OF BRW_LOP IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_P:SELECT-FOCUSED-ROW() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P
&Scoped-define SELF-NAME BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_P IN FRAME DEFAULT-FRAME /* Arbetskoder */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P C-Win
ON VALUE-CHANGED OF BRW_P IN FRAME DEFAULT-FRAME /* Arbetskoder */
DO:
   ASSIGN
   status-ok = BRW_P:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE ptemp THEN DO:
      FIND FIRST loptemp WHERE loptemp.ARBKOD = ptemp.ARBKOD AND loptemp.KATAR = ptemp.KATAR NO-ERROR.
      IF AVAILABLE loptemp THEN ASSIGN BRW_LOP:TITLE = "Löpnr kopplade till " + loptemp.ARBKOD + "- Typ" + STRING(loptemp.TYP).
   
      /*BRW_LOP:TITLE = "Löpnr kopplade till " + ptemp.ARBKOD.*/
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE loptemp.ARBKOD = '" + ptemp.ARBKOD + "'").
      RUN openbdynspec_UI IN brwproc[2].   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME /* Schakt */
DO:
  RUN setcolsortvar_UI IN brwproc[1] (INPUT "SID = " + STRING(hdschakttemp.SID)).
  RUN openbdynspec_UI IN brwproc[1].       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDEGEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDEGEN C-Win
ON CHOOSE OF BTN_ANDEGEN IN FRAME DEFAULT-FRAME /* Ändra EGEN */
DO:
   RUN entryantal_UI.
   RUN btnandegen_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANT C-Win
ON CHOOSE OF BTN_ANT IN FRAME DEFAULT-FRAME /* Antal/Anm. */
DO:
   antal_valda = BRW_KALK:NUM-SELECTED-ROWS.
   antal_raknare = 1. 
   IF antal_valda > 0 THEN DO:  
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_KALK:FETCH-SELECTED-ROW(antal_raknare).   
         IF AVAILABLE hdkalktemp THEN DO:
            valrow = ROWID(hdkalktemp).                    
            RUN ANTKALKU2.W (INPUT FALSE,INPUT ROWID(hdkalktemp)).                  
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(hdkalktemp)).                     
         END.
         antal_raknare = antal_raknare + 1.
      END.   
   END.
   ELSE DO:
      MESSAGE "Markera den eller de kod(er) som Ni vill ändra antalet för."
      VIEW-AS ALERT-BOX TITLE "Meddelande".          
      RETURN NO-APPLY.
   END. 
   RUN refreshbrw_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   antal_valda = BRW_KALK:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   IF antal_valda = 0 THEN RETURN NO-APPLY.
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_KALK:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.   
      IF AVAILABLE hdkalktemp THEN DO:
         bortrow = ROWID(hdkalktemp).                     
         MESSAGE "Vill du ta bort arbetskod:" + hdkalktemp.ARBKOD + " löpnr:" + STRING(hdkalktemp.LOPNR,Guru.Konstanter:varforetypchar[6]) + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
         IF svar THEN DO:
            
            IF hdkalktemp.ARBKOD = "EGEN" THEN DO:
               /*KALKBEFB är redan skapad , måste tas bort*/
               
               EMPTY TEMP-TABLE ehdkalktemp NO-ERROR. 
               CREATE ehdkalktemp.
               BUFFER-COPY hdkalktemp TO ehdkalktemp.
            END.
            FIND hdkalktemp WHERE ROWID(hdkalktemp) = bortrow EXCLUSIVE-LOCK NO-ERROR. 
            DELETE hdkalktemp.
            IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].
         END.           
      END.
      antal_raknare = antal_raknare + 1.
   END.   
   RUN refreshbrw_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   IF AVAILABLE hdkalktemp THEN DO:
      ASSIGN
      arbkodvar = hdkalktemp.ARBKOD
      lopnrvar  = hdkalktemp.LOPNR.
   END.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EGEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EGEN C-Win
ON CHOOSE OF BTN_EGEN IN FRAME DEFAULT-FRAME /* Ny EGEN */
DO:
   
   RUN entryantal_UI.
   RUN btnegen_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN C-Win
ON CHOOSE OF BTN_MIN IN FRAME DEFAULT-FRAME /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 1 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 0!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   {muswait.i} 
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER C-Win
ON CHOOSE OF BTN_OVER IN FRAME DEFAULT-FRAME /* Skapa */
DO:
   RUN entryantal_UI.
   RUN btnskapa_UI.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP C-Win
ON CHOOSE OF BTN_UP IN FRAME DEFAULT-FRAME /* + */
DO: 
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ANTAL C-Win
ON ANY-KEY OF FILL-IN-ANTAL IN FRAME DEFAULT-FRAME /* Antal */
DO:
   ASSIGN
   FILL-IN-VKOD = INPUT FILL-IN-VKOD
   FILL-IN-VLOP = INPUT FILL-IN-VLOP
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   antalvar = FILL-IN-ANTAL.
    IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN C-Win
ON ANY-KEY OF FILL-IN-BEN IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   {TRYCKS.I}
    IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON ANY-KEY OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Arbetskod */
DO:
   {TRYCKS.I}
    IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-VKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VKOD C-Win
ON ANY-KEY OF FILL-IN-VKOD IN FRAME DEFAULT-FRAME /* Arbetskod */
DO:
   ASSIGN
   FILL-IN-VKOD = INPUT FILL-IN-VKOD
   FILL-IN-VLOP = INPUT FILL-IN-VLOP
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   antalvar = FILL-IN-ANTAL.
    IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-VLOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VLOP C-Win
ON ANY-KEY OF FILL-IN-VLOP IN FRAME DEFAULT-FRAME /* Löpnr */
DO:
   ASSIGN
   FILL-IN-VKOD = INPUT FILL-IN-VKOD
   FILL-IN-VLOP = INPUT FILL-IN-VLOP
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   antalvar = FILL-IN-ANTAL.
    IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KALK
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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(extrkodapph) THEN DELETE PROCEDURE extrkodapph.
   IF VALID-HANDLE(extrbefbapph) THEN DELETE PROCEDURE extrbefbapph.
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
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
   FILL-IN-VLOP:FORMAT =  varforetypchar[6].
   EMPTY TEMP-TABLE ptemp NO-ERROR. 
   EMPTY TEMP-TABLE loptemp NO-ERROR. 
   RUN Ptemp_UI IN KalkClasserStart (INPUT 2, OUTPUT TABLE ptemp APPEND).
   RUN Loptempn_UI IN KalkClasserStart (INPUT 2, OUTPUT TABLE loptemp APPEND).
   
   {&WINDOW-NAME}:TITLE = "Komplettering av Kalkylkoder".
   BRW_P:TITLE = "Arbetskoder".
   RUN enable_UI.   
   {FRMSIZE.I}       
   logresult = FALSE.
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "KALKBEFB", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "KALKBEFB", OUTPUT logresult).
   END.   
   IF logresult = FALSE THEN DO:
      BTN_EGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      BTN_ANDEGEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.

   tval = 2.
   FIND hdschakttemp WHERE ROWID(hdschakttemp) = kon_rowid NO-LOCK NO-ERROR.
   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "SID = " + STRING(hdschakttemp.SID)).
   RUN openbdynspec_UI IN brwproc[1].             
   RUN openbdynspec_UI IN brwproc[4].   
   FIND hdschakttemp WHERE ROWID(hdschakttemp) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE hdschakttemp THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(hdschakttemp)).
      RUN lastselectdyn_UI IN brwproc[4].        
   END.      
   RUN visa_UI.
   FIND FIRST hdkalktemp WHERE hdkalktemp.SID = hdschakttemp.SID AND hdkalktemp.ARBKOD = arbkodvar AND hdkalktemp.LOPNR = lopnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE hdkalktemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(hdkalktemp)).
      RUN lastselectdyn_UI IN brwproc[1].        
      APPLY "ENTRY" TO hdkalktemp.ANTAL IN BROWSE BRW_KALK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_KALK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_LOP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_P:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN setcolindex_UI IN brwproc[1] (INPUT "ARBKOD BY LOPNR").  
   RUN setcolindex_UI IN brwproc[4] (INPUT "SID"). 
   
   RUN addfillin_UI IN brwproc[3] 
      (INPUT FILL-IN-KOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ARBKOD").
   RUN addfillin_UI IN brwproc[3] 
      (INPUT FILL-IN-BEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").   
   RUN setfillinproc_UI IN brwproc[3] (INPUT THIS-PROCEDURE, INPUT "updatlop_UI").   

   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_KALK:HANDLE,INPUT "Kommentarer",INPUT "kommentar_UI").   
   RUN addmenuitem_UI IN brwproc[2] (INPUT BRW_LOP:HANDLE,INPUT "Kommentarer",INPUT "btnkom_UI").   
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRKODAPP.P PERSISTENT SET extrkodapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRKODAPP.P PERSISTENT SET extrkodapph.
   END.    
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "KALKBEFB", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "KALKBEFB", OUTPUT logresult).
   END.      
   IF logresult = TRUE THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph.
      END. 
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnandegen_UI C-Win 
PROCEDURE btnandegen_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}   
   antal_valda = BRW_KALK:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:
         BRW_KALK:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR. 
         IF AVAILABLE hdkalktemp THEN DO:
            valrow = ROWID(hdkalktemp).   
            IF hdkalktemp.ARBKOD = "EGEN" THEN DO:                     
               {AVBGOM.I}
               EMPTY TEMP-TABLE ehdkalktemp NO-ERROR.
               CREATE ehdkalktemp.
               BUFFER-COPY hdkalktemp TO ehdkalktemp.
               EMPTY TEMP-TABLE ekalk_temp NO-ERROR. 
               CREATE ekalk_temp.
               BUFFER-COPY hdkalktemp TO ekalk_temp.
               RUN EGENKODB.W (INPUT TRUE).
               IF musz = FALSE THEN DO:
                  FIND FIRST ekalk_temp NO-LOCK NO-ERROR.
                  FIND FIRST ehdkalktemp NO-LOCK NO-ERROR.              
                  BUFFER-COPY ekalk_temp TO ehdkalktemp.
                  EMPTY TEMP-TABLE ekalk_temp NO-ERROR.
                  FIND FIRST hdkalktemp WHERE hdkalktemp.ARBKOD = ehdkalktemp.ARBKOD AND hdkalktemp.LOPNR = ehdkalktemp.LOPNR NO-LOCK NO-ERROR.
                  IF AVAILABLE hdkalktemp THEN BUFFER-COPY ehdkalktemp TO hdkalktemp.                          
                  valrow = ROWID(hdkalktemp).   
               END.
               {AVBFRAM.I}
            END.
            ELSE DO:
               MESSAGE "Detta är ingen EGEN kod och går därmed ej att ändra."
               VIEW-AS ALERT-BOX TITLE "Meddelande".   
            END.    
         END.
         antal_raknare = antal_raknare + 1.
      END.
   END.
   ELSE DO:
      MESSAGE "Markera den egna koden som Ni vill ändra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".            
      RETURN NO-APPLY.
   END.               
   RUN openbdynspec_UI IN brwproc[1].
   FIND FIRST hdkalktemp WHERE ROWID(hdkalktemp) = valrow NO-LOCK NO-ERROR.
   IF AVAILABLE hdkalktemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(hdkalktemp)).
      RUN lastselectdyn_UI IN brwproc[1].        
   END.      
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnegen_UI C-Win 
PROCEDURE btnegen_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   musz = FALSE.
   {AVBGOM.I}
   EMPTY TEMP-TABLE ehdkalktemp NO-ERROR. 
   CREATE ehdkalktemp.   
   RUN hmtegnr_UI IN extrbefbapph (INPUT valaonr,INPUT valomrade,OUTPUT nyttnr).
   
   ASSIGN   
   ehdkalktemp.BERNR = hdschakttemp.BERNR
   ehdkalktemp.OMRADE = hdschakttemp.OMRADE 
   ehdkalktemp.SID = hdschakttemp.SID   
   ehdkalktemp.ARBKOD = "EGEN"
   ehdkalktemp.LOPNR = nyttnr   
   ehdkalktemp.ANTAL = 1   
   valrow = ROWID(ehdkalktemp).   
   EMPTY TEMP-TABLE ekalk_temp NO-ERROR. 
   CREATE ekalk_temp.   
   ASSIGN   
   ekalk_temp.SID = hdschakttemp.SID    
   ekalk_temp.ARBKOD = "EGEN"
   ekalk_temp.LOPNR = nyttnr   
   ekalk_temp.ANTAL = 1.    
   RUN EGENKODB.W (INPUT FALSE).
   {AVBFRAM.I}
    
   IF musz = FALSE THEN DO:
      FIND FIRST ekalk_temp NO-LOCK NO-ERROR.
      FIND FIRST ehdkalktemp NO-LOCK NO-ERROR.
      BUFFER-COPY ekalk_temp TO ehdkalktemp.
      EMPTY TEMP-TABLE ekalk_temp NO-ERROR.
      CREATE hdkalktemp.
      BUFFER-COPY ehdkalktemp TO hdkalktemp.
      hdkalktemp.ANDRAD = TRUE.
      valrow = ROWID(hdkalktemp).   
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST hdkalktemp WHERE ROWID(hdkalktemp) = valrow NO-LOCK NO-ERROR.
      IF AVAILABLE hdkalktemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(hdkalktemp)).
         RUN lastselectdyn_UI IN brwproc[1].        
      END.      
               
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkom2_UI C-Win 
PROCEDURE btnkom2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
  
   antal_raknare = 1.
   DO WHILE antal_raknare LE BRW_KALK:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
      BRW_KALK:FETCH-SELECTED-ROW(antal_raknare).
      EMPTY TEMP-TABLE extraloptemp NO-ERROR.             
      FIND FIRST loptemp WHERE TRIM(loptemp.ARBKOD) = TRIM(hdkalktemp.ARBKOD) AND loptemp.LOPNR = hdkalktemp.LOPNR NO-LOCK NO-ERROR.         
      IF AVAILABLE loptemp THEN DO:
         CREATE extraloptemp.
         BUFFER-COPY loptemp TO extraloptemp.
         
         RUN KALKKOM2.W.
         
      END.
      antal_raknare = antal_raknare + 1.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkom_UI C-Win 
PROCEDURE btnkom_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   antal_raknare = 1.
   DO WHILE antal_raknare LE BRW_LOP:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
      BRW_LOP:FETCH-SELECTED-ROW(antal_raknare).
      EMPTY TEMP-TABLE extraloptemp NO-ERROR. 
      CREATE extraloptemp.
      BUFFER-COPY loptemp TO extraloptemp.
      
      RUN KALKKOM2.W.
      
      antal_raknare = antal_raknare + 1.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnskapa_UI C-Win 
PROCEDURE btnskapa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   antalvar = 1.   
   antal_valda = BRW_LOP:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   IF antal_valda = 0 THEN RETURN NO-APPLY.   
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_LOP:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
      FIND FIRST hdkalktemp WHERE hdkalktemp.SID = hdschakttemp.SID AND 
      hdkalktemp.ARBKOD = loptemp.ARBKOD AND hdkalktemp.LOPNR = loptemp.LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE hdkalktemp THEN DO:
         RUN skapatyp3_UI.                       
         /*RUN ANTKALKU2.W (INPUT TRUE,INPUT ROWID(hdkalktemp)).           */

         FIND hdkalktemp WHERE ROWID(hdkalktemp) = valrow  NO-LOCK NO-ERROR.

         IF AVAILABLE hdkalktemp THEN DO:
            ASSIGN
            arbkodvar = hdkalktemp.ARBKOD
            lopnrvar  = hdkalktemp.LOPNR.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(hdkalktemp)).
         END.
      END.
      antal_raknare = antal_raknare + 1.
   END.
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].           
   
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
  DISPLAY FILL-IN-KOD FILL-IN-BEN FILL-IN-VKOD FILL-IN-VLOP FILL-IN-ANTAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_VAL BRW_LOP BRW_P BRW_KALK BTN_OK FILL-IN-KOD FILL-IN-BEN BTN_OVER 
         BTN_BACK BTN_ANT FILL-IN-VKOD FILL-IN-VLOP FILL-IN-ANTAL BTN_UP 
         BTN_MIN BTN_EGEN BTN_ANDEGEN RECT-22 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryantal_UI C-Win 
PROCEDURE entryantal_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   IF entrymtrlantal = TRUE THEN DO:
      APPLY "LEAVE" TO hdkalktemp.ANTAL IN BROWSE BRW_KALK.
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kommentar_UI C-Win 
PROCEDURE kommentar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   {muswait.i}      
   RUN btnkom2_UI.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ngnkey_UI C-Win 
PROCEDURE ngnkey_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   {TRYCKS.I}  
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      {muswait.i}      
      FIND FIRST loptemp WHERE loptemp.ARBKOD = FILL-IN-VKOD AND
      loptemp.LOPNR = FILL-IN-VLOP USE-INDEX AR NO-LOCK NO-ERROR.
      IF AVAILABLE loptemp THEN DO:     
         FIND FIRST hdkalktemp WHERE hdkalktemp.SID = hdschakttemp.SID AND hdkalktemp.ARBKOD = loptemp.ARBKOD
         AND hdkalktemp.LOPNR = loptemp.LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE hdkalktemp THEN DO:
            RUN skapatyp3_UI.            
         END.  
      END.
      ELSE DO:
         MESSAGE "Arbetskod eller löpnr saknas."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.        
      RUN openbdynspec_UI IN brwproc[1].
      IF AVAILABLE hdkalktemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(hdkalktemp)).
         RUN lastselectdyn_UI IN brwproc[1].        
      END.  
      {musarrow.i}
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapatyp3_UI C-Win 
PROCEDURE skapatyp3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE hdkalktemp.
   ASSIGN   
   hdkalktemp.BERNR = hdschakttemp.BERNR
   hdkalktemp.OMRADE = hdschakttemp.OMRADE 
   hdkalktemp.SID = hdschakttemp.SID
   hdkalktemp.ANTAL = antalvar            
   hdkalktemp.ARBKOD = loptemp.ARBKOD
   hdkalktemp.LOPNR = loptemp.LOPNR
   hdkalktemp.BENAMNING = SUBSTRING(loptemp.BENAMNING,1,49)
   hdkalktemp.ENHET = loptemp.ENHET
   hdkalktemp.ANDRAD = TRUE               
   valrow = ROWID(hdkalktemp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatlop_UI C-Win 
PROCEDURE updatlop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   BRW_P:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viartal_UI C-Win 
PROCEDURE viartal_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER viar AS INTEGER NO-UNDO.
   IF Guru.Konstanter:globforetag = "GRAN" THEN viar = viar - 2.
   ELSE viar = viar - 1.
   IF viar < 0 THEN viar = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI C-Win 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF globforetag NE "ELPA" THEN DO:     
      ASSIGN
      FILL-IN-VKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-VLOP:HIDDEN = TRUE
      FILL-IN-ANTAL:HIDDEN = TRUE
      BTN_UP:HIDDEN = TRUE
      BTN_MIN:HIDDEN = TRUE.   
   END.
   ELSE DO:     
      ASSIGN
      FILL-IN-VKOD:HIDDEN = FALSE
      FILL-IN-VLOP:HIDDEN = FALSE
      FILL-IN-ANTAL:HIDDEN = FALSE
      BTN_UP:HIDDEN = FALSE
      BTN_MIN:HIDDEN = FALSE.     
   END.  
   ENABLE BRW_P WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_LOP WITH FRAME {&FRAME-NAME}.
   RUN setcolsortvar_UI IN brwproc[3] (INPUT "").
   RUN openbdynspec_UI IN brwproc[3].   
   APPLY "VALUE-CHANGED" TO BRW_P IN FRAME {&FRAME-NAME}.     
   
   ASSIGN
   BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN-KOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

