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
&Scoped-define SHARED
{SMTRL.I}
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{BBENAMNTEMP.I}
{LISTMTRL.I}
{KONVALTEMP.I} 
{MTRLTAB.I}    /*lena netto*/
&Scoped-define NEW NEW
{KONVAL2TEMP.I}
{BEREDNINGTEMP.I}
{MTTEMP.I}   
DEFINE BUFFER ordvaL FOR kon_val2. 
DEFINE NEW SHARED VARIABLE avbvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow2 AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE katvar2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkvar2 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrow2 AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE kon_rowid AS ROWID NO-UNDO. 
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE valnum AS INTEGER NO-UNDO.   /*SKA EJ VARA SHARED I DETTA PROGRAM*/
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE kopimtrlapph AS HANDLE NO-UNDO.
DEFINE VARIABLE valdatum AS DATE NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE antvar AS INTEGER NO-UNDO.
DEFINE VARIABLE vlevnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE vlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE oversatt AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjnum AS INTEGER NO-UNDO.
DEFINE VARIABLE hjskapnum  AS INTEGER NO-UNDO.
 
DEFINE NEW SHARED TEMP-TABLE list_mtrl2 
   {LISTMTRLTT.I}
   
DEFINE TEMP-TABLE elist_mtrl2 NO-UNDO LIKE list_mtrl2.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES list_mtrl2 spec_mtrl kon_val2

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL list_mtrl2.ENR list_mtrl2.BENAMNING ~
list_mtrl2.ENHET list_mtrl2.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH list_mtrl2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH list_mtrl2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL list_mtrl2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL list_mtrl2


/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 spec_mtrl.Enr spec_mtrl.Benamning ~
spec_mtrl.Enhet spec_mtrl.ANTAL spec_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 spec_mtrl


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.ID2 ~
SUBSTRING(kon_val2.EXTRA,3) kon_val2.F1 kon_val2.F2 kon_val2.F3 kon_val2.F4 ~
kon_val2.F5 kon_val2.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VAL BTN_ALLBACK FBTN_NYAO BRW_MTRL ~
BRW_MTRL2 BTN_OVER BTN_BACK BTN_ALLOVER FBTN_OK BTN_AVB FILL-IN-BERNR ~
FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR ~
FILL-IN-DELNR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_NYAO 
     LABEL "Välj beredning" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
      VIEW-AS TEXT 
     SIZE 42.5 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Beredning nr" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
      VIEW-AS TEXT 
     SIZE 14 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      list_mtrl2 SCROLLING.

DEFINE QUERY BRW_MTRL2 FOR 
      spec_mtrl SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL C-Win _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      list_mtrl2.ENR FORMAT "X(11)":U
      list_mtrl2.BENAMNING FORMAT "x(256)":U WIDTH 25
      list_mtrl2.ENHET FORMAT "x(5)":U
      list_mtrl2.ANTAL FORMAT ">>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 51.5 BY 12.5
         TITLE "Materiel på konstruktion".

DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 C-Win _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      spec_mtrl.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      spec_mtrl.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 25
      spec_mtrl.Enhet COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      spec_mtrl.ANTAL COLUMN-LABEL "Antal" FORMAT ">>>>>9":U
      spec_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 51.88 BY 12.5
         TITLE "Vald materiel".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.ID2 FORMAT "X(10)":U
      SUBSTRING(kon_val2.EXTRA,3) COLUMN-LABEL "Fri id" FORMAT "x(15)":U
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(12)":U
      kon_val2.F2 FORMAT "X(12)":U
      kon_val2.F3 FORMAT "X(12)":U
      kon_val2.F4 FORMAT "X(12)":U
      kon_val2.F5 FORMAT "X(12)":U
      kon_val2.F6 FORMAT "X(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SEPARATORS SIZE 103.5 BY 10.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_VAL AT ROW 4.54 COL 6.5
     BTN_ALLBACK AT ROW 24.29 COL 53.5
     FBTN_NYAO AT ROW 4.92 COL 110.88
     BRW_MTRL AT ROW 15.46 COL 1.5
     BRW_MTRL2 AT ROW 15.46 COL 58.13
     BTN_OVER AT ROW 20.29 COL 53.5
     BTN_BACK AT ROW 22.58 COL 53.5
     BTN_ALLOVER AT ROW 18.58 COL 53.5
     FBTN_OK AT ROW 28.17 COL 95.88
     BTN_AVB AT ROW 28.17 COL 110.88
     FILL-IN-BERNR AT ROW 1.33 COL 15.75 COLON-ALIGNED
     FILL-IN-BEN AT ROW 2.46 COL 15.75 COLON-ALIGNED
     FILL-IN-AONR AT ROW 3.54 COL 15.75 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 3.54 COL 37.75 COLON-ALIGNED
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
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
      TABLE: list_mtrl2 T "?" NO-UNDO temp-db list_mtrl2
      TABLE: spec_mtrl T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopiering av materiel"
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VAL 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_MTRL FBTN_NYAO DEFAULT-FRAME */
/* BROWSE-TAB BRW_MTRL2 BRW_MTRL DEFAULT-FRAME */
ASSIGN 
       BRW_MTRL:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_MTRL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_MTRL2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.list_mtrl2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.list_mtrl2.ENR
     _FldNameList[2]   > Temp-Tables.list_mtrl2.BENAMNING
"list_mtrl2.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.list_mtrl2.ENHET
     _FldNameList[4]   > Temp-Tables.list_mtrl2.ANTAL
"list_mtrl2.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.spec_mtrl.Enr
"spec_mtrl.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.Enhet
"spec_mtrl.Enhet" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.spec_mtrl.ANTAL
"spec_mtrl.ANTAL" "Antal" ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.spec_mtrl.LEVKOD
"spec_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"SUBSTRING(kon_val2.EXTRA,3)" "Fri id" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val2.F2
"kon_val2.F2" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val2.F3
"kon_val2.F3" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val2.F4
"kon_val2.F4" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kon_val2.F5
"kon_val2.F5" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kon_val2.F6
"kon_val2.F6" ? "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kopiering av materiel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kopiering av materiel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME DEFAULT-FRAME /* Materiel på konstruktion */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON MOUSE-MENU-CLICK OF BRW_VAL IN FRAME DEFAULT-FRAME
DO:                                                                

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RUN vcbrwval_UI.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLBACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLBACK C-Win
ON CHOOSE OF BTN_ALLBACK IN FRAME DEFAULT-FRAME /* Alla i listan */
DO:
   EMPTY TEMP-TABLE spec_mtrl  NO-ERROR.    
   RUN refreshbrw_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLOVER C-Win
ON CHOOSE OF BTN_ALLOVER IN FRAME DEFAULT-FRAME /* Alla i listan */
DO:
   GET FIRST BRW_MTRL NO-LOCK.
   DO WHILE AVAILABLE(list_mtrl2): 
      FIND FIRST spec_mtrl WHERE spec_mtrl.NUM = list_mtrl2.NUM AND 
      spec_mtrl.ENR = list_mtrl2.ENR AND spec_mtrl.LEVKOD = list_mtrl2.LEVKOD
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE spec_mtrl  THEN DO:
         CREATE spec_mtrl.      
         ASSIGN         
         spec_mtrl.NUM = list_mtrl2.NUM
         spec_mtrl.ENR = list_mtrl2.ENR  
         spec_mtrl.BENAMNING = list_mtrl2.BENAMNING 
         spec_mtrl.ENHET = list_mtrl2.ENHET 
         spec_mtrl.ANTAL = list_mtrl2.ANTAL 
         spec_mtrl.NPRIS = list_mtrl2.PRIS 
         spec_mtrl.LEVKOD = list_mtrl2.LEVKOD
         spec_mtrl.LINKAB = list_mtrl2.LINKAB 
         spec_mtrl.MODUL = list_mtrl2.MODUL      
         spec_mtrl.TYPBER = list_mtrl2.TYPBER      
         spec_mtrl.DIAMETER = list_mtrl2.DIAMETER      
         spec_mtrl.SATS = list_mtrl2.SATS.
         spec_rowid = ROWID(spec_mtrl).                             
      END.
      GET NEXT BRW_MTRL NO-LOCK.   
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "").  
   RUN openbdynspec_UI IN brwproc[2].  
   FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = spec_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:      
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(spec_mtrl)).
      RUN lastselectdyn_UI IN brwproc[2].        
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME
DO:   
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).                      
      IF AVAILABLE spec_mtrl THEN DO:
         DELETE spec_mtrl.
         IF antal_raknare = antal_valda THEN DO:
            RUN selnextprevrow_UI IN brwproc[2]. 
         END.
      END.
      antal_raknare = antal_raknare + 1.  
      
   END.
   RUN refreshbrw_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER C-Win
ON CHOOSE OF BTN_OVER IN FRAME DEFAULT-FRAME
DO:      
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                            
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE list_mtrl2 THEN DO:
            FIND FIRST spec_mtrl WHERE spec_mtrl.NUM = list_mtrl2.NUM AND 
            spec_mtrl.ENR = list_mtrl2.ENR AND spec_mtrl.LEVKOD = list_mtrl2.LEVKOD
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE spec_mtrl  THEN DO:
               CREATE spec_mtrl.
               ASSIGN         
               spec_mtrl.NUM = list_mtrl2.NUM
               spec_mtrl.ENR = list_mtrl2.ENR  
               spec_mtrl.BENAMNING = list_mtrl2.BENAMNING 
               spec_mtrl.ENHET = list_mtrl2.ENHET 
               spec_mtrl.ANTAL = list_mtrl2.ANTAL 
               spec_mtrl.NPRIS = list_mtrl2.PRIS 
               spec_mtrl.LEVKOD = list_mtrl2.LEVKOD
               spec_mtrl.LINKAB = list_mtrl2.LINKAB 
               spec_mtrl.MODUL = list_mtrl2.MODUL      
               spec_mtrl.TYPBER = list_mtrl2.TYPBER      
               spec_mtrl.DIAMETER = list_mtrl2.DIAMETER      
               spec_mtrl.SATS = list_mtrl2.SATS.                                   
            END.
         END.
         spec_rowid = ROWID(spec_mtrl).
         antal_raknare = antal_raknare + 1.
      END.
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "").  
      RUN openbdynspec_UI IN brwproc[2].   
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:      
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(spec_mtrl)).
         RUN lastselectdyn_UI IN brwproc[2].        
      END.       
   END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_NYAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NYAO C-Win
ON CHOOSE OF FBTN_NYAO IN FRAME DEFAULT-FRAME /* Välj beredning */
DO:   
   {muswait.i}
   {AVBGOM.I}  
   avbvar = FALSE.        
   RUN VALMENYBEREU.W (INPUT beredningtemp.OMRADE, INPUT beredningtemp.BERNR,OUTPUT valdatum).   
   IF avbvar = FALSE THEN DO:
      RUN berhmt_UI IN kopimtrlapph (INPUT valaonr2,INPUT valomrade2,OUTPUT kalkrow2,
                                     OUTPUT TABLE beredningtemp).
      FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr2) AND
      beredningtemp.OMRADE = valomrade2 USE-INDEX BERNR NO-LOCK NO-ERROR.
      IF AVAILABLE beredningtemp THEN DO:
         ASSIGN
         FILL-IN-BERNR = beredningtemp.BERNR
         FILL-IN-AONR = beredningtemp.AONR
         FILL-IN-DELNR = beredningtemp.DELNR
         FILL-IN-BEN = beredningtemp.BENAMNING.
         DISPLAY FILL-IN-BERNR FILL-IN-AONR FILL-IN-DELNR FILL-IN-BEN WITH FRAME {&FRAME-NAME}.
         FOR EACH kon_val2 WHERE :
            DEFINE VARIABLE enval AS LOGICAL NO-UNDO.
            enval = FALSE.
            IF AVAILABLE kon_val2 THEN DO:
               IF kon_val2.GRUPP = 0 THEN enval = TRUE.   
               ELSE IF LOOKUP(STRING(kon_val2.GRUPP),Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.
            END.
            IF enval = TRUE OR kon_val2.KSKAP = FALSE THEN DO:
               IF kon_val2.ORD = 0 THEN DO:
                  FIND FIRST ordvaL WHERE ordvaL.NUM = kon_val2.NUM AND ordvaL.KSKAP = FALSE NO-LOCK NO-ERROR.
                  kon_val2.ORD = ordval.ORD.
               END.
            END.   
            ELSE DELETE kon_val2.
         END.
         OPEN QUERY BRW_VAL FOR EACH kon_val2 WHERE BY kon_val2.ORD BY kon_val2.NUM BY kon_val2.SKAPNUM .
         APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:
      avbvar = FALSE.
   END.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK C-Win
ON CHOOSE OF FBTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   {muswait.i}    
   FIND kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.   
   FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM NO-LOCK NO-ERROR.
   IF AVAILABLE list_mtrl THEN DO:    
      {MARKKABVAL.I}      
      FOR EACH spec_mtrl.         
         IF enval = TRUE THEN DO:                     
            FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND
            list_mtrl.ENR = spec_mtrl.ENR AND 
            list_mtrl.LEVKOD = spec_mtrl.LEVKOD AND list_mtrl.SKAPNUM = kon_val.SKAPNUM
            NO-LOCK NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST list_mtrl WHERE list_mtrl.NUM = kon_val.NUM AND
            list_mtrl.ENR = spec_mtrl.ENR AND 
            list_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-LOCK NO-ERROR.
         END.   
         IF NOT AVAILABLE list_mtrl THEN DO:
            CREATE list_mtrl.
            ASSIGN  
            list_mtrl.NUM = kon_val.NUM  
            list_mtrl.SKAPNUM = kon_val.SKAPNUM        
            list_mtrl.ENR = spec_mtrl.ENR
            list_mtrl.BENAMNING = spec_mtrl.BENAMNING
            list_mtrl.PRIS = spec_mtrl.NPRIS           
            list_mtrl.LEVKOD = spec_mtrl.LEVKOD
            list_mtrl.ANTAL = spec_mtrl.ANTAL               
            list_mtrl.ENHET = LC(spec_mtrl.ENHET)
            list_mtrl.LINKAB = spec_mtrl.LINKAB
            list_mtrl.MODUL = spec_mtrl.MODUL
            list_mtrl.TYPBER = spec_mtrl.TYPBER
            list_mtrl.DIAMETER = spec_mtrl.DIAMETER
            list_mtrl.SATS = spec_mtrl.SATS.
            IF enval = TRUE THEN DO:
               /* kon_rowid tappas om det hämtas upp nya poster, sätt tillbaka till samma värde som ingång i programmet = mtrl hamnar på rätt skapnum-kabel*/
               IF hjskapnum > 0 AND list_mtrl.SKAPNUM = 0 THEN DO:                  
                  list_mtrl.SKAPNUM = hjskapnum.
               END.
            END.      
         END.                                            
      END.     
   END.
   ELSE DO:
      FOR EACH spec_mtrl.
         CREATE list_mtrl.
         ASSIGN 
         list_mtrl.NUM = kon_val.NUM  
         list_mtrl.SKAPNUM = kon_val.SKAPNUM        
         list_mtrl.ENR = spec_mtrl.ENR
         list_mtrl.BENAMNING = spec_mtrl.BENAMNING
         list_mtrl.PRIS = spec_mtrl.NPRIS         
         list_mtrl.LEVKOD = spec_mtrl.LEVKOD
         list_mtrl.ANTAL = spec_mtrl.ANTAL            
         list_mtrl.ENHET = LC(spec_mtrl.ENHET)
         list_mtrl.LINKAB = spec_mtrl.LINKAB
         list_mtrl.MODUL = spec_mtrl.MODUL
         list_mtrl.TYPBER = spec_mtrl.TYPBER
         list_mtrl.DIAMETER = spec_mtrl.DIAMETER
         list_mtrl.SATS = spec_mtrl.SATS.
      END.                       
   END.      
   valnum = kon_val.NUM.
   {NETTOMARK.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
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
   IF VALID-HANDLE(kopimtrlapph) THEN DELETE PROCEDURE kopimtrlapph.
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
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN berhmt_UI IN kopimtrlapph (INPUT valaonr,INPUT valomrade,OUTPUT kalkrow2,OUTPUT TABLE beredningtemp).
   valdatum = datvar.
   FIND kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val  THEN DO:
      assign
      hjnum = kon_val.NUM
      hjskapnum = kon_val.SKAPNUM.
   END.       
   FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr) AND
   beredningtemp.OMRADE = valomrade USE-INDEX BERNR NO-LOCK NO-ERROR.
   IF AVAILABLE beredningtemp THEN DO:
      ASSIGN
      FILL-IN-BERNR = beredningtemp.BERNR
      FILL-IN-AONR = beredningtemp.AONR
      FILL-IN-DELNR = beredningtemp.DELNR
      FILL-IN-BEN = beredningtemp.BENAMNING
      valomrade2 = beredningtemp.OMRADE.
   END.
   EMPTY TEMP-TABLE kon_val2 NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl2 NO-ERROR.  
  
   OPEN QUERY kq FOR EACH kon_val NO-LOCK.
   GET FIRST kq NO-LOCK.
   DO WHILE AVAILABLE(kon_val):
      {MARKKABVAL.I}
      IF enval = TRUE OR kon_val.KSKAP = FALSE THEN DO:      
         CREATE kon_val2.
         ASSIGN
         kon_val2.GRUPP = kon_val.GRUPP
         kon_val2.F1 = kon_val.F1
         kon_val2.F2 = kon_val.F2   
         kon_val2.F3 = kon_val.F3
         kon_val2.F4 = kon_val.F4
         kon_val2.F5 = kon_val.F5
         kon_val2.F6 = kon_val.F6
         kon_val2.NUM = kon_val.NUM 
         kon_val2.ID = kon_val.ID      
         kon_val2.ID2 = kon_val.ID2
         kon_val2.UPPLAG = kon_val.UPPLAG
         kon_val2.KSKAP = kon_val.KSKAP
         kon_val2.TYP = kon_val.TYP
         kon_val2.SKAPNUM = kon_val.SKAPNUM
         kon_val2.ORD = kon_val.ORD
         kon_val2.EXTRA = kon_val.EXTRA
         kon_val2.EXTRA1 = kon_val.EXTRA1
         kon_val2.EXTRA2 = kon_val.EXTRA2.         
      END.
      GET NEXT kq NO-LOCK.
   END.
   CLOSE QUERY kq.
   FOR EACH kon_val2 WHERE kon_val2.ORD = 0 NO-LOCK:
      FIND FIRST kon_val WHERE kon_val.NUM = kon_val2.NUM AND kon_val.KSKAP = FALSE NO-LOCK NO-ERROR.
      kon_val2.ORD = kon_val.ORD.
   END.
   OPEN QUERY mq FOR EACH list_mtrl NO-LOCK.
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(list_mtrl):
      CREATE list_mtrl2.
      ASSIGN
      list_mtrl2.NUM = list_mtrl.NUM
      list_mtrl2.SKAPNUM = list_mtrl.SKAPNUM
      list_mtrl2.ENR = list_mtrl.ENR  
      list_mtrl2.BENAMNING = list_mtrl.BENAMNING 
      list_mtrl2.ENHET = LC(list_mtrl.ENHET) 
      list_mtrl2.ANTAL = list_mtrl.ANTAL 
      list_mtrl2.PRIS = list_mtrl.PRIS 
      list_mtrl2.LEVKOD = list_mtrl.LEVKOD
      list_mtrl2.LINKAB = list_mtrl.LINKAB 
      list_mtrl2.MODUL = list_mtrl.MODUL      
      list_mtrl2.TYPBER = list_mtrl.TYPBER      
      list_mtrl2.DIAMETER = list_mtrl.DIAMETER      
      list_mtrl2.SATS = list_mtrl.SATS.
      GET NEXT mq NO-LOCK.
   END.
   CLOSE QUERY mq.
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning */               
      EMPTY TEMP-TABLE elist_mtrl2 NO-ERROR.       
      FOR EACH list_mtrl2 WHERE list_mtrl2.num = valnum NO-LOCK:
         FIND FIRST elist_mtrl2 WHERE elist_mtrl2.ENR = list_mtrl2.ENR AND elist_mtrl2.LEVKOD = list_mtrl2.LEVKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE elist_mtrl2 THEN DO:         
            FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl2.LEVKOD AND kmtrltemp.ENR = list_mtrl2.ENR AND kmtrltemp.KALKNR = 0
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kmtrltemp THEN DO:
               CREATE elist_mtrl2.
               BUFFER-COPY list_mtrl2 TO elist_mtrl2.
            END.
         END.
      END.
      RUN linettomark_UI IN nettoh (INPUT TABLE elist_mtrl2, OUTPUT TABLE kmtrltemp APPEND).            
      EMPTY TEMP-TABLE elist_mtrl NO-ERROR. 
   END.
    
   OPEN QUERY BRW_VAL FOR EACH kon_val2 WHERE BY kon_val2.ORD BY kon_val2.NUM BY kon_val2.SKAPNUM .
   
   RUN enable_UI.   
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.   
   {FRMSIZE.I}  
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_MTRL2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN setcolindex_UI IN brwproc[2] (INPUT "ENR").
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).     
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOPIMTRLAPP.P PERSISTENT SET kopimtrlapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KOPIMTRLAPP.P PERSISTENT SET kopimtrlapph.
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
  DISPLAY FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_VAL BTN_ALLBACK FBTN_NYAO BRW_MTRL BRW_MTRL2 BTN_OVER BTN_BACK 
         BTN_ALLOVER FBTN_OK BTN_AVB FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR 
         FILL-IN-DELNR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrvalexc_UI C-Win 
PROCEDURE enrvalexc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: KÖRS EJ      
------------------------------------------------------------------------------*/
   RUN kollev_UI IN kopimtrlapph (INPUT vald_lev,OUTPUT vlevnamn). 
   FOR EACH mttemp:        
      RUN kollev_UI IN kopimtrlapph (INPUT mttemp.LEVKOD,OUTPUT lnamn).       
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
      EMPTY TEMP-TABLE elist_mtrl2 NO-ERROR.                
      IF oversatt = FALSE THEN RUN hmtskapuovers_UI IN kopimtrlapph (INPUT mttemp.ENR,INPUT vlev,OUTPUT TABLE elist_mtrl2).
      ELSE RUN hmtskap_UI IN kopimtrlapph (INPUT mttemp.ENR,INPUT vlev,OUTPUT TABLE elist_mtrl2). 
      FIND FIRST elist_mtrl2 NO-LOCK NO-ERROR.
      IF AVAILABLE elist_mtrl2 THEN DO:         
         ASSIGN
         antvar = INTEGER(mttemp.ANTAL).         
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
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI C-Win 
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
   IF brwname = "BRW_MTRL" THEN valbrw = 1.  
   IF brwname = "BRW_MTRL2" THEN valbrw = 2. 
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/   
   IF valbrw = 1 THEN DO:   
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = list_mtrl2.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = list_mtrl2.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   ELSE IF valbrw = 2 THEN DO:
      status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = spec_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over4_UI C-Win 
PROCEDURE over4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = elist_mtrl2.ENR AND spec_mtrl.LEVKOD = elist_mtrl2.LEVKOD 
   NO-LOCK NO-ERROR. 
   IF AVAILABLE spec_mtrl THEN DO:
      ASSIGN
      spec_rowid = ROWID(spec_mtrl).     
      /*inlagt 20121127 lena om två artiklar är översatta till samma artikel*/
      spec_mtrl.ANTAL = spec_mtrl.ANTAL + antvar.        
   END.      
   ELSE DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.NUM = kon_val.NUM
      spec_mtrl.LINKAB = FALSE
      spec_mtrl.ENR = elist_mtrl2.ENR
      spec_mtrl.BENAMNING = elist_mtrl2.BENAMNING
      spec_mtrl.ENHET = elist_mtrl2.ENHET
      spec_mtrl.ANTAL = antvar
      spec_mtrl.NPRIS = elist_mtrl2.PRIS       
      spec_mtrl.LEVKOD = elist_mtrl2.LEVKOD      
      spec_rowid = ROWID(spec_mtrl).
   END.    
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI C-Win 
PROCEDURE repo_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.  
   IF brwvar = 3 THEN DO:
      &Scoped-define BROWSE-NAME BRW_MTRL
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI C-Win 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.      
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 1).    
   IF brwh:NAME = "BRW_MTRL2" THEN RUN rowdispmtrl_UI (INPUT 2).             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI C-Win 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.   
   DEFINE VARIABLE repeatvar AS INTEGER NO-UNDO.
   IF Guru.Konstanter:varforetypval[29] = 1  THEN.
   ELSE RETURN.
   IF vad = 1 THEN DO:      
      IF AVAILABLE list_mtrl2 THEN DO:         
         REPEAT: 
            FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl2.LEVKOD AND kmtrltemp.ENR = list_mtrl2.ENR AND kmtrltemp.KALKNR = 0
            AND kmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
            IF AVAILABLE kmtrltemp THEN DO:                     
               list_mtrl2.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].                    
            END.
            ELSE DO:
               FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl2.LEVKOD AND kmtrltemp.ENR = list_mtrl2.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:                     
                  list_mtrl2.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].                       
               END.
            END.
            IF AVAILABLE kmtrltemp THEN LEAVE.
            IF  repeatvar < 1 THEN repeatvar = 1.
            ELSE LEAVE.
            RUN enrhmt_UI IN nettoh (INPUT list_mtrl2.LEVKOD,INPUT list_mtrl2.ENR, OUTPUT TABLE kmtrltemp APPEND).
         END.   
      END.
   END.   
   IF vad = 2  THEN DO:
      IF AVAILABLE spec_mtrl THEN DO:         
         REPEAT: 
            FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = spec_mtrl.LEVKOD AND kmtrltemp.ENR = spec_mtrl.ENR AND kmtrltemp.KALKNR = 0
            AND kmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
            IF AVAILABLE kmtrltemp THEN DO:                     
               spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL2 = Guru.Konstanter:varforetypval[28].          
            END.
            ELSE DO:         
               FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = spec_mtrl.LEVKOD AND kmtrltemp.ENR = spec_mtrl.ENR AND kmtrltemp.KALKNR = 0
               AND kmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
               IF AVAILABLE kmtrltemp THEN DO:                     
                  spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL2 = Guru.Konstanter:varforetypval[38].          
               END.
            END.
            IF AVAILABLE kmtrltemp THEN LEAVE.
            IF  repeatvar < 1 THEN repeatvar = 1.
            ELSE LEAVE.
            RUN enrhmt_UI IN nettoh (INPUT spec_mtrl.LEVKOD,INPUT spec_mtrl.ENR, OUTPUT TABLE kmtrltemp APPEND).
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI C-Win 
PROCEDURE vcbrwval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   DEFINE VARIABLE valnum AS INTEGER NO-UNDO.
   FIND FIRST kon_val2 NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val2 THEN DO:
      ASSIGN
      status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp  THEN DO:
         ASSIGN
         kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,15) 
         kon_val2.F2:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B2,1,15)  
         kon_val2.F3:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B3,1,15)
         kon_val2.F4:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B4,1,15)      
         kon_val2.F5:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B5,1,15)      
         kon_val2.F6:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B6,1,15).
      END.         
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      FIND FIRST list_mtrl2 WHERE list_mtrl2.NUM = kon_val2.NUM  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE list_mtrl2 THEN DO:
         valnum = kon_val2.NUM.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR. 
         FOR EACH kon_val2 WHERE kon_val2.NUM = valnum:
            CREATE sparakon_val.
            BUFFER-COPY kon_val2 TO sparakon_val.
            valnum = kon_val2.NUM.
         END.   
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN BERHMTKONMTRL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT beredningtemp.BERNR, INPUT beredningtemp.OMRADE, INPUT valdatum,INPUT valnum, INPUT "", INPUT TABLE sparakon_val, 
            OUTPUT TABLE list_mtrl2 APPEND).
         END.
         ELSE DO:
            RUN BERHMTKONMTRL.P
            (INPUT beredningtemp.BERNR, INPUT beredningtemp.OMRADE, INPUT valdatum,INPUT valnum, INPUT "", INPUT TABLE sparakon_val, 
            OUTPUT TABLE list_mtrl2 APPEND).
         END.
         IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning */               
            EMPTY TEMP-TABLE elist_mtrl2 NO-ERROR.       
            FOR EACH list_mtrl2 WHERE list_mtrl2.num = valnum NO-LOCK:
               FIND FIRST elist_mtrl2 WHERE elist_mtrl2.ENR = list_mtrl2.ENR AND elist_mtrl2.LEVKOD = list_mtrl2.LEVKOD NO-LOCK NO-ERROR.
               IF NOT AVAILABLE elist_mtrl2 THEN DO:         
                  FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = list_mtrl2.LEVKOD AND kmtrltemp.ENR = list_mtrl2.ENR AND kmtrltemp.KALKNR = 0
                  NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE kmtrltemp THEN DO:
                     CREATE elist_mtrl2.
                     BUFFER-COPY list_mtrl2 TO elist_mtrl2.
                  END.
               END.
            END.            
            RUN linettomark_UI IN nettoh (INPUT TABLE elist_mtrl2, OUTPUT TABLE kmtrltemp APPEND).            
            EMPTY TEMP-TABLE elist_mtrl NO-ERROR.             
         END.
         EMPTY TEMP-TABLE sparakon_val NO-ERROR.          
         FIND FIRST kon_val2 WHERE kon_val2.NUM = valnum AND kon_val2.KSKAP = FALSE 
         NO-LOCK NO-ERROR.   
      END.
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}        
   END.
   ELSE DO:
      ASSIGN      
      kon_val2.F1:LABEL IN BROWSE BRW_VAL = "" 
      kon_val2.F2:LABEL IN BROWSE BRW_VAL = ""  
      kon_val2.F3:LABEL IN BROWSE BRW_VAL = ""
      kon_val2.F4:LABEL IN BROWSE BRW_VAL = ""      
      kon_val2.F5:LABEL IN BROWSE BRW_VAL = ""      
      kon_val2.F6:LABEL IN BROWSE BRW_VAL = "".
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.
   
   DEFINE VARIABLE enval AS LOGICAL NO-UNDO.
   enval = FALSE.
   IF AVAILABLE kon_val2 THEN DO:
      IF kon_val2.GRUPP = 0 THEN enval = TRUE.   
      ELSE IF LOOKUP(STRING(kon_val2.GRUPP),Guru.Konstanter:varforetypchar[5]) > 0 THEN enval = TRUE.
      IF enval = TRUE THEN DO:
         OPEN QUERY BRW_MTRL FOR EACH list_mtrl2 WHERE list_mtrl2.NUM = kon_val2.NUM AND list_mtrl2.SKAPNUM = kon_val2.SKAPNUM  
         USE-INDEX NUM NO-LOCK.  
      END.
      ELSE DO:
         OPEN QUERY BRW_MTRL FOR EACH list_mtrl2 WHERE list_mtrl2.NUM = kon_val2.NUM 
         USE-INDEX NUM NO-LOCK.
      END.     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

