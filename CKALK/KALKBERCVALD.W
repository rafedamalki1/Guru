&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: KALKBERCVALD.W   NÄR MAN SKAPAR EN KALKYL AV BEREDNINGEN
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
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{SCADMIN.I}
{KONVALTEMP.I}
{BERIDTEMP.I}
{BBENAMNTEMP.I}
{KONVAL2TEMP.I}
DEFINE INPUT  PARAMETER valaonr AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER valdelnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER togsum AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER togmtrl AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER togkonval AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER kbename AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR kon_val.
DEFINE OUTPUT PARAMETER TABLE FOR skrivhdschakttemp.

/* Local Variable Definitions ---                                       */

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE orgheigtf AS DECIMAL NO-UNDO.
DEFINE VARIABLE orgheigtfk AS DECIMAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.     
DEFINE VARIABLE valdatum AS DATE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE ordning AS INTEGER NO-UNDO.
DEFINE VARIABLE kombnr AS INTEGER NO-UNDO.
DEFINE VARIABLE valkonstapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ktypkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bernumvar AS INTEGER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE datvar99 AS DATE NO-UNDO.
DEFINE BUFFER idbuff FOR beridtemp.
&Scoped-define NEW

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_MTRL2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kon_val skrivhdschakttemp kon_val2 ~
hdschakttemp

/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 kon_val.F1 kon_val.ID1 kon_val.ID2 ~
kon_val.EXTRA1 kon_val.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 kon_val


/* Definitions for BROWSE BRW_SKRIVVAL                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_SKRIVVAL skrivhdschakttemp.SID ~
skrivhdschakttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SKRIVVAL 
&Scoped-define QUERY-STRING-BRW_SKRIVVAL FOR EACH skrivhdschakttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SKRIVVAL OPEN QUERY BRW_SKRIVVAL FOR EACH skrivhdschakttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SKRIVVAL skrivhdschakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SKRIVVAL skrivhdschakttemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.ORD kon_val2.F1 ~
kon_val2.ID1 kon_val2.ID2 kon_val2.EXTRA1 kon_val2.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for BROWSE BRW_VAL-2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL-2 hdschakttemp.SID ~
hdschakttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL-2 
&Scoped-define QUERY-STRING-BRW_VAL-2 FOR EACH hdschakttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL-2 OPEN QUERY BRW_VAL-2 FOR EACH hdschakttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL-2 hdschakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL-2 hdschakttemp


/* Definitions for FRAME FRAME-KONST                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_SUM FILL-IN-KB TOG_MTRL TOG_VALKONST ~
BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS TOG_SUM FILL-IN-KB TOG_MTRL TOG_VALKONST 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-KB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kalkyl benämning" 
     VIEW-AS FILL-IN 
     SIZE 45.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Ta med materiel från berdeningen som Eget Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_SUM AS LOGICAL INITIAL no 
     LABEL "Summera kalkylkoder" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_VALKONST AS LOGICAL INITIAL no 
     LABEL "Bara valda konstruktoner" 
     VIEW-AS TOGGLE-BOX
     SIZE 54 BY .88 NO-UNDO.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BACK-2 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "Ta bort" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_OKVAL AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_OVER-2 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "Skapa" 
     SIZE 4 BY 1.21.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL2 FOR 
      kon_val SCROLLING.

DEFINE QUERY BRW_SKRIVVAL FOR 
      skrivhdschakttemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.

DEFINE QUERY BRW_VAL-2 FOR 
      hdschakttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 Dialog-Frame _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 13
      kon_val.ID1 FORMAT "x(256)":U WIDTH 10
      kon_val.ID2 FORMAT "X(256)":U WIDTH 5
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
      kon_val.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 53 BY 13
         TITLE "Konstruktioner till Kalkyl".

DEFINE BROWSE BRW_SKRIVVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SKRIVVAL Dialog-Frame _STRUCTURED
  QUERY BRW_SKRIVVAL NO-LOCK DISPLAY
      skrivhdschakttemp.SID COLUMN-LABEL "Nr" FORMAT ">>>9":U
      skrivhdschakttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 53 BY 8
         TITLE "Schakt till Kalkyl" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL Dialog-Frame _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.ORD COLUMN-LABEL "Skriv!ord" FORMAT ">>>9":U WIDTH 4
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 11
      kon_val2.ID1 FORMAT "x(256)":U WIDTH 8
      kon_val2.ID2 FORMAT "X(256)":U WIDTH 5
      kon_val2.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
      kon_val2.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 13
         TITLE "Byggnummer".

DEFINE BROWSE BRW_VAL-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL-2 Dialog-Frame _STRUCTURED
  QUERY BRW_VAL-2 NO-LOCK DISPLAY
      hdschakttemp.SID COLUMN-LABEL "Nr" FORMAT ">>>9":U
      hdschakttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 8
         TITLE "Schakt" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     TOG_SUM AT ROW 2.88 COL 2.5 WIDGET-ID 2
     FILL-IN-KB AT ROW 2.88 COL 67.5 COLON-ALIGNED WIDGET-ID 42
     TOG_MTRL AT ROW 3.63 COL 2.5 WIDGET-ID 26
     TOG_VALKONST AT ROW 4.33 COL 2.5 WIDGET-ID 44
     BTN_OK AT ROW 5.67 COL 110.5
     "Glöm inte omvandla utbyte massor till kubikmeter!" VIEW-AS TEXT
          SIZE 53.5 BY 1.17 AT ROW 1.25 COL 2
          FONT 17
     SPACE(71.62) SKIP(27.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Skapa kalkyl av beredning"
         DEFAULT-BUTTON BTN_OK.

DEFINE FRAME FRAME-KONST
     BRW_VAL AT ROW 2.54 COL 1.5 WIDGET-ID 400
     BRW_MTRL2 AT ROW 2.54 COL 60.5 WIDGET-ID 300
     btn_over AT ROW 6.96 COL 56.25 WIDGET-ID 48
     btn_back AT ROW 10.46 COL 56.25 WIDGET-ID 46
     BRW_VAL-2 AT ROW 16.04 COL 1.5 WIDGET-ID 200
     BRW_SKRIVVAL AT ROW 16.04 COL 60.5 WIDGET-ID 100
     BTN_OVER-2 AT ROW 19.29 COL 56.25 WIDGET-ID 40
     BTN_BACK-2 AT ROW 21.5 COL 56.25 WIDGET-ID 38
     BTN_OKVAL AT ROW 24.08 COL 110 WIDGET-ID 50
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 5.29
         SIZE 124 BY 24.24 WIDGET-ID 500.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: hdschakttemp T "?" NO-UNDO temp-db hdschakttemp
      TABLE: kon_val T "?" NO-UNDO temp-db kon_val
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
      TABLE: skrivhdschakttemp T "?" NO-UNDO temp-db skrivhdschakttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-KONST:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-KONST
                                                                        */
/* BROWSE-TAB BRW_VAL 1 FRAME-KONST */
/* BROWSE-TAB BRW_MTRL2 BRW_VAL FRAME-KONST */
/* BROWSE-TAB BRW_VAL-2 btn_back FRAME-KONST */
/* BROWSE-TAB BRW_SKRIVVAL BRW_VAL-2 FRAME-KONST */
ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME FRAME-KONST                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME FRAME-KONST         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KONST = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME FRAME-KONST         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KONST = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val.ID1
"kon_val.ID1" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val.EXTRA1
"kon_val.EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val.UPPLAG
"kon_val.UPPLAG" "Upplag" ">>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SKRIVVAL
/* Query rebuild information for BROWSE BRW_SKRIVVAL
     _TblList          = "Temp-Tables.skrivhdschakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.skrivhdschakttemp.SID
"skrivhdschakttemp.SID" "Nr" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.skrivhdschakttemp.BENAMNING
"skrivhdschakttemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SKRIVVAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.ORD
"kon_val2.ORD" "Skriv!ord" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val2.ID1
"kon_val2.ID1" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val2.EXTRA1
"kon_val2.EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val2.UPPLAG
"kon_val2.UPPLAG" "Upplag" ">>>" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL-2
/* Query rebuild information for BROWSE BRW_VAL-2
     _TblList          = "Temp-Tables.hdschakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.hdschakttemp.SID
"hdschakttemp.SID" "Nr" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.hdschakttemp.BENAMNING
"hdschakttemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Skapa kalkyl av beredning */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(valkonstapph) THEN DELETE PROCEDURE valkonstapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
&Scoped-define FRAME-NAME FRAME-KONST
&Scoped-define SELF-NAME BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 Dialog-Frame
ON VALUE-CHANGED OF BRW_MTRL2 IN FRAME FRAME-KONST /* Konstruktioner till Kalkyl */
DO:
   {muswait.i}
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_VAL IN FRAME FRAME-KONST /* Byggnummer */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL Dialog-Frame
ON VALUE-CHANGED OF BRW_VAL IN FRAME FRAME-KONST /* Byggnummer */
DO:
   {muswait.i}
   status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back Dialog-Frame
ON CHOOSE OF btn_back IN FRAME FRAME-KONST
DO:   
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).                      
      DELETE kon_val.   
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&RIGHT-BROWSE}].
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].   
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   {muswait.i} 
   FILL-IN-KB = INPUT FILL-IN-KB.
   FILL-IN-KB:SCREEN-VALUE = TRIM(FILL-IN-KB:SCREEN-VALUE).
  
   RUN AONRNAMNKOLL.P (INPUT 5, INPUT FILL-IN-KB:SCREEN-VALUE,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.  
   ASSIGN
   kbename = FILL-IN-KB
   togsum = INPUT TOG_SUM
   togmtrl = INPUT TOG_MTRL
   togkonval = INPUT TOG_VALKONST.
    {BORTBRWPROC.I}
   IF VALID-HANDLE(valkonstapph) THEN DELETE PROCEDURE valkonstapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-KONST
&Scoped-define SELF-NAME BTN_OKVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OKVAL Dialog-Frame
ON CHOOSE OF BTN_OKVAL IN FRAME FRAME-KONST /* Ok */
DO:
   {muswait.i} 
   FILL-IN-KB = INPUT FRAME Dialog-Frame FILL-IN-KB .
   FILL-IN-KB = TRIM(FILL-IN-KB).
   DISPLAY FILL-IN-KB WITH FRAME {&FRAME-NAME}.
   
   RUN AONRNAMNKOLL.P (INPUT 5, INPUT FILL-IN-KB:SCREEN-VALUE,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.  
   ASSIGN
   kbename = FILL-IN-KB
   togsum = INPUT TOG_SUM
   togmtrl = INPUT TOG_MTRL
   togkonval = INPUT TOG_VALKONST.
    {BORTBRWPROC.I}
   IF VALID-HANDLE(valkonstapph) THEN DELETE PROCEDURE valkonstapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over Dialog-Frame
ON CHOOSE OF btn_over IN FRAME FRAME-KONST
DO:      
   antal_valda = BRW_VAL:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
         FIND FIRST kon_val WHERE kon_val.BERAONR = kon_val2.BERAONR AND 
         kon_val.OMRADE = kon_val2.OMRADE AND kon_val.NUM = kon_val2.NUM AND kon_val.UPPLAG = kon_val2.UPPLAG
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kon_val THEN DO:
            ordning = ordning + 1.
            CREATE kon_val. 
            BUFFER-COPY kon_val2 TO kon_val.
             DELETE kon_val2. 
            spec_rowid = ROWID(kon_val).
         END.
         ELSE DO:
            spec_rowid = ROWID(kon_val).
         END.
         IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&LEFT-BROWSE}].                   
         antal_raknare = antal_raknare + 1.
      END.
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
      antal_raknare = 0.
      GET FIRST BRW_MTRL2.
      DO WHILE AVAILABLE(kon_val):
         antal_raknare = antal_raknare + 1.
         kon_val.ORD = antal_raknare.
         GET NEXT BRW_MTRL2.
      END.
      FIND FIRST kon_val WHERE ROWID(kon_val) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE kon_val THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      END.
      RUN vcbrwval_UI.
      RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].           
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
   END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME FILL-IN-KB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KB Dialog-Frame
ON LEAVE OF FILL-IN-KB IN FRAME Dialog-Frame /* Kalkyl benämning */
DO:
  FILL-IN-KB = input FILL-IN-KB.
 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VALKONST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VALKONST Dialog-Frame
ON VALUE-CHANGED OF TOG_VALKONST IN FRAME Dialog-Frame /* Bara valda konstruktoner */
DO:
   TOG_VALKONST = INPUT TOG_VALKONST.
   IF TOG_VALKONST = TRUE THEN DO:
      /*
      BRW_VAL:HIDDEN  = FALSE.
      BRW_MTRL2:HIDDEN = FALSE.
      btn_over:HIDDEN = FALSE.
      btn_back:HIDDEN = FALSE.
      BRW_VAL-2:HIDDEN  = FALSE.
      BRW_SKRIVVAL:HIDDEN = FALSE.
      btn_over-2:HIDDEN = FALSE.
      btn_back-2:HIDDEN = FALSE.
      */
      FRAME {&FRAME-NAME}:HEIGHT-CHARS = orgheigtf.
      FRAME FRAME-KONST:HEIGHT-CHARS = orgheigtfk.
      FRAME FRAME-KONST:HIDDEN = FALSE.
      BTN_OK:HIDDEN IN FRAME Dialog-Frame = TRUE. 
   END.
   ELSE DO:
      /*
      BRW_VAL:HIDDEN = TRUE.
      BRW_MTRL2:HIDDEN = TRUE.
      btn_over:HIDDEN = TRUE.
      btn_back:HIDDEN = TRUE.
      BRW_VAL-2:HIDDEN  = TRUE.
      BRW_SKRIVVAL:HIDDEN = TRUE.
      btn_over-2:HIDDEN = TRUE.
      btn_back-2:HIDDEN = TRUE.
      */
      FRAME FRAME-KONST:HIDDEN = TRUE.
       FRAME FRAME-KONST:HEIGHT-CHARS = 2.
      FRAME {&FRAME-NAME}:HEIGHT-CHARS = BTN_OK:ROW + 5.
      BTN_OK:HIDDEN IN FRAME Dialog-Frame = FALSE. 
   END.   
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
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
      DEBUGGER:SET-BREAK().
   
   {DIA_M_START.I}
   IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      BRW_VAL:TITLE = "Byggnummer".

   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ASSIGN
       
      BRW_VAL:TITLE = "Objekt".

   END.
   ELSE DO:
      ASSIGN
      BRW_VAL:TITLE = "Konstruktioner".

   END.
   FRAME Dialog-Frame:TITLE   = "Skapa Kalkyl".
   {muswait.i}
  
   {ALLSTARTDYN.I}
  
   EMPTY TEMP-TABLE kon_val NO-ERROR.
   EMPTY TEMP-TABLE kon_val2 NO-ERROR. 
   EMPTY TEMP-TABLE skrivhdschakttemp NO-ERROR.
   EMPTY TEMP-TABLE hdschakttemp NO-ERROR.
   
   RUN lkon_val_UI IN valkonstapph (INPUT valaonr,INPUT valdelnr, OUTPUT FILL-IN-KB, OUTPUT TABLE kon_val2, OUTPUT TABLE hdschakttemp).   
   
   RUN extrakon_UI.
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[3].
   
   FIND LAST kon_val USE-INDEX ORD NO-LOCK NO-ERROR.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
   END.
   ELSE DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
   END.
   IF Guru.Konstanter:varforetypval[32] = 0 THEN.
   ELSE DO:
      ASSIGN kon_val2.ID1:VISIBLE IN BROWSE BRW_VAL = FALSE.
      ASSIGN kon_val.ID1:VISIBLE IN BROWSE BRW_MTRL2 = FALSE.
   END.  
   TOG_VALKONST:LABEL = "Välj konstruktoner och schakt!".      
   RUN enable_UI.    
   
   {FRMSIZEDF.I}
   TOG_VALKONST = FALSE.
   DISPLAY TOG_VALKONST WITH FRAME {&FRAME-NAME}.  
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME FRAME-KONST.
   APPLY "VALUE-CHANGED" TO BRW_MTRL2 IN FRAME FRAME-KONST.
   APPLY "VALUE-CHANGED" TO BRW_VAL-2 IN FRAME FRAME-KONST.
   APPLY "VALUE-CHANGED" TO BRW_SKRIVVAL IN FRAME FRAME-KONST.
   
   
   
   RUN resetcolindex_UI IN brwproc[{&LEFT-BROWSE}].
/*
   Guru.GlobalaVariabler:collefth = BTN_AVB:HANDLE.
  */
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_OK:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   
   {musarrow.i}
   {DIA_M_SLUT.I}
   orgheigtf = FRAME {&FRAME-NAME}:HEIGHT-CHARS.
   orgheigtfk = FRAME FRAME-KONST:HEIGHT-CHARS.
    
    
   TOG_VALKONST = FALSE.
   APPLY "VALUE-CHANGED" TO TOG_VALKONST IN FRAME {&FRAME-NAME}.
   DISPLAY TOG_VALKONST WITH FRAME {&FRAME-NAME}.  
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_VAL:HANDLE IN FRAME FRAME-KONST).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_MTRL2:HANDLE IN FRAME FRAME-KONST).
   RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "KSKAP = FALSE").
   RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "ORD").
   RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_VAL-2:HANDLE IN FRAME FRAME-KONST).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_SKRIVVAL:HANDLE IN FRAME FRAME-KONST).
   RUN DYNARROW.P PERSISTENT SET brwproc[5] (INPUT BRW_VAL-2:HANDLE,INPUT BRW_SKRIVVAL:HANDLE,
                                                     INPUT BTN_OVER-2:HANDLE, INPUT ?,
                                                     INPUT ?, INPUT BTN_BACK-2:HANDLE).   
   RUN deleteleftbrw_UI IN brwproc[5] (INPUT TRUE).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALKONSTAPP.P PERSISTENT SET valkonstapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALKONSTAPP.P PERSISTENT SET valkonstapph.
   END. 
   tthandle = TEMP-TABLE bbenamntemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
    
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
  HIDE FRAME FRAME-KONST.
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
  DISPLAY TOG_SUM FILL-IN-KB TOG_MTRL TOG_VALKONST 
      WITH FRAME Dialog-Frame.
  ENABLE TOG_SUM FILL-IN-KB TOG_MTRL TOG_VALKONST BTN_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  ENABLE BRW_VAL BRW_MTRL2 btn_over btn_back BRW_VAL-2 BRW_SKRIVVAL BTN_OVER-2 
         BTN_BACK-2 BTN_OKVAL 
      WITH FRAME FRAME-KONST.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extrakon_UI Dialog-Frame 
PROCEDURE extrakon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval2_UI Dialog-Frame 
PROCEDURE vcbrwval2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE kon_val2 THEN DO:
      /*
      RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(kon_val2)).
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].      
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_val2.F1:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.F1:LABEL," v") = 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val2.F1:LABEL," ^") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val2.F1:LABEL," v") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " v".
         IF bbenamntemp.ID1 NE "" THEN DO:
            IF INDEX(kon_val2.ID1:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID1:LABEL," v") = 0 THEN kon_val2.ID1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.ID1,1,20).      
            ELSE IF INDEX(kon_val2.ID1:LABEL," ^") NE 0 THEN kon_val2.ID1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.ID1,1,20) + " ^".      
            ELSE IF INDEX(kon_val2.ID1:LABEL," v") NE 0 THEN kon_val2.ID1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.ID1,1,20) + " v".
         END.         
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id" + " v".      
      END.
      
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}            
   END.
   ELSE DO:
      /*
      ASSIGN      
      kon_val2.F1:LABEL IN BROWSE BRW_VAL = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      */
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI Dialog-Frame 
PROCEDURE vcbrwval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF AVAILABLE kon_val THEN DO:
      /*
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.      
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_val.F1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.F1:LABEL," v") = 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val.F1:LABEL," ^") NE 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val.F1:LABEL," v") NE 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " v".
         IF bbenamntemp.ID1 NE "" THEN DO:
            IF INDEX(kon_val.ID1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID1:LABEL," v") = 0 THEN kon_val.ID1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.ID1,1,20).      
            ELSE IF INDEX(kon_val.ID1:LABEL," ^") NE 0 THEN kon_val.ID1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.ID1,1,20) + " ^".      
            ELSE IF INDEX(kon_val.ID1:LABEL," v") NE 0 THEN kon_val.ID1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.ID1,1,20) + " v".
         END.               
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF INDEX(kon_val.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID2:LABEL," v") = 0 
            THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
         ELSE IF INDEX(kon_val.ID2:LABEL," ^") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " ^".      
         ELSE IF INDEX(kon_val.ID2:LABEL," v") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(kon_val.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID2:LABEL," v") = 0 
            THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
         ELSE IF INDEX(kon_val.ID2:LABEL," ^") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " ^".      
         ELSE IF INDEX(kon_val.ID2:LABEL," v") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(kon_val.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID2:LABEL," v") = 0 
            THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
         ELSE IF INDEX(kon_val.ID2:LABEL," ^") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " ^".      
         ELSE IF INDEX(kon_val.ID2:LABEL," v") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " v".      
      END.
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}         
   END.
   ELSE DO:
       /*
      ASSIGN      
      kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
      END.
      */
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

