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
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{BBENAMNTEMP.I}
&Scoped-define SHARED SHARED
{KONVALTEMP.I}
{KONSTGRUPP.I}
{KONID.I} 
&Scoped-define NEW 
&Scoped-define SHARED
{KONVAL2TEMP.I}

DEFINE TEMP-TABLE flygnrtemp NO-UNDO LIKE kon_val2
   FIELD ENR AS CHARACTER.

DEFINE OUTPUT PARAMETER TABLE FOR flygnrtemp.
/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_MTRL2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES flygnrtemp kon_val2

/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 flygnrtemp.F1 flygnrtemp.ID2 ~
flygnrtemp.Enr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH flygnrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH flygnrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 flygnrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 flygnrtemp


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.F1 kon_val2.ID2 ~
kon_val2.EXTRA1 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_VALORD FILL-IN-FREKVENS BRW_VAL ~
BRW_MTRL2 btn_over btn_back FBTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_VALORD FILL-IN-FREKVENS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_VALORD AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FREKVENS AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Frekvens" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL2 FOR 
      flygnrtemp SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 C-Win _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      flygnrtemp.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U WIDTH 14
      flygnrtemp.ID2 FORMAT "X(10)":U WIDTH 13.38
      flygnrtemp.Enr FORMAT "X(11)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 44.5 BY 19.75
         TITLE "Valda byggnummer".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U WIDTH 14
      kon_val2.ID2 FORMAT "X(10)":U WIDTH 13.38
      kon_val2.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 44.5 BY 19.75
         TITLE "Byggnummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_VALORD AT ROW 2.5 COL 11 NO-LABEL
     FILL-IN-FREKVENS AT ROW 3.88 COL 1
     BRW_VAL AT ROW 5.54 COL 1.5
     BRW_MTRL2 AT ROW 5.54 COL 50.25
     btn_over AT ROW 11.46 COL 46.13
     btn_back AT ROW 14.96 COL 46.13
     FBTN_OK AT ROW 26.75 COL 65.75
     BTN_AVB AT ROW 26.75 COL 80.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.25 BY 27.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: flygnrtemp T "?" NO-UNDO temp-db flygnrtemp
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Konstruktioner med flygnummerskylt"
         HEIGHT             = 27.25
         WIDTH              = 95.5
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 116.5
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 116.5
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
/* BROWSE-TAB BRW_VAL FILL-IN-FREKVENS DEFAULT-FRAME */
/* BROWSE-TAB BRW_MTRL2 BRW_VAL DEFAULT-FRAME */
ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VALORD IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-FREKVENS IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.flygnrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.flygnrtemp.F1
"flygnrtemp.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.flygnrtemp.ID2
"flygnrtemp.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.flygnrtemp.Enr
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kon_val2.EXTRA1
"kon_val2.EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Konstruktioner med flygnummerskylt */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Konstruktioner med flygnummerskylt */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = TRUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
&Scoped-define SELF-NAME BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 C-Win
ON VALUE-CHANGED OF BRW_MTRL2 IN FRAME DEFAULT-FRAME /* Valda byggnummer */
DO:   
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VAL IN FRAME DEFAULT-FRAME /* Byggnummer */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME /* Byggnummer */
DO:   
   status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   musz = TRUE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back C-Win
ON CHOOSE OF btn_back IN FRAME DEFAULT-FRAME
DO:   
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE flygnrtemp THEN DELETE flygnrtemp.
      antal_raknare = antal_raknare + 1.   
   END.
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   RUN refreshbrw_UI IN brwproc[2].

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over C-Win
ON CHOOSE OF btn_over IN FRAME DEFAULT-FRAME
DO:      
   antal_valda = BRW_VAL:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE  kon_val2 THEN DO:
            FIND FIRST flygnrtemp WHERE flygnrtemp.BERAONR = kon_val2.BERAONR AND 
            flygnrtemp.OMRADE = kon_val2.OMRADE AND flygnrtemp.NUM = kon_val2.NUM
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE flygnrtemp THEN DO:
               CREATE flygnrtemp. 
               BUFFER-COPY kon_val2 TO flygnrtemp.
            END.
            RUN flygnr_UI (INPUT flygnrtemp.ID2,OUTPUT flygnrtemp.ENR).
            IF flygnrtemp.ENR = "" THEN DELETE flygnrtemp.
         END.
         antal_raknare = antal_raknare + 1.
      END.
      RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
      RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
      antal_raknare = 0.      
      FIND FIRST flygnrtemp WHERE ROWID(flygnrtemp) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE flygnrtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(flygnrtemp)).
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      END.
      RUN vcbrwval_UI.
      status-ok = BRW_VAL:DESELECT-ROWS().
      APPLY "HOME" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VALORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALORD C-Win
ON VALUE-CHANGED OF CMB_VALORD IN FRAME DEFAULT-FRAME
DO:
   RUN cmbval_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK C-Win
ON CHOOSE OF FBTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   DEFINE VARIABLE varextra AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varbernr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varomrber AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varnum AS INTEGER NO-UNDO.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FREKVENS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREKVENS C-Win
ON ANY-KEY OF FILL-IN-FREKVENS IN FRAME DEFAULT-FRAME /* Frekvens */
DO:
   {TRYCKS.I}   
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN APPLY "ENTRY" TO FILL-IN-FREKVENS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREKVENS C-Win
ON ENTRY OF FILL-IN-FREKVENS IN FRAME DEFAULT-FRAME /* Frekvens */
DO:
   APPLY "LEAVE" TO FILL-IN-FREKVENS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREKVENS C-Win
ON LEAVE OF FILL-IN-FREKVENS IN FRAME DEFAULT-FRAME /* Frekvens */
DO:
   DEFINE VARIABLE hjidvar AS INTEGER NO-UNDO.
   FILL-IN-FREKVENS = INPUT FILL-IN-FREKVENS.
   RUN avmark_UI IN brwproc[{&LEFT-BROWSE}].
   RUN getfirst_UI IN brwproc[{&LEFT-BROWSE}].
   antal_raknare = 1.        
   IF antal_raknare = FILL-IN-FREKVENS THEN DO:
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
   END.
   REPEAT:
      antal_raknare = antal_raknare + 1.      
      IF antal_raknare > FILL-IN-FREKVENS THEN DO:
         antal_raknare = 1.
      END.      
      RUN getnext_UI IN brwproc[{&LEFT-BROWSE}].
      IF NOT AVAILABLE kon_val2 THEN LEAVE.
      IF antal_raknare = FILL-IN-FREKVENS  THEN DO:
         antal_raknare = 0.
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FREKVENS C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FREKVENS IN FRAME DEFAULT-FRAME /* Frekvens */
DO:
   APPLY "ENTRY" TO FILL-IN-FREKVENS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
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
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
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
   FILL-IN-FREKVENS = 1.
   IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda byggnummer" 
      BRW_VAL:TITLE = "Byggnummer".
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda objekt" 
      BRW_VAL:TITLE = "Objekt".
   END.
   ELSE DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda konstruktioner" 
      BRW_VAL:TITLE = "Konstruktioner".
      
   END.
   DEFINE VARIABLE idhj AS INTEGER NO-UNDO.
   FOR EACH kon_val WHERE kon_val.KSKAP = FALSE AND kon_val.ID2 NE "":
      idhj = 0.
      idhj = INTEGER(kon_val.ID2) NO-ERROR.
      IF idhj > 0 THEN DO:
         CREATE kon_val2.
         BUFFER-COPY kon_val TO kon_val2.
      END.   
   END.
   FIND FIRST kon_val2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kon_val2 THEN DO:
      MESSAGE "Du har inga identifierade konstruktioner" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   CMB_VALORD:LIST-ITEMS = "".
   FOR EACH konstgrptemp  USE-INDEX ORD NO-LOCK:
      FIND FIRST kon_id WHERE kon_id.GRUPP = konstgrptemp.KONSKOD 
      AND kon_id.ENDKOMB = FALSE NO-LOCK NO-ERROR.
      IF AVAILABLE kon_id THEN DO:
         IF kon_id.GRUPP NE 0 THEN DO:
            ASSIGN
            status-ok = CMB_VALORD:ADD-LAST(konstgrptemp.BENAMNING) 
            CMB_VALORD:SCREEN-VALUE = konstgrptemp.BENAMNING.   
         END.         
      END.
   END.         
   {muswait.i}
   {ALLSTARTDYN.I}
   {BERTITLE.I}
   IF CMB_VALORD:SCREEN-VALUE = ? THEN DO:
      MESSAGE "Du har inga valda konstruktioner som kan användas i denna funktion!"
      VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.
   END.
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
   END.
   ELSE DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
   END.
   IF flygnrtemp.F1:LABEL IN BROWSE BRW_MTRL2 = "F1" THEN DO:
      flygnrtemp.F1:LABEL IN BROWSE BRW_MTRL2 = kon_val2.F1:LABEL IN BROWSE BRW_VAL.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}  
   FILL-IN-FREKVENS:HIDDEN = TRUE.
   APPLY "VALUE-CHANGED" TO CMB_VALORD.
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_MTRL2 IN FRAME {&FRAME-NAME}.   
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
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_MTRL2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN brwsetupstop_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[{&LEFT-BROWSE}] (INPUT 1).
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").   
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cmbval_UI C-Win 
PROCEDURE cmbval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VALORD:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.    
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE kon_val2.GRUPP = '" + STRING(konstgrptemp.KONSKOD) + "'").
   RUN setcolindex_UI IN brwproc[1] (INPUT "INTEGER(ID2)").
   RUN setcolindex_UI IN brwproc[2] (INPUT "INTEGER(ID2)").
   RUN openbdynspec_UI IN brwproc[1].
   APPLY "CTRL-A" TO BRW_VAL.   
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
  DISPLAY CMB_VALORD FILL-IN-FREKVENS 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_VALORD FILL-IN-FREKVENS BRW_VAL BRW_MTRL2 btn_over btn_back 
         FBTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE flygnr_UI C-Win 
PROCEDURE flygnr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER idnrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER enrvar AS CHARACTER NO-UNDO.
   IF LENGTH(idnrvar) = 1 THEN  idnrvar = "00" + idnrvar.
   IF LENGTH(idnrvar) = 2 THEN  idnrvar = "0" + idnrvar.   
   IF INTEGER(idnrvar) > 245 THEN DO:
      IF SUBSTRING(idnrvar, LENGTH(idnrvar),1) = "0" OR SUBSTRING(idnrvar, LENGTH(idnrvar),1) = "5" THEN enrvar = "0668451".
   END.   
   ELSE IF idnrvar = "245" THEN enrvar = "0668499". 
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "240" THEN enrvar = "0668498".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "235" THEN enrvar = "0668497".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "230" THEN enrvar = "0668496".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "225" THEN enrvar = "0668495".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "220" THEN enrvar = "0668494".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "215" THEN enrvar = "0668493".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "210" THEN enrvar = "0668492".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "205" THEN enrvar = "0668491".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "200" THEN enrvar = "0668490".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "195" THEN enrvar = "0668489".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "190" THEN enrvar = "0668488".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "185" THEN enrvar = "0668487".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "180" THEN enrvar = "0668486".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "175" THEN enrvar = "0668485".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "170" THEN enrvar = "0668484".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "165" THEN enrvar = "0668483".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "160" THEN enrvar = "0668482".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "155" THEN enrvar = "0668481".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "150" THEN enrvar = "0668480".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "145" THEN enrvar = "0668479".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "140" THEN enrvar = "0668478".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "135" THEN enrvar = "0668477".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "130" THEN enrvar = "0668476".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "125" THEN enrvar = "0668475".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "120" THEN enrvar = "0668474".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "115" THEN enrvar = "0668473".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "110" THEN enrvar = "0668472".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "105" THEN enrvar = "0668471".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 2,3) = "100" THEN enrvar = "0668470".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "95" THEN enrvar = "0668469".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "90" THEN enrvar = "0668468".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "85" THEN enrvar = "0668467".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "80" THEN enrvar = "0668466".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "75" THEN enrvar = "0668465".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "70" THEN enrvar = "0668464".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "65" THEN enrvar = "0668463".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "60" THEN enrvar = "0668462".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "55" THEN enrvar = "0668461".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "50" THEN enrvar = "0668460".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "45" THEN enrvar = "0668459".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "40" THEN enrvar = "0668458".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "35" THEN enrvar = "0668457".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "30" THEN enrvar = "0668456".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "25" THEN enrvar = "0668455".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "20" THEN enrvar = "0668454".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "15" THEN enrvar = "0668453".      
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar) - 1,2) = "10" THEN enrvar = "0668452".     
   ELSE IF SUBSTRING(idnrvar,LENGTH(idnrvar),1) = "5"  THEN enrvar =  "0668450".
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval2_UI C-Win 
PROCEDURE vcbrwval2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   IF AVAILABLE kon_val2 THEN DO:
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(kon_val2.F1:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.F1:LABEL," v") = 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val2.F1:LABEL," ^") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val2.F1:LABEL," v") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " v".      
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
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
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
   IF AVAILABLE flygnrtemp THEN DO:      
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = flygnrtemp.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.      
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(flygnrtemp.F1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(flygnrtemp.F1:LABEL," v") = 0 THEN flygnrtemp.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(flygnrtemp.F1:LABEL," ^") NE 0 THEN flygnrtemp.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(flygnrtemp.F1:LABEL," v") NE 0 THEN flygnrtemp.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " v".               
      END.
      IF Guru.Konstanter:globforetag = "SUND" THEN DO:
         IF INDEX(flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(flygnrtemp.ID2:LABEL," v") = 0 
            THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
         ELSE IF INDEX(flygnrtemp.ID2:LABEL," ^") NE 0 THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " ^".      
         ELSE IF INDEX(flygnrtemp.ID2:LABEL," v") NE 0 THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " v".               
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(flygnrtemp.ID2:LABEL," v") = 0 
            THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
         ELSE IF INDEX(flygnrtemp.ID2:LABEL," ^") NE 0 THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " ^".      
         ELSE IF INDEX(flygnrtemp.ID2:LABEL," v") NE 0 THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(flygnrtemp.ID2:LABEL," v") = 0 
            THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
         ELSE IF INDEX(flygnrtemp.ID2:LABEL," ^") NE 0 THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " ^".      
         ELSE IF INDEX(flygnrtemp.ID2:LABEL," v") NE 0 THEN flygnrtemp.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " v".      
      END.      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}         
   END.
   ELSE DO:      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

