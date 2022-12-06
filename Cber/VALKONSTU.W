&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: VALKONSTU.W VÄLJKONST NÄR DU SKA VISA BEREDNING

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

{BERIDTEMP.I}
{BERMTRL.I}
{BBENAMNTEMP.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{KONVALTEMP.I}
&Scoped-define NEW NEW
{KONVAL2TEMP.I}
{BEREDNINGTEMP.I}


DEFINE NEW SHARED VARIABLE avbvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow2 AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE katvar2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkvar2 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrow2 AS ROWID NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE VARIABLE valdatum AS DATE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE ordning AS INTEGER NO-UNDO.
DEFINE VARIABLE kombnr AS INTEGER NO-UNDO.
DEFINE VARIABLE valkonstapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ktypkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bernumvar AS INTEGER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE datvar99 AS DATE NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE list_mtrl2 
   {LISTMTRLTT.I}
DEFINE TEMP-TABLE ot NO-UNDO
      FIELD ORD AS INTEGER
      FIELD OROW AS ROWID
      INDEX ORD ORD
      INDEX NORD ORD DESCENDING.
   
DEFINE BUFFER idbuff FOR beridtemp.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_MTRL2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kon_val kon_val2

/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 kon_val.F1 kon_val.ID1 kon_val.ID2 ~
kon_val.EXTRA1 kon_val.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 kon_val


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.ORD kon_val2.F1 ~
kon_val2.ID1 kon_val2.ID2 kon_val2.EXTRA1 kon_val2.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_ORD FBTN_NYAO BRW_VAL BRW_MTRL2 btn_over ~
BTN_ORDUPP BTN_ORDNER btn_back FBTN_OK BTN_AVB 
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
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_ORD 
     LABEL "Omvänd ordning på de markerade" 
     SIZE 30 BY 1 TOOLTIP "Byt ordning så att den sista kommer först av de markerade.".

DEFINE BUTTON BTN_ORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_ORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_NYAO 
     LABEL "Välj beredning" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Beredning nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL2 FOR 
      kon_val SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 C-Win _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 13
      kon_val.ID1 FORMAT "x(256)":U WIDTH 10
      kon_val.ID2 FORMAT "X(256)":U WIDTH 5
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
      kon_val.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 53.5 BY 15
         TITLE "Valda byggnummer".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.ORD COLUMN-LABEL "Skriv!ord" FORMAT ">>>9":U WIDTH 4
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 11
      kon_val2.ID1 FORMAT "x(256)":U WIDTH 8
      kon_val2.ID2 FORMAT "X(256)":U WIDTH 5
      kon_val2.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
      kon_val2.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 15
         TITLE "Byggnummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-BERNR AT ROW 1.5 COL 16 COLON-ALIGNED
     FILL-IN-BEN AT ROW 2.63 COL 16 COLON-ALIGNED
     FILL-IN-AONR AT ROW 3.71 COL 16 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 3.71 COL 38 COLON-ALIGNED
     BTN_ORD AT ROW 4.5 COL 58.25
     FBTN_NYAO AT ROW 4.5 COL 106
     BRW_VAL AT ROW 6.25 COL 1.5
     BRW_MTRL2 AT ROW 6.25 COL 60.5
     btn_over AT ROW 11.46 COL 56.25
     BTN_ORDUPP AT ROW 11.5 COL 114.63
     BTN_ORDNER AT ROW 14.42 COL 114.63
     btn_back AT ROW 14.96 COL 56.25
     FBTN_OK AT ROW 22 COL 90.75
     BTN_AVB AT ROW 22 COL 105.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.75 BY 22.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: kon_val T "?" NO-UNDO temp-db kon_val
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av byggnummer"
         HEIGHT             = 22.42
         WIDTH              = 120
         MAX-HEIGHT         = 26.42
         MAX-WIDTH          = 124
         VIRTUAL-HEIGHT     = 26.42
         VIRTUAL-WIDTH      = 124
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
/* BROWSE-TAB BRW_VAL FBTN_NYAO DEFAULT-FRAME */
/* BROWSE-TAB BRW_MTRL2 BRW_VAL DEFAULT-FRAME */
ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BTN_ORDNER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-AONR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-BEN:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BERNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-BERNR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DELNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-DELNR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Val av byggnummer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Val av byggnummer */
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
   {muswait.i}
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
   {muswait.i}
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
      DELETE kon_val.   
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&RIGHT-BROWSE}].
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].   
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORD C-Win
ON CHOOSE OF BTN_ORD IN FRAME DEFAULT-FRAME /* Omvänd ordning på de markerade */
DO:
   RUN nerupp_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDNER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDNER C-Win
ON CHOOSE OF BTN_ORDNER IN FRAME DEFAULT-FRAME /* Ner */
DO:   
   RUN btnordnerupp_UI (INPUT FALSE).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ORDUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ORDUPP C-Win
ON CHOOSE OF BTN_ORDUPP IN FRAME DEFAULT-FRAME
DO:
   RUN btnordnerupp_UI (INPUT TRUE).   
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
         FIND FIRST kon_val WHERE kon_val.BERAONR = kon_val2.BERAONR AND 
         kon_val.OMRADE = kon_val2.OMRADE AND kon_val.NUM = kon_val2.NUM AND kon_val.UPPLAG = kon_val2.UPPLAG
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kon_val THEN DO:
            ordning = ordning + 1.
            CREATE kon_val. 
            ASSIGN
            kon_val.BERAONR = kon_val2.BERAONR
            kon_val.OMRADE = kon_val2.OMRADE
            kon_val.F1 = kon_val2.F1
            kon_val.F2 = kon_val2.F2
            kon_val.ID1 = kon_val2.ID1
            kon_val.ID2 = kon_val2.ID2
            kon_val.EXTRA = kon_val2.EXTRA
            kon_val.NUM = kon_val2.NUM
            kon_val.GRUPP = kon_val2.GRUPP
            kon_val.ORD = kon_val2.ORD 
           /*
            kon_val.ORD = ordning
            */
            kon_val.UPPLAG = kon_val2.UPPLAG.
            /*
            kon_val.ORD = INTEGER(kon_val2.ID2) NO-ERROR.
            IF kon_val.ORD = 0 THEN kon_val.ORD = ordning.
            */
            spec_rowid = ROWID(kon_val).
            RUN extrakon_UI.
            
            /*RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").*/            
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
      /*
      status-ok = BRW_VAL:DESELECT-ROWS().
      APPLY "HOME" TO BRW_VAL IN FRAME {&FRAME-NAME}.
      */
      RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].           
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
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
   RUN  VALMENYBEREU.W (INPUT beredningtemp.OMRADE, INPUT beredningtemp.BERNR,OUTPUT valdatum).
   IF avbvar = FALSE THEN DO:
      FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr2) AND
      beredningtemp.OMRADE = valomrade2 USE-INDEX BERNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE beredningtemp THEN DO:
         RUN berhmt_UI IN valkonstapph (INPUT valaonr2,INPUT valomrade2,
                                        OUTPUT TABLE beredningtemp APPEND).
         FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr2) AND
         beredningtemp.OMRADE = valomrade2 USE-INDEX BERNR NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE beredningtemp THEN DO:
         ASSIGN
         FILL-IN-BERNR = beredningtemp.BERNR
         FILL-IN-AONR = beredningtemp.AONR
         FILL-IN-DELNR = beredningtemp.DELNR
         FILL-IN-BEN = beredningtemp.BENAMNING.
         DISPLAY FILL-IN-BERNR FILL-IN-AONR FILL-IN-DELNR FILL-IN-BEN WITH FRAME {&FRAME-NAME}.
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
         /*
         OPEN QUERY BRW_VAL FOR EACH kon_val2 WHERE kon_val2.KSKAP = FALSE
         USE-INDEX ORD NO-LOCK.
         */
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
   ordning = 0.
   GET FIRST BRW_MTRL2.
   DO WHILE AVAILABLE(kon_val):
      ordning = ordning + 1.
      kon_val.ORD = ordning.
      GET NEXT BRW_MTRL2.
   END.
   
            
   APPLY "CLOSE":U TO THIS-PROCEDURE.
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
   IF VALID-HANDLE(valkonstapph) THEN DELETE PROCEDURE valkonstapph.
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
    IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda byggnummer" 
      BRW_VAL:TITLE = "Byggnummer"
      C-Win:TITLE   = "Val av byggnummer".
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda objekt" 
      BRW_VAL:TITLE = "Objekt"
      C-Win:TITLE   = "Val av objekt".
   END.
   ELSE DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda konstruktioner" 
      BRW_VAL:TITLE = "Konstruktioner"
      C-Win:TITLE   = "Val av konstruktioner".
   END.
   {muswait.i}
   {ALLSTARTDYN.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   EMPTY TEMP-TABLE bermtrltemp NO-ERROR. 
   RUN ladda_UI IN valkonstapph (INPUT valaonr,INPUT valomrade,OUTPUT kalkrow,
                                 OUTPUT TABLE beredningtemp,OUTPUT TABLE bermtrltemp).
   
   FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr) AND
   beredningtemp.OMRADE = valomrade USE-INDEX BERNR NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-BERNR = beredningtemp.BERNR
   FILL-IN-AONR = beredningtemp.AONR
   FILL-IN-DELNR = beredningtemp.DELNR
   FILL-IN-BEN = beredningtemp.BENAMNING
   kalkrow2 = kalkrow
   valomrade2 = beredningtemp.OMRADE
   ordning = 0
   kombnr = 0.
   EMPTY TEMP-TABLE kon_val2 NO-ERROR.    
   FIND FIRST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
   bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = FALSE 
   USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE bermtrltemp THEN DO:
      datvar99 = bermtrltemp.DATUM.
   END.
   ELSE DO:
      datvar99 = TODAY.
   END. 
   EMPTY TEMP-TABLE kon_val2 NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl2 NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN VALBER2U.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, INPUT datvar99, OUTPUT TABLE kon_val2,
      OUTPUT TABLE list_mtrl2).
   END.
   ELSE DO:
      RUN VALBER2U.P
      (INPUT valaonr, INPUT valomrade, INPUT datvar99, OUTPUT TABLE kon_val2,
      OUTPUT TABLE list_mtrl2).
   END.  
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   
   FIND LAST kon_val USE-INDEX ORD NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN ordning = kon_val.ORD.
   FIND LAST kon_val USE-INDEX NUM2 NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      IF kon_val.SKAPNUM NE ? THEN
      kombnr = kon_val.SKAPNUM.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
   END.
   ELSE DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
   END.
   ELSE DO:
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
   END.
   IF Guru.Konstanter:varforetypval[32] = 0 THEN.
   ELSE DO:
      ASSIGN kon_val2.ID1:VISIBLE IN BROWSE BRW_VAL = FALSE.
      ASSIGN kon_val.ID1:VISIBLE IN BROWSE BRW_MTRL2 = FALSE.
   END.        
   RUN enable_UI.   
   {FRMSIZE.I}  
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_MTRL2 IN FRAME {&FRAME-NAME}.
   /*
    RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
    */
    RUN resetcolindex_UI IN brwproc[{&LEFT-BROWSE}].
    
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
   /*
   RUN brwsetupstop_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT 1).
   */
   RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "KSKAP = FALSE").
   
   
   RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "ORD").
   
   RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordnerupp_UI C-Win 
PROCEDURE btnordnerupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER upp AS LOGICAL NO-UNDO.
   DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
   DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE till_rowid AS ROWID NO-UNDO.
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      ASSIGN
      frannr = kon_val.ORD
      till_rowid = ROWID(kon_val).      
      IF upp = FALSE THEN status-ok = BRW_MTRL2:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      ELSE status-ok = BRW_MTRL2:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = kon_val.ORD
         kon_val.ORD = frannr.   
         FIND kon_val WHERE ROWID(kon_val) = till_rowid.
         kon_val.ORD = tillnr.
         RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
         FIND kon_val WHERE ROWID(kon_val) = till_rowid NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.   
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
  ENABLE BTN_ORD FBTN_NYAO BRW_VAL BRW_MTRL2 btn_over BTN_ORDUPP BTN_ORDNER 
         btn_back FBTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extrakon_UI C-Win 
PROCEDURE extrakon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {EXTRAKON.I}
  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nerupp_UI C-Win 
PROCEDURE nerupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE ot NO-ERROR. 
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).
         CREATE ot.
         ASSIGN
         ot.OROW = ROWID(kon_val)
         ot.ORD = kon_val.ORD.                            
         antal_raknare = antal_raknare + 1.
      END.
      FIND FIRST ot USE-INDEX ORD NO-LOCK NO-ERROR.
      antal_raknare = ot.ORD.
      FOR EACH ot USE-INDEX NORD:
         FIND kon_val WHERE ROWID(kon_val) = ot.OROW NO-ERROR.
         IF AVAILABLE kon_val THEN kon_val.ORD = antal_raknare.
         antal_raknare = antal_raknare + 1.
      END.     
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
   END.
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI C-Win 
PROCEDURE vcbrwval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
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

