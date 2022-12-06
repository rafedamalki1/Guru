&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:  VALKONKOP.W körs från BERVALSDI2.P

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

{kalktemp2.i}
{BERIDTEMP.I}
{FRITEMP.I}
{BERMTRL.I}
{BBENAMNTEMP.I}
&Scoped-define SHARED SHARED
{KONID.I}   
{KONVALTEMP.I}

&Scoped-define NEW NEW
{KONVAL2TEMP.I}
{BEREDNINGTEMP.I}

DEFINE NEW SHARED TEMP-TABLE list_mtrl2 
   {LISTMTRLTT.I}
DEFINE TEMP-TABLE elist_mtrl2 NO-UNDO LIKE list_mtrl2.
/*
DEFINE TEMP-TABLE ekon_val2 NO-UNDO LIKE kon_val2.
*/
DEFINE TEMP-TABLE ekon_val3 NO-UNDO LIKE kon_val2.
DEFINE INPUT  PARAMETER beridcopy AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR ekon_val.
DEFINE OUTPUT PARAMETER TABLE FOR list_mtrl2.
/*berid copy*/
DEFINE OUTPUT PARAMETER TABLE FOR beridtemp.
DEFINE OUTPUT PARAMETER TABLE FOR eberid2temp.
DEFINE OUTPUT PARAMETER TABLE FOR kalk_temp.
DEFINE OUTPUT PARAMETER TABLE FOR efri_temp.  
/* Local Variable Definitions ---                                       */


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
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE VARIABLE valberapph AS HANDLE NO-UNDO.
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

DEFINE TEMP-TABLE ot NO-UNDO
      FIELD ORD AS INTEGER
      FIELD OROW AS ROWID
      INDEX ORD ORD
      INDEX NORD ORD DESCENDING.
         
DEFINE BUFFER idbuff FOR beridtemp.
DEFINE TEMP-TABLE multiseltemp
   FIELD NUMSEL AS INTEGER
   FIELD SELROW AS ROWID
   INDEX NUM AS PRIMARY NUMSEL.

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
&Scoped-define INTERNAL-TABLES ekon_val2 kon_val2

/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 ekon_val2.F1 ekon_val2.ID2 ~
ekon_val2.EXTRA1 ekon_val2.ORD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH ekon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH ekon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 ekon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 ekon_val2


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
&Scoped-Define ENABLED-OBJECTS BTN_ORD BRW_VAL BRW_MTRL2 FBTN_NYAO btn_over ~
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
      ekon_val2 SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 C-Win _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      ekon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(10)":U WIDTH 14
      ekon_val2.ID2 FORMAT "X(10)":U WIDTH 13.38
      ekon_val2.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(15)":U WIDTH 13
      ekon_val2.ORD FORMAT ">>>9":U
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
     FILL-IN-BERNR AT ROW 1.5 COL 15.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 2.63 COL 15.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 3.71 COL 15.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 3.71 COL 37.5 COLON-ALIGNED
     BTN_ORD AT ROW 4.5 COL 57.75
     BRW_VAL AT ROW 6.25 COL 1.5
     BRW_MTRL2 AT ROW 6.25 COL 50.25
     FBTN_NYAO AT ROW 8 COL 102.75
     btn_over AT ROW 11.46 COL 46.13
     BTN_ORDUPP AT ROW 11.5 COL 96
     BTN_ORDNER AT ROW 14.42 COL 96
     btn_back AT ROW 14.96 COL 46.13
     FBTN_OK AT ROW 26.75 COL 87.75
     BTN_AVB AT ROW 26.75 COL 102.75
     "OBS! Egna kalkylkoder kopieras inte!" VIEW-AS TEXT
          SIZE 43.5 BY 1.75 AT ROW 2.5 COL 64.5 WIDGET-ID 2
          FGCOLOR 12 FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.38 BY 27.13.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ekon_val2 T "?" NO-UNDO temp-db ekon_val2
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
         HEIGHT             = 27.17
         WIDTH              = 116.5
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VAL BTN_ORD DEFAULT-FRAME */
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
     _TblList          = "Temp-Tables.ekon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ekon_val2.F1
"F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ekon_val2.ID2
"ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ekon_val2.EXTRA1
"EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.ekon_val2.ORD
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no "13.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val2.EXTRA1
"kon_val2.EXTRA1" "Fri id" "X(15)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
      IF AVAILABLE ekon_val2 THEN DELETE ekon_val2.
      antal_raknare = antal_raknare + 1.   
   END.
   RUN refreshbrw_UI IN brwproc[2].
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
   EMPTY TEMP-TABLE ekon_val3 NO-ERROR. 
   antal_valda = BRW_VAL:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE kon_val2 THEN DO:
            FIND FIRST ekon_val2 WHERE ekon_val2.BERAONR = kon_val2.BERAONR AND 
            ekon_val2.OMRADE = kon_val2.OMRADE AND ekon_val2.NUM = kon_val2.NUM
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE ekon_val2 THEN DO:
               
               
               CREATE ekon_val3. 
               ASSIGN
               ekon_val3.BERAONR = kon_val2.BERAONR
               ekon_val3.OMRADE = kon_val2.OMRADE
               ekon_val3.F1 = kon_val2.F1
               ekon_val3.ID2 = kon_val2.ID2
               ekon_val3.EXTRA = kon_val2.EXTRA
               ekon_val3.NUM = kon_val2.NUM
               ekon_val3.GRUPP = kon_val2.GRUPP.
               ekon_val3.ORD = kon_val2.ORD.
               /*
               ekon_val3.ORD = INTEGER(kon_val2.ID2) NO-ERROR.
               IF ekon_val3.ORD = 0 THEN ekon_val3 .ORD = ordning.
               */
            
               ASSIGN
               ekon_val3 .EXTRA1 = SUBSTRING(ekon_val3 .EXTRA,3)
               ekon_val3 .EXTRA2 = SUBSTRING(ekon_val3 .EXTRA,1,1).
              
               /*berid copy*/
               IF Guru.GlobalaVariabler:copyfriid = TRUE THEN
               RUN laddaberidnum_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE, INPUT kon_val2.NUM,
                                                     OUTPUT TABLE beridtemp APPEND, OUTPUT TABLE eberid2temp APPEND).
                                                     
               ELSE RUN laddaberid_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                                  OUTPUT TABLE beridtemp).   
               IF Guru.GlobalaVariabler:copyfrikort = TRUE THEN
               RUN laddabfrikort_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE, INPUT kon_val2.NUM,
                                                     OUTPUT TABLE fri_temp APPEND).
            
                                                                                                                         
               FIND FIRST beridtemp WHERE beridtemp.AONR = kon_val2.BERAONR AND 
               beridtemp.OMRADE = kon_val2.OMRADE AND beridtemp.NUM = kon_val2.NUM NO-LOCK NO-ERROR.
               IF AVAILABLE beridtemp THEN DO:
                  IF beridtemp.XKORD NE ? THEN DO:
                     ASSIGN
                     kombnr = kombnr + 1
                     ekon_val3.SKAPNUM = kombnr.
                     OPEN QUERY idq FOR EACH idbuff WHERE idbuff.AONR = kon_val2.BERAONR AND 
                     idbuff.OMRADE = kon_val2.OMRADE AND idbuff.XKORD = beridtemp.XKORD NO-LOCK.
                     GET FIRST idq NO-LOCK.
                     DO WHILE AVAILABLE(idbuff):
                        IF idbuff.NUM = beridtemp.NUM THEN musz = musz.
                        ELSE DO:                                      
                           RUN laddaberval_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                                               INPUT idbuff.NUM,OUTPUT ktypkodvar,OUTPUT bernumvar).
                           CREATE ekon_val3. 
                           ASSIGN
                           ekon_val3.BERAONR = kon_val2.BERAONR
                           ekon_val3.OMRADE = kon_val2.OMRADE
                           ekon_val3.F1 = ktypkodvar                        
                           ekon_val3.NUM = bernumvar
                           ekon_val3.ORD = kon_val2.ORD
                           ekon_val3.SKAPNUM = kombnr.
                           IF idbuff.FRI2 = ? THEN ekon_val3.ID2 = idbuff.NATNR.
                           ELSE ekon_val3.ID2 = STRING(idbuff.FRI2).
                           ekon_val3.EXTRA = idbuff.FRI3.
                           ASSIGN
                           ekon_val3.EXTRA1 = SUBSTRING(ekon_val3.EXTRA,3)
                           ekon_val3.EXTRA2 = SUBSTRING(ekon_val3.EXTRA,1,1).
                           
                        END.
                        GET NEXT idq NO-LOCK.
                     END.
                     CLOSE QUERY idq.
                  END.
                  ELSE DO:
                     ekon_val3.SKAPNUM = ?.
                  END.
               END.
               /*RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").*/
               
            END.
            ELSE DO:
               spec_rowid = ROWID(kon_val).
            END.
         END.
         antal_raknare = antal_raknare + 1.
      END.
      FOR EACH ekon_val3 BY ekon_val3.ORD:
         ordning = ordning + 1.
         CREATE ekon_val2.
         BUFFER-COPY ekon_val3 TO ekon_val2.
         
         ekon_val2.ORD = ordning.
      END.   
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                      
      antal_raknare = 0.
      GET FIRST BRW_MTRL2.
      DO WHILE AVAILABLE(ekon_val2):
         antal_raknare = antal_raknare + 1.
         ekon_val2.ORD = antal_raknare.
         GET NEXT BRW_MTRL2.
      END.
      FIND FIRST ekon_val2 WHERE ROWID(ekon_val2) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE ekon_val2 THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(ekon_val2)).
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      END.
      RUN vcbrwval_UI.
      status-ok = BRW_VAL:DESELECT-ROWS().
      APPLY "HOME" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   END.                     
   EMPTY TEMP-TABLE ekon_val3  NO-ERROR. 
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
   RUN  VALMENYBEREU.W (INPUT beredningtemp.OMRADE, INPUT beredningtemp.BERNR, OUTPUT valdatum).
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
   DEFINE VARIABLE varextra AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varbernr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varomrber AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varnum AS INTEGER NO-UNDO.
   {muswait.i} 
   EMPTY TEMP-TABLE ekon_val NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl2 NO-ERROR. 
   EMPTY TEMP-TABLE ekalk_temp2 NO-ERROR.
   EMPTY TEMP-TABLE eberidtemp NO-ERROR.  
   ordning = 0.
   GET FIRST BRW_MTRL2.
   DO WHILE AVAILABLE(ekon_val2):
      ordning = ordning + 1.
      ekon_val2.ORD = ordning.
      GET NEXT BRW_MTRL2.
   END.  
   REPEAT:
      FIND FIRST ekon_val2  WHERE ekon_val2.KSKAP = FALSE USE-INDEX ord NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ekon_val2 THEN DO:
         LEAVE.
      END.
      ASSIGN
      varextra = ekon_val2.EXTRA
      varnum =  ekon_val2.NUM
      varbernr = ekon_val2.BERAONR
      varomrber = ekon_val2.OMRADE
      ordning = ekon_val2.ORD.
      EMPTY TEMP-TABLE ekon_val3 NO-ERROR. 
      EMPTY TEMP-TABLE elist_mtrl2 NO-ERROR. 
      EMPTY TEMP-TABLE ekalk_temp2 NO-ERROR. 
      /*RÄTT DATUM*/
      RUN berdathmt_UI IN valberapph (INPUT ekon_val2.BERAONR,INPUT ekon_val2.OMRADE,OUTPUT valdatum).
      /*RÄTT KONST*/
      FOR EACH ekon_val2 WHERE ekon_val2.BERAONR = varbernr AND ekon_val2.OMRADE = varomrber AND ekon_val2.NUM = varnum AND
      ekon_val2.KSKAP = FALSE:
         CREATE ekon_val3.
         BUFFER-COPY ekon_val2 TO ekon_val3.
         DELETE ekon_val2.
      END.
      /*MTRL*/
      IF Guru.GlobalaVariabler:KorBerDS = TRUE THEN DO:
         RUN BytBerNr_UI IN Guru.GlobalaVariabler:BerDSapph (INPUT varbernr, INPUT varomrber).
         RUN MtrlkKopiHmt_UI IN Guru.GlobalaVariabler:BerDSapph 
         (INPUT valdatum,INPUT varnum, INPUT "", INPUT TABLE ekon_val3,OUTPUT TABLE elist_mtrl2).
         RUN BytTillbaka_UI IN Guru.GlobalaVariabler:BerDSapph.
      END.       
      ELSE DO:  
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN BERHMTKONMTRL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT varbernr, INPUT varomrber, INPUT valdatum,INPUT varnum, INPUT "", INPUT TABLE ekon_val3, 
            OUTPUT TABLE elist_mtrl2).
         END.
         ELSE DO:
            RUN BERHMTKONMTRL.P
            (INPUT varbernr, INPUT varomrber, INPUT valdatum,INPUT varnum, INPUT "", INPUT TABLE ekon_val3, 
            OUTPUT TABLE elist_mtrl2).
         END. 
         RUN ReMtrluppl_UI IN valberapph (INPUT varbernr, INPUT varomrber, INPUT-OUTPUT TABLE elist_mtrl2).
      END.
      /*kalkyl*/ 
      IF Guru.GlobalaVariabler:KorBerDS = TRUE THEN DO:
         RUN BytBerNr_UI IN Guru.GlobalaVariabler:BerDSapph (INPUT varbernr, INPUT varomrber).
         RUN KalkKoderHmt_UI IN Guru.GlobalaVariabler:BerDSapph 
         (INPUT varnum,INPUT TABLE ekon_val3,OUTPUT TABLE ekalk_temp2 APPEND).
         RUN Byttillbaka_UI IN Guru.GlobalaVariabler:BerDSapph.
      END.
      ELSE DO: 
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN BERHMTKALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT Guru.Konstanter:varforetypchar[48],INPUT varbernr, INPUT varomrber,INPUT varnum,INPUT TABLE ekon_val3,OUTPUT TABLE ekalk_temp2 APPEND).
         END.
         ELSE DO:
            RUN BERHMTKALK.P
            (INPUT Guru.Konstanter:varforetypchar[48],INPUT varbernr, INPUT varomrber,INPUT varnum,INPUT TABLE ekon_val3,OUTPUT TABLE ekalk_temp2 APPEND).
         END.     
      END.
      /*ALLA KONVALAR*/
      RUN konval_UI IN valberapph (INPUT varbernr,INPUT varomrber,INPUT varnum,OUTPUT TABLE ekon_val3).
      
      FOR EACH ekon_val3 BY  ekon_val3.ORD  :
         CREATE ekon_val.
         BUFFER-COPY ekon_val3 TO ekon_val.
         IF ekon_val.KSKAP = FALSE THEN DO:
            ekon_val.EXTRA = varextra.
            ASSIGN
            ekon_val.EXTRA1 = SUBSTRING(ekon_val.EXTRA,3)
            ekon_val.EXTRA2 = SUBSTRING(ekon_val.EXTRA,1,1).
         END.   
         
         IF varbernr = valaonr AND varomrber = valomrade THEN.
         ELSE ekon_val.UPPLAG = ?.
         
         IF ekon_val3.KSKAP = FALSE THEN ekon_val.ORD = ordning.
         ELSE ekon_val.ORD = ordning + 1.
         
         ASSIGN
         ekon_val.EXTRA1 = ""
         ekon_val.ID = FALSE
         ekon_val.NUM = ordning.   
       
         /*berid copy*/
         IF Guru.GlobalaVariabler:copyfriid = TRUE THEN DO:
            IF ekon_val3.KSKAP = FALSE THEN DO:
               FIND FIRST beridtemp WHERE beridtemp.NUM = ekon_val3.NUM NO-LOCK NO-ERROR.  
               IF AVAILABLE beridtemp THEN DO: 
                  CREATE eberidtemp.
                  BUFFER-COPY beridtemp TO eberidtemp.
                  ekon_val.ID = TRUE.
                  eberidtemp.NUM =  ekon_val.NUM.
                  DELETE beridtemp.
               END.
               FIND FIRST eberid2temp WHERE eberid2temp.NUM = ekon_val3.NUM NO-LOCK NO-ERROR.  
               IF AVAILABLE eberid2temp THEN DO:
                  eberid2temp.NUM =  ekon_val.NUM.
               END.   
                 
            END.
         END.   
         IF Guru.GlobalaVariabler:copyfrikort = TRUE THEN DO:
            FOR EACH fri_temp WHERE fri_temp.NUM = ekon_val3.NUM NO-LOCK:
               CREATE efri_temp.
               BUFFER-COPY fri_temp TO efri_temp.
               efri_temp.NUM =  ekon_val.NUM.
               DELETE fri_temp.
            END.
         END.         
      END.
      FOR EACH elist_mtrl2:
         elist_mtrl2.NUM = ordning.
         CREATE list_mtrl2.
         BUFFER-COPY elist_mtrl2 TO list_mtrl2.
         list_mtrl2.ENHET = LC(list_mtrl2.ENHET).
      END.
      
      FOR EACH ekalk_temp2 WHERE NO-LOCK:
         IF ekalk_temp2.ARBKOD = "EGEN" THEN.
         ELSE DO:
            CREATE kalk_temp.
            BUFFER-COPY ekalk_temp2 TO kalk_temp.
            ASSIGN
            kalk_temp.NUM = ordning.
         END.   
      END.
   END.
  EMPTY TEMP-TABLE beridtemp NO-ERROR.
  FOR EACH eberidtemp WHERE NO-LOCK:
     CREATE beridtemp.
     BUFFER-COPY eberidtemp TO beridtemp.
  END. 
  
  
  
   {musarrow.i} 
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
   IF VALID-HANDLE(valberapph) THEN DELETE PROCEDURE valberapph.
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
   {BERTITLE.I}
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
   EMPTY TEMP-TABLE list_mtrl2 NO-ERROR.  
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
   END.
   ELSE DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
   END.
   
   RUN enable_UI.   
   {FRMSIZE.I}  
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_MTRL2 IN FRAME {&FRAME-NAME}.
   IF ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2 = "F1" THEN DO:
      ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2 = kon_val2.F1:LABEL IN BROWSE BRW_VAL.
   END.
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
   RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "KSKAP = FALSE").
   RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "ORD").
   RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALKONSTAPP.P PERSISTENT SET valkonstapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALKONSTAPP.P PERSISTENT SET valkonstapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALBERAPP.P PERSISTENT SET valberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALBERAPP.P PERSISTENT SET valberapph.
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
   IF AVAILABLE ekon_val2 THEN DO:
      ASSIGN
      frannr = ekon_val2.ORD
      till_rowid = ROWID(ekon_val2).      
      IF upp = FALSE THEN status-ok = BRW_MTRL2:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      ELSE status-ok = BRW_MTRL2:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = ekon_val2.ORD
         ekon_val2.ORD = frannr.   
         FIND ekon_val2 WHERE ROWID(ekon_val2) = till_rowid.
         ekon_val2.ORD = tillnr.
         RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
         FIND ekon_val2 WHERE ROWID(ekon_val2) = till_rowid NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(ekon_val2)).
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
  ENABLE BTN_ORD BRW_VAL BRW_MTRL2 FBTN_NYAO btn_over BTN_ORDUPP BTN_ORDNER 
         btn_back FBTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
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
         IF AVAILABLE ekon_val2 THEN DO:
            CREATE ot.
            ASSIGN
            ot.OROW = ROWID(ekon_val2)
            ot.ORD = ekon_val2.ORD.                            
         END.
         antal_raknare = antal_raknare + 1.
      END.
      FIND FIRST ot USE-INDEX ORD NO-LOCK NO-ERROR.
      antal_raknare = ot.ORD.
      FOR EACH ot USE-INDEX NORD:
         FIND ekon_val2 WHERE ROWID(ekon_val2) = ot.OROW NO-ERROR.
         IF AVAILABLE ekon_val2 THEN ekon_val2.ORD = antal_raknare.
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
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
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
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
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
   IF AVAILABLE ekon_val2 THEN DO:
      /*
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = ekon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.      
      IF AVAILABLE bbenamntemp THEN DO:
         IF INDEX(ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(ekon_val2.F1:LABEL," v") = 0 THEN ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(ekon_val2.F1:LABEL," ^") NE 0 THEN ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(ekon_val2.F1:LABEL," v") NE 0 THEN ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " v".               
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF INDEX(ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(ekon_val2.ID2:LABEL," v") = 0 
            THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
         ELSE IF INDEX(ekon_val2.ID2:LABEL," ^") NE 0 THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " ^".      
         ELSE IF INDEX(ekon_val2.ID2:LABEL," v") NE 0 THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(ekon_val2.ID2:LABEL," v") = 0 
            THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
         ELSE IF INDEX(ekon_val2.ID2:LABEL," ^") NE 0 THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " ^".      
         ELSE IF INDEX(ekon_val2.ID2:LABEL," v") NE 0 THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(ekon_val2.ID2:LABEL," v") = 0 
            THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
         ELSE IF INDEX(ekon_val2.ID2:LABEL," ^") NE 0 THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " ^".      
         ELSE IF INDEX(ekon_val2.ID2:LABEL," v") NE 0 THEN ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " v".      
      END.
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}         
   END.
   ELSE DO:
       /*
      ASSIGN      
      ekon_val2.F1:LABEL IN BROWSE BRW_MTRL2 = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
      END.
      ELSE DO:
         ekon_val2.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
      END.
      */
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

