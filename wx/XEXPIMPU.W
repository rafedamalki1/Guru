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
/*Ändring av Mikael*/
/* DEFINE INPUT PARAMETER valrec AS RECID NO-UNDO. */
DEFINE INPUT PARAMETER typval AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&SCOPED-DEFINE SHARED SHARED
{BERTEMPTAB.I}
&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{BEREDNINGTEMP.I}
DEFINE NEW SHARED VARIABLE valimpomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valanv AS CHARACTER NO-UNDO.


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(78)" NO-UNDO.       
DEFINE VARIABLE tmpattrlist AS CHARACTER FORMAT "X(78)" NO-UNDO. 
DEFINE VARIABLE tmpdirlist AS CHARACTER FORMAT "X(78)" NO-UNDO.  
DEFINE VARIABLE textstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE delbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE varbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE varomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar3 AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttnr AS INTEGER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER FORMAT "X(256)" NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE tempomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempnum AS INTEGER NO-UNDO.
DEFINE VARIABLE listobj AS CHARACTER NO-UNDO.
DEFINE VARIABLE typdator AS INTEGER NO-UNDO.
DEFINE STREAM dirstrom.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE skrivaover AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE expimpapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE avcadaccessh AS HANDLE NO-UNDO.
DEFINE VARIABLE avbryt AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE kon_grupp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE TEMP-TABLE avcadtemp
   FIELD ID AS INTEGER LABEL "Id"
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD COMMENT AS CHARACTER LABEL "Kommentar"
   INDEX ID IS PRIMARY ID.

/* DEFINE TEMP-TABLE beredningtemp      */
/*    FIELD OMRADE AS CHARACTER         */
/*    FIELD BERNR AS INTEGER            */
/*    FIELD BENAMNING AS CHARACTER      */
/*    INDEX OMR OMRADE BERNR ASCENDING. */
DEFINE TEMP-TABLE valtemp                  
   FIELD OMRADE AS CHARACTER
   FIELD BERNR AS INTEGER   
   FIELD BENAMNING AS CHARACTER
   FIELD NYTTNR AS INTEGER INITIAL 0
   INDEX OMR OMRADE BERNR ASCENDING.
DEFINE TEMP-TABLE imptemp NO-UNDO
   FIELD invar AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AVCAD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avcadtemp beredningtemp konstgrptemp

/* Definitions for BROWSE BRW_AVCAD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVCAD avcadtemp.ID avcadtemp.BENAMNING ~
avcadtemp.COMMENT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVCAD 
&Scoped-define QUERY-STRING-BRW_AVCAD FOR EACH avcadtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVCAD OPEN QUERY BRW_AVCAD FOR EACH avcadtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVCAD avcadtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVCAD avcadtemp


/* Definitions for BROWSE BRW_BER                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BER beredningtemp.OMRADE ~
beredningtemp.BERNR beredningtemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BER 
&Scoped-define QUERY-STRING-BRW_BER FOR EACH beredningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BER OPEN QUERY BRW_BER FOR EACH beredningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BER beredningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BER beredningtemp


/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON konstgrptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH konstgrptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH konstgrptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON konstgrptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON konstgrptemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_KON}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_BARBAR MBTN_HANDDATOR MBTN_AVCAD ~
BTN_IMP MBTN_HMT SEL-IMP BRW_AVCAD BRW_BER TOG_BER TOG_MTRL TOG_UPP ~
TOG_UPP2 BRW_KON BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR ~
FILL-IN-DELNR SEL-IMP TOG_BER TOG_MTRL TOG_UPP TOG_UPP2 FILL-IN-BER ~
FILL-IN-BER-2 

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

DEFINE BUTTON BTN_IMP 
     LABEL "Hämta data från handdator" 
     SIZE 27 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_AVCAD 
     LABEL "AvCAD" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_BARBAR 
     LABEL "Bärbar dator" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_HANDDATOR 
     LABEL "Handdator" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_HMT  NO-FOCUS
     LABEL "Sök beredning" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 39.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Välj beredning för import" 
      VIEW-AS TEXT 
     SIZE 39.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Beredning nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bärbardator", 1,
"Handdator", 2,
"AvCAD", 3
     SIZE 53.5 BY .75 NO-UNDO.

DEFINE VARIABLE SEL-IMP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 94 BY 13 NO-UNDO.

DEFINE VARIABLE TOG_BER AS LOGICAL INITIAL no 
     LABEL "Beredning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Materielkatalog" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Beredningsupplägg" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_UPP2 AS LOGICAL INITIAL no 
     LABEL "Beredningsupplägg/Materielkatalog" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.5 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVCAD FOR 
      avcadtemp SCROLLING.

DEFINE QUERY BRW_BER FOR 
      beredningtemp SCROLLING.

DEFINE QUERY BRW_KON FOR 
      konstgrptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVCAD C-Win _STRUCTURED
  QUERY BRW_AVCAD NO-LOCK DISPLAY
      avcadtemp.ID FORMAT "->,>>>,>>9":U
      avcadtemp.BENAMNING FORMAT "X(256)":U WIDTH 40
      avcadtemp.COMMENT FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 84.25 BY 13
         TITLE "Välj beredning(ar) för import" EXPANDABLE.

DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER C-Win _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      beredningtemp.OMRADE FORMAT "x(6)":U
      beredningtemp.BERNR COLUMN-LABEL "Beredningsnr" FORMAT ">>>>>>9":U
      beredningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 75 BY 13
         TITLE "Välj beredning(ar) för import" EXPANDABLE.

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON C-Win _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konstgrptemp.BENAMNING FORMAT "X(256)":U WIDTH 47
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 50 BY 10.25
         TITLE "Välj konstruktionsgrupp(er)" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MBTN_BARBAR AT ROW 1.08 COL 1.13
     MBTN_HANDDATOR AT ROW 1.08 COL 15.13
     MBTN_AVCAD AT ROW 1.08 COL 29.13
     RAD_VAL AT ROW 1.25 COL 59.5 NO-LABEL
     FILL-IN-BERNR AT ROW 4.79 COL 16.5 COLON-ALIGNED
     BTN_IMP AT ROW 5.79 COL 36.13
     FILL-IN-BEN AT ROW 5.96 COL 16.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 7.08 COL 16.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 7.08 COL 38.5 COLON-ALIGNED
     MBTN_HMT AT ROW 7.29 COL 98.5
     SEL-IMP AT ROW 7.29 COL 2.63 NO-LABEL
     BRW_AVCAD AT ROW 7.29 COL 12
     BRW_BER AT ROW 7.29 COL 12.13
     TOG_BER AT ROW 9.04 COL 28.5
     TOG_MTRL AT ROW 9.04 COL 49.5
     TOG_UPP AT ROW 10.29 COL 28.5
     TOG_UPP2 AT ROW 10.29 COL 28.5
     BRW_KON AT ROW 11.54 COL 24
     BTN_OK AT ROW 23.54 COL 83.5
     BTN_AVB AT ROW 23.54 COL 98.5
     FILL-IN-BER AT ROW 2.5 COL 2.5 NO-LABEL
     FILL-IN-BER-2 AT ROW 3.75 COL 2.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.38 BY 24.25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: avcadtemp T "?" NO-UNDO temp-db avcadtemp
      TABLE: beredningtemp T "?" NO-UNDO TEMP-DB beredningtemp
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av byggnummer"
         HEIGHT             = 24.25
         WIDTH              = 112.38
         MAX-HEIGHT         = 26.42
         MAX-WIDTH          = 124.88
         VIRTUAL-HEIGHT     = 26.42
         VIRTUAL-WIDTH      = 124.88
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
/* BROWSE-TAB BRW_AVCAD SEL-IMP DEFAULT-FRAME */
/* BROWSE-TAB BRW_BER BRW_AVCAD DEFAULT-FRAME */
/* BROWSE-TAB BRW_KON TOG_UPP2 DEFAULT-FRAME */
ASSIGN 
       BRW_AVCAD:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_BER:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_KON:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-BER IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BER-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-BER-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BERNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DELNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       MBTN_AVCAD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_BARBAR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_HANDDATOR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_HMT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       SEL-IMP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVCAD
/* Query rebuild information for BROWSE BRW_AVCAD
     _TblList          = "Temp-Tables.avcadtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.avcadtemp.ID
     _FldNameList[2]   > Temp-Tables.avcadtemp.BENAMNING
"avcadtemp.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.avcadtemp.COMMENT
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVCAD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "Temp-Tables.beredningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.beredningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.beredningtemp.BERNR
"beredningtemp.BERNR" "Beredningsnr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.beredningtemp.BENAMNING
"beredningtemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konstgrptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstgrptemp.BENAMNING
"konstgrptemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "47" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_KON */
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


&Scoped-define BROWSE-NAME BRW_AVCAD
&Scoped-define SELF-NAME BRW_AVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVCAD C-Win
ON LEFT-MOUSE-CLICK OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj beredning(ar) för import */
DO:
   status-ok = BRW_AVCAD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVCAD C-Win
ON VALUE-CHANGED OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj beredning(ar) för import */
DO:
   status-ok = BRW_AVCAD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   IF VALID-HANDLE(avcadaccessh) THEN RUN close_UI IN avcadaccessh.
   musz = TRUE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMP C-Win
ON CHOOSE OF BTN_IMP IN FRAME DEFAULT-FRAME /* Hämta data från handdator */
DO:
   {muswait.i}      
   IF typdator = 2 THEN DO:
      RUN IPOCKETPC.P (INPUT fildir, OUTPUT TABLE beredningtemp, INPUT 1, INPUT TABLE valtemp, INPUT Guru.Konstanter:globanv).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE IF typdator = 3 THEN DO:
      RUN avcad_UI IN avcadaccessh (OUTPUT TABLE avcadtemp).
      RUN setcolsortvar_UI IN brwproc[3] (INPUT "").
      RUN openbdynspec_UI IN brwproc[3].
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   
   {muswait.i}      
   ASSIGN
   TOG_BER = INPUT TOG_BER
   TOG_MTRL = INPUT TOG_MTRL
   SEL-IMP = INPUT SEL-IMP
   TOG_UPP = INPUT TOG_UPP
   TOG_UPP2 = INPUT TOG_UPP2.
   /*Export*/
   
   IF typval = 1 THEN DO:
      RUN export_UI.      
   END.
   ELSE IF typval = 2 THEN DO:
      /*Import*/
      RUN import_UI.      
   END.
   /*OS-DELETE VALUE(fildir).*/
   ASSIGN
   valomrade = valimpomrade.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_AVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_AVCAD C-Win
ON CHOOSE OF MBTN_AVCAD IN FRAME DEFAULT-FRAME /* AvCad */
DO:
   RAD_VAL = 3.
   BTN_IMP:LABEL = "Hämta data från AvCAD".
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_BARBAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_BARBAR C-Win
ON CHOOSE OF MBTN_BARBAR IN FRAME DEFAULT-FRAME /* Bärbar dator */
DO:
  RAD_VAL = 1.
  APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_HANDDATOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_HANDDATOR C-Win
ON CHOOSE OF MBTN_HANDDATOR IN FRAME DEFAULT-FRAME /* Handdator */
DO:
  RAD_VAL = 2.
  BTN_IMP:LABEL = "Hämta data från handdator".
  APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_HMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_HMT C-Win
ON CHOOSE OF MBTN_HMT IN FRAME DEFAULT-FRAME /* Sök beredning */
DO:     
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO FORMAT "X(50)".
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den fil som Ni vill koppla"
   FILTERS        "All Files (*.d)"  "*.d"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.   
   IF OKvald = TRUE THEN DO:
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      RUN in_UI.  
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
/*    RAD_VAL = INPUT RAD_VAL. */
   typdator = RAD_VAL.
   IF typdator = 1 THEN DO:
      ASSIGN
      BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      /*import*/
      IF typval = 2 THEN DO:     
         FILL-IN-BER = "Import från bärbar dator.".
         MBTN_HMT:HIDDEN = FALSE.
         IF TOG_BER = TRUE THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ELSE SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         IF Guru.SharedVariable:singel = TRUE THEN DO:
            ASSIGN
            TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
         ELSE DO:
            ASSIGN
            TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
      END.
      ELSE DO:
         /*Export*/
         FILL-IN-BER = "Export till bärbar dator.".
         ASSIGN
         TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                   
      END.
   END.
   ELSE IF typdator = 2 THEN DO:   
      ASSIGN
      MBTN_HMT:HIDDEN = TRUE
      SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF typval = 1 THEN DO:
         FILL-IN-BER = "Export till handdator.".
         TOG_UPP2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
      END.
      IF TOG_BER = TRUE THEN DO:
         IF typval = 2 THEN DO:      
            FILL-IN-BER = "Import från handdator.".
            ASSIGN
            BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
      END.
      ELSE DO:
         ASSIGN
         BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
      IF TOG_UPP2 = TRUE THEN DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.      
      ELSE DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
   ELSE IF typdator = 3 THEN DO:
      IF NOT VALID-HANDLE(avcadaccessh) THEN DO:
         MESSAGE "AvCad är inte uppstartad! Kontakta Elpool i Umeå AB 090-184540" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      FILL-IN-BER = "Import från AvCad.".
      RUN load_UI IN avcadaccessh (OUTPUT musz).
      IF musz = FALSE THEN DO:
         IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
         RETURN NO-APPLY.
      END.
      musz = FALSE.
      ASSIGN
      MBTN_HMT:HIDDEN = TRUE
      SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF TOG_BER = TRUE THEN DO:
         IF typval = 2 THEN DO:      
            ASSIGN
            BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
      END.
      ELSE DO:
         ASSIGN
         BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.      
   END.
   DISPLAY FILL-IN-BER WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL-IMP C-Win
ON VALUE-CHANGED OF SEL-IMP IN FRAME DEFAULT-FRAME
DO:
   tempnum = SEL-IMP:LOOKUP(SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
   tempomr = ENTRY(tempnum, listobj, ";").
   tempbernr = ENTRY(1, tempomr, ",").
   tempomr = ENTRY(2, tempomr, ",").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BER C-Win
ON VALUE-CHANGED OF TOG_BER IN FRAME DEFAULT-FRAME /* Beredning */
DO:
   TOG_BER = INPUT TOG_BER.
   IF (valomrade = ? OR valomrade = "") AND typval = 1 THEN DO:
      MESSAGE "Ingen beredning vald.".
      ASSIGN TOG_BER = FALSE. 

      DISPLAY TOG_BER WITH FRAME {&FRAME-NAME}.
   END.
   IF typval = 2 THEN DO:
      IF typdator = 1 THEN DO:      
         SEL-IMP:HIDDEN = NOT TOG_BER.
      END.
      ELSE DO:
         IF TOG_BER = TRUE THEN DO:
            ASSIGN
            BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
         ELSE DO:
            ASSIGN
            BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.         
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRL C-Win
ON VALUE-CHANGED OF TOG_MTRL IN FRAME DEFAULT-FRAME /* Materielkatalog */
DO:
   TOG_MTRL = INPUT TOG_MTRL.
   IF typdator = 1 THEN DO:
      BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:   
      IF typdator = 2 AND TOG_MTRL = TRUE THEN DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE IF typdator = 2 AND TOG_UPP = TRUE THEN DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_UPP C-Win
ON VALUE-CHANGED OF TOG_UPP IN FRAME DEFAULT-FRAME /* Beredningsupplägg */
DO:
   TOG_UPP = INPUT TOG_UPP.
   IF typdator = 1 THEN DO:
      BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:   
      IF typdator = 2 AND TOG_UPP = TRUE THEN DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE IF typdator = 2 AND TOG_MTRL = TRUE THEN DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_UPP2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_UPP2 C-Win
ON VALUE-CHANGED OF TOG_UPP2 IN FRAME DEFAULT-FRAME /* Beredningsupplägg/Materielkatalog */
DO:
   TOG_UPP2 = INPUT TOG_UPP2.
   IF typdator = 1 THEN DO:
      BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:   
      IF typdator = 2 AND TOG_UPP2 = TRUE THEN DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE DO:
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
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
DO:
   {BORTBRWPROC.I}   
   IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
   IF VALID-HANDLE(expimpapph) THEN DELETE PROCEDURE expimpapph NO-ERROR.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.
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
   skrivaover = TRUE.
   IF typval = 1 THEN DO:   
      FILL-IN-BER = "Export till bärbar dator.".
      {&WINDOW-NAME}:TITLE = "Export".
     /*Ändring av Mikael*/
      EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
      tthandle = TEMP-TABLE beredningtemp:HANDLE.
      IF valomrade = "" OR valomrade = ? THEN DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT "").
         ASSIGN
         FILL-IN-BERNR = ?
         FILL-IN-AONR = '?'
         FILL-IN-DELNR = ? 
         FILL-IN-BEN = '?'.  
      END.
      ELSE DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT " WHERE OMRADE = '" + valomrade + "'").
         FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = valomrade AND 
         beredningtemp.BERNR = valbernr NO-LOCK NO-ERROR.   
         ASSIGN
         FILL-IN-BERNR = beredningtemp.BERNR
         FILL-IN-AONR = beredningtemp.AONR
         FILL-IN-DELNR = beredningtemp.DELNR
         FILL-IN-BEN = beredningtemp.BENAMNING.   
      END.      
   END.
   ELSE DO:
      {&WINDOW-NAME}:TITLE = "Import".
      FILL-IN-BER = "Import från bärbar dator.".
   END.
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.                                                                     
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN enable_UI. 
   {FRMSIZE.I}
   IF Guru.SharedVariable:singel = FALSE THEN DO:
   /*client-server*/
      IF typval = 2 THEN DO:      
         /*import*/
         ASSIGN
         TOG_BER = TRUE
         TOG_MTRL = FALSE
         TOG_UPP = FALSE
         TOG_UPP2 = FALSE
         TOG_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_UPP2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         TOG_BER:ROW = 4. 
         DISP  TOG_BER WITH FRAME {&FRAME-NAME}. 
      END.
      ELSE DO:
         /*export*/
         ASSIGN         
         TOG_UPP2 = FALSE
         TOG_UPP2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
   ELSE DO:
      /*singel*/
      IF typval = 1 THEN DO:               
      END.
      ELSE DO:
         /*import*/
         ASSIGN     
         TOG_BER = TRUE
         TOG_UPP2 = FALSE
         TOG_UPP2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_BER:ROW = 4.
         ASSIGN 
         TOG_MTRL:ROW = TOG_BER:ROW
         TOG_UPP:ROW = TOG_BER:ROW + 1.25.
         DISP TOG_BER WITH FRAME {&FRAME-NAME}. 
      END.
   END.
   IF typval = 1 THEN DO:
      ASSIGN
      FILL-IN-BER-2:HIDDEN = TRUE
      MBTN_HMT:HIDDEN = TRUE.
   END.
   IF typval = 2 THEN DO:
      ASSIGN
      FILL-IN-BERNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-DELNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ASSIGN
   SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globanv = "ELPAO" THEN DO:
      ASSIGN
      typdator = 1
      RAD_VAL = 1.      
   END.
   ELSE IF Guru.Konstanter:globforetag = "GRAN" AND Guru.Konstanter:globanv = "ELPAO" THEN DO:      
      ASSIGN
      typdator = 1
      RAD_VAL = 1.
      IF Guru.SharedVariable:singel = TRUE THEN DO:
         ASSIGN
         MBTN_BARBAR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         MBTN_HANDDATOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
   ELSE IF Guru.Konstanter:globforetag = "GRAN" AND Guru.Konstanter:globanv = "andersl" THEN DO:      
      ASSIGN
      typdator = 1
      RAD_VAL = 1.
      IF Guru.SharedVariable:singel = TRUE THEN DO:
         ASSIGN
         MBTN_BARBAR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         MBTN_HANDDATOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
   ELSE DO:
      ASSIGN
      MBTN_HANDDATOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      ASSIGN
      typdator = 1
      RAD_VAL = 1.
   END.
   TOG_MTRL:ROW = TOG_UPP:ROW.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN selfill_UI.    
   IF NOT VALID-HANDLE(avcadaccessh) THEN MBTN_AVCAD:HIDDEN = TRUE.
   IF typval = 1 THEN MBTN_AVCAD:HIDDEN = TRUE.
   IF MBTN_AVCAD:HIDDEN = TRUE THEN DO:
      IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BER:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_KON:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_AVCAD:HANDLE IN FRAME {&FRAME-NAME}).

   RUN setcolindex_UI IN brwproc[1] (INPUT "OMRADE"). 
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[3] (INPUT "ID"). 
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      RUN AVCADACCESS.P PERSISTENT SET avcadaccessh.
   END.

   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPUAPP.P PERSISTENT SET expimpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPUAPP.P PERSISTENT SET expimpapph.
   END. 
   tthandle = TEMP-TABLE konstgrptemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
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
  DISPLAY FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR SEL-IMP TOG_BER 
          TOG_MTRL TOG_UPP TOG_UPP2 FILL-IN-BER FILL-IN-BER-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE MBTN_BARBAR MBTN_HANDDATOR MBTN_AVCAD BTN_IMP MBTN_HMT SEL-IMP 
         BRW_AVCAD BRW_BER TOG_BER TOG_MTRL TOG_UPP TOG_UPP2 BRW_KON BTN_OK 
         BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export_UI C-Win 
PROCEDURE export_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF typdator = 2 THEN DO:
      EMPTY TEMP-TABLE kon_grupp NO-ERROR. 
      IF TOG_UPP2 = TRUE THEN DO:
         ASSIGN            
         antal_valda = BRW_KON:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         IF antal_valda = 0 THEN DO:
            MESSAGE "Ni måste välja en eller flera konstruktionsgupper"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN NO-APPLY.
         END.
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_KON:FETCH-SELECTED-ROW(antal_raknare).                                  
            CREATE kon_grupp.
            ASSIGN
            kon_grupp.KONSKOD = konstgrptemp.KONSKOD
            kon_grupp.BENAMNING = konstgrptemp.BENAMNING
            antal_raknare = antal_raknare + 1.                                 
         END.  
         MESSAGE "Vill Ni skriva över beredningsupplägg/materielkatalog som finns i handdatorn?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar3.
         IF svar3 THEN skrivaover = TRUE.
         ELSE skrivaover = FALSE.
      END.         
   END.
   IF TOG_BER = TRUE THEN DO: 
      filnamn = fildir + beredningtemp.BERAONR + "!" + beredningtemp.OMRADE + "!".
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      svar2 = FALSE.
      RUN typval_UI IN expimpapph (INPUT valomrade,INPUT valbernr,
                                   INPUT typval,INPUT typdator,
                                   INPUT TOG_BER,INPUT TOG_UPP2,INPUT Guru.Konstanter:globanv,
                                   OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar2.
         IF svar2 = FALSE THEN RETURN NO-APPLY.
      END.
      IF svar2 = FALSE THEN DO:
         RUN beranv_UI IN expimpapph (INPUT beredningtemp.BERAONR,
                                      INPUT beredningtemp.OMRADE,
                                      INPUT Guru.Konstanter:globanv).
      END.

      IF typdator = 1 THEN DO:         
          varforetypchar[50] = "".
         RUN EXPBER.P
         (INPUT filnamn, INPUT beredningtemp.BERAONR, INPUT beredningtemp.OMRADE, 
         INPUT TOG_BER, INPUT TOG_MTRL, INPUT TOG_UPP).
         IF TOG_BER = TRUE THEN MESSAGE "Beredningen är exporterad till "  filnamn VIEW-AS ALERT-BOX.
         IF TOG_MTRL = TRUE THEN MESSAGE "Materielkatalogen är exporterad till "  filnamn VIEW-AS ALERT-BOX.
         IF TOG_UPP = TRUE THEN MESSAGE "Materielupplägget är exporterat till "  filnamn VIEW-AS ALERT-BOX.
         IF TOG_BER = TRUE THEN DO:
            MESSAGE "Vill du skicka beredningen som E-post?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
            CASE svar:
               WHEN TRUE THEN DO:
                  RUN EMEDD.W (INPUT FALSE).
                  varforetypchar[50] = "".
               END.
               WHEN FALSE THEN DO:
                      
               END.
            END CASE.
         END.
      END.
      ELSE DO:            
         RUN EPOCKETPC2.P (INPUT beredningtemp.BERAONR, INPUT beredningtemp.OMRADE, INPUT Guru.Konstanter:globforetag, 
         INPUT TOG_BER, INPUT TOG_UPP2, INPUT TABLE kon_grupp, INPUT fildir, INPUT skrivaover).
      END.
   END.
   ELSE DO:
      filnamn = fildir.
      IF typdator = 1 THEN DO:         
         RUN EXPBER.P
         (INPUT filnamn, INPUT "", INPUT "",
         INPUT TOG_BER, INPUT TOG_MTRL, INPUT TOG_UPP).
         IF TOG_BER = TRUE THEN MESSAGE "Beredningen är exporterad till "  filnamn VIEW-AS ALERT-BOX.
         IF TOG_MTRL = TRUE THEN MESSAGE "Materielkatalogen är exporterad till "  filnamn VIEW-AS ALERT-BOX.
         IF TOG_UPP = TRUE THEN MESSAGE "Materielupplägget är exporterat till "  filnamn VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         RUN EPOCKETPC2.P (INPUT "", INPUT "", INPUT Guru.Konstanter:globforetag, 
         INPUT TOG_BER, INPUT TOG_UPP2, INPUT TABLE kon_grupp, INPUT fildir, INPUT skrivaover).
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import_UI C-Win 
PROCEDURE import_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF typdator = 1 THEN DO:  
      
      IF SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? AND TOG_BER = TRUE THEN DO: 
         MESSAGE "Du har inte valt någon beredning." VIEW-AS ALERT-BOX
         TITLE "Meddelande".
         IF TOG_UPP = TRUE THEN.
         ELSE RETURN NO-APPLY.
      END.
      ELSE filnamn = fildir + tempbernr + "!" + tempomr + "!".
      /*koll om beredning finns + FIXAR UPPLÄGGET*/
      RUN IMPBER.P
      (INPUT filnamn, INPUT TOG_BER, INPUT TOG_MTRL, INPUT TOG_UPP, 
      OUTPUT varbernr, OUTPUT varomrade, INPUT ?, INPUT ?).    
      IF TOG_BER = TRUE THEN DO:
         /*det finns en beredning sedan tidigare*/
         IF varbernr NE "" THEN DO:
            EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
            RUN typval2_UI IN expimpapph (INPUT varomrade,INPUT varbernr,
                                   INPUT TOG_BER,INPUT TOG_UPP,
                                   OUTPUT TABLE felmeddtemp).
            FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar.                              
            END.
            IF svar THEN DO:
               ASSIGN 
               delbernr = varbernr.
               RUN nybere_UI IN expimpapph (INPUT varomrade,OUTPUT nyttnr).
                /*skapar ny beredning*/
               RUN IMPBER.P
               (INPUT filnamn, INPUT TOG_BER, INPUT FALSE, INPUT FALSE, 
               OUTPUT varbernr, OUTPUT varomrade, INPUT nyttnr, INPUT varomrade). 
               valbernr = nyttnr.
               /*Tar bort beredning*/
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN DELBERE.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT delbernr, INPUT valomrade).
               END.
               ELSE DO:
                  RUN DELBERE.P
                  (INPUT delbernr, INPUT valomrade).                     
               END.  
               RUN delbere_UI IN expimpapph (INPUT delbernr,INPUT valomrade).
               FIND FIRST beredningtemp WHERE beredningtemp.BERAONR = delbernr AND 
               beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
               IF AVAILABLE beredningtemp THEN DELETE beredningtemp.
               RUN delberanv_UI IN expimpapph (INPUT delbernr,INPUT valomrade).               
            END.
            ELSE DO:
               MESSAGE "Vill Ni skapa en ny beredning?" SKIP
                       "Den kommer att hamna på " + gomrk + " " + varomrade + "."
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ny beredning?" UPDATE svar.         
               IF svar THEN DO:
                  RUN nybere_UI IN expimpapph (INPUT varomrade,OUTPUT nyttnr).
                  MESSAGE "Ditt nya beredningsnummer är: " + gomrk + " " + varomrade + " " + STRING(nyttnr)
                  VIEW-AS ALERT-BOX TITLE "Meddelande".   
                  RUN IMPBER.P
                  (INPUT filnamn, INPUT TOG_BER, INPUT FALSE, INPUT FALSE, 
                  OUTPUT varbernr, OUTPUT varomrade, INPUT nyttnr, INPUT varomrade).   
                  /************************* Tillägg av Mikael ****************************/
   /*                         MESSAGE "varomrade:" varomrade " nyttnr:" nyttnr VIEW-AS ALERT-BOX. */
                  EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
                  valbernr = nyttnr.
                  RUN bortberkopp_UI IN expimpapph (INPUT varomrade,INPUT varbernr,
                                                    INPUT valimpomrade,INPUT nyttnr,
                                                    OUTPUT tempaonr,OUTPUT tempdelnr,
                                                    OUTPUT TABLE felmeddtemp).
                  FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
                  IF AVAILABLE felmeddtemp THEN DO:
                     FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = valimpomrade AND
                     beredningtemp.BERNR = nyttnr NO-LOCK NO-ERROR.
                     IF AVAILABLE beredningtemp THEN DO:
                        ASSIGN
                        beredningtemp.AONR = ?
                        beredningtemp.DELNR = ?.
                     END.
                     MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.                       
                  END.
                  RUN delberanv_UI IN expimpapph (INPUT varbernr,INPUT varomrade).
               END.
               ELSE DO:
                  MESSAGE "Import av beredning avbruten!"
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  RETURN NO-APPLY.
               END.
            END.
         END.                                                                                              
         ELSE DO:
            IF varomrade = "" THEN DO:
               MESSAGE gomrk + " för den valda beredningen saknas." SKIP 
                  "Ni måste skapa en ny beredning." SKIP
                  "Den kommer att hamna på " + gomrk + " " + globomr + "."
                  "Vill Ni det?" VIEW-AS ALERT-BOX                            
               QUESTION BUTTONS YES-NO TITLE "Ny beredning?" UPDATE svar.         
               IF svar THEN DO:                           
                  RUN nybere_UI IN expimpapph (INPUT globomr,OUTPUT nyttnr).
                  MESSAGE "Ditt nya beredningsnummer är: " + gomrk + " " + globomr + " " + STRING(nyttnr)
                  VIEW-AS ALERT-BOX TITLE "Meddelande".   
                  RUN IMPBER.P
                  (INPUT filnamn, INPUT TOG_BER, INPUT FALSE, INPUT FALSE, 
                  OUTPUT varbernr, OUTPUT varomrade, INPUT nyttnr, INPUT globomr).   
                  /************************* Tillägg av Mikael ****************************/
   /*                         MESSAGE "varomrade:" varomrade " nyttnr:" nyttnr VIEW-AS ALERT-BOX. */
                  EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
                  ASSIGN
                  valimpomrade = globomr
                  valbernr = nyttnr.
                  RUN bortberkopp2_UI IN expimpapph (INPUT globomr,INPUT nyttnr,                                                       
                                                    OUTPUT TABLE felmeddtemp).
                  FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
                  IF AVAILABLE felmeddtemp THEN DO:
                     FIND FIRST beredningtemp WHERE beredningtemp.OMRADE = globomr AND
                     beredningtemp.BERNR = nyttnr NO-LOCK NO-ERROR.
                     IF AVAILABLE beredningtemp THEN DO:
                        ASSIGN
                        beredningtemp.AONR = ?
                        beredningtemp.DELNR = ?.
                     END.
                     MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.                       
                  END.                    
               END.
               ELSE DO:
                  MESSAGE "Import av beredning avbruten!"
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  RETURN NO-APPLY.
               END.                  
            END.
         END.
      END.
   END.
   ELSE IF typdator = 2 THEN DO:
      IF TOG_BER = TRUE THEN DO:
         EMPTY TEMP-TABLE valtemp NO-ERROR.             
         ASSIGN
         antal_valda = BRW_BER:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
         antal_raknare = 1.         
         DO WHILE antal_raknare LE antal_valda:
            status-ok = BRW_BER:FETCH-SELECTED-ROW(antal_raknare).         
            CREATE valtemp.
            BUFFER-COPY beredningtemp TO valtemp.
            antal_raknare = antal_raknare + 1.  
         END.   
         FOR EACH valtemp:
            EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
            RUN typval3_UI IN expimpapph (INPUT valtemp.BERNR,INPUT valtemp.OMRADE,
                                          OUTPUT TABLE felmeddtemp).
            FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               IF felmeddtemp.FELMEDD = "" THEN DO:
                  IF felmeddtemp.VAL = 1 THEN DO:
                     ASSIGN valtemp.NYTTNR = valtemp.BERNR.
                  END.
               END.
               ELSE DO:
                  MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar.         
               END.
            END.
            IF svar THEN DO:
               RUN delberanv_UI IN expimpapph (INPUT valtemp.BERNR,INPUT valtemp.OMRADE).
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN DELBERE.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT STRING(valtemp.BERNR), INPUT valtemp.OMRADE).
               END.
               ELSE DO:
                  RUN DELBERE.P
                  (INPUT STRING(valtemp.BERNR), INPUT valtemp.OMRADE).
               END.
               RUN delbere_UI IN expimpapph (INPUT valtemp.BERNR,INPUT valtemp.OMRADE).
               valtemp.NYTTNR = valtemp.BERNR.
            END. 
            ELSE DO:
               MESSAGE "Vill Ni skapa en ny beredningen?"  SKIP
                       "Den kommer att hamna på " + gomrk + " " + valtemp.OMRADE + "."
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ny beredning?" UPDATE svar.         
               IF svar THEN DO:
                  RUN nybere_UI IN expimpapph (INPUT valtemp.OMRADE,
                                               OUTPUT nyttnr).
                  MESSAGE "Ditt nya beredningsnummer är: " + gomrk + " " + valtemp.OMRADE + " " + STRING(nyttnr)
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  valtemp.NYTTNR = nyttnr.
               END.
               ELSE DO:
                  MESSAGE "Import av beredning " + STRING(valtemp.BERNR) + " avbruten!"
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  DELETE valtemp.
                  RETURN NO-APPLY.
               END.
            END.                        
         END.                                          
         RUN IPOCKETPC.P (INPUT fildir, OUTPUT TABLE beredningtemp, INPUT 2, INPUT TABLE valtemp, INPUT Guru.Konstanter:globanv).         
      END.
   END.
   ELSE IF typdator = 3 THEN DO:      
     IF valort = "" THEN valort = avcadtemp.BENAMNING.
     ASSIGN
     valomrade = globomr
     valaonr = ?
     vart = "NYA"
     avbryt = FALSE.
     RUN NYBEREU.W (INPUT 1).
     valimpomrade = valomrade.
     IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE "Import avbruten!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
     END.
     RUN berhmt_UI IN expimpapph (INPUT valbernr,INPUT valomrade,OUTPUT TABLE beredningtemp).
     FIND FIRST beredningtemp WHERE beredningtemp.BERNR = valbernr AND 
     beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
     FIND FIRST valberedningtemp WHERE valberedningtemp.BERNR = valbernr
     AND valberedningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
     IF AVAILABLE beredningtemp THEN DO:
        RUN ok_UI IN avcadaccessh (INPUT avcadtemp.ID,INPUT-OUTPUT TABLE beredningtemp).  
        FIND FIRST beredningtemp WHERE beredningtemp.BERNR = valbernr AND 
        beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
        valberedningtemp.BENAMNING = beredningtemp.BENAMNING. 
     END.         
   END.
   MESSAGE "Import klar!" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE in_UI C-Win 
PROCEDURE in_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
   INPUT STREAM dirstrom FROM OS-DIR(fildir) NO-ECHO.
   REPEAT:
      /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
      SET STREAM dirstrom tmpfilnamn tmpdirlist tmpattrlist.
      IF typval = 2 THEN DO:
         IF tmpfilnamn MATCHES "*beredning.d" THEN DO:
            INPUT FROM VALUE(tmpdirlist). /* convert target "iso8859-1" source "iso8859-1"*/
               IMPORT UNFORMATTED textstring.
            INPUT CLOSE.
            IF listobj = "" THEN listobj = ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
            ELSE listobj = listobj + ";" + ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
            tempstring = "Bernr: " + ENTRY(1,tmpfilnamn,"!") + " Område: " + ENTRY(2,tmpfilnamn,"!") +
               " Benm: " + ENTRY(4,textstring,"|").
            FIND FIRST imptemp WHERE imptemp.INVAR = tempstring NO-LOCK NO-ERROR.
            IF NOT AVAILABLE imptemp THEN DO:
               CREATE imptemp.
               imptemp.INVAR = tempstring.
               SEL-IMP:ADD-LAST(tempstring) IN FRAME {&FRAME-NAME}.  
            END.            
         END.
      END.
   END.
   INPUT STREAM dirstrom CLOSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selfill_UI C-Win 
PROCEDURE selfill_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/   
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
      fildir = "\\SRV00486\APPL\GURU\DOKUMENT\BEREDNING\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "CLULE" THEN DO:
      fildir = "\\195.196.19.8\DOKUMENT\BEREDNING\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
      fildir = "\\pc012\d\DELAD\dokument\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "STRA" THEN DO:
      fildir = "\\srv07web\DELAD\Gemensam bärbart\" + globanv + "\".
   END.
   ELSE DO:
      fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   END. 
   OS-CREATE-DIR VALUE(fildir) NO-ERROR. 
   mappvar = fildir.
   RUN in_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

