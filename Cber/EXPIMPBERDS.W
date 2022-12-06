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
CREATE WIDGET-POOL "DynTableEXIM" NO-ERROR.
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
/*Ändring av Mikael*/
/* DEFINE INPUT PARAMETER valrec AS RECID NO-UNDO. */
DEFINE INPUT PARAMETER typval AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{TTEXPIMPHD.I}
&SCOPED-DEFINE SHARED SHARED
{OMRTEMPW.I}
{BERTEMPTAB.I}
{LISTMTRL.I}
&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{HUVLEVTEMP.I}
{KONSTRMTRL.I}
{BEREDNINGTEMP.I}
{EXTRADATA.I}
{EXTRATAB.I}  
{HOPALLA.I}
{KalkylimportTT.I}
DEFINE VARIABLE valnlevmtrlkat AS CHARACTER NO-UNDO.
DEFINE VARIABLE BerKalkDS AS HANDLE NO-UNDO.
DEFINE VARIABLE utschemalong AS LONGCHAR NO-UNDO.
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.

DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ekoppdataapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE valimpomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valanv AS CHARACTER NO-UNDO.




DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vbytlevapph3 AS HANDLE NO-UNDO.
DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE dynxmlOE10 AS HANDLE NO-UNDO.
DEFINE VARIABLE attrlist AS CHARACTER FORMAT "x(6)" LABEL "Attributes" NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvarAVCAD AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpattrlist AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tmpdirlist AS CHARACTER FORMAT "X(68)" NO-UNDO.  
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
DEFINE VARIABLE filnamnuppxml AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE tempomr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbernr AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempnum AS INTEGER NO-UNDO.
DEFINE VARIABLE listobj AS CHARACTER NO-UNDO.
DEFINE VARIABLE typdator AS INTEGER NO-UNDO.
DEFINE VARIABLE spartypdator AS INTEGER NO-UNDO.
DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO. 
DEFINE STREAM dirstrom.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE skrivaover AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE expimpapph AS HANDLE NO-UNDO.
DEFINE VARIABLE skapanyttnummer AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaprochspar AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE avcadaccessh AS HANDLE NO-UNDO.
DEFINE VARIABLE avbryt AS LOGICAL NO-UNDO.
DEFINE VARIABLE korfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE dbhand AS CHARACTER NO-UNDO.
DEFINE VARIABLE felhand AS LOGICAL NO-UNDO.
DEFINE VARIABLE xmlsparfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE aofinns AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparasing AS LOGICAL NO-UNDO.
DEFINE VARIABLE nodtabhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE lnamnex AS CHARACTER NO-UNDO.
DEFINE VARIABLE antmtrlber AS INTEGER NO-UNDO.
DEFINE VARIABLE unikhelp AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE kon_grupp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE TEMP-TABLE avcadtemp
   FIELD ID AS INTEGER LABEL "Id"
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD COMMENT AS CHARACTER LABEL "Kommentar"
   FIELD AVVAGNING AS INTEGER LABEL "Avvägning"
   INDEX ID IS PRIMARY ID.

DEFINE TEMP-TABLE xmltabelfile NO-UNDO
   FIELD BufferH AS HANDLE
   FIELD TempttH AS HANDLE
   FIELD TableName AS CHARACTER
   FIELD TempName AS CHARACTER
   FIELD NodtabName AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORDNING ORDNING.   

DEFINE TEMP-TABLE valtemp                  
   FIELD OMRADE AS CHARACTER
   FIELD BERNR AS INTEGER   
   FIELD BENAMNING AS CHARACTER
   FIELD NYTTNR AS INTEGER INITIAL 0
   INDEX OMR OMRADE BERNR ASCENDING.
DEFINE TEMP-TABLE imptemp NO-UNDO
   FIELD INVAR AS CHARACTER
   FIELD INDIR AS CHARACTER.
DEFINE TEMP-TABLE berfil NO-UNDO
   FIELD INDIR AS CHARACTER
   FIELD FILNAMNS AS CHARACTER
   FIELD ROWBER  AS ROWID
   INDEX ROWBER ROWBER.
{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AVCAD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES avcadtemp beredningtemp

/* Definitions for BROWSE BRW_AVCAD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_AVCAD avcadtemp.ID avcadtemp.BENAMNING ~
avcadtemp.Avvagning avcadtemp.COMMENT 
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


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_BARBAR MBTN_AVCAD RECT-SOK ~
MBTN_SOKAVCAD IMAGE-SOK MBTN_HMT FILL-IN-AVCADDB BTN_IMP FILL-IN-MINSTAG ~
TOG_MTRLPRISBER SEL-IMP BRW_AVCAD BRW_BER SEL_XML FILL-IN-MAPP ~
FILL-IN-FILNAMN BTN_FILNAMN FILL-IN-SID FILL-IN-SBEN FILL-IN-SAV BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BERNR FILL-IN-MINSTAG FILL-IN-BEN ~
FILL-IN-AONR FILL-IN-DELNR TOG_MTRLPRISBER SEL-IMP SEL_XML FILL-IN-MAPP ~
FILL-IN-FILNAMN FILL-IN-SID FILL-IN-SBEN FILL-IN-SAV FILL-IN-BER ~
FILL-IN-BER-2 FILL-IN-HITTA 

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

DEFINE BUTTON BTN_FILNAMN 
     LABEL "Import från Filnamn" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMP 
     LABEL "Hämta data från handdator" 
     SIZE 27 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_AVCAD 
     LABEL "AvCAD" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_BARBAR 
     LABEL "Bärbar dator" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_HMT  NO-FOCUS
     LABEL "Sök beredning" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_SOKAVCAD  NO-FOCUS
     LABEL "Välj AvCad databas" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVCADDB AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 57.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 30.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BER-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Välj beredning för import" 
      VIEW-AS TEXT 
     SIZE 30 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Beredning nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FILNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HITTA AS CHARACTER FORMAT "X(256)":U INITIAL "Hittar du inte din beredning, skriv in mapp och filnamn här!" 
      VIEW-AS TEXT 
     SIZE 81.5 BY 1.21
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mapp" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MINSTAG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Minsta stagarea" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Välj min stagarea. Om 0 får man avvägningens stagarea" NO-UNDO.

DEFINE VARIABLE FILL-IN-SAV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avvägning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SBEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SID AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Id" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-SOK
     FILENAME "BILDER/sokpa.gif":U
     SIZE 9.25 BY .83.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bärbardator", 1,
"Handdator", 2,
"AvCAD", 3
     SIZE 53.5 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 81 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE SEL-IMP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 94 BY 11 DROP-TARGET NO-UNDO.

DEFINE VARIABLE SEL_XML AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 52.5 BY 16.5 NO-UNDO.

DEFINE VARIABLE TOG_MTRLPRISBER AS LOGICAL INITIAL no 
     LABEL "Ta med pris i beredningen" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AVCAD FOR 
      avcadtemp SCROLLING.

DEFINE QUERY BRW_BER FOR 
      beredningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVCAD C-Win _STRUCTURED
  QUERY BRW_AVCAD NO-LOCK DISPLAY
      avcadtemp.ID FORMAT "->,>>>,>>9":U
      avcadtemp.BENAMNING FORMAT "X(256)":U WIDTH 49
      avcadtemp.Avvagning FORMAT "->,>>>,>>9":U WIDTH 9
      avcadtemp.COMMENT FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS DROP-TARGET SIZE 84.25 BY 16.5
         TITLE "Välj Avvägning för import" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER C-Win _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      beredningtemp.OMRADE FORMAT "x(6)":U
      beredningtemp.BERNR COLUMN-LABEL "Beredningsnr" FORMAT ">>>>>>9":U
      beredningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 75 BY 16.5
         TITLE "Välj beredning för import" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MBTN_BARBAR AT ROW 1.08 COL 1.13
     MBTN_AVCAD AT ROW 1.08 COL 28.5
     MBTN_SOKAVCAD AT ROW 5.5 COL 21.5 WIDGET-ID 30
     MBTN_HMT AT ROW 7.29 COL 98.5
     RAD_VAL AT ROW 1.25 COL 59.5 NO-LABEL
     tmpfilnamn AT ROW 4 COL 65.5 COLON-ALIGNED NO-LABEL WIDGET-ID 28
     FILL-IN-AVCADDB AT ROW 5.5 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FILL-IN-BERNR AT ROW 6.08 COL 16.5 COLON-ALIGNED
     BTN_IMP AT ROW 6.92 COL 36.13
     FILL-IN-MINSTAG AT ROW 7 COL 77 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-BEN AT ROW 7.25 COL 16.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 8.38 COL 16.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 8.38 COL 38.5 COLON-ALIGNED
     TOG_MTRLPRISBER AT ROW 9.5 COL 36.5 WIDGET-ID 22
     SEL-IMP AT ROW 11 COL 2.63 NO-LABEL
     BRW_AVCAD AT ROW 11 COL 12
     BRW_BER AT ROW 11 COL 12.13
     SEL_XML AT ROW 11 COL 60 NO-LABEL
     FILL-IN-MAPP AT ROW 23.88 COL 10 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-FILNAMN AT ROW 25.25 COL 10 COLON-ALIGNED WIDGET-ID 6
     BTN_FILNAMN AT ROW 25.25 COL 68 WIDGET-ID 8
     FILL-IN-SID AT ROW 27.75 COL 16.5 COLON-ALIGNED
     FILL-IN-SBEN AT ROW 27.75 COL 32.5
     FILL-IN-SAV AT ROW 27.75 COL 65.5 COLON-ALIGNED
     BTN_OK AT ROW 28.04 COL 83.5
     BTN_AVB AT ROW 28.04 COL 98.5
     FILL-IN-BER AT ROW 3.63 COL 2.5 NO-LABEL
     FILL-IN-BER-2 AT ROW 4.88 COL 2.5 NO-LABEL
     FILL-IN-HITTA AT ROW 22.25 COL 3 NO-LABEL WIDGET-ID 2
     RECT-SOK AT ROW 27.71 COL 1.5
     IMAGE-SOK AT ROW 27.92 COL 2.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.38 BY 28.42.


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
         HEIGHT             = 28.42
         WIDTH              = 112.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 112.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 112.38
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
/* BROWSE-TAB BRW_AVCAD SEL-IMP DEFAULT-FRAME */
/* BROWSE-TAB BRW_BER BRW_AVCAD DEFAULT-FRAME */
ASSIGN 
       BRW_AVCAD:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_BER:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-AVCADDB IN FRAME DEFAULT-FRAME
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-AVCADDB:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE
       FILL-IN-AVCADDB:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

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
/* SETTINGS FOR FILL-IN FILL-IN-HITTA IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-HITTA:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SBEN IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       MBTN_AVCAD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_BARBAR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_HMT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       MBTN_SOKAVCAD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       SEL-IMP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN tmpfilnamn IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       tmpfilnamn:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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
"avcadtemp.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "49" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.avcadtemp.Avvagning
"avcadtemp.Avvagning" ? ? "integer" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.avcadtemp.COMMENT
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVCAD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "Temp-Tables.beredningtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.beredningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.beredningtemp.BERNR
"beredningtemp.BERNR" "Beredningsnr" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.beredningtemp.BENAMNING
"beredningtemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BER */
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
ON DROP-FILE-NOTIFY OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj Avvägning för import */
DO:
   DEFINE VARIABLE n AS INTEGER NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
   DO n = 1 TO 1:
      fildir = BRW_AVCAD:GET-DROPPED-FILE(n).
      IF fildir MATCHES "*.mdb" THEN.
      ELSE DO:
         MESSAGE "Det är inte rätt fil!"
         VIEW-AS ALERT-BOX.
         RETURN  NO-APPLY.
      END.    
      FILL-IN-AVCADDB = fildir.
      DISPLAY FILL-IN-AVCADDB WITH FRAME {&FRAME-NAME}.
      Guru.SharedVariable:ValdAvCADdb = FILL-IN-AVCADDB.
      RAD_VAL = 3.
      RUN AnslutAvCad_UI. 
      
   END.
   BRW_AVCAD:END-FILE-DROP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVCAD C-Win
ON LEFT-MOUSE-CLICK OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj Avvägning för import */
DO:
   status-ok = BRW_AVCAD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVCAD C-Win
ON VALUE-CHANGED OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj Avvägning för import */
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


&Scoped-define SELF-NAME BTN_FILNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FILNAMN C-Win
ON CHOOSE OF BTN_FILNAMN IN FRAME DEFAULT-FRAME /* Import från Filnamn */
DO:
        
   ASSIGN
   FILL-IN-MAPP = INPUT FILL-IN-MAPP
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   TOG_MTRLPRISBER = INPUT TOG_MTRLPRISBER
   SEL-IMP = INPUT SEL-IMP.   
   IF typval = 2 THEN DO:
      IF LENGTH(FILL-IN-MAPP) < 2 THEN RETURN.
      IF SUBSTRING(FILL-IN-MAPP,LENGTH(FILL-IN-MAPP),1) NE "\" THEN FILL-IN-MAPP = FILL-IN-MAPP + "\".      
      IF LENGTH(FILL-IN-FILNAMN) = 0 THEN RETURN.
      IF SUBSTRING(FILL-IN-MAPP,1) = "\" THEN FILL-IN-MAPP = SUBSTRING(FILL-IN-MAPP,2).
      IF INDEX(FILL-IN-FILNAMN,"$.XML") = 0 THEN DO:
         IF SUBSTRING(FILL-IN-FILNAMN,LENGTH(FILL-IN-FILNAMN),1) = "$" THEN FILL-IN-FILNAMN = FILL-IN-FILNAMN + ".XML".
         ELSE FILL-IN-FILNAMN = FILL-IN-FILNAMN + "$.XML".         
      END.   
      IF SEARCH(FILL-IN-MAPP + FILL-IN-FILNAMN) = ? THEN DO:
         MESSAGE "Filen " FILL-IN-MAPP + FILL-IN-FILNAMN " finns inte!"
         VIEW-AS ALERT-BOX.
         RETURN.
      END.         
      SEL-IMP:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
      DISPLAY SEL-IMP WITH FRAME {&FRAME-NAME}.       
      EMPTY TEMP-TABLE imptemp NO-ERROR. 
      EMPTY TEMP-TABLE berfil NO-ERROR. 
      EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
      fildir = FILL-IN-MAPP.
      tmpfilnamn = FILL-IN-FILNAMN.    
      RUN inxmlen_UI.  
   END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMP C-Win
ON CHOOSE OF BTN_IMP IN FRAME DEFAULT-FRAME /* Hämta data från handdator */
DO:
   IF Guru.SharedVariable:demokvar = TRUE THEN DO:
      MESSAGE "Handdator funktionen är en extra funktion som offereras separat." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
    {muswait.i}
   IF typdator = 3 THEN DO:
      RAD_VAL = 3.
      RUN AnslutAvCad_UI.
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   DEFINE VARIABLE avblg AS LOGICAL NO-UNDO.
   {muswait.i}      
   ASSIGN
   FILL-IN-MINSTAG = INPUT FILL-IN-MINSTAG
   FILL-IN-MAPP = INPUT FILL-IN-MAPP
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   TOG_MTRLPRISBER = INPUT TOG_MTRLPRISBER
   SEL-IMP = INPUT SEL-IMP.   
   /*Export*/   
   IF typval = 1 THEN DO:
      RUN export_UI.      
   END.
   ELSE IF typval = 2 THEN DO:
      /*Import*/      
      RUN import_UI (OUTPUT avblg).
      IF avblg = TRUE THEN RETURN NO-APPLY.
   END.
   {musarrow.i}
   ASSIGN
   valomrade = valimpomrade.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_AVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_AVCAD C-Win
ON CHOOSE OF MBTN_AVCAD IN FRAME DEFAULT-FRAME /* AvCAD */
DO:
   IF Guru.SharedVariable:demokvar = TRUE THEN DO:
      MESSAGE "AvCad funktionen är en extra funktion som offereras separat." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RAD_VAL = 3.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   IF Guru.SharedVariable:ValdAvCADdb NE "" THEN FILL-IN-AVCADDB = Guru.SharedVariable:ValdAvCADdb.
   DISPLAY FILL-IN-AVCADDB WITH FRAME {&FRAME-NAME}.
   
   RUN AnslutAvCad_UI.
              
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


&Scoped-define SELF-NAME MBTN_HMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_HMT C-Win
ON CHOOSE OF MBTN_HMT IN FRAME DEFAULT-FRAME /* Sök beredning */
DO:     
   
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO FORMAT "X(50)".
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   IF typdator = 1 THEN DO:
      SYSTEM-DIALOG GET-FILE fildir
      TITLE          "Välj den fil som Ni villläsa in"
      FILTERS        "All Files (*$.xml)"  "*$.xml"   
      INITIAL-DIR    mappvar
      RETURN-TO-START-DIR
      UPDATE OKvald.  
      IF OKvald = TRUE THEN DO:
         tmpfilnamn = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1). 
         fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).         
         RUN inxmlen_UI.
         RETURN.
      END.          
   END.   
   IF OKvald = TRUE THEN SEL-IMP:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".   
   IF OKvald = TRUE THEN DO:
      MESSAGE fildir
      VIEW-AS ALERT-BOX.
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      EMPTY TEMP-TABLE imptemp NO-ERROR. 
      EMPTY TEMP-TABLE berfil NO-ERROR. 
      EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
      tmpfilnamn = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1).       
      RUN in_UI.     
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_SOKAVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_SOKAVCAD C-Win
ON CHOOSE OF MBTN_SOKAVCAD IN FRAME DEFAULT-FRAME /* Välj AvCad databas */
DO:     
   IF mappvarAVCAD = "" THEN mappvarAVCAD = mappvar.
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO FORMAT "X(256)".
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den fil som Ni villläsa in"
   FILTERS        "All Files (*.mdb)"  "*.mdb"   
   INITIAL-DIR    mappvarAVCAD
   RETURN-TO-START-DIR
   UPDATE OKvald.  
   IF OKvald = TRUE THEN DO:
      FILL-IN-AVCADDB = fildir.
      Guru.SharedVariable:ValdAvCADdb = FILL-IN-AVCADDB.
      tmpfilnamn = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1). 
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      mappvarAVCAD = fildir.
      DISPLAY FILL-IN-AVCADDB WITH FRAME {&FRAME-NAME}.
      EMPTY TEMP-TABLE avcadtemp NO-ERROR.  
      RUN close_UI IN avcadaccessh.
      RUN setcolsortvar_UI IN brwproc[3] (INPUT "").
      RUN openbdynspec_UI IN brwproc[3].       
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   spartypdator = typdator.
   typdator = RAD_VAL.

   mvar = "*.xml".
   ASSIGN
   BTN_FILNAMN:HIDDEN = TRUE
   FILL-IN-HITTA:HIDDEN = TRUE
   FILL-IN-MAPP:HIDDEN = TRUE
   FILL-IN-FILNAMN:HIDDEN = TRUE.
   FILL-IN-MINSTAG:HIDDEN = TRUE.
   IF typdator = 1 THEN DO:
      FILL-IN-BER-2 = "Välj beredning för import".
      ASSIGN
      BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      MBTN_SOKAVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
      RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
      FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
      FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
      FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      mvar = "*$.xml".
      /*import*/
      IF typval = 2 THEN DO:  
         FILL-IN-BER = "Import från annan dator.".
         MBTN_HMT:HIDDEN = FALSE.         
         SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
         ASSIGN
         BTN_FILNAMN:HIDDEN = FALSE
         FILL-IN-HITTA:HIDDEN = FALSE
         FILL-IN-MAPP:HIDDEN = FALSE
         FILL-IN-FILNAMN:HIDDEN = FALSE.         
      END. 
      ELSE DO:
         /*Export*/
         FILL-IN-BER = "Export till annan dator.".
         IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:                               
         END.
      END.
      RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).      
   END.
   ELSE IF typdator = 3 THEN DO:
      ASSIGN
      MBTN_HMT:HIDDEN = TRUE
      SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MTRLPRISBER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF typval = 2 THEN DO:      
         ASSIGN
         FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         MBTN_SOKAVCAD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
         RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
         FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
         FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
         FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         BTN_IMP:LABEL = "Ladda data från AvCAD på nytt".
      END.
      ELSE DO:
         ASSIGN
         FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.      
      RUN btnvit_UI (INPUT MBTN_AVCAD:HANDLE).
      
   END.
 
   DISPLAY FILL-IN-BER FILL-IN-BER-2 WITH FRAME {&FRAME-NAME}.
   RUN PlaceraKnapp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL-IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL-IMP C-Win
ON DROP-FILE-NOTIFY OF SEL-IMP IN FRAME DEFAULT-FRAME
DO:
   DEFINE VARIABLE n AS INTEGER NO-UNDO.
   DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
   DO n = 1 TO SEL-IMP:NUM-DROPPED-FILES:
      fildir = SEL-IMP:GET-DROPPED-FILE(n).
      tmpfilnamn = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1). 
      fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
      RUN inxmlen_UI.
   END.
   SEL-IMP:END-FILE-DROP(). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME TOG_MTRLPRISBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRLPRISBER C-Win
ON VALUE-CHANGED OF TOG_MTRLPRISBER IN FRAME DEFAULT-FRAME /* Ta med pris i beredningen */
DO:
   TOG_MTRLPRISBER = INPUT TOG_MTRLPRISBER.
   
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
      
   IF VALID-HANDLE(ekoppdataapph) THEN DELETE PROCEDURE ekoppdataapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
   IF VALID-HANDLE(expimpapph) THEN DELETE PROCEDURE expimpapph NO-ERROR.
   
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.
   IF VALID-HANDLE(dynxmlOE10) THEN DELETE PROCEDURE dynxmlOE10 NO-ERROR.
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
   
   IF Guru.Konstanter:globomr = "" THEN DO:
      FIND FIRST omrtemp NO-LOCK NO-ERROR.
      Guru.Konstanter:globomr = omrtemp.OMRADE.
   END.      
   skrivaover = TRUE.
   IF typval = 1 THEN DO:       
      FILL-IN-BER = "Export till annan dator.".
      {&WINDOW-NAME}:TITLE = "Export".     
      EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
      tthandle = TEMP-TABLE beredningtemp:HANDLE.
      IF valomrade = "" OR valomrade = ? THEN DO:
         MESSAGE "Ingen beredning vald."
         VIEW-AS ALERT-BOX. 
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK.  
      END.
      ELSE DO:
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEREDNING", INPUT " WHERE OMRADE = '" + valomrade + "'" + 
         " AND BEREDNING.BERNR = " + STRING(valbernr)).
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
      FILL-IN-BER = "Import från annan dator.".     
   END.
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.                                                                     
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN enable_UI. 
   FILL-IN-AVCADDB:HIDDEN = TRUE. 
   SEL_XML:HIDDEN = TRUE.
   {FRMSIZE.I}     
   TOG_MTRLPRISBER = FALSE. 
   DISP  TOG_MTRLPRISBER WITH FRAME {&FRAME-NAME}. 
   IF musz = TRUE THEN DO:
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.    
   IF Guru.SharedVariable:singel = FALSE THEN DO:
   /*client-server*/
   END.
   ELSE DO:
      /*singel*/
      IF typval = 1 THEN DO:               
      END.
      ELSE DO:
         /*import*/         
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
   BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
    
   ASSIGN
   typdator = 1
   RAD_VAL = 1.   
   APPLY "VALUE-CHANGED" TO RAD_VAL. 
   IF typval = 1 THEN DO:
      ASSIGN
      FILL-IN-BER-2:HIDDEN = TRUE
      MBTN_HMT:HIDDEN = TRUE.
   END. 
   RUN selfill_UI.   
   IF typval = 2 THEN RUN in_UI.
   IF NOT VALID-HANDLE(avcadaccessh) THEN DO:
      MBTN_AVCAD:HIDDEN = TRUE.      
   END.   
   IF typval = 1 THEN DO:
      MBTN_AVCAD:HIDDEN = TRUE.      
   END.   
   IF MBTN_AVCAD:HIDDEN = TRUE THEN DO:
      IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
   END.   
   ASSIGN
   MBTN_BARBAR:COLUMN = 1.
   ASSIGN
   Guru.GlobalaVariabler:collefth = MBTN_BARBAR:HANDLE.
   IF MBTN_AVCAD:HIDDEN = FALSE THEN DO:
      Guru.GlobalaVariabler:colrighth = MBTN_AVCAD:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   MBTN_BARBAR:LOAD-IMAGE("BILDER\xbtn_barb.gif") NO-ERROR.
  /* MBTN_AVCAD:LOAD-IMAGE("BILDER\xbtn_avcad.gif") NO-ERROR.*/ 
   MBTN_AVCAD:LOAD-IMAGE("BILDER\AvCad.png") NO-ERROR.     
   RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
   IF typval = 1 THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN.
   IF typval = 1 THEN DO:
   END.
   ELSE TOG_MTRLPRISBER:HIDDEN = TRUE.
   FILL-IN-MINSTAG:HIDDEN = TRUE.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN TOG_MTRLPRISBER:HIDDEN = TRUE.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_AVCAD:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addfillin_UI IN brwproc[3]                                  
      (INPUT FILL-IN-SID:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ID").
   RUN addfillin_UI IN brwproc[3]                                  
      (INPUT FILL-IN-SBEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").
   RUN addfillin_UI IN brwproc[3]                                  
      (INPUT FILL-IN-SAV:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AVVAGNING").
   RUN setcolindex_UI IN brwproc[1] (INPUT "OMRADE"). 
 
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   /*
      RUN setcolindex_UI IN brwproc[3] (INPUT "ID").
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).
*/  
   IF typval = 2 THEN DO:
      RUN AVCADACCESS.P PERSISTENT SET avcadaccessh (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag).
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPUAPPHD.P PERSISTENT SET expimpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPUAPPHD.P PERSISTENT SET expimpapph.
   END. 
   RUN READWRIXMLOE.P PERSISTENT SET dynxmlOE10.
   /*HD HÄR*/    
   
   tthandle = TEMP-TABLE konstgrptemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRATABHMT.P PERSISTENT SET ekoppdataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRATABHMT.P PERSISTENT SET ekoppdataapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN VBYTLEVAPP3.P PERSISTENT SET vbytlevapph3 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VBYTLEVAPP3.P PERSISTENT SET vbytlevapph3.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnslutAvCad_UI C-Win 
PROCEDURE AnslutAvCad_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE avcadtemp NO-ERROR.
   FILL-IN-BER-2 = "Välj Avvägning för import!".
   FILL-IN-BER = "Import från AvCAD.".  
   IF NOT VALID-HANDLE(avcadaccessh) THEN DO:
      MESSAGE "AvCad är inte uppstartad! Kontakta Elpool i Umeå AB 090-184540" VIEW-AS ALERT-BOX.
      ASSIGN
      RAD_VAL = spartypdator
      typdator = spartypdator.
      RETURN NO-APPLY.
   END.      
   DISPLAY FILL-IN-BER FILL-IN-BER-2 WITH FRAME DEFAULT-FRAME.
   RUN close_UI IN avcadaccessh.
   RUN load_UI IN avcadaccessh (INPUT FILL-IN-AVCADDB, OUTPUT musz, OUTPUT FILL-IN-AVCADDB).
   IF musz = FALSE THEN DO:
      IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
      ASSIGN
      RAD_VAL = spartypdator
      typdator = spartypdator.
      RETURN.
   END.       
   DISPLAY FILL-IN-AVCADDB WITH FRAME {&FRAME-NAME}.
   BTN_IMP:LABEL = "Ladda data från AvCAD på nytt".
   FILL-IN-MINSTAG:HIDDEN = FALSE.
   musz = FALSE.
   ASSIGN
   MBTN_HMT:HIDDEN = TRUE
   SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_MTRLPRISBER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   DISPLAY FILL-IN-BER FILL-IN-BER-2 WITH FRAME {&FRAME-NAME}.
   RUN avcad_UI IN avcadaccessh (OUTPUT TABLE avcadtemp).
   RUN setcolsortvar_UI IN brwproc[3] (INPUT "").
   RUN openbdynspec_UI IN brwproc[3].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvit_UI C-Win 
PROCEDURE btnvit_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER btnh AS HANDLE NO-UNDO.
   DEFINE VARIABLE btnlabel AS CHARACTER NO-UNDO.
   IF btnnovit NE ? THEN DO:
      btnlabel = btnnovit:IMAGE.
      btnlabel = REPLACE(btnlabel,"_vit.gif",".gif"). 
      btnnovit:LOAD-IMAGE (btnlabel) NO-ERROR.
   END.
   btnlabel = btnh:IMAGE.
   btnlabel = REPLACE(btnlabel,".gif","_vit.gif"). 
   btnh:LOAD-IMAGE (btnlabel) NO-ERROR.
   btnnovit = btnh.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CreateCustomQuery C-Win 
PROCEDURE CreateCustomQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "DynTableEXIM".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
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
  DISPLAY FILL-IN-BERNR FILL-IN-MINSTAG FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR 
          TOG_MTRLPRISBER SEL-IMP SEL_XML FILL-IN-MAPP FILL-IN-FILNAMN 
          FILL-IN-SID FILL-IN-SBEN FILL-IN-SAV FILL-IN-BER FILL-IN-BER-2 
          FILL-IN-HITTA 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE MBTN_BARBAR MBTN_AVCAD RECT-SOK MBTN_SOKAVCAD IMAGE-SOK MBTN_HMT 
         FILL-IN-AVCADDB BTN_IMP FILL-IN-MINSTAG TOG_MTRLPRISBER SEL-IMP 
         BRW_AVCAD BRW_BER SEL_XML FILL-IN-MAPP FILL-IN-FILNAMN BTN_FILNAMN 
         FILL-IN-SID FILL-IN-SBEN FILL-IN-SAV BTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExportBer_UI C-Win 
PROCEDURE ExportBer_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fildir AS CHARACTER FORMAT "X(256)" NO-UNDO.
   DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.
   RUN selxml_UI (INPUT "Beredningen sammanställs").   
   DELETE OBJECT BerKalkDS NO-ERROR.
   BerKalkDS = ?.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN EXPORTAVBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:AppSpringSet[1],INPUT valaonr, INPUT valomrade, INPUT TOG_MTRLPRISBER,OUTPUT DATASET-HANDLE BerKalkDS, OUTPUT utschemalong,OUTPUT TABLE xmltabelfile). 
                                        
   END.
   ELSE DO:
      RUN EXPORTAVBER.P
      (INPUT Guru.Konstanter:AppSpringSet[1],INPUT valaonr, INPUT valomrade, INPUT TOG_MTRLPRISBER,OUTPUT DATASET-HANDLE BerKalkDS, OUTPUT utschemalong,OUTPUT TABLE xmltabelfile).                               
   END.
  
   /*
   COPY-LOB FROM utschemalong TO FILE "C:\PROTEMP11\FAN.XML".
   BerKalkDS:READ-XMLSCHEMA ("LONGCHAR", utschemalong,FALSE).
   */
   RUN xmltabelfileReload.   
   mvarhj = "$.xml".
   FIND FIRST beredningtemp WHERE NO-LOCK NO-ERROR.
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   filnamn = fildir + beredningtemp.BERAONR + "!" + beredningtemp.OMRADE + "!" + beredningtemp.BENAMNING + mvarhj .
   
   RUN WritexmlDS_UI IN dynxmlOE10 (INPUT filnamn,false,INPUT DATASET-HANDLE BerKalkDS BIND).
   
   /*fix för äldre versions*/ 
   COPY-LOB FROM FILE filnamn TO utfillong .
   utfillong = REPLACE(utfillong,'< xmlns:xsi="http','<ProDataSet xmlns:xsi="http').
   utfillong = REPLACE(utfillong,'<xsd:element name="" prodata:proDataSet="true">','<xsd:element name="ProDataSet" prodata:proDataSet="true">').
   utfillong = REPLACE(utfillong,'</>','</ProDataSet>').
   COPY-LOB FROM utfillong TO FILE filnamn.   
   BerKalkDS:EMPTY-DATASET().
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
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".      
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   svar2 = FALSE.
   RUN selxml_UI (INPUT "Beredningen kontrolleras").               
   RUN typval_UI IN expimpapph (INPUT valomrade,INPUT valbernr,
                                INPUT typval,INPUT typdator,
                                INPUT TRUE,INPUT Guru.Konstanter:globanv,
                                OUTPUT TABLE felmeddtemp).
   
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar2.
      IF svar2 = FALSE THEN DO:
         SEL_XML:HIDDEN = TRUE.
         RETURN NO-APPLY.
      END.
   END.
   IF svar2 = FALSE THEN DO:
      RUN beranv_UI IN expimpapph (INPUT beredningtemp.BERAONR,
                                   INPUT beredningtemp.OMRADE,
                                   INPUT Guru.Konstanter:globanv).
   END.
   IF typdator = 1 THEN DO:         
      RUN expxml_UI.                          
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expxml_UI C-Win 
PROCEDURE expxml_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN ExportBer_UI.
   MESSAGE "Beredningen är exporterad till "  filnamn VIEW-AS ALERT-BOX.   
   Guru.Konstanter:varforetypchar[50] = "".   
   MESSAGE "Vill du skicka beredningen som E-post?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
   CASE svar:
      WHEN TRUE THEN DO:
         Guru.Konstanter:varforetypchar[50] = filnamn.               
      END.
      WHEN FALSE THEN DO:
         filnamn = "".               
      END.
   END CASE.
   IF Guru.Konstanter:varforetypchar[50] NE ""  THEN DO:      
      DEFINE VARIABLE infiler AS CHARACTER NO-UNDO.
      infiler = filnamn.      
      DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
      FILE-INFO:FILE-NAME = ".".
      orgdir = FILE-INFO:FULL-PATHNAME.
      RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT infiler).
      RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).      
      Guru.Konstanter:varforetypchar[50] = "".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImportBer_UI C-Win 
PROCEDURE ImportBer_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tempglobforetag NO-ERROR.
   DEFINE VARIABLE ttberh AS HANDLE NO-UNDO.
   DEFINE VARIABLE klogidvar AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.
   ELSE DO:
      RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar.      
   END.
   /*import beredning*/     
   RUN selxml_UI (INPUT "Beredningen läses in i Guru!").    
  /*Anders Olsson Elpool i Umeå AB  17 okt 2017 08:31:02 
  hämtar tomt dataset 
  */  
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN EXPORTAVBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:AppSpringSet[1],INPUT ?, INPUT ?, INPUT TOG_MTRLPRISBER,OUTPUT DATASET-HANDLE BerKalkDS,OUTPUT utschemalong,OUTPUT TABLE xmltabelfile).                                         
   END.
   ELSE DO:
      RUN EXPORTAVBER.P
      (INPUT Guru.Konstanter:AppSpringSet[1],INPUT ?, INPUT ?, INPUT TOG_MTRLPRISBER,OUTPUT DATASET-HANDLE BerKalkDS,OUTPUT utschemalong,OUTPUT TABLE xmltabelfile).                                     
   END.   
   RUN xmltabelfileReload.
   FIND FIRST berfil WHERE berfil.ROWBER = ROWID(beredningtemp) NO-LOCK NO-ERROR.
   RUN ReadxmlDS_UI IN dynxmlOE10 (INPUT berfil.INDIR + berfil.FILNAMNS, INPUT-OUTPUT DATASET-HANDLE BerKalkDS BIND).   
   FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "tempglobforetag" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE xmltabelfile THEN DO:
      MESSAGE "Den här beredningen går ej att läsa in. Den har fel version!"
      VIEW-AS ALERT-BOX.
      DELETE valtemp.
      RETURN.
   END.   
   lnamnex = "".
   IF AVAILABLE xmltabelfile THEN DO:         
      xmltabelfile.BufferH:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF xmltabelfile.BufferH:AVAILABLE THEN DO:
         lnamnex = xmltabelfile.BufferH:BUFFER-FIELD("LEVNAMNEX"):BUFFER-VALUE.
         IF xmltabelfile.BufferH:BUFFER-FIELD("GFORETAG"):BUFFER-VALUE = "TRAS" OR xmltabelfile.BufferH:BUFFER-FIELD("GFORETAG"):BUFFER-VALUE = "nylb" THEN DO:
            MESSAGE "Den här beredningen går ej att läsa in. Den har en ogiltig version!"
            VIEW-AS ALERT-BOX.
            RETURN.
         END.   
         ELSE IF Guru.Konstanter:globforetag = "GETB" OR Guru.Konstanter:globforetag = "GETBO" THEN DO:
            IF TODAY > 01/02/19 THEN DO:
               MESSAGE "Den här beredningen går ej att läsa in. Den har en ogiltig version!"
               VIEW-AS ALERT-BOX.
               RETURN.
            END.     
         END.
      END.   
   END.  
   ELSE DO:
      MESSAGE "Den här beredningen går ej att läsa in. Den har fel version!"
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   FIND FIRST valtemp NO-LOCK NO-ERROR.
   /*Ska sedan  ligga i import*/
   IF skapanyttnummer = TRUE THEN DO:
      FIND FIRST omrtemp WHERE  omrtemp.OMRADE = valtemp.OMRADE NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         valtemp.OMRADE = Guru.Konstanter:globomr.
      END.
      ASSIGN
      valort = beredningtemp.BENAMNING
      valomrade = valtemp.OMRADE
      valaonr = ?
      vart = "NYA"
      avbryt = FALSE.
      RUN NYBEREU.W (INPUT 1,INPUT lnamnex ).
      valimpomrade = valomrade.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE "Import avbruten!" VIEW-AS ALERT-BOX INFO BUTTONS OK.
         IF VALID-HANDLE(laddaprochspar) THEN DELETE PROCEDURE laddaprochspar NO-ERROR.   
         musz = TRUE.   
         RETURN NO-APPLY.
      END.                  
      /*nya nr */
      valtemp.OMRADE = valomrade.
      valtemp.NYTTNR = valbernr.
      RUN berhmt_UI IN expimpapph (INPUT valbernr,INPUT valomrade,OUTPUT TABLE eberedningtemp).      
   END.
   RUN selxml_UI (INPUT "Beredningen sparas i Guru!").   
   IF skapanyttnummer = TRUE THEN DO:
      /*MESSAGE "Ditt nya beredningsnummer är: " + gomrk + " " + valtemp.OMRADE + " " + STRING(nyttnr) VIEW-AS ALERT-BOX TITLE "Meddelande". */
   END.
   RUN NyttBernr.
   {muswait.i}      
   ASSIGN
   valimpomrade = valtemp.OMRADE
   valbernr =  valtemp.NYTTNR.   
   IF Guru.Konstanter:kalk2sekvar[1] = FALSE OR Guru.Konstanter:kalk2sekvar[2] = FALSE OR Guru.Konstanter:kalk2sekvar[4] = FALSE THEN.       
   ELSE IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:    
      FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "Berkalkkopptt" NO-LOCK NO-ERROR.
      IF AVAILABLE xmltabelfile THEN DO:
         MESSAGE "Det finns en kalkylk på denna beredning. vill du läsa in den?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE kalksvar AS LOGICAL.   
      END.   
      IF kalksvar = TRUE THEN DO:         
         RUN selxml_UI (INPUT "Inläsning av kalkyl pågår!").         
         IF unikhelp = Guru.Konstanter:AppSpringSet[1] THEN DO:            
            FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "kalkhuvtt"NO-LOCK NO-ERROR.
            xmltabelfile.BufferH:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
            IF xmltabelfile.BufferH:AVAILABLE THEN DO:
               klogidvar =  xmltabelfile.BufferH:BUFFER-FIELD("KLOGID"):BUFFER-VALUE.
            END.                  
         END.
         ELSE klogidvar = 0.    
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN IMPORTAVBER.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT klogidvar,INPUT valtemp.NYTTNR,INPUT valtemp.OMRADE,INPUT  Guru.Konstanter:globanv,INPUT DATASET-HANDLE BerKalkDS,OUTPUT TABLE KalkylimportTT).                                             
         END.
         ELSE DO:
            RUN IMPORTAVBER.P
            (INPUT klogidvar,INPUT valtemp.NYTTNR,INPUT valtemp.OMRADE,INPUT  Guru.Konstanter:globanv,INPUT DATASET-HANDLE BerKalkDS,OUTPUT TABLE KalkylimportTT).                                 
         END.
         FIND FIRST KalkylimportTT WHERE NO-LOCK NO-ERROR.
         IF AVAILABLE KalkylimportTT THEN DO:
            IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
            RUN ImportBerKalkSkapa_UI IN KalkClasserStart 
            (INPUT KalkylimportTT.KALKNR, INPUT valtemp.NYTTNR,INPUT KalkylimportTT.OMRADE,  INPUT TABLE KalkylimportTT).
         END.   
      END.    
   END.
   RUN selxml_UI (INPUT "Inläsning av beredningen klar!").
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
   EMPTY TEMP-TABLE xmltabelfile NO-ERROR.  
   RETURN.  
    
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
   DEFINE OUTPUT PARAMETER avblg AS LOGICAL NO-UNDO.
   DEFINE VARIABLE bortsvar AS LOGICAL NO-UNDO.  
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".   
   IF typdator = 1 THEN DO:
      EMPTY TEMP-TABLE valtemp NO-ERROR.
      IF SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? THEN DO: 
         MESSAGE "Du har inte valt någon beredning." VIEW-AS ALERT-BOX
         TITLE "Meddelande".
         avblg = TRUE.
         RETURN NO-APPLY.
      END.
      FIND FIRST beredningtemp WHERE beredningtemp.BERAONR = tempbernr AND  beredningtemp.OMRADE = tempomr 
      NO-ERROR.
      IF AVAILABLE beredningtemp THEN DO:
         CREATE valtemp.
         BUFFER-COPY beredningtemp TO valtemp.
         filnamn = fildir + tempbernr + "!" + tempomr + "!" + beredningtemp.BENAMNING.
      END.                 
      FIND FIRST valtemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valtemp THEN DO:
         MESSAGE "Import av beredning avbruten!"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         SEL_XML:HIDDEN = TRUE.
         RETURN.
      END.   
      sparasing = FALSE.      
      IF AVAILABLE valtemp THEN DO:
        skapanyttnummer = TRUE.
      END.          
      /*import*/  
      RUN ImportBer_UI. 
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.    
      FOR EACH  berfil:
         MESSAGE "Vill du ta bort xml-filen för " berfil.FILNAMNS " ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bortsvar.
         CASE bortsvar:
            WHEN TRUE THEN DO:
               OS-DELETE VALUE(berfil.INDIR + berfil.FILNAMNS).            
            END.
            WHEN FALSE THEN DO:
               filnamn = "".               
            END.
         END CASE.
      END.
   END.
   ELSE IF typdator = 3 THEN DO:      
      IF NOT AVAILABLE avcadtemp THEN DO:
         RETURN.
      END.
      IF valort = "" THEN valort = avcadtemp.BENAMNING.      
      ASSIGN
      valomrade = Guru.Konstanter:globomr
      valaonr = ?
      vart = "NYA"
      avbryt = FALSE.
      RUN NYBEREU.W (INPUT 1,INPUT "").
      {muswait.i}      
      valimpomrade = valomrade.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE "Import avbruten !" VIEW-AS ALERT-BOX INFO BUTTONS OK.
         avblg = TRUE.
         RETURN NO-APPLY.
      END.
      RUN berhmt_UI IN expimpapph (INPUT valbernr,INPUT valomrade,OUTPUT TABLE beredningtemp).
      FIND FIRST beredningtemp WHERE beredningtemp.BERNR = valbernr AND 
      beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
      FIND FIRST valberedningtemp WHERE valberedningtemp.BERNR = valbernr
      AND valberedningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
      IF AVAILABLE beredningtemp THEN DO:
         RUN ok_UI IN avcadaccessh (INPUT FILL-IN-MINSTAG, INPUT avcadtemp.ID,INPUT avcadtemp.AVVAGNING,INPUT-OUTPUT TABLE beredningtemp).           
         FIND FIRST beredningtemp WHERE beredningtemp.BERNR = valbernr AND 
         beredningtemp.OMRADE = valomrade NO-LOCK NO-ERROR.
         valberedningtemp.BENAMNING = beredningtemp.BENAMNING. 
      END.         
   END.
   MESSAGE "Import klar!" VIEW-AS ALERT-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inxmlen_ui C-Win 
PROCEDURE inxmlen_ui :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE inber AS INTEGER NO-UNDO.
   DEFINE VARIABLE inomrade AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inbenamning AS CHARACTER NO-UNDO.
   DEFINE VARIABLE h1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE h2 AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   mvarhj = REPLACE(mvar,"*","").
   EMPTY TEMP-TABLE imptemp NO-ERROR. 
            EMPTY TEMP-TABLE berfil NO-ERROR. 
            EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
            SEL-IMP:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
            listobj = "".
   ASSIGN 
   tempstring  = "".      
   IF tmpfilnamn MATCHES mvar THEN DO:
     IF INDEX(tmpfilnamn,"!",1) > 0 THEN DO:
         inber = INTEGER(SUBSTRING(tmpfilnamn,1,INDEX(tmpfilnamn,"!",1) - 1)).            
         IF INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1) > 0 THEN DO: 
            IF listobj = "" THEN listobj = ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
            ELSE listobj = listobj + ";" + ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
            h1 = INDEX(tmpfilnamn,"!",1) + 1.
            h2 = INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1)).
            inomrade = SUBSTRING(tmpfilnamn,h1,h2 - h1).
            inbenamning = SUBSTRING(tmpfilnamn,h2 + 1,INDEX(tmpfilnamn,mvarhj) - (h2 + 1)).
            IF inbenamning NE "" THEN DO:
               CREATE beredningtemp.
               ASSIGN
               beredningtemp.BERAONR = STRING(inber)
               beredningtemp.OMRADE = inomrade
               beredningtemp.BERNR = inber
               beredningtemp.BENAMNING = inbenamning.               
               IF beredningtemp.OMRADE = "" THEN beredningtemp.OMRADE = Guru.Konstanter:globomr.
               tempstring = "Bernr: " + beredningtemp.BERAONR + " " + Guru.Konstanter:gomrk + ": " + beredningtemp.OMRADE + 
               " Benm: " + beredningtemp.BENAMNING.                    
               FIND FIRST imptemp WHERE imptemp.INVAR = tempstring NO-LOCK NO-ERROR.
               IF NOT AVAILABLE imptemp THEN DO:
                  CREATE imptemp.
                  ASSIGN
                  imptemp.INDIR = fildir
                  imptemp.INVAR = tempstring.
                  SEL-IMP:ADD-LAST(tempstring) IN FRAME {&FRAME-NAME}.  
               END.
               
               CREATE berfil.
               ASSIGN
               berfil.INDIR = fildir
               berfil.FILNAMNS = tmpfilnamn
               berfil.ROWBER = ROWID(beredningtemp).
            END.
         END.
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inxml_UI C-Win 
PROCEDURE inxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE VARIABLE inber AS INTEGER NO-UNDO.
   DEFINE VARIABLE inomrade AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inbenamning AS CHARACTER NO-UNDO.
   DEFINE VARIABLE h1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE h2 AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   mvarhj = REPLACE(mvar,"*","").
   ASSIGN
   listobj = ""
   tempstring  = ""
   tmpfilnamn = "".
   IF fildir = "" THEN RETURN.
   INPUT STREAM dirstrom FROM OS-DIR(fildir) NO-ECHO.
   REPEAT:
      /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/      
      SET STREAM dirstrom  tmpfilnamn 
      tmpdirlist VIEW-AS EDITOR INNER-CHARS 70 INNER-LINES 3 attrlist 
      tmpattrlist  NO-ERROR.     
      IF tmpfilnamn MATCHES mvar THEN DO:         
         IF INDEX(tmpfilnamn,"!",1) > 0 THEN DO:
            inber = INTEGER(SUBSTRING(tmpfilnamn,1,INDEX(tmpfilnamn,"!",1) - 1)).            
            IF INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1) > 0 THEN DO: 
               IF listobj = "" THEN listobj = ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
               ELSE listobj = listobj + ";" + ENTRY(1,tmpfilnamn,"!") + "," + ENTRY(2,tmpfilnamn,"!").
               h1 = INDEX(tmpfilnamn,"!",1) + 1.
               h2 = INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",INDEX(tmpfilnamn,"!",1) + 1)).
               inomrade = SUBSTRING(tmpfilnamn,h1,h2 - h1).
               inbenamning = SUBSTRING(tmpfilnamn,h2 + 1,INDEX(tmpfilnamn,mvarhj) - (h2 + 1)).
               IF inbenamning NE "" THEN DO:
                  CREATE beredningtemp.
                  ASSIGN
                  beredningtemp.BERAONR = STRING(inber)
                  beredningtemp.OMRADE = inomrade
                  beredningtemp.BERNR = inber
                  beredningtemp.BENAMNING = inbenamning.
                  
                  IF beredningtemp.OMRADE = "" THEN beredningtemp.OMRADE = Guru.Konstanter:globomr.
                  tempstring = "Bernr: " + beredningtemp.BERAONR + " " + Guru.Konstanter:gomrk + ": " + beredningtemp.OMRADE + 
                  " Benm: " + beredningtemp.BENAMNING.                    
                  FIND FIRST imptemp WHERE imptemp.INVAR = tempstring NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE imptemp THEN DO:
                     CREATE imptemp.
                     ASSIGN
                     imptemp.INDIR = fildir
                     imptemp.INVAR = tempstring.
                     SEL-IMP:ADD-LAST(tempstring) IN FRAME {&FRAME-NAME}.  
                  END.                  
                  CREATE berfil.
                  ASSIGN
                  berfil.INDIR = fildir
                  berfil.FILNAMNS = tmpfilnamn
                  berfil.ROWBER = ROWID(beredningtemp).
               END.
            END.
         END.
      END.
   END.
   INPUT STREAM dirstrom CLOSE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE in_UI C-Win 
PROCEDURE in_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN inxml_UI.          
   DISPLAY SEL-IMP WITH FRAME {&FRAME-NAME}. 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttBernr C-Win 
PROCEDURE NyttBernr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     
       
------------------------------------------------------------------------------*/
DEFINE VARIABLE kalkkollh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE qh AS HANDLE NO-UNDO.
DEFINE VARIABLE q2h AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery2 AS CHARACTER NO-UNDO.
  DEBUGGER:SET-BREAK().
  FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "tempglobforetag" NO-ERROR.
   IF AVAILABLE xmltabelfile THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      xmltabelfile.BufferH:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
      IF xmltabelfile.BufferH:AVAILABLE THEN DO:
         unikhelp = xmltabelfile.BufferH:BUFFER-FIELD("GFORETAGUNIK"):BUFFER-VALUE.
         IF xmltabelfile.BufferH:BUFFER-FIELD("GFORETAGUNIK"):BUFFER-VALUE = Guru.Konstanter:AppSpringSet[1] THEN.
         ELSE DO:
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "BERFORE"  
            inextradatatemp.HUVUDCH = valtemp.OMRADE
            inextradatatemp.HUVUDINT = valtemp.NYTTNR
            inextradatatemp.SOKCHAR[1] = xmltabelfile.BufferH:BUFFER-FIELD("GFORETAG"):BUFFER-VALUE 
            inextradatatemp.SOKCHAR[2] = xmltabelfile.BufferH:BUFFER-FIELD("GFORETAGUNIK"):BUFFER-VALUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         END.        
      END.
      DELETE xmltabelfile.
   END.   
   FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "tempaobersing" NO-ERROR.
   IF AVAILABLE xmltabelfile THEN DELETE xmltabelfile.
   FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "beredningtemp" NO-ERROR.
   IF AVAILABLE xmltabelfile THEN DELETE xmltabelfile. 
   FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "MARKSTN" NO-ERROR.
   IF AVAILABLE xmltabelfile THEN DO:
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      EMPTY TEMP-TABLE extrakopptemp NO-ERROR.
      tth = xmltabelfile.BufferH.
      kommandoquery = "FOR EACH " + tth:TABLE + " WHERE " + " NO-LOCK". 
      RUN CreateCustomQuery(INPUT tth,INPUT kommandoquery,OUTPUT qh).
      qH:GET-FIRST().  
      DO WHILE qH:QUERY-OFF-END = FALSE:
         tth:BUFFER-FIELD("KOPPLACHAR2"):BUFFER-VALUE = valtemp.OMRADE .
         tth:BUFFER-FIELD("KOPPLACHAR1"):BUFFER-VALUE = STRING(valtemp.NYTTNR) .
         CREATE inextrakopptemp.
         ASSIGN
         inextrakopptemp.PROGRAM = "MARKSTN"                   
         inextrakopptemp.KOPPLACHAR1 = tth:BUFFER-FIELD("KOPPLACHAR1"):BUFFER-VALUE      
         inextrakopptemp.KOPPLACHAR2 = tth:BUFFER-FIELD("KOPPLACHAR2"):BUFFER-VALUE      
         inextrakopptemp.KOPPLAINT1 = ?  
         inextrakopptemp.KOPPLAINT2 =  ?  
         inextrakopptemp.SOKINT[1] = tth:BUFFER-FIELD("SOKINT1"):BUFFER-VALUE 
         inextrakopptemp.SOKINT[2] = tth:BUFFER-FIELD("SOKINT2"):BUFFER-VALUE 
         inextrakopptemp.SOKCHAR[1] =  tth:BUFFER-FIELD("SOKCHAR1"):BUFFER-VALUE . 
         qH:GET-NEXT().                 
      END.
      RUN extraspar_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp). 
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
      DELETE xmltabelfile.
   END.
   FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "KALKBER" NO-ERROR.
   IF AVAILABLE xmltabelfile THEN DELETE xmltabelfile.    
   FOR EACH xmltabelfile WHERE NO-LOCK:     
      xmltabelfile.BufferH:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.      
      IF xmltabelfile.BufferH:AVAILABLE THEN DO:
         kalkkollh = xmltabelfile.BufferH:BUFFER-FIELD("KALKNR") NO-ERROR.
         IF kalkkollh = ? THEN DO:
            tth = xmltabelfile.BufferH.
            kommandoquery = "FOR EACH " + tth:TABLE + " WHERE " + " NO-LOCK". 
            RUN CreateCustomQuery(INPUT tth,INPUT kommandoquery,OUTPUT qh).
            qH:GET-FIRST().  
            DO WHILE qH:QUERY-OFF-END = FALSE:
               tth:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = valtemp.OMRADE NO-ERROR.
               tth:BUFFER-FIELD("AONR"):BUFFER-VALUE =  STRING(valtemp.NYTTNR) NO-ERROR.
               tth:BUFFER-FIELD("BERNR"):BUFFER-VALUE =  valtemp.NYTTNR NO-ERROR.
               IF xmltabelfile.NodtabName = "BERVAL" THEN DO:
                  IF STRING(tth:BUFFER-FIELD("UPPLAG"):BUFFER-VALUE) = "0" THEN tth:BUFFER-FIELD("UPPLAG"):BUFFER-VALUE = ?.
                  IF STRING(tth:BUFFER-FIELD("UPPLAG"):BUFFER-VALUE) = "" THEN tth:BUFFER-FIELD("UPPLAG"):BUFFER-VALUE = ?. 
               END.
               IF xmltabelfile.NodtabName = "BERID" THEN DO:
                  tth:BUFFER-FIELD("DELNR"):BUFFER-VALUE = ?.
                  tth:BUFFER-FIELD("YKORD"):BUFFER-VALUE = ?.
                  IF tth:BUFFER-FIELD("YKORD"):BUFFER-VALUE = 0 THEN tth:BUFFER-FIELD("YKORD"):BUFFER-VALUE = ?. 
               END.
               IF xmltabelfile.NodtabName = "BERVAL" THEN DO:
                  xmltabelfile.BufferH:FIND-FIRST("WHERE NodtabName = 'BERID' AND NUM = " + STRING(tth:BUFFER-FIELD("NUM"):BUFFER-VALUE),NO-LOCK) NO-ERROR.
                  IF xmltabelfile.BufferH:AVAILABLE THEN.
                  ELSE DO:
                     xmltabelfile.BufferH:BUFFER-CREATE().
                     xmltabelfile.BufferH:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = valtemp.OMRADE.
                     xmltabelfile.BufferH:BUFFER-FIELD("AONR"):BUFFER-VALUE = STRING(valtemp.NYTTNR).
                     xmltabelfile.BufferH:BUFFER-FIELD("NUM"):BUFFER-VALUE = tth:BUFFER-FIELD("NUM"):BUFFER-VALUE.
                  END.
               END. 
               qH:GET-NEXT().        
            END.
            RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE xmltabelfile.TempttH, INPUT xmltabelfile.NodtabName).
         END.
      END. 
   END.
   
   IF unikhelp NE Guru.Konstanter:AppSpringSet[1] THEN DO:
      FIND FIRST xmltabelfile WHERE xmltabelfile.NodtabName = "BERMTRL" NO-ERROR.
      IF AVAILABLE xmltabelfile THEN DO:
         EMPTY TEMP-TABLE tidut NO-ERROR.
         EMPTY TEMP-TABLE elist_mtrl NO-ERROR.
         tth = xmltabelfile.BufferH.
         kommandoquery = "FOR EACH " + tth:TABLE + " WHERE " + " NO-LOCK". 
         RUN CreateCustomQuery(INPUT tth,INPUT kommandoquery,OUTPUT qh).
         qH:GET-FIRST().  
         DO WHILE qH:QUERY-OFF-END = FALSE:
            CREATE elist_mtrl.
            BUFFER elist_mtrl:HANDLE:BUFFER-COPY(tth).
            qH:GET-NEXT().        
         END. 
         RUN skapalistaatg_UI IN vbytlevapph3 
         (INPUT STRING(valtemp.NYTTNR),
         INPUT valtemp.OMRADE, 
         INPUT vald_lev,
         INPUT antmtrlber,
         INPUT TABLE elist_mtrl,
         OUTPUT musz2,
         OUTPUT TABLE tidut).
         EMPTY TEMP-TABLE tidut NO-ERROR.
         EMPTY TEMP-TABLE elist_mtrl NO-ERROR. 
      END. 
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PlaceraKnapp_UI C-Win 
PROCEDURE PlaceraKnapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   Guru.GlobalaVariabler:StartKolumnForKnappar = FILL-IN-AVCADDB:COLUMN  IN FRAME {&FRAME-NAME}. 
   Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-AVCADDB:HANDLE,TRUE).  
   Guru.Konstanter:PlaceraKnapparVagratt(MBTN_SOKAVCAD:HANDLE,TRUE).  
  /* 
   FILL-IN-MINSTAG
   */
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selfill_UI C-Win 
PROCEDURE selfill_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.    
   OS-CREATE-DIR VALUE(fildir) NO-ERROR. 
   mappvar = fildir.
   FILL-IN-MAPP = mappvar.
   IF FILL-IN-MAPP:HIDDEN IN FRAME DEFAULT-FRAME = FALSE THEN DISPLAY FILL-IN-MAPP WITH FRAME DEFAULT-FRAME. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE selxml_UI C-Win 
PROCEDURE selxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER txtvar AS CHARACTER NO-UNDO.
   SEL_XML:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   SEL_XML:ADD-LAST(txtvar + " " + STRING(TIME,"HH:MM:SS")).
   DISPLAY SEL_XML WITH FRAME {&FRAME-NAME}.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tname_UI C-Win 
PROCEDURE Tname_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xmltabelfileCreate C-Win 
PROCEDURE xmltabelfileCreate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE iEntry AS INTEGER NO-UNDO.
   REPEAT: 
      iEntry = iEntry + 1.
      
      IF BerKalkDS:GET-BUFFER-HANDLE(iEntry) = ? THEN LEAVE.
      ELSE DO:
         CREATE xmltabelfile.
         ASSIGN 
         xmltabelfile.BufferH = BerKalkDS:GET-BUFFER-HANDLE(iEntry)
         xmltabelfile.TempttH = xmltabelfile.BufferH:TABLE-HANDLE
         xmltabelfile.NodtabName = xmltabelfile.BufferH:XML-NODE-NAME.
         ASSIGN 
         xmltabelfile.TempName = xmltabelfile.TempttH:NAME  
         xmltabelfile.TableName = xmltabelfile.BufferH:XML-NODE-NAME.
        
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xmltabelfileReload C-Win 
PROCEDURE xmltabelfileReload :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE iEntry AS INTEGER NO-UNDO.  
    REPEAT: 
      iEntry = iEntry + 1.
      IF BerKalkDS:GET-BUFFER-HANDLE(iEntry) = ? THEN LEAVE.
      ELSE DO:        
         FIND FIRST xmltabelfile  WHERE xmltabelfile.TempName = BerKalkDS:GET-BUFFER-HANDLE(iEntry):NAME NO-LOCK NO-ERROR.
         IF AVAILABLE xmltabelfile THEN DO:
            xmltabelfile.BufferH = BerKalkDS:GET-BUFFER-HANDLE(iEntry).
            xmltabelfile.TempttH =  xmltabelfile.BufferH:TABLE-HANDLE.
            xmltabelfile.BufferH:XML-NODE-NAME = xmltabelfile.TableName.
            IF  xmltabelfile.NodtabName = "" THEN xmltabelfile.NodtabName = xmltabelfile.BufferH:XML-NODE-NAME.
         END.
         ELSE DO:
            CREATE xmltabelfile.
            ASSIGN 
            xmltabelfile.BufferH = BerKalkDS:GET-BUFFER-HANDLE(iEntry)
            xmltabelfile.TempttH = xmltabelfile.BufferH:TABLE-HANDLE
            xmltabelfile.NodtabName = xmltabelfile.BufferH:XML-NODE-NAME.
            ASSIGN 
            xmltabelfile.TempName = xmltabelfile.TempttH:NAME  
            xmltabelfile.TableName = xmltabelfile.BufferH:XML-NODE-NAME.
         END.            
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

