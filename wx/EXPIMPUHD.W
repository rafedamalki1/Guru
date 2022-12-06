&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: körs inte!!!!

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
 
{TTEXPIMPHD.I}
&SCOPED-DEFINE SHARED SHARED
{OMRTEMPW.I}
{BERTEMPTAB.I}
/*lena*/
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

DEFINE VARIABLE valnlevmtrlkat AS CHARACTER NO-UNDO.



DEFINE SHARED VARIABLE globunikdb AS CHARACTER NO-UNDO.
DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ekoppdataapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE valimpomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valanv AS CHARACTER NO-UNDO.





/*LENA 20120302 byte lev import när det är olika företag*/
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vbytlevapph3 AS HANDLE NO-UNDO.
DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.



DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
/*
DEFINE VARIABLE tempfilnamn AS CHARACTER NO-UNDO.
*/
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
DEFINE NEW SHARED VARIABLE expimphdh AS HANDLE NO-UNDO.
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
DEFINE TEMP-TABLE kon_grupp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE TEMP-TABLE avcadtemp
   FIELD ID AS INTEGER LABEL "Id"
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD COMMENT AS CHARACTER LABEL "Kommentar"
   FIELD AVVAGNING AS INTEGER LABEL "Avvägning"
   INDEX ID IS PRIMARY ID.
DEFINE TEMP-TABLE tempnamn NO-UNDO
   FIELD NAMNH AS HANDLE
   FIELD TORGTH AS HANDLE
   FIELD NODTAB AS CHARACTER.

DEFINE TEMP-TABLE tempaobersing2 NO-UNDO LIKE tempaobersing.


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
&Scoped-define INTERNAL-TABLES avcadtemp beredningtemp konstgrptemp

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
MBTN_HMT RECT-SOK IMAGE-SOK FILL-IN-AVCADDB BTN_IMP FILL-IN-MINSTAG TOG_BER ~
TOG_MTRLPRISBER BTN_UPHAND RAD_MTR CMB_LEV TOG_MTRLPRIS SEL-IMP BRW_AVCAD ~
BRW_BER BRW_KON SEL_XML FILL-IN-MAPP FILL-IN-FILNAMN BTN_FILNAMN ~
FILL-IN-SID FILL-IN-SBEN FILL-IN-SAV BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BERNR FILL-IN-MINSTAG FILL-IN-BEN ~
FILL-IN-AONR FILL-IN-DELNR TOG_BER TOG_MTRLPRISBER RAD_MTR CMB_LEV ~
TOG_MTRLPRIS SEL-IMP SEL_XML FILL-IN-MAPP FILL-IN-FILNAMN FILL-IN-SID ~
FILL-IN-SBEN FILL-IN-SAV FILL-IN-BER FILL-IN-BER-2 FILL-IN-HITTA 

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

DEFINE BUTTON BTN_UPHAND 
     LABEL "Uppdatera PDA" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_AVCAD 
     LABEL "AvCAD" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_BARBAR 
     LABEL "Bärbar dator" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_HANDDATOR 
     LABEL "Handdator" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_HMT  NO-FOCUS
     LABEL "Sök beredning" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

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

DEFINE IMAGE IMAGE-SOK
     FILENAME "BILDER/sokpa.gif":U
     SIZE 9.25 BY .83.

DEFINE VARIABLE RAD_MTR AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Beredningsupplägg", 1,
"Materielkatalog", 2
     SIZE 37 BY .71 NO-UNDO.

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

DEFINE VARIABLE TOG_BER AS LOGICAL INITIAL no 
     LABEL "Beredning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_MTRLPRIS AS LOGICAL INITIAL no 
     LABEL "Ta med pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .75 NO-UNDO.

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

DEFINE QUERY BRW_KON FOR 
      konstgrptemp SCROLLING.
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
    WITH NO-ROW-MARKERS SIZE 84.25 BY 16.5
         TITLE "Välj beredning för import" FIT-LAST-COLUMN.

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

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON C-Win _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konstgrptemp.BENAMNING FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 43 BY 16.5
         TITLE "Välj konstruktionsgrupp(er)" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     MBTN_BARBAR AT ROW 1.08 COL 1.13
     MBTN_HANDDATOR AT ROW 1.08 COL 15.13
     MBTN_AVCAD AT ROW 1.08 COL 28.5
     MBTN_HMT AT ROW 7.29 COL 98.5
     RAD_VAL AT ROW 1.25 COL 59.5 NO-LABEL
     FILL-IN-AVCADDB AT ROW 5.5 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     FILL-IN-BERNR AT ROW 6.08 COL 16.5 COLON-ALIGNED
     BTN_IMP AT ROW 6.92 COL 36.13
     FILL-IN-MINSTAG AT ROW 7 COL 77 COLON-ALIGNED WIDGET-ID 10
     FILL-IN-BEN AT ROW 7.25 COL 16.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 8.38 COL 16.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 8.38 COL 38.5 COLON-ALIGNED
     TOG_BER AT ROW 9.38 COL 19.88
     TOG_MTRLPRISBER AT ROW 9.5 COL 36.5 WIDGET-ID 22
     BTN_UPHAND AT ROW 9.5 COL 98.5
     RAD_MTR AT ROW 10.33 COL 19.88 NO-LABEL WIDGET-ID 18
     CMB_LEV AT ROW 10.33 COL 66.5 COLON-ALIGNED WIDGET-ID 16
     TOG_MTRLPRIS AT ROW 10.33 COL 88.88 WIDGET-ID 14
     SEL-IMP AT ROW 11 COL 2.63 NO-LABEL
     BRW_AVCAD AT ROW 11 COL 12
     BRW_BER AT ROW 11 COL 12.13
     BRW_KON AT ROW 11 COL 16
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
/* BROWSE-TAB BRW_KON BRW_BER DEFAULT-FRAME */
ASSIGN 
       BRW_AVCAD:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_BER:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_KON:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BTN_UPHAND:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konstgrptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstgrptemp.BENAMNING
"konstgrptemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON LEFT-MOUSE-CLICK OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj beredning för import */
DO:
   status-ok = BRW_AVCAD:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVCAD C-Win
ON VALUE-CHANGED OF BRW_AVCAD IN FRAME DEFAULT-FRAME /* Välj beredning för import */
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
   TOG_BER = INPUT TOG_BER
   RAD_MTR = INPUT RAD_MTR
   TOG_MTRLPRIS = INPUT TOG_MTRLPRIS
   TOG_MTRLPRISBER = INPUT TOG_MTRLPRISBER
   CMB_LEV = INPUT CMB_LEV
   SEL-IMP = INPUT SEL-IMP.
   /*
   TOG_XML = INPUT TOG_XML.
   */
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
      /*
      SEL-IMP:ADD-LAST(FILL-IN-MAPP + FILL-IN-FILNAMN) IN FRAME {&FRAME-NAME}.
      SEL-IMP = FILL-IN-MAPP + FILL-IN-FILNAMN.
      */
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
   IF demokvar = TRUE THEN DO:
      MESSAGE "Handdator funktionen är en extra funktion som offereras separat." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {muswait.i}      
   IF typdator = 2 THEN DO:
      EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
      xmlsparfilnamn = "".
      RUN ut_UI (INPUT "berToPC"). 
      /* logga här */
      IF Guru.Konstanter:appcon THEN DO:
         RUN HDLOG.P  ON Guru.Konstanter:apphand (INPUT Guru.Konstanter:globanv,INPUT valbernr, INPUT valomrade). 
      END.
      ELSE DO:
         RUN HDLOG.P (INPUT Guru.Konstanter:globanv,INPUT valbernr, INPUT valomrade).
      END. 
                           
      RUN inxml_UI.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE IF typdator = 3 THEN DO:
      EMPTY TEMP-TABLE avcadtemp NO-ERROR. 
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
   FILL-IN-MINSTAG = INPUT FILL-IN-MINSTAG
   FILL-IN-MAPP = INPUT FILL-IN-MAPP
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   TOG_BER = INPUT TOG_BER
   RAD_MTR = INPUT RAD_MTR
   TOG_MTRLPRIS = INPUT TOG_MTRLPRIS
   TOG_MTRLPRISBER = INPUT TOG_MTRLPRISBER
   CMB_LEV = INPUT CMB_LEV
   SEL-IMP = INPUT SEL-IMP.
   
   /*
   TOG_XML = INPUT TOG_XML.
   */
   /*Export*/
   
   IF typval = 1 THEN DO:
      CMB_LEV = INPUT CMB_LEV.
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV
      USE-INDEX LEV NO-LOCK NO-ERROR.
      valnlevmtrlkat = levtemp.LEVKOD.
      RUN export_UI.      
   END.
   ELSE IF typval = 2 THEN DO:
      /*Import*/
      CMB_LEV = INPUT CMB_LEV.
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV
      USE-INDEX LEV NO-LOCK NO-ERROR.
      valnlevmtrlkat = levtemp.LEVKOD.
      RUN import_UI.
   END.
   {musarrow.i}
   /*OS-DELETE VALUE(fildir).*/
   ASSIGN
   valomrade = valimpomrade.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPHAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPHAND C-Win
ON CHOOSE OF BTN_UPHAND IN FRAME DEFAULT-FRAME /* Uppdatera PDA */
DO:
  /*ccc*/
  RUN uphand_UI.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_AVCAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_AVCAD C-Win
ON CHOOSE OF MBTN_AVCAD IN FRAME DEFAULT-FRAME /* AvCad */
DO:
   IF demokvar = TRUE THEN DO:
      MESSAGE "AvCad funktionen är en extra funktion som offereras separat." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {muswait.i}
   RAD_VAL = 3.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   {musarrow.i}
           
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
   IF demokvar = TRUE THEN DO:
      MESSAGE "Handdator funktionen är en extra funktion som offereras separat." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.

   RAD_VAL = 2.
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
      IF TOG_BER = TRUE THEN DO:
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den fil som Ni villläsa in"
         FILTERS        "All Files (*$.xml)"  "*$.xml"   
         INITIAL-DIR    mappvar
         RETURN-TO-START-DIR
         UPDATE OKvald.  
         IF OKvald = TRUE THEN DO:
            tmpfilnamn = SUBSTRING(fildir,R-INDEX(fildir,"\") + 1). 
            fildir = SUBSTRING(fildir,1,R-INDEX(fildir,"\")).
            /*
            EMPTY TEMP-TABLE imptemp NO-ERROR. 
            EMPTY TEMP-TABLE berfil NO-ERROR. 
            EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
            SEL-IMP:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
            listobj = "".*/
            RUN inxmlen_UI.
            RETURN.
         END.          
      END.
      ELSE DO:
         SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den fil som Ni villläsa in"
         FILTERS        "All Files (*.xml)"  "*.xml"   
         INITIAL-DIR    mappvar
         RETURN-TO-START-DIR
         UPDATE OKvald.         
      END.
   END.
   ELSE DO:
      SYSTEM-DIALOG GET-FILE fildir
      TITLE          "Välj den fil som Ni villläsa in"
      FILTERS        "All Files (*.xml)"  "*.xml"   
      INITIAL-DIR    mappvar
      RETURN-TO-START-DIR
      UPDATE OKvald.         
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


&Scoped-define SELF-NAME RAD_MTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_MTR C-Win
ON VALUE-CHANGED OF RAD_MTR IN FRAME DEFAULT-FRAME
DO:
   RAD_MTR = INPUT RAD_MTR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
/*    RAD_VAL = INPUT RAD_VAL. */
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
      BTN_UPHAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. /* uppdatera handdator */
      ASSIGN
      BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
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
         IF TOG_BER = TRUE THEN.
         ELSE IF RAD_MTR = 1 THEN mvar = "*UppBere.xml".
         ELSE IF RAD_MTR = 2 THEN mvar = "*mtrl.xml".
         FILL-IN-BER = "Import från bärbar dator.".
         MBTN_HMT:HIDDEN = FALSE.         
         IF TOG_BER = TRUE THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ELSE SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         TOG_BER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ASSIGN
         BTN_FILNAMN:HIDDEN = FALSE
         FILL-IN-HITTA:HIDDEN = FALSE
         FILL-IN-MAPP:HIDDEN = FALSE
         FILL-IN-FILNAMN:HIDDEN = FALSE.
         
      END. 
      ELSE DO:
         /*Export*/
         FILL-IN-BER = "Export till bärbar dator.".
         IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            ASSIGN
            CMB_LEV:HIDDEN = FALSE 
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
                               
         END.
         ELSE DO:
            TOG_MTRLPRIS = FALSE.
            ASSIGN
            CMB_LEV:HIDDEN = true 
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
           DISABLE TOG_BER   WITH FRAME DEFAULT-FRAME.
         END.
      END.
      RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
      /*
      TOG_XML:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      */
   END.
   ELSE IF typdator = 2 THEN DO:  
      korfil = SEARCH("wguru.exe").
      IF korfil = ? THEN DO:
         MESSAGE "Handdatorn är inte installerad korrekt!" VIEW-AS ALERT-BOX.
         felhand = TRUE.        
         RETURN NO-APPLY.
      END.
      ASSIGN
      BTN_UPHAND:HIDDEN IN FRAME {&FRAME-NAME} = FALSE /* uppdatera handdator */
      MBTN_HMT:HIDDEN = TRUE
      SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
      IF typval = 1 THEN DO:
         FILL-IN-BER = "Export till handdator.". 
         BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         
      END.
      IF typval = 2 THEN DO:      
         BTN_IMP:LABEL = "Hämta data från handdator".
         FILL-IN-BER = "Import från handdator.".
         ASSIGN
         TOG_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_MTRLPRISBER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         CMB_LEV:HIDDEN = TRUE
         TOG_MTRLPRIS:HIDDEN = TRUE 
         FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
         FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
         FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
         FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.        
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
      RUN btnvit_UI (INPUT MBTN_HANDDATOR:HANDLE).
   END.
   ELSE IF typdator = 3 THEN DO:
      IF NOT VALID-HANDLE(avcadaccessh) THEN DO:
         MESSAGE "AvCad är inte uppstartad! Kontakta Elpool i Umeå AB 090-184540" VIEW-AS ALERT-BOX.
         ASSIGN
         RAD_VAL = spartypdator
         typdator = spartypdator.
         RETURN NO-APPLY.
      END.
      
      RUN load_UI IN avcadaccessh (OUTPUT musz, OUTPUT FILL-IN-AVCADDB).
      IF musz = FALSE THEN DO:
         IF VALID-HANDLE(avcadaccessh) THEN DELETE PROCEDURE avcadaccessh NO-ERROR.
         ASSIGN
         RAD_VAL = spartypdator
         typdator = spartypdator.
         RETURN NO-APPLY.
      END.
       
      DISPLAY FILL-IN-AVCADDB WITH FRAME {&FRAME-NAME}.
      BTN_IMP:LABEL = "Hämta data från AvCAD".
      FILL-IN-BER = "Import från AvCad.".
      FILL-IN-MINSTAG:HIDDEN = FALSE.
      musz = FALSE.
      ASSIGN
      BTN_UPHAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE /* uppdatera handdator */
      MBTN_HMT:HIDDEN = TRUE
      SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MTRLPRISBER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MTRLPRIS:HIDDEN = TRUE
      CMB_LEV:HIDDEN = TRUE .      
      IF TOG_BER = TRUE THEN DO:
         IF typval = 2 THEN DO:      
            ASSIGN
            BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
            RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
            FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
            FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE   
            FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
            BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         END.
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
   DISPLAY FILL-IN-BER WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME TOG_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BER C-Win
ON VALUE-CHANGED OF TOG_BER IN FRAME DEFAULT-FRAME /* Beredning */
DO:
   TOG_BER = INPUT TOG_BER.
   IF typval = 2 THEN DO:
      IF TOG_BER = TRUE THEN DO:
         ASSIGN
         TOG_MTRLPRIS = FALSE
          
         TOG_MTRLPRIS:HIDDEN = TRUE
         CMB_LEV:HIDDEN = TRUE  
         RAD_MTR:HIDDEN = TRUE.         
         ASSIGN
         BTN_FILNAMN:HIDDEN = FALSE
         FILL-IN-HITTA:HIDDEN = FALSE
         FILL-IN-MAPP:HIDDEN = FALSE
         FILL-IN-FILNAMN:HIDDEN = FALSE.
      END.
      ELSE DO:
         ASSIGN
         BTN_FILNAMN:HIDDEN = TRUE
         FILL-IN-HITTA:HIDDEN = TRUE
         FILL-IN-MAPP:HIDDEN = TRUE
         FILL-IN-FILNAMN:HIDDEN = TRUE.
         IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN DO:
            ASSIGN
            CMB_LEV:HIDDEN = FALSE 
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
            IF typval = 1 THEN TOG_MTRLPRIS:HIDDEN = FALSE.   
            /*IF typval = 1 THEN CMB_LEV:HIDDEN = FALSE.*/                
         END.
         ELSE DO:
            ASSIGN
            
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            TOG_MTRLPRIS:HIDDEN = TRUE 
            CMB_LEV:HIDDEN = TRUE.
            DISABLE TOG_BER TOG_MTRLPRISBER WITH FRAME DEFAULT-FRAME.
         END.
         
      END.
   END.
   IF (valomrade = ? OR valomrade = "") AND typval = 1 THEN DO:
      MESSAGE "Ingen beredning vald."
      VIEW-AS ALERT-BOX.
      ASSIGN 
      TOG_BER = FALSE
      TOG_MTRLPRISBER = FALSE.  
      DISPLAY TOG_BER TOG_MTRLPRISBER WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN.
      ELSE DO:
         musz = TRUE.
         
         RETURN NO-APPLY.
         
      END.    
   END.
   IF typval = 2 THEN DO:
      IF typdator = 1 THEN DO:      
         SEL-IMP:HIDDEN = NOT TOG_BER.
      END.
      ELSE DO:
         IF TOG_BER = TRUE THEN DO:
            ASSIGN
            FILL-IN-AVCADDB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_AVCAD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            IMAGE-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
            RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
            FILL-IN-SID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
            FILL-IN-SBEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
            FILL-IN-SAV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_BER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
            BTN_IMP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
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
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MTRLPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRLPRIS C-Win
ON VALUE-CHANGED OF TOG_MTRLPRIS IN FRAME DEFAULT-FRAME /* Ta med pris */
DO:
   TOG_MTRLPRIS = INPUT TOG_MTRLPRIS.
   
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
   IF VALID-HANDLE(expimphdh) THEN DELETE PROCEDURE expimphdh NO-ERROR.   
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
   DEBUGGER:SET-BREAK().
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   
   RAD_MTR = 1.
   IF Guru.Konstanter:globomr = "" THEN DO:
      FIND FIRST omrtemp NO-LOCK NO-ERROR.
      globomr = omrtemp.OMRADE.
   END.      
   skrivaover = TRUE.
  
   status-ok = CMB_LEV:DELETE("0").
   RUN levhmt_UI IN expimpapph (OUTPUT TABLE levtemp,OUTPUT TABLE huvlevtemp). 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE 
   USE-INDEX LEV NO-LOCK:
      IF levtemp.LEVKOD = "99" THEN DO:
      END.
      ELSE DO:
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN).
      END.      
   END.
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD = huvlevtemp.LEVKOD NO-LOCK NO-ERROR.
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   END.
  
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
   FILL-IN-AVCADDB:HIDDEN = TRUE. 

   SEL_XML:HIDDEN = TRUE.
   {FRMSIZE.I}
     
   TOG_BER = TRUE. 
   TOG_MTRLPRISBER = FALSE. 
   DISP  TOG_BER TOG_MTRLPRISBER WITH FRAME {&FRAME-NAME}. 
   APPLY "VALUE-CHANGED" TO TOG_BER.
   IF musz = TRUE THEN DO:
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.    
   IF Guru.SharedVariable:singel = FALSE THEN DO:
   /*client-server*/
      IF typval = 2 THEN DO:      
         /*import*/
         IF Guru.Konstanter:globanv = "Celpao" THEN .
         ELSE DO:
            ASSIGN 
            RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            TOG_MTRLPRIS:HIDDEN = TRUE
            CMB_LEV:HIDDEN = TRUE. 
            
         /*   TOG_BER:ROW = 4.*/ 
         END.
         ASSIGN
         TOG_BER = TRUE
         TOG_MTRLPRIS = FALSE. 
         DISP  TOG_BER WITH FRAME {&FRAME-NAME}. 
         APPLY "VALUE-CHANGED" TO TOG_BER.
      END.
      ELSE DO:
         /*export*/
         
      END.
   END.
   ELSE DO:
      /*singel*/
      TOG_BER = TRUE.
      IF typval = 1 THEN DO:               
      END.
      ELSE DO:
         /*import*/
         
         ASSIGN     
         TOG_BER = TRUE.
         /*
         TOG_BER:ROW = 4.
         ASSIGN 
         TOG_MTRL:ROW = TOG_BER:ROW
         TOG_UPP:ROW = TOG_BER:ROW + 1.25.
         */
         DISP TOG_BER WITH FRAME {&FRAME-NAME}. 
         APPLY "VALUE-CHANGED" TO TOG_BER.
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
   RAD_VAL = 1
   MBTN_HANDDATOR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   /*
   TOG_MTRL:ROW = TOG_UPP:ROW.
   */
   APPLY "VALUE-CHANGED" TO RAD_VAL.
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
   
   IF Guru.Konstanter:globanv = "Celpao" OR Guru.Konstanter:globforetag = "CGRAN" THEN DO:
      MBTN_HANDDATOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ASSIGN
   MBTN_BARBAR:COLUMN = 1.
   ASSIGN
   Guru.GlobalaVariabler:collefth = MBTN_BARBAR:HANDLE.
   IF MBTN_HANDDATOR:HIDDEN = FALSE THEN DO:
      Guru.GlobalaVariabler:colrighth = MBTN_HANDDATOR:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   IF MBTN_AVCAD:HIDDEN = FALSE THEN DO:
      Guru.GlobalaVariabler:colrighth = MBTN_AVCAD:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   MBTN_HANDDATOR:LOAD-IMAGE("BILDER\xbtn_hand.gif") NO-ERROR.
   MBTN_BARBAR:LOAD-IMAGE("BILDER\xbtn_barb.gif") NO-ERROR.
   MBTN_AVCAD:LOAD-IMAGE("BILDER\xbtn_avcad.gif") NO-ERROR. 
       
   RUN btnvit_UI (INPUT MBTN_BARBAR:HANDLE).
   IF typval = 1 THEN SEL-IMP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   TOG_BER = TRUE.  
   DISP  TOG_BER WITH FRAME {&FRAME-NAME}. 
   APPLY "VALUE-CHANGED" TO TOG_BER.
   IF Guru.Konstanter:beresekvar[10] = TRUE AND Guru.Konstanter:mtrlsekvar[6] = FALSE THEN.
   ELSE DO:
      ASSIGN
      CMB_LEV:HIDDEN = TRUE
      TOG_MTRLPRIS:HIDDEN = TRUE 
      
      RAD_MTR:HIDDEN = TRUE.
      DISABLE TOG_BER   WITH FRAME DEFAULT-FRAME.
   END.
   IF typval = 1 THEN DO:
      IF MBTN_HANDDATOR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE THEN DO:
         IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DISABLE TOG_BER   WITH FRAME DEFAULT-FRAME.
         ELSE DO:
            ASSIGN
            RAD_MTR:HIDDEN = FALSE
            CMB_LEV:HIDDEN = FALSE. 

            IF typval = 1 THEN TOG_MTRLPRIS:HIDDEN = FALSE.
            /*
            IF typval = 1 THEN CMB_LEV:HIDDEN = FALSE.
            */
         END.
      END.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_KON:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_AVCAD:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addfillin_UI IN brwproc[3]                                  
      (INPUT FILL-IN-SID:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ID").
   RUN addfillin_UI IN brwproc[3]                                  
      (INPUT FILL-IN-SBEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").
   RUN addfillin_UI IN brwproc[3]                                  
      (INPUT FILL-IN-SAV:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AVVAGNING").
   RUN setcolindex_UI IN brwproc[1] (INPUT "OMRADE"). 
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[3] (INPUT "ID"). 
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).
   IF typval = 2 THEN DO:
                               
/*                               /*CLAS OCH ALAMAA VATTENFALL test*/
      IF Guru.Konstanter:globanv = "elpao" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR 
         Guru.Konstanter:globforetag = "GETB" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELTE" OR 
         Guru.Konstanter:globforetag = "HANA" OR Guru.Konstanter:globforetag = "VAST" THEN DO:
         RUN AVCADACCESS.P PERSISTENT SET avcadaccessh (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag).      
      END.
      */
      RUN AVCADACCESS.P PERSISTENT SET avcadaccessh (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag).
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPUAPPHD.P PERSISTENT SET expimpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPUAPPHD.P PERSISTENT SET expimpapph.
   END. 
   /*HD HÄR*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXPIMPHD.P PERSISTENT SET expimphdh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXPIMPHD.P PERSISTENT SET expimphdh.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE checkfiles_UI C-Win 
PROCEDURE checkfiles_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER errorvar AS CHARACTER NO-UNDO.

   errorvar = ?.
   IF SEARCH("Pguru.exe") = ? THEN DO:
      errorvar = errorvar + "Pguru.exe".
   END. 
   IF SEARCH("Wguru.sdf") = ? THEN DO:
      errorvar = errorvar + "Wguru.exe".
   END.
   IF SEARCH("pput.exe") = ? THEN DO:
      errorvar = errorvar + "pput.exe".
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
  DISPLAY FILL-IN-BERNR FILL-IN-MINSTAG FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR 
          TOG_BER TOG_MTRLPRISBER RAD_MTR CMB_LEV TOG_MTRLPRIS SEL-IMP SEL_XML 
          FILL-IN-MAPP FILL-IN-FILNAMN FILL-IN-SID FILL-IN-SBEN FILL-IN-SAV 
          FILL-IN-BER FILL-IN-BER-2 FILL-IN-HITTA 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE MBTN_BARBAR MBTN_HANDDATOR MBTN_AVCAD MBTN_HMT RECT-SOK IMAGE-SOK 
         FILL-IN-AVCADDB BTN_IMP FILL-IN-MINSTAG TOG_BER TOG_MTRLPRISBER 
         BTN_UPHAND RAD_MTR CMB_LEV TOG_MTRLPRIS SEL-IMP BRW_AVCAD BRW_BER 
         BRW_KON SEL_XML FILL-IN-MAPP FILL-IN-FILNAMN BTN_FILNAMN FILL-IN-SID 
         FILL-IN-SBEN FILL-IN-SAV BTN_OK BTN_AVB 
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
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
   IF typdator = 2 THEN DO:
      EMPTY TEMP-TABLE kon_grupp NO-ERROR. 
      IF RAD_MTR = 1 THEN DO:
         MESSAGE "Vill Ni skriva över beredningsupplägg/materielkatalog som finns i handdatorn?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar3.
         IF svar3 THEN skrivaover = TRUE.
         ELSE DO:
            EMPTY TEMP-TABLE kon_grupp NO-ERROR. 
            skrivaover = FALSE.
            RETURN.
         END.
      END.         
   END. 
   IF TOG_BER = TRUE THEN DO: 
      filnamn = fildir + beredningtemp.BERAONR + "!" + beredningtemp.OMRADE + "!".
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      svar2 = FALSE.
      RUN selxml_UI (INPUT "Beredningen kontrolleras").               
      RUN typval_UI IN expimpapph (INPUT valomrade,INPUT valbernr,
                                   INPUT typval,INPUT typdator,
                                   INPUT TOG_BER,INPUT Guru.Konstanter:globanv,
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
      ELSE DO:
         IF TOG_BER = TRUE THEN DO:
            RUN inkopkoll_UI IN expimpapph (INPUT valaonr, INPUT valomrade,OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.
               MESSAGE "Denna bereding har ett inköpsprotokoll och kan inte exporteras till handdator!" VIEW-AS ALERT-BOX.
               SEL_XML:HIDDEN = TRUE.
               RETURN NO-APPLY.
            END.
         END.
         RUN expxml_UI.                    
      END.
   END.
   ELSE DO:
      filnamn = fildir.
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
   IF TOG_BER = TRUE THEN DO:
      RUN selxml_UI (INPUT "Beredningen sammanställs").
     
      RUN skapaexport_UI IN expimpapph (INPUT singel, INPUT valaonr, INPUT valomrade,TOG_MTRLPRISBER, 
                                        OUTPUT TABLE temp_ber,     
                                        OUTPUT TABLE tempberval,   
                                        OUTPUT TABLE tempberord,   
                                        OUTPUT TABLE tempfrikort,  
                                        OUTPUT TABLE tempberid,    
                                        OUTPUT TABLE tempberkalk,  
                                        OUTPUT TABLE tempbermtrl,  
                                        OUTPUT TABLE tempberlinkab,
                                        OUTPUT TABLE tempkskydd,   
                                        OUTPUT TABLE tempberupp,
                                        OUTPUT TABLE tempberid2,
                                        OUTPUT TABLE tempaobersing,
                                        OUTPUT TABLE tempberpunkt).
                                        
       
      RUN hamtlev_UI IN expimpapph (INPUT valomrade,INPUT valaonr,OUTPUT lnamnex).
                                       
      EMPTY TEMP-TABLE tempglobforetag NO-ERROR.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BERFORE"  
      inextradatatemp.HUVUDCH  = valomrade
      inextradatatemp.HUVUDINT = INTEGER(valaonr).
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      
      CREATE tempglobforetag.
      ASSIGN 
      tempglobforetag.OMRADE = valomrade       
      tempglobforetag.BERNR  = INTEGER(valaonr)
      tempglobforetag.GFORETAGUNIK = globunikdb
      tempglobforetag.GFORETAG = globforetag.
      tempglobforetag.LEVNAMNEX = lnamnex. 
      IF AVAILABLE extradatatemp THEN DO:
         tempglobforetag.GFORETAG = extradatatemp.SOKCHAR[1]. 
      END.
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "MARKSTN"                   
      inextrakopptemp.KOPPLACHAR1 = valaonr               
      inextrakopptemp.KOPPLAINT1 = ?
      inextrakopptemp.KOPPLACHAR2 = valomrade            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN etabhamt_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp). 
      FOR EACH extrakopptemp:
         CREATE tempextramarkstn.
         ASSIGN
         tempextramarkstn.KOPPLACHAR1      = extrakopptemp.KOPPLACHAR1
         tempextramarkstn.KOPPLACHAR2      = extrakopptemp.KOPPLACHAR2
         tempextramarkstn.SOKINT1          = extrakopptemp.SOKINT[1]
         tempextramarkstn.SOKINT2          = extrakopptemp.SOKINT[2]
         tempextramarkstn.SOKCHAR1         = extrakopptemp.SOKCHAR[1].
      END.
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
      /*KALKYL-BEREDNING SUNDSVALL*/
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "KALKBER"   
      inextrakopptemp.KOPPLACHAR1 = ?
      inextrakopptemp.KOPPLAINT1 =  ?
      inextrakopptemp.KOPPLACHAR2 = valomrade
      inextrakopptemp.KOPPLAINT2 =  INTEGER(valaonr).
      RUN etabhamt_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp). 
      FOR EACH extrakopptemp:
         CREATE tempextrakalkber.
         ASSIGN
         tempextrakalkber.KOPPLACHAR1      = extrakopptemp.KOPPLACHAR1
         tempextrakalkber.KOPPLACHAR2      = extrakopptemp.KOPPLACHAR2
         tempextrakalkber.KOPPLAINT1       = extrakopptemp.KOPPLAINT1
         tempextrakalkber.KOPPLAINT2       = extrakopptemp.KOPPLAINT2.
      END.
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
  
         /*HD HÄR*/
      
      RUN skapaexport_UI IN expimphdh (INPUT INTEGER(valaonr), INPUT valomrade,
                                             OUTPUT TABLE hdpunkttemp,     
                                             OUTPUT TABLE hdschaktfortemp, 
                                             OUTPUT TABLE hdschakttemp,    
                                             OUTPUT TABLE hdkabellinjetemp,
                                             OUTPUT TABLE hdforlkabtemp,   
                                             OUTPUT TABLE hdforlsamtemp,
                                             OUTPUT  TABLE hdprotkopbertemp,
                                             OUTPUT  TABLE hdschaktkalkspectemp,
                                             OUTPUT  TABLE hdschakprottemp,
                                             OUTPUT  TABLE hdschakprothandtemp,
                                             OUTPUT  TABLE hdkalkbefbtemp,
                                             OUTPUT  TABLE hdkalktemp).


      
   END.
   IF TOG_BER = FALSE THEN DO: 
      IF RAD_MTR = 2 THEN DO:
         RUN selxml_UI (INPUT "Materielkatalogen sammanställs").
         RUN expmtrlkat_UI IN expimpapph (INPUT valnlevmtrlkat,INPUT TOG_MTRLPRIS, OUTPUT TABLE tempmtrl).   
      END.
     
      IF RAD_MTR = 1 THEN DO:           
         RUN selxml_UI (INPUT "Konstruktionsupplägget sammanställs").
         EMPTY TEMP-TABLE kon_grupp NO-ERROR. 
         /*IF typdator = 1 THEN DO:*/
            FOR EACH konstgrptemp:
               CREATE kon_grupp.
               BUFFER-COPY konstgrptemp TO kon_grupp.
            END.
         /*END.*/
         /*HD HÄR*/
          
         RUN expupplagghd_UI IN expimpapph (
                                        INPUT TOG_MTRLPRIS,
                                        INPUT TABLE kon_grupp,
                                        OUTPUT TABLE tempkonstgrupp,   
                                        OUTPUT TABLE tempbbenamning,    
                                        OUTPUT TABLE tempedigrupp,      
                                        OUTPUT TABLE tempkonstruktion,  
                                        OUTPUT TABLE tempkonstval,      
                                        OUTPUT TABLE tempmtrlber,       
                                        OUTPUT TABLE tempberstolp,      
                                        OUTPUT TABLE tempberskap,                                             
                                        OUTPUT TABLE tempkalkber,
                                        OUTPUT TABLE tempforlagg,
                                        OUTPUT TABLE tempsamforlagg,
                                        OUTPUT TABLE tempytbelagg,
                                        OUTPUT TABLE hdhandtemp,
                                        OUTPUT TABLE hdkkopptemp).
         /*
         FIND FIRST tempglobforetag NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tempglobforetag THEN DO:
            CREATE tempglobforetag.
            tempglobforetag.GFORETAG = globforetag. 
         END.
         */
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "KABLAR"
         inextradatatemp.HUVUDCH = ? 
         inextradatatemp.HUVUDINT = ?. 
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FOR EACH extradatatemp:
            IF extradatatemp.SOKLOG[1] = TRUE THEN DO:
               IF typdator = 1 THEN DO:
                  CREATE tempkabel.
                  ASSIGN
                  tempkabel.KABEL = extradatatemp.HUVUDCH
                  tempkabel.KABID = extradatatemp.HUVUDINT 
                  tempkabel.DIAMETER = extradatatemp.SOKINT[1]
                  tempkabel.BORTTAGEN = extradatatemp.SOKLOG[1].
               END.
            END.
            ELSE DO:
               CREATE tempkabel.
               ASSIGN
               tempkabel.KABEL = extradatatemp.HUVUDCH
               tempkabel.KABID = extradatatemp.HUVUDINT 
               tempkabel.DIAMETER = extradatatemp.SOKINT[1]
               tempkabel.BORTTAGEN = extradatatemp.SOKLOG[1].
            END.
         END.            
         EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      END.
   END.   
   /*export*/
   RUN utinxml_UI (INPUT 1). 
   IF TOG_BER = TRUE THEN DO:
      IF typdator = 1 THEN MESSAGE "Beredningen är exporterad till "  filnamn VIEW-AS ALERT-BOX.
      IF typdator = 2 THEN MESSAGE "Beredningen är exporterad till PDA" VIEW-AS ALERT-BOX.
   END.
   ELSE DO:          
      IF RAD_MTR = 1 THEN MESSAGE "Upplägget är exporterat till " filnamnuppxml VIEW-AS ALERT-BOX.
      IF RAD_MTR = 2 THEN MESSAGE "Matteriellistan är exporterat till " filnamnuppxml VIEW-AS ALERT-BOX.  
   END.
   IF typdator = 1 THEN DO:
      varforetypchar[50] = "".
      IF TOG_BER = TRUE THEN DO:
         MESSAGE "Vill du skicka beredningen som E-post?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
         CASE svar:
            WHEN TRUE THEN DO:
               varforetypchar[50] = filnamn.               
            END.
            WHEN FALSE THEN DO:
               filnamn = "".               
            END.
         END CASE.
      END.
      ELSE DO:
         MESSAGE "Vill du skicka upplägget som E-post?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svaru AS LOGICAL.
         CASE svaru:
            WHEN TRUE THEN DO:
               IF varforetypchar[50] = "" THEN varforetypchar[50] = filnamnuppxml.                    
               ELSE varforetypchar[50] = varforetypchar[50] + "," + filnamnuppxml.              

                    /*filnamnuppxml = ", " + filnamnuppxml.*/
            END.
            WHEN FALSE THEN DO:
               filnamnuppxml = "".
            END.
         END CASE.
      END.
      IF varforetypchar[50] NE ""  THEN DO:
         /*RUN EMEDD.W (INPUT FALSE).*/
         DEFINE VARIABLE infiler AS CHARACTER NO-UNDO.
         IF TOG_BER = TRUE THEN infiler = filnamn.
         ELSE IF RAD_MTR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE THEN infiler = filnamnuppxml.
         
         DEFINE VARIABLE orgdir AS CHARACTER NO-UNDO.
         file-info:file-name = ".".
         orgdir = file-info:full-pathname.

         RUN SPECIALMAPI.P (INPUT "", INPUT "", INPUT infiler).
         RUN SetCurrentDirectoryA IN Guru.Konstanter:hpApi  (INPUT orgdir).
      END.
      varforetypchar[50] = "".
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
   DEFINE VARIABLE bortsvar AS LOGICAL NO-UNDO.
   
   SEL_XML:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".   
   IF typdator = 2 OR (typdator = 1) THEN DO:
      /*import handdator*/
      EMPTY TEMP-TABLE valtemp NO-ERROR.
      IF TOG_BER = TRUE THEN DO:             
         IF typdator = 2 THEN DO:
            ASSIGN
            antal_valda = BRW_BER:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
            IF antal_valda = 0 THEN DO:
               MESSAGE "Du har inte valt någon beredning." VIEW-AS ALERT-BOX TITLE "Meddelande".
               RETURN NO-APPLY.
            END.
            antal_raknare = 1.         
            DO WHILE antal_raknare LE antal_valda:
               status-ok = BRW_BER:FETCH-SELECTED-ROW(antal_raknare).         
               CREATE valtemp.
               BUFFER-COPY beredningtemp TO valtemp.
               antal_raknare = antal_raknare + 1.  
            END.   
         END.
         ELSE DO:
            IF SEL-IMP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ? AND TOG_BER = TRUE THEN DO: 
               MESSAGE "Du har inte valt någon beredning." VIEW-AS ALERT-BOX
               TITLE "Meddelande".
               RETURN NO-APPLY.
            END.
            
            
            FIND FIRST beredningtemp WHERE beredningtemp.BERAONR = tempbernr AND  beredningtemp.OMRADE = tempomr 
            NO-ERROR.
            IF AVAILABLE beredningtemp THEN DO:
               CREATE valtemp.
               BUFFER-COPY beredningtemp TO valtemp.
               filnamn = fildir + tempbernr + "!" + tempomr + "!" + beredningtemp.BENAMNING.
            END.                 
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
            
            /*hämta noder tempaobersing */
            RUN utinxml_UI (INPUT 4).
                              
            IF NOT AVAILABLE tempaobersing THEN DO:
               /*skapa nytt nr */
               skapanyttnummer = TRUE.
            END.
            ELSE DO: 
               /* ersätt */
               EMPTY TEMP-TABLE tempaobersing2 NO-ERROR. 
               BUFFER-COPY tempaobersing TO tempaobersing2.
   
               /*Kolla globforetag*/
               IF tempglobforetag.GFORETAG NE globforetag THEN DO:
                  skapanyttnummer = TRUE.   
               END.
               ELSE DO:
                  IF Guru.SharedVariable:singel = TRUE THEN DO:
                     skapanyttnummer = TRUE.
                     sparasing = TRUE.                  
                     /* SPARA SINGBERPROJ*/
                  END.
                  ELSE DO: /* WEB */
                     /*Hämta beredning from AONR */
                     RUN berfromao_UI IN expimpapph (INPUT tempaobersing2.AONRWEB, INPUT tempaobersing2.DELNRWEB, OUTPUT valtemp.BERNR, OUTPUT valtemp.OMRADE, OUTPUT aofinns).
                     
                     IF aofinns = TRUE THEN DO: /* får endast chans att skriva över om ao finns */
                        IF valtemp.BERNR = 0 THEN DO: /* Ber finns ej */
                           skapanyttnummer = TRUE.
                        END.
                        ELSE DO: /* ber finns (normalfall) */
                           RUN selxml_UI (INPUT "Beredningen kontrolleras"). 
                           RUN inkopkoll_UI IN expimpapph (INPUT STRING(valtemp.BERNR), INPUT valtemp.OMRADE,OUTPUT musz).
                           IF musz = TRUE THEN DO: /* inköp finns */
                              musz = FALSE.
                              MESSAGE "Denna bereding har ett inköpsprotokoll och kan inte importeras direkt. Vill du skapa en ny beredning?" 
                              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skapa en ny?" UPDATE inkopny AS LOGICAL.         
                              IF inkopny = FALSE THEN DO:
                                 SEL_XML:HIDDEN = TRUE.
                                 RETURN NO-APPLY.
                              END.
                              ELSE DO:
                                 skapanyttnummer = TRUE.
                                 /*NYTT BEREDNINGSNUMMER*/              
                              END.
                           END.
                           ELSE DO: /* inköp finns inte */
                              /* skriva över? */
                              EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
                              RUN typval3_UI IN expimpapph (INPUT valtemp.BERNR,INPUT valtemp.OMRADE,OUTPUT TABLE felmeddtemp).
      
                              FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
                              IF AVAILABLE felmeddtemp THEN DO:
                                 IF felmeddtemp.FELMEDD = "" THEN DO:
                                    IF felmeddtemp.VAL = 1 THEN DO:
                                       /*
                                                                     ASSIGN valtemp.NYTTNR = valtemp.BERNR.
                                                                     */
                                    END.
                                 END.
                                 ELSE DO:
                                    MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE svar.         
                                 END.
                              END.
                              
                              IF svar THEN DO: /* SKRIVA ÖVER */
                                 /*Ta bort anvspärr*/
                                 RUN selxml_UI (INPUT "Beredningen görs klar för att skrivas över").               
                                 RUN delberanv_UI IN expimpapph (INPUT valtemp.BERNR,INPUT valtemp.OMRADE).
                                 /*Tar bort hela beredningen , men inte tabellen BEREDNING*/
                                 IF Guru.Konstanter:appcon THEN DO:                           
                                    RUN DELBERE.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                                    (INPUT STRING(valtemp.BERNR), INPUT valtemp.OMRADE).                                 
                                 END.
                                 ELSE DO:
                                    RUN DELBERE.P
                                    (INPUT STRING(valtemp.BERNR), INPUT valtemp.OMRADE).                                 
                                 END.
                                 /*TAR BORT TABELLEN BEREDNING*/
                                 /*RUN delbere_UI IN expimpapph (INPUT valtemp.BERNR,INPUT valtemp.OMRADE,OUTPUT TABLE eberedningtemp).*/
                                 valtemp.NYTTNR = valtemp.BERNR.
                              END. 
                              ELSE DO: /* Inte skriva över */
                                 skapanyttnummer = FALSE.
                                 valtemp.OMRADE = globomr.
                                 MESSAGE "Vill Ni skapa en ny beredning?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ny beredning?" UPDATE svar.
                                 IF svar THEN DO:
                                    skapanyttnummer = TRUE.
                                    /*NYTT BEREDNINGSNUMMER*/              
                                 END.
                                 ELSE DO:
                                    MESSAGE "Import av beredning " + STRING(valtemp.BERNR) + " avbruten!"
                                    VIEW-AS ALERT-BOX TITLE "Meddelande".
                                    DELETE valtemp.
                                    SEL_XML:HIDDEN = TRUE.
                                    RETURN NO-APPLY.
                                 END.
                              END.
                           END.
                        END.
                     END.
                     ELSE DO:
                        skapanyttnummer = TRUE.               
                     END.   
                  END. /*slut web*/                               
               END. /*slut globforetag*/            
            END. /* slut aobersing */
         END. /* slut valtemp */
      END.          
      /*import*/   
      
      RUN utinxml_UI (INPUT 2).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.    
      IF TOG_BER = TRUE THEN DO:
         IF Guru.Konstanter:appcon THEN DO:
            RUN IMPBERKALKSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT varforetypchar[48],INPUT STRING(valtemp.NYTTNR),INPUT valtemp.OMRADE).
         END.
         ELSE DO:
            RUN IMPBERKALKSKAP.P (INPUT varforetypchar[48],INPUT STRING(valtemp.NYTTNR),INPUT valtemp.OMRADE).
         END.
         
         IF Guru.Konstanter:globanv = "celpao" THEN.
         ELSE DO:   
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
      END.      
   END.   
   ELSE IF typdator = 3 THEN DO:      
      IF NOT AVAILABLE avcadtemp THEN DO:
         RETURN.
      END.
      IF valort = "" THEN valort = avcadtemp.BENAMNING.
      
      ASSIGN
      valomrade = globomr
      valaonr = ?
      vart = "NYA"
      avbryt = FALSE.
      RUN NYBEREU.W (INPUT 1,input "").
      {muswait.i}      
      valimpomrade = valomrade.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE "Import avbruten !" VIEW-AS ALERT-BOX INFO BUTTONS OK.
         
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
 /*  listobj = ""*/
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
               
               IF beredningtemp.OMRADE = "" THEN beredningtemp.OMRADE = globomr.
               tempstring = "Bernr: " + beredningtemp.BERAONR + " " + gomrk + ": " + beredningtemp.OMRADE + 
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
   DEBUGGER:SET-BREAK().
   INPUT STREAM dirstrom FROM OS-DIR(fildir) NO-ECHO.
   REPEAT:
      /*Hämtar filnamn, hela sökvägen och vilken typ av fil det är*/
      
      SET STREAM dirstrom tmpfilnamn tmpdirlist tmpattrlist NO-ERROR.
      
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
                  
                  IF beredningtemp.OMRADE = "" THEN beredningtemp.OMRADE = globomr.
                  tempstring = "Bernr: " + beredningtemp.BERAONR + " " + gomrk + ": " + beredningtemp.OMRADE + 
                  " Benm: " + beredningtemp.BENAMNING.                    
                  FIND FIRST imptemp WHERE imptemp.INVAR = tempstring NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE imptemp THEN DO:
                     CREATE imptemp.
                     ASSIGN
                     imptemp.INDIR = fildir
                     imptemp.INVAR = tempstring.
                     SEL-IMP:ADD-LAST(tempstring) IN FRAME {&FRAME-NAME}.  
                  END.
                  /*
                  RUN omrkoll_UI IN expimpapph (INPUT beredningtemp.OMRADE,OUTPUT musz).
                  IF musz = TRUE THEN musz = FALSE.
                  ELSE beredningtemp.OMRADE = globomr.
                  */
                  
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
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      IF OS-GETENV("SystemDrive") NE ? THEN webclienttempdir = OS-GETENV("SystemDrive") + "\temp\".
      ELSE webclienttempdir = "c:\temp\".
      OS-CREATE-DIR VALUE(webclienttempdir) . 
      OS-CREATE-DIR VALUE(webclienttempdir + globanv) .
      
      IF OS-ERROR NE 0  THEN DO:
         webclienttempdir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv.
         OS-CREATE-DIR VALUE(webclienttempdir + globanv).
         webclienttempdir = webclienttempdir + "\".  
              
      END.
      ELSE webclienttempdir = webclienttempdir + globanv + "\".
              
   END.
   
   IF Guru.Konstanter:globforetag = "cGRAN" THEN DO:   
      fildir = "\\SRV00486\APPL\GURU\DOKUMENT\BEREDNING\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "CLULE" THEN DO:
      fildir = "\\195.196.19.21\DOKUMENT\BEREDNING\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
      fildir = "\\PC112\DELAD\dokument\" + globanv + "\".
   END.
   ELSE IF Guru.Konstanter:globforetag = "STRA" THEN DO:
      fildir = "\\srv07web\DELAD\Gemensam bärbart\" + globanv + "\".
   END.
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uphand_UI C-Win 
PROCEDURE uphand_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE VARIABLE errorvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE volvo AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempkor AS CHARACTER NO-UNDO.
   DEFINE VARIABLE pguru AS CHARACTER NO-UNDO.
   DEFINE VARIABLE wguru AS CHARACTER NO-UNDO.
   DEFINE VARIABLE pdaPath AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE uppfraga AS LOGICAL NO-UNDO.
   DEFINE VARIABLE tempdir AS CHARACTER NO-UNDO.
   MESSAGE "PDA:ns databas kommer att tömmas, är du säker att du inte har någon viktig information i PDA:n?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fråga" UPDATE uppfraga.
   
   IF uppfraga THEN DO:
      {muswait.i}
      
      RUN checkfiles_UI(OUTPUT errorvar).
      IF errorvar NE ? THEN DO:
         MESSAGE "Du saknar följande filer: " + errorvar VIEW-AS ALERT-BOX.
         RETURN.
      END.
      
      pdaPath = "\\Program Files\\Pguru\\".
            
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         
         pguru = REPLACE(SEARCH("Pguru.exe"),guruvar,"..\").
         wguru = REPLACE(SEARCH("Wguru.sdf"),guruvar,"..\").
         volvo = REPLACE(SEARCH("pput.exe"),guruvar,"..\").
                  
         /*tempdir = webclienttempdir.*/
      END.
      ELSE DO:
         /*tempdir = VALUE(SESSION:TEMP-DIRECTORY).*/
         
         pguru = SEARCH("Pguru.exe").
         OS-COPY VALUE(pguru) VALUE(SESSION:TEMP-DIRECTORY).
         pguru = SESSION:TEMP-DIRECTORY + "Pguru.exe".
         
         wguru = SEARCH("Wguru.sdf").
         OS-COPY VALUE(wguru) VALUE(SESSION:TEMP-DIRECTORY).
         wguru = SESSION:TEMP-DIRECTORY + "Wguru.sdf".
         
         volvo = SEARCH("pput.exe").
         OS-COPY VALUE(volvo) VALUE(SESSION:TEMP-DIRECTORY).
         volvo = SESSION:TEMP-DIRECTORY + "pput.exe".         
      END.                   
            
      tempkor = volvo + ' "' + pguru + '"' + ' "' + pdaPath  + "Pguru.exe".
      OS-COMMAND SILENT VALUE(tempkor).            
      
      tempkor = volvo + ' "' + wguru + '"' + ' "' + pdaPath + "Wguru.sdf".            
      OS-COMMAND SILENT VALUE(tempkor).      
      
      {musarrow.i}
      
      MESSAGE "Handdator Uppdaterad, kom ihåg att exportera beredningsupplägg igen!"   VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      MESSAGE "Uppdatering avbruten."   VIEW-AS ALERT-BOX.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utinxml_UI C-Win 
PROCEDURE utinxml_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
   DEFINE VARIABLE dynxml AS HANDLE NO-UNDO.
   DEFINE VARIABLE sparfilnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE okand AS LOGICAL NO-UNDO.
   IF typdator = 1 THEN DO:
      okand = TRUE.
   END.
   IF typdator = 2 THEN DO:
      okand = FALSE.
   END.
   mvarhj = REPLACE(mvar,"*","").
   IF fildir = "" THEN  RUN selfill_UI.
   
   filnamnuppxml = fildir + "mtrl.xml".
   {UTXKMLKOLL.I}
   RUN VALUE(vxmlpro) PERSISTENT SET dynxml (INPUT okand).
   EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   
   IF vad = 1 THEN DO: /* 1 = export (skapa xml fil) */
      IF TOG_BER = TRUE THEN DO:
         RUN uttab_UI (INPUT 1,INPUT TRUE).
         
         fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
         {SESSIONTEMPDIR.I}
         IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
         IF typdator = 1 THEN filnamn = fildir + beredningtemp.BERAONR + "!" + beredningtemp.OMRADE + "!" + beredningtemp.BENAMNING + mvarhj .
         IF typdator = 2 THEN filnamn = fildir + beredningtemp.BERAONR + "!" + beredningtemp.OMRADE + "!" + beredningtemp.BENAMNING + mvarhj.
         filnamn = REPLACE(filnamn,",","").
         sparfilnamn = filnamn.
         xmlsparfilnamn = "".
         xmlsparfilnamn = SUBSTRING(filnamn,R-INDEX(filnamn,"\") + 1). 
         /*xmlsparfilnamn = xmlsparfilnamn + ".xml".*/
         RUN selxml_UI (INPUT "Berednings filen skapas").
         FOR EACH tempnamn:
            IF sparfilnamn NE "" THEN RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE,INPUT "start",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT sparfilnamn).
            ELSE RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE ,INPUT "", INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT "").
            sparfilnamn = "".
         END.
         RUN xmlutslut_UI IN dynxml.
         RUN xmlslut_UI IN dynxml.
         IF typdator = 2 THEN DO:
            RUN selxml_UI (INPUT "Beredningen kopieras till handdator").
            RUN ut_UI (INPUT "berToPDA").         
         END.
      END.
      EMPTY TEMP-TABLE tempnamn NO-ERROR.    
      sparfilnamn = filnamnuppxml.
      
      IF TOG_BER = TRUE THEN RUN creatnod_UI IN dynxml.
      
      ELSE IF RAD_MTR = 1 THEN DO:
         RUN uttab_UI (INPUT 2,INPUT TRUE).
      END.   
      ELSE IF RAD_MTR = 2 THEN DO:
         RUN uttab_UI (INPUT 3,INPUT TRUE).
      END.    
      IF TOG_BER = FALSE THEN DO:
         IF RAD_MTR = 1 THEN  filnamnuppxml = fildir + "uppmtrl.xml".
         ELSE filnamnuppxml = fildir + cmb_lev + "mtrl.xml".
         sparfilnamn = filnamnuppxml.
         RUN selxml_UI (INPUT "Materielkatalog och/eller Konstruktionsuppläggets filen skapas").         
         FOR EACH tempnamn:
            IF sparfilnamn NE "" THEN RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE,INPUT "start",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT sparfilnamn).
            ELSE RUN xmlutstart_UI IN dynxml (INPUT tempnamn.NAMNH,INPUT tempnamn.NAMNH:TABLE ,INPUT "",INPUT tempnamn.NODTAB, INPUT "FOR EACH ",INPUT "").
            sparfilnamn = "".
         END.
         RUN xmlutslut_UI IN dynxml.
         RUN xmlslut_UI IN dynxml.
         EMPTY TEMP-TABLE tempnamn NO-ERROR. 
         IF typdator = 2 THEN DO:
            RUN selxml_UI (INPUT "Materielkatalog och/eller Konstruktionsuppläggets kopieras till handdator").         
            filnamn = filnamnuppxml.
            RUN ut_UI (INPUT "ulToPDA").         
            filnamnuppxml = REPLACE(filnamnuppxml,"\\","\").
            OS-DELETE VALUE(filnamnuppxml).            
         END.
         
      END.
      RUN selxml_UI (INPUT "Kopieringen färdig").               
      
   END.
   /*import*/
   IF vad = 2 THEN DO:
      EMPTY TEMP-TABLE tempaobersing NO-ERROR.
      EMPTY TEMP-TABLE tempglobforetag NO-ERROR.
      IF Guru.Konstanter:appcon THEN DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
      END.
      ELSE DO:
         RUN DYNSPARATEMP.P PERSISTENT SET laddaprochspar.      
      END.
      /*import beredning*/
      
      IF TOG_BER = TRUE THEN DO:
         RUN uttab_UI (INPUT 1,INPUT TRUE).   
                
         RUN selxml_UI (INPUT "Beredningen läses in i Guru").
         FIND FIRST berfil WHERE berfil.ROWBER = ROWID(beredningtemp) NO-LOCK NO-ERROR.
         RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT berfil.INDIR + berfil.FILNAMNS).
         lnamnex = "".
         FIND FIRST tempglobforetag NO-LOCK NO-ERROR.         
         IF AVAILABLE tempglobforetag THEN DO:
            IF tempglobforetag.LEVNAMNEX NE "" THEN lnamnex = tempglobforetag.LEVNAMNEX.            
            IF tempglobforetag.GFORETAG = "TRAS" OR tempglobforetag.GFORETAG = "nylb" THEN DO:
               MESSAGE "Den här beredningen går ej att läsa in. Den har en ogiltig version!"
               VIEW-AS ALERT-BOX.
               RETURN.
            END.   
         END.  
         ELSE DO:
            MESSAGE "Den här beredningen går ej att läsa in. Den har fel version!"
            VIEW-AS ALERT-BOX.
            RETURN.
         END.
                  
         
                  
         FIND FIRST temp_ber NO-LOCK NO-ERROR.
         FIND FIRST valtemp NO-LOCK NO-ERROR.
         /*Ska sedan  ligga i import*/
         IF skapanyttnummer = TRUE THEN DO:
            FIND FIRST omrtemp WHERE  omrtemp.OMRADE = valtemp.OMRADE NO-ERROR.
            IF NOT AVAILABLE omrtemp THEN DO:
               valtemp.OMRADE = globomr.
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
               IF VALID-HANDLE(dynxml) THEN DELETE PROCEDURE dynxml. 
               musz = TRUE.   
               RETURN NO-APPLY.
            END.
                        
            /*nya nr */
            valtemp.OMRADE = valomrade.
            valtemp.NYTTNR = valbernr.
            
            FIND FIRST tempaobersing NO-ERROR.
            
            IF AVAILABLE tempaobersing THEN DO:
               ASSIGN
               tempaobersing.BEROMRADESING = valomrade
               tempaobersing.BERNRSING = valbernr.
               RUN sparasing_UI IN expimpapph (INPUT TABLE tempaobersing).
            END.
            
            
            RUN berhmt_UI IN expimpapph (INPUT valbernr,INPUT valomrade,OUTPUT TABLE eberedningtemp).
            
            
         END.
         /*befintlig beredning*/
         
         /*Behövs ej, skapas i NYBEREU.W */
         /*
         FIND FIRST eberedningtemp NO-LOCK NO-ERROR.
         IF AVAILABLE eberedningtemp THEN DO:
            FOR EACH temp_ber NO-LOCK:
               BUFFER-COPY eberedningtemp TO temp_ber.
            END.
         END.
         ELSE DO:
            ASSIGN
            temp_ber.AKTIV = TRUE
            temp_ber.ANVANDARE = Guru.Konstanter:globanv
            temp_ber.AONR = ?
            temp_ber.BENAMNING = "Från handdator " + temp_ber.BENAMNING
            temp_ber.KALKYL = TRUE 
            temp_ber.KATAR = ?.
            IF typdator = 1 THEN DO:
               temp_ber.BENAMNING = "Import via xml-fil " + beredningtemp.BENAMNING.
            END.
         END.
         FOR EACH temp_ber:
            ASSIGN
            temp_ber.OMRADE = valtemp.OMRADE            
            temp_ber.BERNR  = valtemp.NYTTNR            
            temp_ber.BERAONR = STRING(valtemp.NYTTNR).

            IF temp_ber.AONR = "" THEN temp_ber.AONR = ?.
         END.*/
         RUN selxml_UI (INPUT "Beredningen sparas i Guru").
         /*
         FIND FIRST tempnamn WHERE tempnamn.NODTAB = "BEREDNING" NO-LOCK NO-ERROR.
         RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB). /*ccc*/ 
         */
         IF AVAILABLE tempnamn THEN DELETE tempnamn.
         IF skapanyttnummer = TRUE THEN DO:
            /*MESSAGE "Ditt nya beredningsnummer är: " + gomrk + " " + valtemp.OMRADE + " " + STRING(nyttnr) VIEW-AS ALERT-BOX TITLE "Meddelande". */
         END.
         {muswait.i}      
         FOR EACH tempberval:
            ASSIGN
            tempberval.OMRADE = valtemp.OMRADE 
            tempberval.AONR =  STRING(valtemp.NYTTNR). 
            IF STRING(tempberval.UPPLAG) = "0" THEN tempberval.UPPLAG = ?.
            /*
            IF tempberval.UPPLAG < 0 OR tempberval.UPPLAG = ? THEN tempberval.UPPLAG = ?.
            tempberval.UPPLAG = ?.
            */
         END.
         FOR EACH tempberord:
            ASSIGN
            tempberord.OMRADE = valtemp.OMRADE 
            tempberord.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempfrikort:
            ASSIGN
            tempfrikort.OMRADE = valtemp.OMRADE 
            tempfrikort.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempberid:
            ASSIGN
            tempberid.OMRADE = valtemp.OMRADE 
            tempberid.AONR =  STRING(valtemp.NYTTNR)
            tempberid.DELNR = ?
            tempberid.YKORD = ?.
            IF tempberid.XKORD = 0 THEN tempberid.XKORD = ?.
         END.
         FOR EACH tempberid2:
            ASSIGN
            tempberid2.OMRADE = valtemp.OMRADE 
            tempberid2.AONR =  STRING(valtemp.NYTTNR).            
         END.
         FOR EACH tempberval:
            FIND FIRST tempberid WHERE tempberid.NUM = tempberval.NUM NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tempberid THEN DO:
               CREATE tempberid.
               ASSIGN
               tempberid.OMRADE = valtemp.OMRADE 
               tempberid.AONR =  STRING(valtemp.NYTTNR)
               tempberid.NUM = tempberval.NUM.
            END.
         END.
         FOR EACH tempberkalk:
            ASSIGN
            tempberkalk.OMRADE = valtemp.OMRADE 
            tempberkalk.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempbermtrl:
            ASSIGN
            tempbermtrl.OMRADE = valtemp.OMRADE 
            tempbermtrl.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempberlinkab:
            ASSIGN
            tempberlinkab.OMRADE = valtemp.OMRADE 
            tempberlinkab.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempkskydd:
            ASSIGN
            tempkskydd.OMRADE = valtemp.OMRADE 
            tempkskydd.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempberupp:
            ASSIGN
            tempberupp.OMRADE = valtemp.OMRADE 
            tempberupp.AONR =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempberpunkt WHERE NO-LOCK:
            ASSIGN 
            tempberpunkt.OMRADE = valtemp.OMRADE 
            tempberpunkt.AONR =  STRING(valtemp.NYTTNR).
         END.
         /*HD HÄR*/
         FOR EACH hdpunkttemp:
            ASSIGN
            hdpunkttemp.OMRADE = valtemp.OMRADE 
            hdpunkttemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdschaktfortemp:
            ASSIGN
            hdschaktfortemp.OMRADE = valtemp.OMRADE 
            hdschaktfortemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdschakttemp:
            ASSIGN
            hdschakttemp.OMRADE = valtemp.OMRADE 
            hdschakttemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdkabellinjetemp:
            ASSIGN
            hdkabellinjetemp.OMRADE = valtemp.OMRADE 
            hdkabellinjetemp.BERNR =  valtemp.NYTTNR. 
         END.
   
         FOR EACH hdforlkabtemp:
            ASSIGN
            hdforlkabtemp.OMRADE = valtemp.OMRADE 
            hdforlkabtemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdforlsamtemp:
            ASSIGN
            hdforlsamtemp.OMRADE = valtemp.OMRADE 
            hdforlsamtemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdprotkopbertemp:
            ASSIGN
            hdprotkopbertemp.OMRADE = valtemp.OMRADE 
            hdprotkopbertemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdschaktkalkspectemp NO-LOCK:
            ASSIGN
            hdschaktkalkspectemp.OMRADE = valtemp.OMRADE 
            hdschaktkalkspectemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdschakprottemp NO-LOCK:
            ASSIGN
            hdschakprottemp.OMRADE = valtemp.OMRADE 
            hdschakprottemp.BERNR =  valtemp.NYTTNR.    
         END.
         FOR EACH hdschakprothandtemp NO-LOCK:
            ASSIGN
            hdschakprothandtemp.OMRADE = valtemp.OMRADE 
            hdschakprothandtemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdkalktemp  NO-LOCK:
            ASSIGN
            hdkalktemp.OMRADE = valtemp.OMRADE 
            hdkalktemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH hdkalkbefbtemp :
            ASSIGN
            hdkalkbefbtemp.OMRADE = valtemp.OMRADE 
            hdkalkbefbtemp.BERNR =  valtemp.NYTTNR. 
         END.
         FOR EACH tempextramarkstn  NO-LOCK:
            ASSIGN
            tempextramarkstn.KOPPLACHAR2 = valtemp.OMRADE 
            tempextramarkstn.KOPPLACHAR1 =  STRING(valtemp.NYTTNR). 
         END.
         FOR EACH tempextrakalkber  NO-LOCK:
            ASSIGN
            tempextrakalkber.KOPPLACHAR2 = valtemp.OMRADE 
            tempextrakalkber.KOPPLAINT2  = valtemp.NYTTNR. 
         END.
         EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
         EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
         
         FOR EACH tempextramarkstn  NO-LOCK:
            CREATE inextrakopptemp.
            ASSIGN
            inextrakopptemp.PROGRAM = "MARKSTN"                   
            inextrakopptemp.KOPPLACHAR1 = tempextramarkstn.KOPPLACHAR1      
            inextrakopptemp.KOPPLACHAR2 = tempextramarkstn.KOPPLACHAR2      
            inextrakopptemp.KOPPLAINT1 = ?  
            inextrakopptemp.KOPPLAINT2 =  ?  
            inextrakopptemp.SOKINT[1] = tempextramarkstn.SOKINT1
            inextrakopptemp.SOKINT[2] = tempextramarkstn.SOKINT2
            inextrakopptemp.SOKCHAR[1] =  tempextramarkstn.SOKCHAR1.                  
         END.
         RUN extraspar_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp). 
         EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
         EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
         /*KALKYL-BEREDNING SUNDSVALL*/        
         FOR EACH tempextrakalkber NO-LOCK:
            CREATE inextrakopptemp.          
            ASSIGN
            inextrakopptemp.PROGRAM = "KALKBER"                   
            inextrakopptemp.KOPPLACHAR1 = tempextrakalkber.KOPPLACHAR1
            inextrakopptemp.KOPPLAINT1 = tempextrakalkber.KOPPLAINT1 
            inextrakopptemp.KOPPLACHAR2 = tempextrakalkber.KOPPLACHAR2
            inextrakopptemp.KOPPLAINT2 =  tempextrakalkber.KOPPLAINT2.
         END.
         RUN extraspar_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp).
         EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
         EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
         FIND FIRST tempnamn WHERE tempnamn.NODTAB = "MARKSTN" NO-ERROR.
         IF AVAILABLE tempnamn THEN DELETE tempnamn.
         /*KALKYL-BEREDNING SUNDSVALL*/
         FIND FIRST tempnamn WHERE tempnamn.NODTAB = "KALKBER" NO-ERROR.
         IF AVAILABLE tempnamn THEN DELETE tempnamn. 
         EMPTY TEMP-TABLE tempextramarkstn NO-ERROR. 
         EMPTY TEMP-TABLE tempextrakalkber NO-ERROR. 
         FIND FIRST tempglobforetag NO-LOCK NO-ERROR.
         IF AVAILABLE tempglobforetag THEN DO:
            
            /*
            IF tempglobforetag.GFORETAG = globforetag THEN.
            */
            IF tempglobforetag.GFORETAGUNIK = globunikdb THEN.
            ELSE DO:
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "BERFORE"  
               inextradatatemp.HUVUDCH = valtemp.OMRADE
               inextradatatemp.HUVUDINT = valtemp.NYTTNR
               inextradatatemp.SOKCHAR[1] = tempglobforetag.GFORETAG.
               inextradatatemp.SOKCHAR[2] = tempglobforetag.GFORETAGUNIK.
               RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
               EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            END.                      
         END.
         FIND FIRST tempnamn WHERE tempnamn.NODTAB = "tempglobforetag" NO-ERROR.
         IF AVAILABLE tempnamn THEN DELETE tempnamn. 
         ASSIGN
         valimpomrade = valtemp.OMRADE
         valbernr =  valtemp.NYTTNR.
         /*DUMMY*/
         FOR EACH tempnamn:
            IF tempnamn.NODTAB = "tempaobersing" THEN. /* inte spara tempaobersing */
            ELSE IF tempnamn.NODTAB = "BEREDNING" THEN.
            ELSE RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).  
         END.
         
         IF tempglobforetag.GFORETAGUNIK ne globunikdb THEN DO:
         /*
         IF tempglobforetag.GFORETAG NE globforetag THEN DO:
            */
            /* Lena Om annat företag - byt leverantör till huvudlev */
            EMPTY TEMP-TABLE tidut NO-ERROR.
            EMPTY TEMP-TABLE elist_mtrl NO-ERROR.  
            FOR EACH tempbermtrl NO-LOCK:
               CREATE elist_mtrl.
               BUFFER-COPY tempbermtrl TO elist_mtrl.
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
         RUN xmlslut_UI IN dynxml.
         RUN selxml_UI (INPUT "Inläsning av beredningen klar").
         /*
         OS-DELETE VALUE(berfil.INDIR + berfil.FILNAMNS).*/
         
         EMPTY TEMP-TABLE tempnamn NO-ERROR.  
         RETURN.  
      END.
      /*import kataloger*/
      
      IF typdator = 1 THEN DO:
         IF RAD_MTR = 1 THEN  filnamnuppxml = fildir + "uppmtrl.xml".
         ELSE filnamnuppxml = fildir + cmb_lev + "mtrl.xml".
         sparfilnamn = filnamnuppxml.
      
         MESSAGE "Vill Ni skriva över Materielkatalog eller Konstruktionsupplägget som finns i denna dator?" SKIP
         "Filen som läses in ska vara : "  filnamnuppxml      
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Skriva över?" UPDATE mtrkat AS LOGICAL.
         IF mtrkat THEN. 
         ELSE DO:
            RETURN NO-APPLY.           
         END.
         IF RAD_MTR = 1 THEN DO:
            RUN uttab_UI (INPUT 2,INPUT TRUE).
            RUN selxml_UI (INPUT "Det gamla Konstruktionsupplägget tas bort!").
            RUN delupplagg_UI IN expimpapph.
         END.   
         IF RAD_MTR = 2 THEN DO:
            RUN uttab_UI (INPUT 3,INPUT TRUE).
            RUN selxml_UI (INPUT "De gamla katalogerna tas bort!").     
            RUN delmtrl_UI IN expimpapph (INPUT valnlevmtrlkat).
         END.    
         IF TOG_BER = FALSE THEN DO:
            RUN selxml_UI (INPUT "Materielkatalog eller Konstruktionsupplägget läses in i Guru").         
            RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT filnamnuppxml).
            RUN xmlslut_UI IN dynxml.
            FOR EACH tempnamn:
             
               IF tempnamn.NODTAB = "TEMPKABEL" THEN.
               ELSE IF tempnamn.NODTAB = "tempglobforetag" THEN.
               ELSE IF tempnamn.NODTAB = "tempaobersing" THEN.
               ELSE  IF tempnamn.NODTAB = "MTRL" THEN RUN sparamtrltemp_UI IN laddaprochspar (INPUT valnlevmtrlkat,INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).
               ELSE IF tempnamn.NODTAB = "tFORLAGG" OR tempnamn.NODTAB = "tYTBELAGG" THEN DO:
                  nodtabhj = tempnamn.NODTAB.
                  tempnamn.NODTAB = "".
                  tempnamn.NODTAB = SUBSTRING(nodtabhj,2).
                  RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).
               END.   
               ELSE RUN sparatemp_UI IN laddaprochspar (INPUT TABLE-HANDLE tempnamn.TORGTH, INPUT tempnamn.NODTAB).  
            END.            
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            FOR EACH tempkabel:
               CREATE inextradatatemp.          
               ASSIGN
               inextradatatemp.PROGRAM = "KABLAR"  
               inextradatatemp.HUVUDCH = tempkabel.KABEL
               inextradatatemp.HUVUDINT = tempkabel.KABID
               inextradatatemp.SOKINT[1] = tempkabel.DIAMETER
               inextradatatemp.SOKLOG[1] = tempkabel.BORTTAGEN.
            END.
            RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            RUN selxml_UI (INPUT "Kopieringen färdig").               
         END.
      END.
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
   END.
   ELSE IF vad = 4 THEN DO:
      /*läser in för att kolla rätt företag + rätt aonr */
      RUN uttab_UI (INPUT 0,INPUT TRUE).
      RUN xmlinstart_UI IN dynxml (INPUT TABLE tempnamn,INPUT berfil.INDIR + berfil.FILNAMNS).
      RUN xmlslut_UI IN dynxml.
      FIND FIRST tempaobersing NO-ERROR.
   END.
   
   IF VALID-HANDLE(laddaprochspar) THEN DELETE PROCEDURE laddaprochspar NO-ERROR.   
   IF VALID-HANDLE(dynxml) THEN DELETE PROCEDURE dynxml.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uttab_UI C-Win 
PROCEDURE uttab_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER delvar AS LOGICAL NO-UNDO.
  
   IF delvar = TRUE THEN DO: 
      EMPTY TEMP-TABLE tempnamn NO-ERROR. 
     /* singel kopp till aotab (fr. extrakopplingar) */
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempaobersing:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempaobersing:HANDLE.
      tempnamn.NODTAB = "tempaobersing".
   END.   
   IF vad = 0 THEN DO:
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempglobforetag:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempglobforetag:HANDLE.
      tempnamn.NODTAB = "tempglobforetag".
   END.
   
   IF vad = 1 THEN DO:
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER temp_ber:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE temp_ber:HANDLE.
      tempnamn.NODTAB = "BEREDNING".
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempberval:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempberval:HANDLE.
      tempnamn.NODTAB = "BERVAL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberord:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempberord:HANDLE.
      tempnamn.NODTAB = "BERORD".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempfrikort:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempfrikort:HANDLE.
      tempnamn.NODTAB = "FRIKORT".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberid:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempberid:HANDLE.
      tempnamn.NODTAB = "BERID".
      /*
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberkalk:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempberkalk:HANDLE.
      tempnamn.NODTAB = "BERKALK".
      */
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempbermtrl:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempbermtrl:HANDLE.
      tempnamn.NODTAB = "BERMTRL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberlinkab:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempberlinkab:HANDLE.
      tempnamn.NODTAB = "BERLINKAB".
      /*
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkskydd:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempkskydd:HANDLE.
      tempnamn.NODTAB = "KSKYDD".
      */
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberupp:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempberupp:HANDLE.
      tempnamn.NODTAB = "BERUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberid2:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempberid2:HANDLE.
      tempnamn.NODTAB = "BERID2".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempglobforetag:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempglobforetag:HANDLE.
      tempnamn.NODTAB = "tempglobforetag".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberpunkt:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempberpunkt:HANDLE.
      tempnamn.NODTAB = "BERPUNKT".
      
      
      
      /*HD HÄR*/
     
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdpunkttemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdpunkttemp:HANDLE.
      tempnamn.NODTAB = "HDPUNKT".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschaktfortemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschaktfortemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTFOR".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschakttemp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE hdschakttemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKT".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkabellinjetemp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE hdkabellinjetemp:HANDLE.
      tempnamn.NODTAB = "HDKABELLINJE".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdforlkabtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdforlkabtemp:HANDLE.
      tempnamn.NODTAB = "HDFORLKAB".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdforlsamtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdforlsamtemp:HANDLE.
      tempnamn.NODTAB = "HDFORLSAM".   
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdprotkopbertemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdprotkopbertemp:HANDLE.
      tempnamn.NODTAB = "HDPROTKOPPBER".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschaktkalkspectemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschaktkalkspectemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTKALKSPEC".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschakprottemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschakprottemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTPROT".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschakprothandtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschakprothandtemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTPROTHAND".                 
      /*
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkalkbefbtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdkalkbefbtemp:HANDLE.
      tempnamn.NODTAB = "KALKBEFB".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkalktemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdkalktemp:HANDLE.
      tempnamn.NODTAB = "HDKALK".   
      CREATE tempnamn.
      */        
      tempnamn.NAMNH = BUFFER tempextramarkstn:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempextramarkstn:HANDLE.
      tempnamn.NODTAB = "MARKSTN".   
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempextrakalkber:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempextrakalkber:HANDLE.
      tempnamn.NODTAB = "KALKBER".
   
   END.
   IF vad = 2 THEN DO:
      /*HD HÄR*/
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempbbenamning:HANDLE.      
      tempnamn.TORGTH = TEMP-TABLE tempbbenamning:HANDLE.
      tempnamn.NODTAB = "BBENAMNING".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempedigrupp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempedigrupp:HANDLE.
      tempnamn.NODTAB = "EDIGRUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkonstgrupp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE tempkonstgrupp:HANDLE.
      tempnamn.NODTAB = "KONSTGRUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkonstruktion:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempkonstruktion:HANDLE.
      tempnamn.NODTAB = "KONSTRUKTION".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkonstval:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempkonstval:HANDLE.
      tempnamn.NODTAB = "KONSTVAL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmtrlber:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempmtrlber:HANDLE.
      tempnamn.NODTAB = "MTRLBER".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberstolp:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempberstolp:HANDLE.
      tempnamn.NODTAB = "BERSTOLP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberskap:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempberskap:HANDLE.
      tempnamn.NODTAB = "BERSKAP".
      /*KALKYL-BEREDNING SUNDSVALL*/
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkalkber:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempkalkber:HANDLE.
      tempnamn.NODTAB = "KALKBER".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempforlagg:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempforlagg:HANDLE.
      tempnamn.NODTAB = "tFORLAGG".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempsamforlagg:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempsamforlagg:HANDLE.
      tempnamn.NODTAB = "SAMFORLAGG".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempytbelagg:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempytbelagg:HANDLE.
      tempnamn.NODTAB = "tYTBELAGG".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkabel:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempkabel:HANDLE.
      tempnamn.NODTAB = "TEMPKABEL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdhandtemp:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE hdhandtemp:HANDLE.
      tempnamn.NODTAB = "HDHANDELSE".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkkopptemp:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE hdkkopptemp:HANDLE.
      tempnamn.NODTAB = "HDKKOPP".
      
      
      FIND FIRST tempnamn WHERE tempnamn.NODTAB = "tempglobforetag" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tempnamn THEN DO:
         CREATE tempnamn.        
         tempnamn.NAMNH = BUFFER tempglobforetag:HANDLE.
         tempnamn.TORGTH = TEMP-TABLE tempglobforetag:HANDLE.
         tempnamn.NODTAB = "tempglobforetag".
      END.                  
               /*OBS EXTRADATA FÖR KABLAR*/
   END.
      

   IF vad = 3 THEN DO:
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempmtrl:HANDLE.       
      tempnamn.TORGTH = TEMP-TABLE tempmtrl:HANDLE.
      tempnamn.NODTAB = "MTRL".
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI C-Win 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER txtvar AS CHARACTER NO-UNDO.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      korfil = SEARCH("wguru.exe").
      korfil = REPLACE(korfil,guruvar,"..\").      
   END.
   ELSE DO:
      
      korfil = SEARCH("pget.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      korfil = SEARCH("pput.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      
      korfil = SEARCH("wguru.exe").
      OS-COPY VALUE(korfil) VALUE(SESSION:TEMP-DIRECTORY).
      korfil = SESSION:TEMP-DIRECTORY + "wguru.exe". 
   END.    
   filnamn = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN filnamn = webclienttempdir.
   filnamn = REPLACE(filnamn,"\","\\").
   xmlsparfilnamn = filnamn + xmlsparfilnamn.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      dbhand = webclienttempdir.
      dbhand = REPLACE(webclienttempdir,"\","\\").
   END.
   ELSE DO:
      dbhand = REPLACE(SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv,"\","\\").
   END.
   /*korfil = korfil + ' "' + txtvar + '"' + ' "' + filnamn + '"' + ' "' + dbhand + '"'.*/
   korfil = korfil + ' "' + txtvar + '"' + ' "' + xmlsparfilnamn + '"' + ' "' + dbhand + '"'.
   /*MESSAGE korfil VIEW-AS ALERT-BOX.*/
   OS-COMMAND SILENT VALUE(korfil).      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

