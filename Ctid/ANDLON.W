&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/07/05 - 10:41 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{AVDTEMP.I}
{TIDALLT.I}
DEFINE INPUT PARAMETER lonbil AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER flerval AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{LONTILLAGG.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
{PHMT.I}
{OMRTEMPW.I}

{SOKDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE lonrakn AS INTEGER NO-UNDO.
DEFINE BUFFER tidbuff FOR tidallt.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE spantal AS DECIMAL NO-UNDO.
DEFINE VARIABLE varformat AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr egnaao lontilltemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR utsokaonr


/* Definitions for BROWSE BRW_EAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_EAONR egnaao.OMRADE egnaao.AONR ~
egnaao.DELNR egnaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_EAONR egnaao.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_EAONR egnaao
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_EAONR egnaao
&Scoped-define QUERY-STRING-BRW_EAONR FOR EACH egnaao NO-LOCK
&Scoped-define OPEN-QUERY-BRW_EAONR OPEN QUERY BRW_EAONR FOR EACH egnaao NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_EAONR egnaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_EAONR egnaao


/* Definitions for BROWSE BRW_LON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LON lontilltemp.VILART ~
lontilltemp.LONKODTEXT lontilltemp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LON lontilltemp.VILART 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LON lontilltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LON lontilltemp
&Scoped-define QUERY-STRING-BRW_LON FOR EACH lontilltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LON OPEN QUERY BRW_LON FOR EACH lontilltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LON lontilltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LON lontilltemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_LBL FILL-IN_VILART BRW_LON ~
FILL-IN_LONTILLANTAL FILL-IN-AONR FILL-IN-DELNR FILL-IN-RESMAL BTN_REG ~
BTN_AVS CMB_OMR CMB_AVD TOG_LAVD RECT-22 
&Scoped-Define DISPLAYED-OBJECTS TOG_LBL FILL-IN-DAG FILL-IN_VILART ~
FILL-IN-SORT FILL-IN_LONTILLANTAL FILL-IN-AONR FILL-IN-DELNR FILL-IN-RESMAL ~
CMB_OMR FILL-IN-TEXT FILL-IN-PKOD FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock100 DIALOG-1 
FUNCTION klock100 RETURNS DECIMAL
   ( INPUT ber60 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_AONR-3 
       MENU-ITEM m_Arbetsuppgift-3 LABEL "Arbetsuppgift" .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKAPEN 
     LABEL "Spara favorit":L 
     SIZE 14.5 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DAG AS CHARACTER FORMAT "X(3)":U 
     LABEL "Dag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "mån ","tis","ons","tor","fre","lör","sön" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOMS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Varav moms" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Resmål" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SORT AS CHARACTER FORMAT "X(2)":U 
     LABEL "Sort" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-VECKO AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Veckonummer" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONAUTO AS LOGICAL FORMAT "Auto/Manuell" INITIAL NO 
     LABEL "Auto/Man" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONS AS CHARACTER FORMAT "X(6)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONTILLAGG AS CHARACTER FORMAT "X(5)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONTILLANTAL AS DECIMAL FORMAT "->>>>>9.99" INITIAL 0 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_TEXT AS CHARACTER FORMAT "x(40)" 
     LABEL "Text" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_VILART AS CHARACTER FORMAT "X(5)" 
     LABEL "Löneart" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", 1,
"Fasta aonr", 2,
"Favorit aonr", 3
     SIZE 43.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61.63 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_LAVD AS LOGICAL INITIAL no 
     LABEL "Lunch erhållen" 
     VIEW-AS TOGGLE-BOX
     SIZE 17.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_LBL AS LOGICAL INITIAL no 
     LABEL "Personligt utlägg mall" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_EAONR FOR 
      egnaao SCROLLING.

DEFINE QUERY BRW_LON FOR 
      lontilltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 61.63 BY 12
         TITLE "Aktiva arbetsordernummer".

DEFINE BROWSE BRW_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EAONR DIALOG-1 _STRUCTURED
  QUERY BRW_EAONR NO-LOCK DISPLAY
      egnaao.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      egnaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      egnaao.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      egnaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      egnaao.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 61.63 BY 12
         TITLE "Favorit arbetsordernummer".

DEFINE BROWSE BRW_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LON DIALOG-1 _STRUCTURED
  QUERY BRW_LON NO-LOCK DISPLAY
      lontilltemp.VILART COLUMN-LABEL "Löneart" FORMAT "X(8)":U
      lontilltemp.LONKODTEXT COLUMN-LABEL "Text" FORMAT "x(256)":U
            WIDTH 40
      lontilltemp.ENHET COLUMN-LABEL "Sort" FORMAT "x(2)":U
  ENABLE
      lontilltemp.VILART
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 61.63 BY 12
         TITLE "Löneartslista".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_EAONR AT ROW 5.71 COL 42.63
     TOG_LBL AT ROW 7.71 COL 27 WIDGET-ID 2
     FILL-IN-VECKO AT ROW 6.17 COL 15.75 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 4.67 COL 15.5 COLON-ALIGNED NO-LABEL
     FILL-IN-DATUM AT ROW 6.17 COL 15.75 COLON-ALIGNED
     FILL-IN-DAG AT ROW 6.17 COL 30.63 NO-LABEL
     CMB_DAG AT ROW 6.17 COL 15.5 COLON-ALIGNED
     FILL-IN_VILART AT ROW 7.63 COL 15.5 COLON-ALIGNED
     BRW_LON AT ROW 5.42 COL 42.63
     FILL-IN-SORT AT ROW 9.96 COL 15.5 COLON-ALIGNED
     FILL-IN_LONTILLANTAL AT ROW 11.46 COL 15.5 COLON-ALIGNED
     FILL-IN-MOMS AT ROW 14.46 COL 15.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 15.96 COL 15.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 17.46 COL 15.5 COLON-ALIGNED
     BRW_AONR AT ROW 5.71 COL 42.63
     FILL-IN-RESMAL AT ROW 23.83 COL 15.5 COLON-ALIGNED
     BTN_REG AT ROW 23.83 COL 75.25
     BTN_AVS AT ROW 23.83 COL 90.25
     FILL-IN_LONS AT ROW 18.25 COL 61.5 COLON-ALIGNED
     FILL-IN_AONRS AT ROW 18.25 COL 61.5 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 18.25 COL 81 COLON-ALIGNED
     FILL-IN_TEXT AT ROW 18.25 COL 81 COLON-ALIGNED
     CMB_OMR AT ROW 3.92 COL 79.75 COLON-ALIGNED NO-LABEL
     FILL-IN_LONTILLAGG AT ROW 1.75 COL 32.5 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 1.71 COL 79.5 COLON-ALIGNED NO-LABEL
     BTN_NVE AT ROW 5.79 COL 27
     BTN_FVE AT ROW 6.67 COL 27
     FILL-IN-PKOD AT ROW 3.17 COL 15.5 COLON-ALIGNED
     FILL-IN_LONAUTO AT ROW 12.96 COL 15.5 COLON-ALIGNED
     FILL-IN-SKP AT ROW 18.25 COL 41.5 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 2.67 COL 79.75 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 4.29 COL 36.75 NO-LABEL
     BTN_SKAPEN AT ROW 15.96 COL 27
     FILL-IN_FORNAMN-2 AT ROW 3.17 COL 26.63 NO-LABEL
     TOG_LAVD AT ROW 8.75 COL 18 WIDGET-ID 8
     RECT-22 AT ROW 18.04 COL 42.63
     SPACE(0.48) SKIP(7.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera lönetillägg":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: egnaao T "?" NO-UNDO temp-db egnaao
      TABLE: lontilltemp T "?" NO-UNDO temp-db lontilltemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_EAONR 1 DIALOG-1 */
/* BROWSE-TAB BRW_LON FILL-IN_VILART DIALOG-1 */
/* BROWSE-TAB BRW_AONR FILL-IN-DELNR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

/* SETTINGS FOR BROWSE BRW_EAONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_EAONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_EAONR:POPUP-MENU IN FRAME DIALOG-1             = MENU POPUP-MENU-BRW_AONR-3:HANDLE
       BRW_EAONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

ASSIGN 
       BRW_LON:ALLOW-COLUMN-SEARCHING IN FRAME DIALOG-1 = TRUE
       BRW_LON:COLUMN-RESIZABLE IN FRAME DIALOG-1       = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_SKAPEN IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_SKAPEN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_DAG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_DAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MANAD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MOMS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MOMS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-RESMAL:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SORT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-VECKO IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VECKO:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_LONAUTO IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LONAUTO:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_LONS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LONS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_LONTILLAGG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_LONTILLAGG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_TEXT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_TEXT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_LAVD IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_LAVD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK "
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EAONR
/* Query rebuild information for BROWSE BRW_EAONR
     _TblList          = "Temp-Tables.egnaao"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.egnaao.OMRADE
"egnaao.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.egnaao.AONR
"egnaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.egnaao.DELNR
"egnaao.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.egnaao.ORT
"egnaao.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_EAONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LON
/* Query rebuild information for BROWSE BRW_LON
     _TblList          = "Temp-Tables.lontilltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.lontilltemp.VILART
"lontilltemp.VILART" "Löneart" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.lontilltemp.LONKODTEXT
"lontilltemp.LONKODTEXT" "Text" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.lontilltemp.ENHET
"lontilltemp.ENHET" "Sort" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera lönetillägg */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   musz = TRUE.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera lönetillägg */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON MOUSE-MENU-CLICK OF BRW_AONR IN FRAME DIALOG-1 /* Aktiva arbetsordernummer */
DO:
  ASSIGN
   sok1 = utsokaonr.AONR       
   sok2 = utsokaonr.DELNR
   sok4 = "".
   RUN nyupp_UI (INPUT 20).
   IF LENGTH(sok3) > 0 THEN DO:
      MESSAGE sok3 VIEW-AS ALERT-BOX TITLE "Arbetsuppgift".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON VALUE-CHANGED OF BRW_AONR IN FRAME DIALOG-1 /* Aktiva arbetsordernummer */
DO:
   IF musz = FALSE THEN DO:
      RUN fillinupdate_UI.             
   END.
   musz = FALSE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_EAONR
&Scoped-define SELF-NAME BRW_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EAONR DIALOG-1
ON MOUSE-MENU-CLICK OF BRW_EAONR IN FRAME DIALOG-1 /* Favorit arbetsordernummer */
DO:
   ASSIGN
   sok1 = egnaao.AONR       
   sok2 = egnaao.DELNR
   sok4 = "".
   RUN nyupp_UI (INPUT 20).
   IF LENGTH(sok3) > 0 THEN DO:
      MESSAGE sok3 VIEW-AS ALERT-BOX TITLE "Arbetsuppgift".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EAONR DIALOG-1
ON VALUE-CHANGED OF BRW_EAONR IN FRAME DIALOG-1 /* Favorit arbetsordernummer */
DO:
   IF musz = FALSE THEN DO:
      RUN fillinupdate_UI.             
   END.
   musz = FALSE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LON
&Scoped-define SELF-NAME BRW_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LON DIALOG-1
ON VALUE-CHANGED OF BRW_LON IN FRAME DIALOG-1 /* Löneartslista */
DO:                                
   IF AVAILABLE lontilltemp THEN DO:
      ASSIGN
      FILL-IN-SORT = lontilltemp.ENHET. 
      FILL-IN-RESMAL:HIDDEN = FALSE.
      IF FILL-IN-SORT = "MI" OR FILL-IN-SORT = "KM" THEN FILL-IN-RESMAL:LABEL = "Resmål".
      ELSE FILL-IN-RESMAL:LABEL = "Kommentar".
      IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN_LONTILLAGG = "756" THEN FILL-IN-RESMAL:LABEL = "Namn på passagerarna".
      ASSIGN
      FILL-IN_LONAUTO = FALSE
      FILL-IN_VILART = lontilltemp.VILART
      FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG.   
      DISPLAY FILL-IN-SORT FILL-IN_LONAUTO  FILL-IN_VILART WITH FRAME {&FRAME-NAME}.
      TOG_LAVD:HIDDEN = TRUE.
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF FILL-IN_VILART = "778" THEN DO:
            TOG_LAVD = FALSE.
            TOG_LAVD:HIDDEN = FALSE.
            DISPLAY TOG_LAVD WITH FRAME {&FRAME-NAME}.
         END.               
         ELSE DO:
            TOG_LAVD = FALSE.
            TOG_LAVD:HIDDEN = TRUE.            
         END.    
         
         IF  FILL-IN_VILART = "821" OR FILL-IN_VILART = "823" OR FILL-IN_VILART = "825" 
         OR FILL-IN_VILART = "831" OR FILL-IN_VILART = "832" OR FILL-IN_VILART = "812" THEN DO:
            TOG_LBL = TRUE.
            DISPLAY TOG_LBL WITH FRAME {&FRAME-NAME}.            
         END.
         ELSE DO:
            TOG_LBL = FALSE.   
            DISPLAY TOG_LBL WITH FRAME {&FRAME-NAME}.
         END.   
      END.   
      ELSE TOG_LBL = TRUE.
      IF vart = "NYA" THEN DO:
         IF lontilltemp.MOMS = TRUE THEN DO:
            FILL-IN-MOMS:HIDDEN = FALSE.
            ENABLE FILL-IN-MOMS WITH FRAME {&FRAME-NAME}.   
         END.   
         ELSE FILL-IN-MOMS:HIDDEN = TRUE.         
      END.   
      IF lontilltemp.AONROBL = ? THEN DO:
         ASSIGN
         FILL-IN-AONR = ""
         FILL-IN-DELNR = 0.
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.          
      END.
      RUN styraonr_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE DIALOG-1
ON CHOOSE OF BTN_FVE IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM - 1.   
   IF FILL-IN-DATUM = 0 THEN FILL-IN-DATUM = 1.
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         FILL-IN-DATUM = DAY(tillochmeddatum + 1).
      END.
   END.
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.     
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   FILL-IN-DAG = regdagnamn.
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE DIALOG-1
ON CHOOSE OF BTN_NVE IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.   
   FILL-IN-DATUM = FILL-IN-DATUM + 1.
   IF MONTH(extratidallt.DATUM) = 12 THEN datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).
   
   ELSE datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.   
   IF FILL-IN-DATUM > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM = DAY(datkoll).
   END.      
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   RUN REGDAG.P.
   FILL-IN-DAG = regdagnamn.
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}
   RUN btnreg_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON ENDKEY OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   musz = FALSE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKAPEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEN DIALOG-1
ON CHOOSE OF BTN_SKAPEN IN FRAME DIALOG-1 /* Spara favorit */
DO:   
  
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR =  FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 47
      soktemp.SOKCHAR[1] = FILL-IN-AONR
      soktemp.SOKINT[1] = FILL-IN-DELNR.
      {SOKANROP.I}      
      IF soktemp.SOKCHAR[2] = ? THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.                        
      END.
      ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
      ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.               
         RETURN.      
      END.      
   END.
   ELSE DO:   
      FIND FIRST egnaao WHERE egnaao.AONR = FILL-IN-AONR AND egnaao.DELNR = FILL-IN-DELNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE egnaao THEN DO:
         CREATE egnaao.
         BUFFER-COPY utsokaonr TO egnaao.      
         tthandle = TEMP-TABLE egnaao:HANDLE.
         FIND FIRST sparaladdatemp NO-ERROR.
         IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
         ASSIGN
         sparaladdatemp.GLOBANV = pkod /*Tidredovisningens aonr spara på personalkod istället för användare*/
         sparaladdatemp.BENAMNING = "AONRE" /*Benämnings sufix, i detta fall ELPAO$STOR*/
         sparaladdatemp.TABVAL = "AONRTAB" /*Tabellnamn*/
         sparaladdatemp.FALTVALAO = "AONR" /*Character field*/
         sparaladdatemp.FALTVALDEL = "DELNR" /*Integer field*/
         sparaladdatemp.FALTVALDATE = "".  /*DATE field*/
         RUN sparabrw_UI IN brwproc[3] 
         (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).
         RAD_FAST = 3. 
         RUN openbdynspec_UI IN brwproc[3].      
         FIND FIRST egnaao WHERE egnaao.AONR = FILL-IN-AONR AND egnaao.DELNR = FILL-IN-DELNR AND
         egnaao.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE egnaao THEN DO:            
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(egnaao)).
            RUN lastselectdyn_UI IN brwproc[3].            
         END.      
         ASSIGN
         BRW_AONR:HIDDEN = TRUE   
         BRW_EAONR:HIDDEN = FALSE
         CMB_OMR:HIDDEN = TRUE
         CMB_AVD:HIDDEN = TRUE
         FILL-IN-SKP:HIDDEN = TRUE.   
         ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.
         DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEN DIALOG-1
ON GO OF BTN_SKAPEN IN FRAME DIALOG-1 /* Spara favorit */
DO:
   {BORTBRWPROC.I}
   
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD DIALOG-1
ON VALUE-CHANGED OF CMB_AVD IN FRAME DIALOG-1
DO:
   CMB_AVD = INPUT CMB_AVD.   
   omravdand = 1.
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1]. 
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DAG DIALOG-1
ON LEAVE OF CMB_DAG IN FRAME DIALOG-1 /* Dag */
DO:
  CMB_DAG = INPUT CMB_DAG.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1
DO:
   CMB_OMR = INPUT CMB_OMR.            
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN sparomrade = sparomrade.
   ELSE sparomrade = CMB_OMR.           
   omravdand = 2.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   
   ASSIGN
   FILL-IN-SKP = "Sök på:"
   FILL-IN_LONS:HIDDEN = TRUE
   FILL-IN_TEXT:HIDDEN = TRUE
   CMB_OMR:HIDDEN = FALSE
   CMB_AVD:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE
   BRW_LON:HIDDEN = TRUE.
   ENABLE  FILL-IN_AONRS FILL-IN_ORTS RAD_FAST BTN_SKAPEN WITH FRAME {&FRAME-NAME}.
   ASSIGN
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE   
   FILL-IN_ORTS:HIDDEN = FALSE
   BTN_SKAPEN:HIDDEN = FALSE.   
   RUN styraonr_UI.   
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      IF FILL-IN_VILART = "227" THEN DO:      
         ASSIGN FILL-IN-AONR = "144013" FILL-IN-DELNR = 0.            
      END.         
   END.
   RUN aoladda_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
  IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      RUN hao2_UI.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.         
   IF MONTH(extratidallt.DATUM) = 12 THEN datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).   
   ELSE datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.   
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
   END.            
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX. 
      END.
   END.   
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   FILL-IN-DAG = regdagnamn.
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:
   
   IF INPUT FILL-IN-AONR = "" AND INPUT FILL-IN-DELNR = 0 THEN DO:
      ASSIGN
      FILL-IN-AONR = INPUT FILL-IN-AONR
      FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   END.    
   ELSE DO:                            
      ASSIGN
      FILL-IN-AONR = INPUT FILL-IN-AONR
      FILL-IN-DELNR = INPUT FILL-IN-DELNR.       
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 47
         soktemp.SOKCHAR[1] = FILL-IN-AONR
         soktemp.SOKINT[1] = FILL-IN-DELNR.
         {SOKANROP.I}      
         IF soktemp.SOKCHAR[2] = ? THEN DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.               
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.      
         END.
         ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
         ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.               
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.      
         END.         
      END.
      ELSE DO:                                             
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      musz = FALSE.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
   END.   
   RUN hao2_UI.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MOMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MOMS DIALOG-1
ON LEAVE OF FILL-IN-MOMS IN FRAME DIALOG-1 /* Varav moms */
DO:
  FILL-IN-MOMS = INPUT FILL-IN-MOMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-RESMAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-RESMAL DIALOG-1
ON LEAVE OF FILL-IN-RESMAL IN FRAME DIALOG-1 /* Resmål */
DO:
   FILL-IN-RESMAL = INPUT FILL-IN-RESMAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AONRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON ANY-KEY OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON LEAVE OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   IF FILL-IN_AONRS = '' THEN DO:                                      
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.  
      APPLY "ENTRY" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.           
      RETURN NO-APPLY.                                                 
   END.                              
   IF RAD_FAST = 1 OR RAD_FAST = 2 THEN DO:   
      RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AONRS).
   END.
   ELSE DO:
      RUN sokurvaldyn_UI IN brwproc[3] (INPUT "AONR", INPUT FILL-IN_AONRS).
   END.   
   RUN fillinupdate_UI.
   APPLY "ENTRY" TO FILL-IN_AONRS IN FRAME {&FRAME-NAME}.                                                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LONS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONS DIALOG-1
ON ANY-KEY OF FILL-IN_LONS IN FRAME DIALOG-1 /* Löneart */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_LONS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONS DIALOG-1
ON LEAVE OF FILL-IN_LONS IN FRAME DIALOG-1 /* Löneart */
DO:
   FILL-IN_LONS = INPUT FILL-IN_LONS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_LONS IN FRAME DIALOG-1 /* Löneart */
DO:
   FILL-IN_LONS = INPUT FILL-IN_LONS.
   IF FILL-IN_LONS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "VILART", INPUT FILL-IN_LONS).
   RUN fillinupdate_UI.
   APPLY "ENTRY" TO FILL-IN_LONS IN FRAME {&FRAME-NAME}.                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LONTILLAGG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONTILLAGG DIALOG-1
ON ENTRY OF FILL-IN_LONTILLAGG IN FRAME DIALOG-1 /* Löneart */
DO:                            
   ASSIGN
   FILL-IN-SKP = "Sök på:"
   BRW_AONR:HIDDEN = TRUE 
   BRW_EAONR:HIDDEN = TRUE 
   RAD_FAST:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE
   FILL-IN_AONRS:HIDDEN = TRUE 
   FILL-IN_ORTS:HIDDEN = TRUE
   BTN_SKAPEN:HIDDEN = TRUE.
   DISPLAY FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_LON FILL-IN_LONS FILL-IN_TEXT WITH FRAME {&FRAME-NAME}.
   FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND 
   lontilltemp.LONTILLAGG = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lontilltemp THEN DO:      
      APPLY "HOME" TO BRW_LON.
      status-ok = BRW_LON:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE DO:        
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(lontilltemp)).
      RUN lastselectdyn_UI IN brwproc[2].            
   END. 
   BRW_LON:HIDDEN = FALSE.
         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_LONTILLANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONTILLANTAL DIALOG-1
ON ENTRY OF FILL-IN_LONTILLANTAL IN FRAME DIALOG-1 /* Antal */
DO:
   varformat = FILL-IN_LONTILLANTAL:FORMAT.
   ASSIGN FILL-IN-MOMS = 0.
   FILL-IN_VILART = INPUT FILL-IN_VILART.
   FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND 
   lontilltemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lontilltemp THEN DO:
      musz = musz.
   END.
   ELSE DO:
      IF vart = "NYA" THEN DO:
         IF lontilltemp.MOMS = TRUE THEN DO:
            FILL-IN-MOMS:HIDDEN = FALSE.
            ENABLE FILL-IN-MOMS WITH FRAME {&FRAME-NAME}.   
         END.   
         ELSE FILL-IN-MOMS:HIDDEN = TRUE.         
      END.   
      FILL-IN-SORT = lontilltemp.ENHET. 
      FILL-IN-RESMAL:HIDDEN = FALSE.
      IF FILL-IN-SORT = "KM" THEN DO:
         FILL-IN_LONTILLANTAL:FORMAT = "->>>>>9".         
      END.
      IF FILL-IN-SORT = "MI" OR FILL-IN-SORT = "KM" THEN DO:
         FILL-IN-RESMAL:LABEL = "Resmål".
      END.
      ELSE FILL-IN-RESMAL:LABEL = "Kommentar".      
      IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN_LONTILLAGG = "756" THEN FILL-IN-RESMAL:LABEL = "Namn på passagerarna".
      IF lontilltemp.AONROBL = ? THEN DO:
         ASSIGN
         FILL-IN-AONR = ""
         FILL-IN-DELNR = 0.
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.          
      END.
      RUN styraonr_UI.
   END.
   DISPLAY FILL-IN-SORT WITH FRAME {&FRAME-NAME}. 
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONTILLANTAL DIALOG-1
ON LEAVE OF FILL-IN_LONTILLANTAL IN FRAME DIALOG-1 /* Antal */
DO:
   FILL-IN_LONTILLANTAL = INPUT FILL-IN_LONTILLANTAL.
   FILL-IN_LONTILLANTAL:FORMAT = varformat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORTS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON ANY-KEY OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON LEAVE OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
   IF FILL-IN_ORTS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.         
   IF RAD_FAST = 1 OR RAD_FAST = 2 THEN DO:   
      RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORTS).
   END.
   ELSE DO:
      RUN sokurvaldyn_UI IN brwproc[3] (INPUT "ORT", INPUT FILL-IN_ORTS).
   END.
   RUN fillinupdate_UI.
   APPLY "ENTRY" TO FILL-IN_ORTS IN FRAME {&FRAME-NAME}.                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEXT DIALOG-1
ON ANY-KEY OF FILL-IN_TEXT IN FRAME DIALOG-1 /* Text */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_TEXT IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEXT DIALOG-1
ON LEAVE OF FILL-IN_TEXT IN FRAME DIALOG-1 /* Text */
DO:
   FILL-IN_TEXT = INPUT FILL-IN_TEXT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEXT DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_TEXT IN FRAME DIALOG-1 /* Text */
DO:
   FILL-IN_TEXT = INPUT FILL-IN_TEXT.
   IF FILL-IN_TEXT = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_TEXT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   RUN sokurvaldyn_UI IN brwproc[2] (INPUT "LONKODTEXT", INPUT FILL-IN_TEXT).
   RUN fillinupdate_UI.
   APPLY "ENTRY" TO FILL-IN_TEXT IN FRAME {&FRAME-NAME}.                         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VILART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VILART DIALOG-1
ON ENTRY OF FILL-IN_VILART IN FRAME DIALOG-1 /* Löneart */
DO:                            
   ASSIGN
   FILL-IN-SKP = "Sök på:"
   BRW_AONR:HIDDEN = TRUE 
   BRW_EAONR:HIDDEN = TRUE 
   RAD_FAST:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE
   FILL-IN_AONRS:HIDDEN = TRUE 
   FILL-IN_ORTS:HIDDEN = TRUE
   BTN_SKAPEN:HIDDEN = TRUE.
   DISPLAY FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_LON FILL-IN_LONS FILL-IN_TEXT WITH FRAME {&FRAME-NAME}.
   FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND 
   lontilltemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE lontilltemp THEN DO:      
      APPLY "HOME" TO BRW_LON.
      status-ok = BRW_LON:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   ELSE DO:        
      ASSIGN FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG.                  
      IF vart = "NYA" THEN DO:
         IF lontilltemp.MOMS = TRUE THEN DO:
            FILL-IN-MOMS:HIDDEN = FALSE.
            ENABLE FILL-IN-MOMS WITH FRAME {&FRAME-NAME}.   
         END.   
         ELSE FILL-IN-MOMS:HIDDEN = TRUE.         
      END.   
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(lontilltemp)).
      RUN lastselectdyn_UI IN brwproc[2].  
      IF lontilltemp.AONROBL = ? THEN DO:
         ASSIGN
         FILL-IN-AONR = ""
         FILL-IN-DELNR = 0.
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}. 
      END.
      RUN styraonr_UI.
   END. 
   BRW_LON:HIDDEN = FALSE.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Arbetsuppgift-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Arbetsuppgift-3 DIALOG-1
ON CHOOSE OF MENU-ITEM m_Arbetsuppgift-3 /* Arbetsuppgift */
DO:
  ASSIGN
   sok1 = utsokaonr.AONR       
   sok2 = utsokaonr.DELNR
   sok4 = "".
   RUN nyupp_UI (INPUT 20).
   IF LENGTH(sok3) > 0 THEN DO:
      MESSAGE sok3 VIEW-AS ALERT-BOX TITLE "Arbetsuppgift".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF RAD_FAST = 1 THEN DO:
      CMB_OMR = sparomrade.
      CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade NO-ERROR.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   END.  
   IF Guru.Konstanter:globforetag = "cELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*CMB_OMR visa "område alla" för fasta aonr*/
      IF RAD_FAST = 2 THEN DO:
         ASSIGN 
         sparomrade = CMB_OMR. 
         CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.      
      END.
   END.
   RUN nycolsortprep_UI (INPUT 1).
   IF RAD_FAST = 3 THEN DO:      
      RUN openbdynspec_UI IN brwproc[3].
      ASSIGN
      BRW_AONR:HIDDEN = TRUE   
      BRW_EAONR:HIDDEN = FALSE
      CMB_OMR:HIDDEN = TRUE
      CMB_AVD:HIDDEN = TRUE
      FILL-IN-TEXT:HIDDEN = TRUE.   
      ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:         
      RUN openbdynspec_UI IN brwproc[1].
      ASSIGN
      BRW_AONR:HIDDEN = FALSE   
      BRW_EAONR:HIDDEN = TRUE
      CMB_OMR:HIDDEN = FALSE
      CMB_AVD:HIDDEN = FALSE
      FILL-IN-TEXT:HIDDEN = FALSE.   
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   END.
   IF AVAILABLE utsokaonr THEN DO:   
      status-ok = BRW_AONR:DESELECT-FOCUSED-ROW() NO-ERROR.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   ASSIGN
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   BRW_EAONR:TITLE = "Favorit " + LC(Guru.Konstanter:gaol)
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok 
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:". 
   {TILLFAST2.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   BTN_SKAPEN:TOOLTIP = "Spara valt "  + LC(Guru.Konstanter:gaok) + " som favorit ".  
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.      
       
   FIND FIRST extratidallt NO-LOCK NO-ERROR.   
   RUN anst_UI.
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").
   {ANVAVDSO.I}      
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.   
   
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   FILL-IN-MANAD = regmannamn.  
   RUN grundtid_UI. 
   IF musz = TRUE THEN DO:
      musz = TRUE.
      LEAVE MAIN-BLOCK.
   END.
   RUN enable_UI.     
   /*Egna aonr*/   
   IF Guru.Konstanter:varforetypval[24] = 1 THEN DO:         
   END.
   ELSE DO:            
      ASSIGN
      BRW_EAONR:HIDDEN = TRUE
      BTN_SKAPEN:HIDDEN = TRUE.
      status-ok = RAD_FAST:DELETE("Favorit" + " " + LC(Guru.Konstanter:gaok)).            
   END.
   ASSIGN    
   CMB_OMR:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE
   FILL-IN-RESMAL:HIDDEN = FALSE   
   FILL-IN-SKP = "Sök på:".   
   DISPLAY FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_LON FILL-IN_LONS FILL-IN_TEXT WITH FRAME {&FRAME-NAME}.
   IF FILL-IN-SORT = "MI" OR FILL-IN-SORT = "KM" THEN FILL-IN-RESMAL:LABEL = "Resmål".
   ELSE FILL-IN-RESMAL:LABEL = "Kommentar".   
   IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN_LONTILLAGG = "756" THEN FILL-IN-RESMAL:LABEL = "Namn på passagerarna".
   IF vart = "AND" OR vart = "ANN" THEN DISABLE CMB_DAG WITH FRAME {&FRAME-NAME}.
   IF vart = "NYA" THEN DO:   
       FILL-IN-RESMAL = "".
       DISPLAY FILL-IN-RESMAL WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN.
   ELSE DO:
      TOG_LBL:HIDDEN IN FRAME {&FRAME-NAME}= TRUE.
      TOG_LAVD:HIDDEN IN FRAME {&FRAME-NAME}= TRUE.
   END.   
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:         
      TOG_LAVD = FALSE.
      TOG_LAVD:HIDDEN = TRUE.
      IF FILL-IN_VILART = "778" THEN DO:
         TOG_LAVD:HIDDEN = FALSE.
         DISPLAY TOG_LAVD WITH FRAME {&FRAME-NAME}.
      END.                                 
   END.
   IF lonbil = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:   
         tempvar = "lontilltemp.KOD = " + """" + ansttemp.KOD + """" + " AND SUBSTRING(lontilltemp.TYPKOD,1,3) NE " + '"' + "BIL" + '"'.           
      END.
      ELSE DO:               
         tempvar = "lontilltemp.KOD = " + """" + ansttemp.KOD + """".      
      END.
   END.
   ELSE IF lonbil = 2 THEN DO:         
      tempvar = "lontilltemp.KOD = " + """" + ansttemp.KOD + """" + " AND SUBSTRING(lontilltemp.TYPKOD,1,3) = " + '"' + "BIL" + '"'.      
      ASSIGN
      FRAME DIALOG-1:TITLE  = "Registrera bilar"     
      lontilltemp.VILART:LABEL IN BROWSE BRW_LON = "Bil"   
      FILL-IN_VILART:LABEL = "Bil"
      FILL-IN_LONS:LABEL = "Bil".     
   END. 
   RUN setcolsortvar_UI IN brwproc[2] (INPUT tempvar).
   RUN openbdynspec_UI IN brwproc[2].
   IF flerval > 1 THEN DO:
      ASSIGN
      BTN_FVE:HIDDEN = TRUE
      BTN_NVE:HIDDEN = TRUE
      FILL-IN-DAG:HIDDEN = TRUE
      FILL-IN-DATUM:HIDDEN = TRUE
      FILL-IN-AONR :HIDDEN = TRUE
      FILL-IN-DELNR:HIDDEN = TRUE      
      FILL-IN_LONTILLANTAL:HIDDEN = TRUE.                  
   END.
   
   {FRMSIZED.I}      
   {musarrow.i}  
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.

RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   utsokaonr.OMRADE:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   lontilltemp.VILART:READ-ONLY IN BROWSE BRW_LON = TRUE.
   egnaao.OMRADE:READ-ONLY IN BROWSE BRW_EAONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE).         
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_LON:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_EAONR:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   RUN sattindex_UI IN brwproc[3] (INPUT "OMRADE").
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anst_UI DIALOG-1 
PROCEDURE anst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN LONKODHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   ELSE DO:
      RUN LONKODHMT.P  
      (INPUT personaltemp.PERSONALKOD,OUTPUT TABLE lontilltemp,OUTPUT TABLE lonkorttemp).
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      /*tjr lontilltemp innehåller inte lönearter för tjänsteresa  lontilltemp2 har lönearter för tjänsteresa
      Visa dessa lönearter enbart om man går på ändra med en tjänsteresalöneart*/
      FOR EACH lontilltemp:
         IF lontilltemp.VILART = "770" OR  lontilltemp.VILART = "771" OR lontilltemp.VILART = "772" OR
         lontilltemp.VILART = "773" OR lontilltemp.VILART = "774" OR  lontilltemp.VILART = "775" OR 
         lontilltemp.VILART = "776" OR lontilltemp.VILART = "777" OR lontilltemp.VILART = "838" OR lontilltemp.VILART = "839" OR lontilltemp.VILART = "842" OR
         lontilltemp.VILART = "843" OR lontilltemp.VILART = "844" OR lontilltemp.VILART = "880" OR lontilltemp.VILART = "881" OR lontilltemp.VILART = "260" OR
         lontilltemp.VILART = "515" or  lontilltemp.VILART = "779" THEN DO:
            ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
      END.
      IF Guru.Konstanter:globforetag = "CSNAT"  THEN DO:
         /*LÅT MÅLTIDSAVDRAG VARA KVAR TILL ENDAGS  släckt 20180830*/
         FOR EACH lontilltemp:
            IF lontilltemp.VILART = "843" THEN ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "   ".
         END.
      END.      
      IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         /*Släck fyllnadstid och Arbete under veckovila som ger flex */
         /*släckte lart 190 av misstag lade in VILART = 191 för att slippa lägga ny version*/
         FOR EACH lontilltemp:
            IF lontilltemp.VILART = "200" OR lontilltemp.VILART = "215" /*OR lontilltemp.VILART = "190"*/  THEN ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
      END.   
      EMPTY TEMP-TABLE lontilltemp2 NO-ERROR. 
      FOR EACH lontilltemp NO-LOCK:
         CREATE lontilltemp2.
         BUFFER-COPY lontilltemp TO lontilltemp2.
      END.
      FOR EACH lontilltemp WHERE SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR" :
         DELETE lontilltemp.
      END.
   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*tjr lontilltemp innehåller inte lönearter för tjänsteresa  lontilltemp2 har lönearter för tjänsteresa
      Visa dessa lönearter enbart om man går på ändra med en tjänsteresalöneart*/
      FOR EACH lontilltemp:
         IF lontilltemp.VILART = "FÄRL" OR  lontilltemp.VILART = "FÄRH" OR lontilltemp.VILART = "REHA" OR
         lontilltemp.VILART = "REHE" OR lontilltemp.VILART = "KLUM" OR
         lontilltemp.VILART = "UTLT" OR lontilltemp.VILART = "TFHE" OR lontilltemp.VILART = "TLHA" OR lontilltemp.VILART = "TLHE" OR
         lontilltemp.VILART = "TFHA" OR
         lontilltemp.VILART = "MUTL" OR lontilltemp.VILART = "EN10" OR lontilltemp.VILART = "END6"  OR
         lontilltemp.VILART = "020" /*OR lontilltemp.VILART = "212" OR lontilltemp.VILART = "230"*/ OR lontilltemp.VILART = "MOMS"   THEN DO:
            ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
         /*NYTT LÖNESYSTEM 20111109*/
         IF lontilltemp.VILART = "770" OR  lontilltemp.VILART = "771" OR lontilltemp.VILART = "790" OR
         lontilltemp.VILART = "791" OR lontilltemp.VILART = "881" OR
         lontilltemp.VILART = "845" OR lontilltemp.VILART = "842" OR lontilltemp.VILART = "848" OR lontilltemp.VILART = "843" OR
         lontilltemp.VILART = "849" OR lontilltemp.VILART = "879" OR lontilltemp.VILART = "847" OR
         lontilltemp.VILART = "844" OR lontilltemp.VILART = "780" OR lontilltemp.VILART = "781"  OR
         lontilltemp.VILART = "010" /*OR lontilltemp.VILART = "212" OR lontilltemp.VILART = "230"*/ OR lontilltemp.VILART = "839"   THEN DO:
            ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
         /*NYTT LÖNESYSTEM 20130101*/
         IF lontilltemp.VILART = "740" OR  lontilltemp.VILART = "739" OR lontilltemp.VILART = "711" OR
         lontilltemp.VILART = "710" OR lontilltemp.VILART = "894" OR
         lontilltemp.VILART = "845" OR lontilltemp.VILART = "842" OR lontilltemp.VILART = "848" OR lontilltemp.VILART = "843" OR
         lontilltemp.VILART = "849" OR lontilltemp.VILART = "784" OR lontilltemp.VILART = "785" OR
         lontilltemp.VILART = "844" OR lontilltemp.VILART = "780" OR lontilltemp.VILART = "781"  OR
         lontilltemp.VILART = "010" /*OR lontilltemp.VILART = "212" OR lontilltemp.VILART = "230"*/ OR lontilltemp.VILART = "839"   THEN DO:
            ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
      END.
      EMPTY TEMP-TABLE lontilltemp2 NO-ERROR. 
      FOR EACH lontilltemp NO-LOCK:
         CREATE lontilltemp2.
         BUFFER-COPY lontilltemp TO lontilltemp2.
      END.
      FOR EACH lontilltemp WHERE SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR" :
         DELETE lontilltemp.
      END.
   END.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      FOR EACH lontilltemp:
         IF lontilltemp.VILART = "080" OR  lontilltemp.VILART = "081"
         OR lontilltemp.VILART = "440" OR  lontilltemp.VILART = "441" OR lontilltemp.VILART = "560" OR
         lontilltemp.VILART = "881" OR lontilltemp.VILART = "882" OR   lontilltemp.VILART = "861" OR 
         lontilltemp.VILART = "841" OR lontilltemp.VILART = "843" OR  lontilltemp.VILART = "FLMI" OR 
         lontilltemp.VILART = "FRMI" OR lontilltemp.VILART = "FRLU" OR lontilltemp.VILART = "FRUK" OR
         lontilltemp.VILART = "MIDD" OR lontilltemp.VILART = "KFRU" OR lontilltemp.VILART = "KMID" OR
         lontilltemp.VILART = "KLUN" OR lontilltemp.VILART = "LUMI" OR lontilltemp.VILART = "LUNC" OR lontilltemp.VILART = "792"  THEN DO:
            ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
      END.
      EMPTY TEMP-TABLE lontilltemp2 NO-ERROR. 
      FOR EACH lontilltemp NO-LOCK:
         CREATE lontilltemp2.
         BUFFER-COPY lontilltemp TO lontilltemp2.
      END.
      FOR EACH lontilltemp WHERE SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR" :
         DELETE lontilltemp.
      END.
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      /*tjr lontilltemp innehåller inte lönearter för tjänsteresa  lontilltemp2 har lönearter för tjänsteresa
      Visa dessa lönearter enbart om man går på ändra med en tjänsteresalöneart*/
      FOR EACH lontilltemp:
         IF lontilltemp.VILART = "310" OR  lontilltemp.VILART = "311" OR lontilltemp.VILART = "312" OR
         lontilltemp.VILART = "770" OR lontilltemp.VILART = "771" OR  lontilltemp.VILART = "837" OR  lontilltemp.VILART = "838" OR 
         lontilltemp.VILART = "844" OR lontilltemp.VILART = "845"  THEN DO:
            ASSIGN SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR".
         END.
      END.
      EMPTY TEMP-TABLE lontilltemp2 NO-ERROR. 
      FOR EACH lontilltemp NO-LOCK:
         CREATE lontilltemp2.
         BUFFER-COPY lontilltemp TO lontilltemp2.
      END.
      FOR EACH lontilltemp WHERE SUBSTRING(lontilltemp.TYPKOD,4,3) = "TJR" :
         DELETE lontilltemp.
      END.
   END.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoladda_UI DIALOG-1 
PROCEDURE aoladda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/            
 
   ASSIGN
   musz = FALSE
   FILL-IN-SKP = "Sök på:"
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   CMB_AVD:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE.       
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.   
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "cELPA" )  AND Guru.Konstanter:globallao = TRUE THEN DO:
      IF AVAILABLE utsokaonr THEN DO:
         aonrrec = RECID(utsokaonr).
         IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
         IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.         
      END.
      sparomrade = Guru.Konstanter:gomrk + " : alla".
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.            
   END.   
   ELSE DO:
      IF AVAILABLE utsokaonr THEN DO:                  
         IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
            USE-INDEX OMR NO-LOCK NO-ERROR.           
            aonrrec = RECID(utsokaonr).
            IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
            IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.            
            ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
            IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
               CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            END.
         END.
         ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
            USE-INDEX OMR NO-LOCK NO-ERROR.           
            aonrrec = RECID(utsokaonr).
            IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
            IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
            /*RAD_FAST = utsokaonr.FASTAAONR.*/
            ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
            IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
               CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            END.
         END.
         ELSE DO:        
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE omrtemp THEN DO:
               IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
                   sparomrade = Guru.Konstanter:gomrk + " : alla".
               END.
               ELSE DO:
                  IF sparomrade = "" THEN DO:
                     FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
                     USE-INDEX OMR NO-LOCK NO-ERROR.
                     IF NOT AVAILABLE omrtemp THEN DO:
                        FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
                     END.
                     sparomrade = omrtemp.NAMN.
                  END.
               END.
               ASSIGN 
               CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
               CMB_OMR = INPUT CMB_OMR.
            END.
            ELSE DO:         
               IF sparomrade = "" THEN DO:
                  FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
                  USE-INDEX OMR NO-LOCK NO-ERROR.
                  IF NOT AVAILABLE omrtemp THEN DO:
                     FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
                  END.
                  sparomrade = omrtemp.NAMN.
               END.
               ELSE sparomrade = omrtemp.NAMN.
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
               USE-INDEX OMR NO-LOCK NO-ERROR.           
               ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
               IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
                  CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
               END.
            END.
            aonrrec = RECID(utsokaonr).
            IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
            IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.            
         END.
      END.
      ELSE DO:
         IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
            sparomrade = Guru.Konstanter:gomrk + " : alla".
            ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
         END.
         ELSE DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF NOT AVAILABLE omrtemp THEN DO:
               FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
            END.
            sparomrade = omrtemp.NAMN.
            CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
            IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
               CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            END.
            CMB_OMR = INPUT CMB_OMR.
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
         END.
         aonrrec = 0.
         RAD_FAST = 1.         
      END.           
   END.
   IF Guru.Konstanter:varforetypval[24] = 1 THEN DO:   
      IF FILL-IN-AONR = ""  THEN DO:
         FIND FIRST egnaao NO-LOCK NO-ERROR.                  
      END.            
      ELSE DO:         
         FIND FIRST egnaao WHERE egnaao.AONR = FILL-IN-AONR AND egnaao.DELNR = FILL-IN-DELNR AND
         egnaao.AONRAVDATUM = 01/01/1991 NO-LOCK NO-ERROR.         
      END.
      IF AVAILABLE egnaao THEN DO:      
         ASSIGN RAD_FAST = 3.
      END.
   END.   
   DISPLAY RAD_FAST FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:varforetypval[24] = 1 AND RAD_FAST = 3 THEN DO:         
      ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.      
      RUN openbdynspec_UI IN brwproc[3].      
      IF FILL-IN-AONR NE "" THEN DO:
         FIND FIRST egnaao WHERE egnaao.AONR = FILL-IN-AONR AND egnaao.DELNR = FILL-IN-DELNR AND
         egnaao.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE egnaao THEN DO:            
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(egnaao)).
            RUN lastselectdyn_UI IN brwproc[3].            
         END.
      END.      
      ASSIGN
      BRW_EAONR:HIDDEN = FALSE
      BRW_AONR:HIDDEN = TRUE
      CMB_OMR:HIDDEN = TRUE
      CMB_AVD:HIDDEN = TRUE
      FILL-IN-TEXT:HIDDEN = TRUE.       
   END.
   ELSE DO:   
      ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      omravdand = 2.
      RUN nycolsortprep_UI (INPUT 1).
      RUN openbdynspec_UI IN brwproc[1].
      IF FILL-IN-AONR NE "" THEN DO:
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
         utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
      END.      
      ASSIGN
      BRW_AONR:HIDDEN = FALSE
      CMB_OMR:HIDDEN = FALSE
      CMB_AVD:HIDDEN = FALSE
      FILL-IN-TEXT:HIDDEN = FALSE.       
   END.
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST BTN_SKAPEN WITH FRAME {&FRAME-NAME}.
   ASSIGN   
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE
   BTN_SKAPEN:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg2_UI DIALOG-1 
PROCEDURE btnreg2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/

 DEFINE OUTPUT PARAMETER returvar AS LOGICAL NO-UNDO.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.            
      IF FILL-IN_LONTILLAGG = "833" THEN DO:      
         IF personaltemp.BEREDSKAPSAVTAL = "I" THEN DO:
            MESSAGE 
               FILL-IN_VILART " används bara vid beredskap. "
               VIEW-AS ALERT-BOX.
               returvar = TRUE.
               RETURN.
         END.
         ELSE IF personaltemp.BEREDSKAPSAVTAL BEGINS "P" THEN DO:
            IF FILL-IN-AONR = "241" AND FILL-IN-DELNR = 0 THEN musz = musz.
            ELSE DO:             
               MESSAGE 
               "Vid lönetillägg" FILL-IN_VILART " skall " + LC(Guru.Konstanter:gaol) + "241 000 anges." 
               VIEW-AS ALERT-BOX.
               returvar = TRUE.
               RETURN.
            END.              
         END.            
         ELSE IF personaltemp.BEREDSKAPSAVTAL BEGINS "F" THEN DO:                         
            IF FILL-IN-AONR = "245" AND FILL-IN-DELNR = 0 THEN musz = musz.
            ELSE DO:             
               MESSAGE 
               "Vid lönetillägg" FILL-IN_VILART " skall " + LC(Guru.Konstanter:gaol) + "245 000 anges." 
               VIEW-AS ALERT-BOX.
               returvar = TRUE.
               RETURN.
            END.              
         END.
         ELSE DO:
            IF FILL-IN-AONR = "100293"  AND FILL-IN-DELNR = 0 THEN musz = musz.                               
            ELSE DO:             
               MESSAGE 
               "Vid lönetillägg" FILL-IN_VILART " skall " + LC(Guru.Konstanter:gaol) + "100293 000 anges." 
               VIEW-AS ALERT-BOX.
               returvar = TRUE.
               RETURN.            
            END.            
         END.
      END.                  
      IF FILL-IN_LONTILLAGG = "350" OR FILL-IN_LONTILLAGG = "351" THEN DO:      
         IF personaltemp.BEREDSKAPSAVTAL BEGINS "P" THEN musz = musz.
         ELSE DO:
            MESSAGE 
            "Lart" FILL-IN_VILART " används bara för dem med beredskap produktion." 
            VIEW-AS ALERT-BOX.
            returvar = TRUE.
            RETURN.
         END.
            
      END.         
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      IF FILL-IN_LONTILLAGG = "227" THEN DO:      
         IF FILL-IN-AONR = "144013"  AND FILL-IN-DELNR = 0 THEN musz = musz.
         ELSE DO:
            MESSAGE 
            "Vid lönetillägg" FILL-IN_VILART " skall " + LC(Guru.Konstanter:gaol) + "144013 000 anges." 
            VIEW-AS ALERT-BOX.
            returvar = TRUE.
            RETURN.               
         END.                        
      END.         
   END.           
   IF  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
      IF AVAILABLE utsokaonr THEN DO:            
         IF FILL-IN_LONTILLANTAL > 0 AND utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:
            IF FILL-IN_LONTILLAGG  = "5089" THEN musz = musz.
            ELSE DO:               
               MESSAGE "Lönetillägg kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
               returvar = TRUE.
               RETURN.                  
            END.
         END.
      END.
   END.         
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      IF AVAILABLE utsokaonr THEN DO:        
         IF lonbil = 2 THEN DO:            
            IF FILL-IN_LONTILLANTAL > 0 AND utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:
               MESSAGE "Lönetillägg kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
               returvar = TRUE.
               RETURN.                  
            END.
         END.
      END.
   END.         
   IF  Guru.Konstanter:globforetag = "LULE" THEN DO:
      IF AVAILABLE utsokaonr THEN DO:            
         IF FILL-IN_LONTILLANTAL > 0 AND utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:
            IF FILL-IN_LONTILLAGG  = "851" OR FILL-IN_LONTILLAGG  = "852" THEN DO:               
               MESSAGE "Lönetillägg kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
               returvar = TRUE.
               RETURN.                  
            END.
         END.
      END.
   END.         
      
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:
      IF FILL-IN_LONTILLAGG = "778"  THEN DO:          
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Ange resmål och avstånd till resmål. Resmålet måste vara på större avstånd än 20 km från stationeringsorten. "  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.        
         END.
      END.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      IF FILL-IN_LONTILLAGG = "8305"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Resmål är obligatorisk för kilometerersättning"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.        
         END.
      END.
      IF FILL-IN_LONTILLAGG = "041"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.        
         END.
      END.
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF  FILL-IN_VILART = "821" OR FILL-IN_VILART = "823" OR FILL-IN_VILART = "825" 
         OR FILL-IN_VILART = "831" OR FILL-IN_VILART = "832" OR FILL-IN_VILART = "812" THEN DO:
            IF FILL-IN-RESMAL = "" THEN DO:
               MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
               returvar = TRUE.
               RETURN.        
            END.
         END.   
      END.
      IF FILL-IN_LONTILLAGG = "4506" OR FILL-IN_LONTILLAGG = "4574" OR FILL-IN_LONTILLAGG = "0237" THEN DO: 
         IF ( FILL-IN_LONTILLANTAL - TRUNCATE(FILL-IN_LONTILLANTAL,0)) = 0 THEN musz = musz.
         ELSE IF ( FILL-IN_LONTILLANTAL - TRUNCATE(FILL-IN_LONTILLANTAL,0)) = 0.30 THEN musz = musz.
         ELSE DO:            
            MESSAGE "Endast jämna halvtimmar kan läggas in med denna löneart"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.          
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      IF FILL-IN_LONTILLAGG = "801"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Resmål är obligatorisk för kilometerersättning"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.         
   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF FILL-IN_LONTILLAGG = "753"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Resmål är obligatorisk för kilometerersättning"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.         
      IF FILL-IN_LONTILLAGG = "756"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Ange namn på passagerare."  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.         
      IF FILL-IN_LONTILLAGG = "764"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.               

   END.   
   IF FILL-IN_LONTILLANTAL = 0 THEN DO:
      MESSAGE "Vill du verkligen registrera 0 i antal?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.        
      IF val = FALSE THEN DO:
         returvar = TRUE.
         RETURN.
      END.
   END.
   
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:   
      IF FILL-IN_LONTILLAGG = "730"  THEN DO: 
         IF FILL-IN-RESMAL = "" THEN DO:
            MESSAGE "Resmål är obligatorisk för milersättning"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.         
      IF vart = "NYA" AND FILL-IN_VILART = "791" THEN DO:
         IF FILL-IN-MOMS = 0 THEN DO:
            MESSAGE "Har du glömt att fylla i momsen?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val6 AS LOGICAL.        
            IF val6 = TRUE THEN DO:
               returvar = TRUE.
               RETURN.
            END.
          
         END.
         MESSAGE "Obs! Kvitton måste redovisas till lönekontoret före den 10:e påföljande månad " SKIP
                 "med blanketten som finns på Intranätet under Personal / Mallar / Kvittoredovisning" VIEW-AS ALERT-BOX.          
         
      END.

   END.
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:   
      IF vart = "NYA" AND FILL-IN_LONTILLAGG = "764" THEN DO:  /*VILART = 825*/
         IF FILL-IN-MOMS = 0 THEN DO:
            MESSAGE "Har du glömt att fylla i momsen?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val7 AS LOGICAL.        
            IF val7 = TRUE THEN DO:
               returvar = TRUE.
               RETURN.
            END.
         END.       
         IF FILL-IN-MOMS > FILL-IN_LONTILLANTAL THEN DO:
            MESSAGE "Momsen kan inte vara högre än summan"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.
         END.   
         MESSAGE "Obs! Kvitton måste redovisas till lönekontoret före den 10:e påföljande månad " SKIP
                 "med blanketten som finns på Intranätet under Personal / Mallar / Kvittoredovisning" VIEW-AS ALERT-BOX.          

      END.      
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
               
      IF Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF FILL-IN_LONTILLAGG = "303" THEN DO:         
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 103
            soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD
            soktemp.SOKCHAR[2] = FILL-IN_VILART
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}                           
            IF vart = "NYA" THEN DO:                                             
               IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL ) > 31 THEN DO:
                  MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 31 gånger per månad. Lönearten finns redan registrerad med " STRING(soktemp.SOKINT[1])  " st denna månad." VIEW-AS ALERT-BOX.      
                  returvar = TRUE.
                  RETURN.                  
               END.                              
            END.
         END.            
      END.
         
      
      IF FILL-IN-SORT = "ST" AND FILL-IN_LONTILLANTAL > 10  THEN DO:
         IF Guru.Konstanter:globforetag = "MISV" AND FILL-IN_LONTILLAGG = "303" THEN.
         ELSE DO:            
            MESSAGE "Enheten på denna löneart är STYCK. Är det riktigt att du vill registrera " + STRING(FILL-IN_LONTILLANTAL) + " styck?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.        
            IF val5 = FALSE THEN DO:
               returvar = TRUE.
               RETURN.           
            END.
         END.            
      END.
      IF FILL-IN_LONTILLAGG = "6990" AND FILL-IN_LONTILLANTAL > 1000 THEN DO:
         MESSAGE "Är det riktigt att du vill registrera ett utlägg större än 1000 kr?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.        
         IF val2 = FALSE THEN DO:
            returvar = TRUE.
            RETURN.           
         END.
      END.         
      IF FILL-IN_LONTILLAGG = "5897" AND FILL-IN_LONTILLANTAL > 1 THEN DO:
         MESSAGE "Är det riktigt att du vill registrera " + STRING(FILL-IN_LONTILLANTAL) + " utryckningsersättningar lösta hemifrån (500kr)?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.        
         IF val3 = FALSE THEN DO:
            returvar = TRUE.
            RETURN.           
         END.
      END.         
      IF FILL-IN_LONTILLAGG = "5898" AND FILL-IN_LONTILLANTAL > 1 THEN DO:
         MESSAGE "Är det riktigt att du vill registrera " + STRING(FILL-IN_LONTILLANTAL) + " utryckningsersättningar ( 1500kr)?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.        
         IF val4 = FALSE THEN DO:
            returvar = TRUE.
            RETURN.           
         END.
      END.         
      IF FILL-IN_LONTILLAGG = "0116"  THEN DO:         
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 91
         soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD
         soktemp.SOKCHAR[2] = FILL-IN_VILART
         soktemp.SOKDATE[1] = regdatum.
         {SOKANROP.I}      
         /*Elnät st som ändras till kr i lönekörning . SEAB kr*/
         IF  soktemp.SOKCHAR[3] = "ST" THEN DO:            
            IF vart = "NYA" THEN DO:         
               IF FILL-IN_LONTILLAGG = "0116" THEN DO:         
                  IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL )  > 2 THEN DO:
                     MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 2 gånger per månad. Lönearten finns redan registrerad " soktemp.SOKINT[1]  " gånger denna månad." VIEW-AS ALERT-BOX.      
                     returvar = TRUE.
                     RETURN.               
                  END.
               END.
               
            END.
            ELSE DO:
               IF FILL-IN_LONTILLAGG = "0116" THEN DO:         
                  IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL - spantal )  > 2 THEN DO:
                     MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 2 gånger per månad. Lönearten finns redan registrerad " soktemp.SOKINT[1]  " gånger denna månad." VIEW-AS ALERT-BOX.      
                     returvar = TRUE.
                     RETURN.                  
                  END.
               END.                  
            END.
         END.
      END.
      IF FILL-IN_LONTILLAGG = "0117" THEN DO:         
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 93
         soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD
         soktemp.SOKCHAR[2] = FILL-IN_VILART
         soktemp.SOKDATE[1] = regdatum.
         {SOKANROP.I}      
         /*Elnät st som ändras till kr i lönekörning . SEAB kr*/         
         IF  soktemp.SOKCHAR[3] = "ST" THEN DO:             
            IF FILL-IN_LONTILLANTAL > 1 THEN DO:
               MESSAGE "Antal kan inte vara mer än ett  för Löneart " + FILL-IN_VILART VIEW-AS ALERT-BOX.      
               returvar = TRUE.
               RETURN.            
            END.
            IF vart = "NYA" THEN DO:                                             
               IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL ) > 4 THEN DO:
                  MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 4 gånger per månad. Lönearten finns redan registrerad på " soktemp.SOKCHAR[4]  "  denna månad." VIEW-AS ALERT-BOX.      
                  returvar = TRUE.
                  RETURN.                  
               END.                  
            END.
            ELSE DO:                                 
               IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL - spantal ) > 4 THEN DO:
                  MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 4 gånger per månad. Lönearten finns redan registrerad på" soktemp.SOKCHAR[4]  " denna månad." VIEW-AS ALERT-BOX.      
                  returvar = TRUE.
                  RETURN.                  
               END.                  
            END.
         END.         
      END.         
      IF FILL-IN_LONTILLAGG = "5896" THEN DO:         
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 96
         soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD            
         soktemp.SOKDATE[1] = regdatum.
         {SOKANROP.I}                             
         IF FILL-IN_LONTILLANTAL > 1 THEN DO:
            MESSAGE "Antal kan inte vara mer än ett  för Löneart " + FILL-IN_VILART VIEW-AS ALERT-BOX.      
            returvar = TRUE.
            RETURN.            
         END.
         IF vart = "NYA" THEN DO:                                             
            IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL ) > 1 THEN DO:
               MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 1 gång per dag." VIEW-AS ALERT-BOX.      
               returvar = TRUE.
               RETURN.                  
            END.                  
         END.
         ELSE DO:                                 
            IF ( soktemp.SOKINT[1] + FILL-IN_LONTILLANTAL - spantal ) > 1 THEN DO:
               MESSAGE "Löneart " + FILL-IN_VILART + " får inte registreras mer än 1 gång per dag." VIEW-AS ALERT-BOX.      
               returvar = TRUE.
               RETURN.                  
            END.                  
         END.
         
      END.         
      IF FILL-IN_LONTILLAGG = "4372"  THEN DO:         
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 94
         soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD            
         soktemp.SOKDATE[1] = regdatum.
         {SOKANROP.I}                           
         IF vart = "NYA" THEN DO:                        
            IF ( soktemp.SOKDEC[1] + klock100(FILL-IN_LONTILLANTAL))  > 8.25 THEN DO:
               MESSAGE "Arbete under veckovila Löneart " + FILL-IN_VILART + " utges högst för en full arbetsdag." VIEW-AS ALERT-BOX.      
               returvar = TRUE.
               RETURN.               
            END.                              
         END.
         ELSE DO:               
            IF ( soktemp.SOKDEC[1] + klock100(FILL-IN_LONTILLANTAL) - klock100(spantal))  > 8.25 THEN DO:
               MESSAGE "Arbete under veckovila Löneart " + FILL-IN_VILART + " utges högst för en full arbetsdag." VIEW-AS ALERT-BOX.      
               returvar = TRUE.
               RETURN.                  
            END.               
         END.            
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 95
         soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD            
         soktemp.SOKDATE[1] = regdatum.
         {SOKANROP.I}      
         IF soktemp.SOKLOG[1] = FALSE THEN DO:
            MESSAGE "Arbete under veckovila Löneart " + FILL-IN_VILART + " får bara läggas in i samband med beredskap.Registrera beredskapen först" VIEW-AS ALERT-BOX.      
            returvar = TRUE.
            RETURN.               
         END.                              
         IF SUBSTRING(STRING(FILL-IN_LONTILLANTAL,"99999.99"),LENGTH(STRING(FILL-IN_LONTILLANTAL,"99999.99")) - 1,2) = "30" THEN musz = musz.
         ELSE IF SUBSTRING(STRING(FILL-IN_LONTILLANTAL,"99999.99"),LENGTH(STRING(FILL-IN_LONTILLANTAL,"99999.99")) - 1,2) = "00" THEN musz = musz.
         ELSE DO:               
            MESSAGE "Arbete under veckovila Löneart " + FILL-IN_VILART + " skall anges jämna halvtimmar.Tex: 1.30"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.      
      IF FILL-IN_LONTILLAGG = "4506" OR FILL-IN_LONTILLAGG = "4574" OR FILL-IN_LONTILLAGG = "220" THEN DO:      
         MESSAGE "Är det riktigt att du vill registrera " + STRING(FILL-IN_LONTILLANTAL) + " timmar " SKIP  
         "av löneart " + FILL-IN_VILART + " " + lontilltemp.LONKODTEXT + " ?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val8 AS LOGICAL.        
         IF val8 = FALSE THEN DO:
            returvar = TRUE.
            RETURN.           
         END.
      END.      
   END.
   IF Guru.Konstanter:globforetag = "GRAN"  
    OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF FILL-IN_VILART = "261" OR FILL-IN_VILART = "262" THEN DO:
         IF SUBSTRING(STRING(FILL-IN_LONTILLANTAL,"99999.99"),LENGTH(STRING(FILL-IN_LONTILLANTAL,"99999.99")) - 1,2) = "30" THEN musz = musz.
         ELSE IF SUBSTRING(STRING(FILL-IN_LONTILLANTAL,"99999.99"),LENGTH(STRING(FILL-IN_LONTILLANTAL,"99999.99")) - 1,2) = "00" THEN musz = musz.
         ELSE DO:               
            MESSAGE "Restid skall anges jämna halvtimmar.Tex: 1.30"  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
         IF FILL-IN_LONTILLANTAL > 16 THEN DO:
            MESSAGE "Restid mer än 16 timmar på en dag är inte rimligt "  VIEW-AS ALERT-BOX. 
            returvar = TRUE.
            RETURN.               
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "GRAN"   OR 
      Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "CGKAL"  THEN DO:
      musz = FALSE.
      IF FILL-IN_LONTILLANTAL > 0 THEN DO:         
         IF FILL-IN_VILART = "720" OR FILL-IN_VILART = "721" OR FILL-IN_VILART = "722"
         OR FILL-IN_VILART = "726" OR FILL-IN_VILART = "727" OR FILL-IN_VILART = "728" OR FILL-IN_VILART = "729" THEN DO:           
            FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
            tidallt.DATUM = regdatum AND tidallt.TRAKTKOD NE "" NO-LOCK NO-ERROR.
            IF AVAILABLE tidallt THEN musz = TRUE.  
            ELSE DO:
               FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
               tidallt.DATUM = regdatum AND tidallt.LONTILLAGG =  "710" NO-LOCK NO-ERROR.
               IF AVAILABLE tidallt THEN musz = TRUE.  
               ELSE DO:
                  FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
                  tidallt.DATUM = regdatum AND tidallt.LONTILLAGG =  "711" NO-LOCK NO-ERROR.
                  IF AVAILABLE tidallt THEN musz = TRUE.  
                  ELSE DO:
                     FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
                     tidallt.DATUM = regdatum AND tidallt.LONTILLAGG =  "717" NO-LOCK NO-ERROR.
                     IF AVAILABLE tidallt THEN musz = TRUE.  
                  END.
               END.
            END.
            IF musz = TRUE THEN musz = FALSE.
            ELSE DO:            
               MESSAGE "Måltidsavdrag skall bara registreras om traktamente är registrerat"  VIEW-AS ALERT-BOX. 
               returvar = TRUE.
               RETURN.
            
            END.
         END.
      END.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg_UI DIALOG-1 
PROCEDURE btnreg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}         
   DEBUGGER:SET-BREAK().
   ASSIGN FILL-IN_VILART = INPUT FRAME {&FRAME-NAME} FILL-IN_VILART .
   ASSIGN TOG_LBL = INPUT FRAME {&FRAME-NAME} TOG_LBL .
   ASSIGN TOG_LAVD = INPUT FRAME {&FRAME-NAME} TOG_LAVD .
   FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND 
   lontilltemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
   IF AVAILABLE lontilltemp THEN ASSIGN FILL-IN_LONTILLAGG = lontilltemp.LONTILLAGG.
   ELSE ASSIGN FILL-IN_LONTILLAGG = " ".               
   ASSIGN
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.         
   IF MONTH(extratidallt.DATUM) = 12 THEN datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).   
   ELSE datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.   
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      RETURN.      
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      RETURN.      
   END.     
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         RETURN.   
      END.            
   END. 
   IF Guru.Konstanter:globforetag = "gkal" THEN DO:   
      IF INPUT FILL-IN_LONTILLANTAL < 0 THEN DO:
         MESSAGE "Antal måste vara större än 1"
         VIEW-AS ALERT-BOX.
         RETURN.      
      END.            
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      IF FILL-IN_VILART = "778" AND INPUT FILL-IN_LONTILLANTAL > 1   THEN DO:
         MESSAGE "Antal får inte vara större än 1 per dag"
         VIEW-AS ALERT-BOX.
         RETURN.      
      END.

      IF FILL-IN_VILART = "778" AND  TOG_LAVD = FALSE THEN DO:
         MESSAGE "Har du blivit bjuden på lunch? Glöm inte att bocka i ”lunch erhållen”." VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.        
         IF val = TRUE THEN  RETURN.         
      END.            
   END.
   
   
         
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN
   CMB_DAG = regdagnamn
   FILL-IN-VECKO = regvnr   
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR
   FILL-IN-RESMAL = INPUT FILL-IN-RESMAL
   FILL-IN_LONTILLANTAL = INPUT FILL-IN_LONTILLANTAL
   musz = FALSE.            
   IF vart = "ANN" THEN DO TRANSACTION:
      /*INGEN ÄNDRING*/
      IF extratidallt.LONTILLANTAL = FILL-IN_LONTILLANTAL AND 
      extratidallt.LONTILLAGG = FILL-IN_LONTILLAGG AND 
      extratidallt.AONR = FILL-IN-AONR AND
      extratidallt.DELNR = FILL-IN-DELNR
      THEN musz = TRUE.      
   END.
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:                                     
      ASSIGN
      regvnr = FILL-IN-VECKO
      regdagnamn = CMB_DAG      
      regdatumspar = regdatum
      regdagspar = regdagnamn.
      FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND 
      lontilltemp.VILART = FILL-IN_VILART USE-INDEX VILART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE lontilltemp THEN DO:
         MESSAGE "Lönetillägg" fill-in_VILART "finns inte." VIEW-AS ALERT-BOX.
         RETURN.         
      END.
      ELSE IF lontilltemp.VALBAR = FALSE THEN DO:
         MESSAGE "Lönetillägg" fill-in_VILART "går ej att använda." VIEW-AS ALERT-BOX.         
         RETURN.         
      END.
      ELSE DO:
         IF AVAILABLE lontilltemp THEN DO:
            IF lontilltemp.AONROBL = TRUE THEN DO:
               IF FILL-IN-AONR = "" THEN DO:    
                  MESSAGE 
                  "Lönetillägg" FILL-IN_VILART "kräver " + LC(Guru.Konstanter:gaol) + "." VIEW-AS ALERT-BOX.
                  
                  RETURN.                  
               END.
            END. 
            IF lontilltemp.AONROBL = ? THEN DO:
               IF FILL-IN-AONR = "" AND FILL-IN-DELNR = 0 THEN musz = musz.
               ELSE DO:    
                  MESSAGE 
                  "Vid lönetillägg" FILL-IN_VILART "krävs " + LC(Guru.Konstanter:gaol) + " är blankt." 
                  VIEW-AS ALERT-BOX.
                  
                  RETURN.                  
               END.
            END.            
         END.      
         IF FILL-IN-AONR = "" AND FILL-IN-DELNR = 0 THEN musz = musz.
         ELSE DO:    
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
            utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
            IF NOT AVAILABLE utsokaonr THEN DO:               
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 47
               soktemp.SOKCHAR[1] = FILL-IN-AONR
               soktemp.SOKINT[1] = FILL-IN-DELNR.
               {SOKANROP.I}      
               IF soktemp.SOKCHAR[2] = ? THEN DO:
                  MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.      
                  
                  RETURN.                  
               END.
               ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
               ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
                  MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.      
                  
                  RETURN.                  
               END.
               IF Guru.Konstanter:appcon THEN DO:                           
                  RUN AOVALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
                   INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
                   OUTPUT TABLE felmeddtemp).            
               END.
               ELSE DO:
                  RUN AOVALK.P 
                  (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
                   INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
                   OUTPUT TABLE felmeddtemp).            
               END.  
               FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
               IF AVAILABLE felmeddtemp THEN DO:
                  MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
                  DELETE felmeddtemp.
                  
                  RETURN.
               END.
               
            END.
            ELSE DO:                            
               {AOKOLLERS.I}
               IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
               utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
               ELSE DO:
                  MESSAGE 
                  Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
                  
                  RETURN.                  
               END.
            END.
         END.     
      END.       
      RUN lulkollaonr_UI.
      IF musz = TRUE THEN DO:
         musz = FALSE .
         MESSAGE 
         Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "används bara för beredskap och lart 833." VIEW-AS ALERT-BOX.
         
         RETURN.
      END.
      DEFINE VARIABLE returvar AS LOGICAL NO-UNDO.
      RUN btnreg2_UI (OUTPUT returvar).
      IF returvar = TRUE THEN DO:
         
         RETURN.
      END.

      /* arbetstidsförkortning*/      
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
         FIND FIRST extratidallt NO-ERROR.
         IF AVAILABLE extratidallt THEN ASSIGN extratidallt.LONTILLAGG = FILL-IN_LONTILLAGG.
         IF FILL-IN_VILART = "ATL" THEN DO:         
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN AOTIDKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
               INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,INPUT vart ,INPUT FILL-IN_LONTILLANTAL,
               OUTPUT TABLE felmeddtemp ,INPUT TABLE extratidallt ).            
            END.
            ELSE DO:
               RUN AOTIDKOLL.P 
               (INPUT regdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
               INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,INPUT vart,INPUT FILL-IN_LONTILLANTAL,
               OUTPUT TABLE felmeddtemp ,INPUT TABLE extratidallt).            
            END. 
         
            FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
            IF AVAILABLE felmeddtemp THEN DO:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
               DELETE felmeddtemp.
               APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.      
            END.          
         END.
      END.

      DO TRANSACTION:  
         IF vart = "ANN" THEN DO:
            FIND FIRST tidallt WHERE tidallt.RECTIDVIS = extratidallt.RECTIDVIS AND
            tidallt.LONTILLAGG NE ""
            NO-ERROR.         
            ASSIGN 
            tidallt.TIDLOG = FALSE
            tidallt.LONTILLAGG = ''
            tidallt.LONTILLANTAL = 0
            tidallt.LONAUTO = FALSE             
            /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
            SUBSTRING(tidallt.PROGRAM,1,158) = "ANDLON" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.      
            CREATE tidallt.
            BUFFER-COPY extratidallt TO tidallt.      
            ASSIGN
            tidallt.RECTIDVIS = ?
            tidallt.PERSONALKOD = "".                 
         END.
         ELSE DO:
            IF extratidallt.RECTIDVIS = ? THEN DO:
               CREATE tidallt.
               BUFFER-COPY extratidallt TO tidallt.
               tidallt.PERSONALKOD = "".               
            END.
            ELSE DO:
               FIND FIRST tidallt WHERE tidallt.RECTIDVIS = extratidallt.RECTIDVIS NO-ERROR.
               BUFFER-COPY extratidallt TO tidallt.      
               tidallt.PERSONALKOD = "".                 
            END.
         END.
         ASSIGN
         regvnr = FILL-IN-VECKO
         regdagnamn = CMB_DAG.
         IF vart = "NYA" AND FILL-IN-MOMS > 0 THEN
         ASSIGN FILL-IN_LONTILLANTAL = FILL-IN_LONTILLANTAL - FILL-IN-MOMS.         
         ASSIGN
         tidallt.DATUM = regdatum
         tidallt.AONR = FILL-IN-AONR 
         tidallt.DELNR = FILL-IN-DELNR
         /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
         SUBSTRING(tidallt.PROGRAM,1,158) = "ANDLON" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv         
         tidallt.PERSONALKOD = ""
         tidallt.DAG = CMB_DAG
         tidallt.VECKONUMMER = FILL-IN-VECKO
         tidallt.SLUT = 7.00
         tidallt.START = 7.00 
         tidallt.TIDLOG = FALSE
         tidallt.LONTILLAGG = FILL-IN_LONTILLAGG
         tidallt.VILART = FILL-IN_VILART
         tidallt.LONTILLANTAL = FILL-IN_LONTILLANTAL  
         tidallt.RESMAL = FILL-IN-RESMAL
         tidallt.LONAUTO = FALSE.                       
         IF vart = "NYA" AND FILL-IN-MOMS > 0 THEN DO:
            FIND FIRST lontilltemp WHERE lontilltemp.LONKODTEXT = 'MOMS'
            NO-LOCK NO-ERROR.            
            IF AVAILABLE lontilltemp THEN DO:
               CREATE tidbuff.
               ASSIGN
               tidbuff.DATUM = tidallt.DATUM 
               tidbuff.AONR = tidallt.AONR  
               tidbuff.DELNR = tidallt.DELNR 
               tidbuff.PROGRAM = tidallt.PROGRAM 
               tidallt.PERSONALKOD = ""
               tidbuff.DAG = tidallt.DAG 
               tidbuff.VECKONUMMER = tidallt.VECKONUMMER 
               tidbuff.TIDLOG = FALSE
               tidbuff.SLUT = 7.00
               tidbuff.START = 7.00 
               tidbuff.LONTILLAGG = lontilltemp.LONTILLAGG
               tidbuff.LONTILLANTAL = FILL-IN-MOMS  
               tidbuff.RESMAL = FILL-IN-RESMAL
               tidbuff.LONAUTO = FALSE.                
            END.  
            IF NOT AVAILABLE lontilltemp THEN DO:
               FIND FIRST lontilltemp2 WHERE lontilltemp2.LONKODTEXT = 'MOMS'
               NO-LOCK NO-ERROR.            
               IF AVAILABLE lontilltemp2 THEN DO:
                  CREATE tidbuff.
                  ASSIGN
                  tidbuff.DATUM = tidallt.DATUM 
                  tidbuff.AONR = tidallt.AONR  
                  tidbuff.DELNR = tidallt.DELNR 
                  tidbuff.PROGRAM = tidallt.PROGRAM 
                  tidallt.PERSONALKOD = ""
                  tidbuff.DAG = tidallt.DAG 
                  tidbuff.VECKONUMMER = tidallt.VECKONUMMER 
                  tidbuff.TIDLOG = FALSE
                  tidbuff.SLUT = 7.00
                  tidbuff.START = 7.00 
                  tidbuff.LONTILLAGG = lontilltemp2.LONTILLAGG
                  tidbuff.LONTILLANTAL = FILL-IN-MOMS  
                  tidbuff.RESMAL = FILL-IN-RESMAL
                  tidbuff.LONAUTO = FALSE.                
               END.  
            END.
         END.
        
         IF Guru.Konstanter:globforetag = "snat" THEN DO:
            IF vart = "NYA"  THEN DO:
               IF FILL-IN_VILART = "778"  AND TOG_LAVD = TRUE THEN DO:
                  /* endagsmåltidsavdrag*/
                  /*FIND FIRST lontilltemp2 WHERE lontilltemp2.VILART = "843" NO-LOCK NO-ERROR.
                  NY LART 779 20200320 lena*/
                  FIND FIRST lontilltemp2 WHERE lontilltemp2.VILART = "779" NO-LOCK NO-ERROR.            
                  IF AVAILABLE lontilltemp THEN DO:
                     CREATE tidbuff.
                     ASSIGN
                     tidbuff.DATUM = tidallt.DATUM 
                     tidbuff.AONR = tidallt.AONR  
                     tidbuff.DELNR = tidallt.DELNR 
                     tidbuff.PROGRAM = tidallt.PROGRAM 
                     tidallt.PERSONALKOD = ""
                     tidbuff.DAG = tidallt.DAG 
                     tidbuff.VECKONUMMER = tidallt.VECKONUMMER 
                     tidbuff.TIDLOG = FALSE
                     tidbuff.SLUT = 7.00
                     tidbuff.START = 7.00 
                     tidbuff.LONTILLAGG = lontilltemp2.LONTILLAGG
                     tidbuff.LONTILLANTAL = 1  
                     tidbuff.RESMAL = FILL-IN-RESMAL
                     tidbuff.LONAUTO = FALSE.                
                  END.
               END.   
            END.
         END.
         IF Guru.Konstanter:globforetag = "csund" THEN DO:
            /*De ska ha lart 320/321/322 för motsvarand 310/311/312 under passet när de reggat 041. 
            Problem med automatik om de bara är samordare för del av passet eller jobbar 22-24 0-7 22-24 osv*/
            IF vart = "NYA"  THEN DO:
               IF FILL-IN_VILART = "041"  THEN DO:                  
                  {SOKSTART.I}
                  ASSIGN
                  soktemp.SOKVAL = 102
                  soktemp.SOKCHAR[1] = personaltemp.PERSONALKOD                  
                  soktemp.SOKDATE[1] = tidallt.DATUM.
                  {SOKANROP.I}      
                  FOR EACH soktemp WHERE soktemp.SOKCHAR[2] = "OB" NO-LOCK:
                     CREATE tidbuff.
                     ASSIGN
                     tidbuff.DATUM = tidallt.DATUM 
                     tidbuff.AONR = tidallt.AONR  
                     tidbuff.DELNR = tidallt.DELNR 
                     tidbuff.PROGRAM = tidallt.PROGRAM 
                     tidallt.PERSONALKOD = ""
                     tidbuff.DAG = tidallt.DAG 
                     tidbuff.VECKONUMMER = tidallt.VECKONUMMER 
                     tidbuff.TIDLOG = FALSE
                     tidbuff.SLUT = 7.00
                     tidbuff.START = 7.00 
                     tidbuff.LONTILLAGG = soktemp.SOKCHAR[3]
                     tidbuff.LONTILLANTAL = soktemp.SOKDECI[1] 
                     tidbuff.RESMAL = FILL-IN-RESMAL
                     tidbuff.LONAUTO = FALSE.
                  END.                           
               END.   
            END.
         END.         
      END.
      DEBUGGER:SET-BREAK().
      ASSIGN
      bdatum = regdatum
      avdatum = regdatum.
      EMPTY TEMP-TABLE extratidallt NO-ERROR. 
      FOR EACH tidallt WHERE tidallt.PERSONALKOD = "":
         CREATE extratidallt.
         BUFFER-COPY tidallt TO extratidallt.
         DELETE tidallt.
      END.
      IF Guru.Konstanter:globforetag = "SNAT" AND TOG_LBL = TRUE THEN DO:                      
         EMPTY TEMP-TABLE etidallt NO-ERROR.
         FOR EACH extratidallt:                                                  
            CREATE etidallt.
            BUFFER-COPY extratidallt TO etidallt.            
            etidallt.PERSONALKOD = pkod.                     
         END.                           
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LONREG.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT Guru.Konstanter:globanv,INPUT 1,INPUT pkod,INPUT FILL-IN_LONTILLAGG,INPUT TABLE extratidallt,
         OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END.
      ELSE DO:
         RUN LONREG.P 
         (INPUT Guru.Konstanter:globanv,INPUT 1,INPUT pkod,INPUT FILL-IN_LONTILLAGG,INPUT TABLE extratidallt,
         OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
      END. 
      OPEN QUERY tq FOR EACH tidallt WHERE 
      tidallt.PERSONALKOD = pkod AND tidallt.DATUM = regdatum AND tidallt.LONAUTO = TRUE 
      USE-INDEX PSTART NO-LOCK.
      DO TRANSACTION:
         GET FIRST tq EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(tidallt):     
            FIND FIRST lonkorttemp WHERE lonkorttemp.LONTILLAGG = FILL-IN_LONTILLAGG AND
            lonkorttemp.KORTLON = tidallt.LONTILLAGG NO-LOCK NO-ERROR.
            IF NOT AVAILABLE lonkorttemp THEN musz = musz.
            ELSE DO:
               ASSIGN 
               tidallt.LONTILLAGG = " " 
               tidallt.LONTILLANTAL = 0
               tidallt.LONAUTO = FALSE.
            END.
            GET NEXT tq EXCLUSIVE-LOCK.
         END.
      END.
   END.   
   IF Guru.Konstanter:globforetag = "SNAT" AND TOG_LBL = TRUE THEN DO:            
      FIND FIRST etidallt /*WHERE etidallt.VILART NE ""*/  NO-ERROR.
      IF AVAILABLE etidallt THEN DO:                     
         RUN LONBLANKETT.P (INPUT TABLE etidallt).
      END.                
   END.
   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY TOG_LBL FILL-IN-DAG FILL-IN_VILART FILL-IN-SORT FILL-IN_LONTILLANTAL 
          FILL-IN-AONR FILL-IN-DELNR FILL-IN-RESMAL CMB_OMR FILL-IN-TEXT 
          FILL-IN-PKOD FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
  ENABLE TOG_LBL FILL-IN_VILART BRW_LON FILL-IN_LONTILLANTAL FILL-IN-AONR 
         FILL-IN-DELNR FILL-IN-RESMAL BTN_REG BTN_AVS CMB_OMR CMB_AVD TOG_LAVD 
         RECT-22 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillinupdate_UI DIALOG-1 
PROCEDURE fillinupdate_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF RAD_FAST = 1 OR RAD_FAST = 2 THEN DO:   
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN
         FILL-IN-AONR = utsokaonr.AONR
         FILL-IN-DELNR = utsokaonr.DELNR.       
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.         
      END.
      APPLY "VALUE-CHANGED" TO BRW_AONR IN FRAME {&FRAME-NAME}. 
   END.
   ELSE DO:
      IF AVAILABLE egnaao THEN DO:
         ASSIGN
         FILL-IN-AONR = egnaao.AONR
         FILL-IN-DELNR = egnaao.DELNR.      
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.         
      END.
      APPLY "VALUE-CHANGED" TO BRW_EAONR IN FRAME {&FRAME-NAME}. 
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE grundtid_UI DIALOG-1 
PROCEDURE grundtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/   
   FILL-IN-VECKO = regvnr. 
   FILL-IN_LONAUTO = FALSE.
   IF vart = "NYA" THEN DO:
      ASSIGN
      regdagnamn = extratidallt.DAG.      
      ASSIGN FILL-IN-DATUM = DAY(extratidallt.DATUM).         
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN FILL-IN-DATUM = DAY(tillochmeddatum + 1).         
      END.         
      DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN-DATUM BTN_FVE BTN_NVE WITH FRAME {&FRAME-NAME}.      
      ASSIGN
      FILL-IN-AONR = extratidallt.AONR
      FILL-IN-DELNR = extratidallt.DELNR
      FILL-IN_LONTILLAGG = extratidallt.LONTILLAGG
      FILL-IN_LONTILLANTAL = extratidallt.LONTILLANTAL
      spantal = FILL-IN_LONTILLANTAL
      FILL-IN-SORT = ""
      regdatum = DATE((regmnr),FILL-IN-DATUM,regar).       
      RUN REGDAG.P.   
      FILL-IN-DAG = regdagnamn.
      DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
      FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND
      lontilltemp.LONTILLAGG = extratidallt.LONTILLAGG
      USE-INDEX LON NO-LOCK NO-ERROR.      
      IF AVAILABLE lontilltemp THEN DO:
         FILL-IN-SORT = lontilltemp.ENHET.
         IF lontilltemp.AONROBL = ? THEN DO:
            ASSIGN
            FILL-IN-AONR = ""
            FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}. 
         END.
         RUN styraonr_UI.
      END.          
      IF NOT AVAILABLE lontilltemp AND lonbil = 1 THEN DO:
         /* TJR tjänstereselönearter ska inte föreslås sundsvall*/
         ASSIGN         
         FILL-IN_LONTILLAGG = ""
         FILL-IN_LONTILLANTAL = 0         
         FILL-IN-SORT = "".
      END.
      FILL-IN-RESMAL:HIDDEN = FALSE.
      IF FILL-IN-SORT = "MI" OR FILL-IN-SORT = "KM" THEN FILL-IN-RESMAL:LABEL = "Resmål".
      ELSE FILL-IN-RESMAL:LABEL = "Kommentar".        
      IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN_LONTILLAGG = "756" THEN FILL-IN-RESMAL:LABEL = "Namn på passagerarna".
      FILL-IN-RESMAL = extratidallt.RESMAL.      
      IF AVAILABLE lontilltemp THEN FILL-IN_VILART = lontilltemp.VILART.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
         IF FILL-IN_LONTILLAGG = "" AND lonbil = 2 THEN DO:
            FIND LAST tidbuff WHERE tidbuff.PERSONALKOD = personaltemp.PERSONALKOD
            AND tidbuff.LONTILLAGG NE "" USE-INDEX PSTART NO-LOCK NO-ERROR.             
            IF AVAILABLE tidbuff THEN DO:            
               FIND FIRST lontilltemp WHERE lontilltemp.VILART = tidbuff.LONTILLAGG AND
               lontilltemp.KOD = ansttemp.KOD AND SUBSTRING(lontilltemp.TYPKOD,1,3) = "BIL" USE-INDEX VILART NO-LOCK NO-ERROR.   
               IF NOT AVAILABLE lontilltemp THEN DO:
                  lonrakn = 1.
                  bilkoll:
                  REPEAT:                 
                     FIND PREV tidbuff WHERE tidbuff.PERSONALKOD = personaltemp.PERSONALKOD
                     AND tidbuff.LONTILLAGG NE "" USE-INDEX PSTART NO-LOCK NO-ERROR.             
                     IF NOT AVAILABLE tidbuff THEN LEAVE bilkoll.
                     ELSE DO:            
                        FIND FIRST lontilltemp WHERE lontilltemp.VILART = tidbuff.LONTILLAGG AND
                        lontilltemp.KOD = ansttemp.KOD AND SUBSTRING(lontilltemp.TYPKOD,1,3) = "BIL" USE-INDEX VILART NO-LOCK NO-ERROR.   
                        IF AVAILABLE lontilltemp THEN LEAVE bilkoll.
                        ELSE DO:
                           lonrakn = lonrakn + 1.
                           IF lonrakn GE 7 THEN LEAVE bilkoll.
                        END.
                     END.
                  END.
               END.
            END.
            IF AVAILABLE lontilltemp THEN DO:     
               ASSIGN
               FILL-IN_VILART = lontilltemp.VILART
               FILL-IN-SORT = lontilltemp.ENHET
               FILL-IN_LONAUTO = FALSE.
               DISPLAY FILL-IN_VILART FILL-IN-SORT FILL-IN_LONAUTO  WITH FRAME {&FRAME-NAME}.      
            END.
         END.         
         IF FILL-IN_LONTILLAGG = "" AND lonbil = 1 THEN DO:            
            FIND LAST tidbuff WHERE tidbuff.PERSONALKOD = personaltemp.PERSONALKOD
            AND tidbuff.LONTILLAGG NE "" USE-INDEX PSTART NO-LOCK NO-ERROR.             
            IF AVAILABLE tidbuff THEN DO:            
               FIND FIRST lontilltemp WHERE lontilltemp.VILART = tidbuff.LONTILLAGG AND
               lontilltemp.KOD = ansttemp.KOD AND SUBSTRING(lontilltemp.TYPKOD,1,3) NE "BIL"
               AND SUBSTRING(lontilltemp.TYPKOD,4,3) NE "TJR" USE-INDEX VILART NO-LOCK NO-ERROR.   
               IF NOT AVAILABLE lontilltemp THEN DO:
                  lonrakn = 1.
                  bilkoll:
                  REPEAT:                 
                     FIND PREV tidbuff WHERE tidbuff.PERSONALKOD = personaltemp.PERSONALKOD
                     AND tidbuff.LONTILLAGG NE "" USE-INDEX PSTART NO-LOCK NO-ERROR.             
                     IF NOT AVAILABLE tidbuff THEN LEAVE bilkoll.
                     ELSE DO:            
                        FIND FIRST lontilltemp WHERE lontilltemp.VILART = tidbuff.LONTILLAGG AND
                        lontilltemp.KOD = ansttemp.KOD AND SUBSTRING(lontilltemp.TYPKOD,1,3) NE "BIL"
                        AND SUBSTRING(lontilltemp.TYPKOD,4,3) NE "TJR"  USE-INDEX VILART NO-LOCK NO-ERROR.   
                        IF AVAILABLE lontilltemp THEN LEAVE bilkoll.
                        ELSE DO:
                           lonrakn = lonrakn + 1.
                           IF lonrakn GE 7 THEN LEAVE bilkoll.
                        END.
                     END.
                  END.
               END.
            END.
            IF AVAILABLE lontilltemp THEN DO:   
               ASSIGN
               FILL-IN_VILART = lontilltemp.VILART
               FILL-IN-SORT = lontilltemp.ENHET
               FILL-IN_LONAUTO = FALSE.               
               DISPLAY FILL-IN_VILART FILL-IN-SORT FILL-IN_LONAUTO  WITH FRAME {&FRAME-NAME}.                     
            END.

         END.
      END.
   END.   
   ELSE DO:
      ASSIGN  FILL-IN-DATUM = DAY(extratidallt.DATUM).
      DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.                   
      ASSIGN      
      regdatum = extratidallt.DATUM
      regvnr = extratidallt.VECKONUMMER
      FILL-IN-VECKO = extratidallt.VECKONUMMER
      FILL-IN-AONR = extratidallt.AONR
      FILL-IN-DELNR = extratidallt.DELNR
      FILL-IN_LONTILLAGG = extratidallt.LONTILLAGG
      FILL-IN_LONTILLANTAL = extratidallt.LONTILLANTAL
      spantal = FILL-IN_LONTILLANTAL.
      FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND
      lontilltemp.LONTILLAGG = extratidallt.LONTILLAGG
      USE-INDEX LON NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE lontilltemp THEN DO:
         /* "tjr" tjänsteresalönearter skall bara visas om man går på ändra en redan gjord tjänsterese-registreing*/
         EMPTY TEMP-TABLE lontilltemp NO-ERROR. 
         FOR EACH lontilltemp2:
            CREATE lontilltemp.
            BUFFER-COPY lontilltemp2 TO lontilltemp.            
         END.
         FIND FIRST lontilltemp WHERE lontilltemp.KOD = ansttemp.KOD AND
         lontilltemp.LONTILLAGG = extratidallt.LONTILLAGG
         USE-INDEX LON NO-LOCK NO-ERROR.      
      END.
      IF lontilltemp.AONROBL = ? THEN DO:
         ASSIGN
         FILL-IN-AONR = ""
         FILL-IN-DELNR = 0.
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.          
      END.
      RUN styraonr_UI.
      RUN REGDAG.P.   
      FILL-IN-DAG = regdagnamn.
      DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.      
      IF AVAILABLE lontilltemp THEN FILL-IN-SORT = lontilltemp.ENHET.
      FILL-IN-RESMAL:HIDDEN = FALSE.
      IF FILL-IN-SORT = "MI" OR FILL-IN-SORT = "KM" THEN FILL-IN-RESMAL:LABEL = "Resmål".
      ELSE FILL-IN-RESMAL:LABEL = "Kommentar".
      IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN_LONTILLAGG = "756" THEN FILL-IN-RESMAL:LABEL = "Namn på passagerarna".
      FILL-IN-RESMAL = extratidallt.RESMAL.  
      IF AVAILABLE lontilltemp THEN FILL-IN_VILART = lontilltemp.VILART.      
      IF AVAILABLE lontilltemp THEN DO:
         IF lontilltemp.VALBAR = FALSE THEN DO:
            MESSAGE "Det går ej att ändra på denna löneart." VIEW-AS ALERT-BOX.
            musz = TRUE.
            RETURN.
         END.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
            IF SUBSTRING(lontilltemp.TYPKOD,1,3) = "BIL" THEN ASSIGN lonbil = 2.
            ELSE ASSIGN lonbil = 1.
         END.
      END.   
   END.  
   ASSIGN
   FILL-IN-PKOD = personaltemp.PERSONALKOD
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hao2_UI DIALOG-1 
PROCEDURE hao2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
      IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
      FIND FIRST egnaao WHERE egnaao.AONR = FILL-IN-AONR AND egnaao.DELNR = FILL-IN-DELNR AND
      egnaao.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE egnaao THEN RAD_FAST = 3.
      /*RAD_FAST = utsokaonr.FASTAAONR.*/
      DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.         
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN CMB_OMR = Guru.Konstanter:gomrk + " : alla".
      ELSE DO:         
         IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN musz = musz.                        
         ELSE IF utsokaonr.OMRADE = ""  THEN DO:
            CMB_OMR = Guru.Konstanter:gomrk + " : alla".
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.         
         END.
         ELSE IF utsokaonr.OMRADE NE omrtemp.OMRADE THEN DO: 
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.omrade NO-LOCK NO-ERROR.
            CMB_OMR = omrtemp.NAMN.
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.         
         END.       
      END.         
      IF RAD_FAST = 3 THEN DO:               
         RUN openbdynspec_UI IN brwproc[3].
         IF FILL-IN-AONR NE "" THEN DO:
            FIND FIRST egnaao WHERE egnaao.AONR = FILL-IN-AONR AND egnaao.DELNR = FILL-IN-DELNR AND
            egnaao.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
            IF AVAILABLE egnaao THEN DO:            
               RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(egnaao)).
               RUN lastselectdyn_UI IN brwproc[3].            
            END.
         END.      
         ASSIGN
         BRW_AONR:HIDDEN = TRUE   
         BRW_EAONR:HIDDEN = FALSE
         CMB_OMR:HIDDEN = TRUE
         CMB_AVD:HIDDEN = TRUE
         FILL-IN-TEXT:HIDDEN = TRUE.   
         ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:         
         RUN nycolsortprep_UI (INPUT 1).
         RUN openbdynspec_UI IN brwproc[1].  
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
         utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].  
         ASSIGN
         BRW_AONR:HIDDEN = FALSE   
         BRW_EAONR:HIDDEN = TRUE
         CMB_OMR:HIDDEN = FALSE
         CMB_AVD:HIDDEN = FALSE
         FILL-IN-TEXT:HIDDEN = FALSE.   
         ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      END.                  
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lulkollaonr_UI DIALOG-1 
PROCEDURE lulkollaonr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   musz = FALSE.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.                  
      IF FILL-IN_LONTILLAGG = "833" THEN musz = musz.
      ELSE DO:
         IF FILL-IN-AONR = "003005" AND FILL-IN-DELNR = 1 THEN musz = TRUE.
         IF FILL-IN-AONR = "100293" AND FILL-IN-DELNR = 0 THEN musz = TRUE.
         IF FILL-IN-AONR = "241" AND FILL-IN-DELNR = 0 THEN musz = TRUE.
         IF FILL-IN-AONR = "245" AND FILL-IN-DELNR = 0 THEN musz = TRUE.         
      END.
   END.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nycolsortprep_UI DIALOG-1 
PROCEDURE nycolsortprep_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   {NYCOL2.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyupp_UI DIALOG-1 
PROCEDURE nyupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER sok0 AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT sok0,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT sok0,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omrvisa_UI DIALOG-1 
PROCEDURE omrvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   &Scoped-define FORMATNAMN utsokaonr.AONR
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE
   &Scoped-define BROWSE-NAME BRW_AONR
   {OMRAOFORMAT.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE styraonr_UI DIALOG-1 
PROCEDURE styraonr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.                  
      IF FILL-IN_LONTILLAGG = "833" THEN DO:                     
         IF personaltemp.BEREDSKAPSAVTAL BEGINS "M" THEN DO:      
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = "003005" 
            AND utsokaonr.DELNR = 1 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
            IF AVAILABLE utsokaonr THEN DO:
               ASSIGN FILL-IN-AONR = "003005" FILL-IN-DELNR = 1.
               DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
            END.         
         END.
         ELSE IF personaltemp.BEREDSKAPSAVTAL BEGINS "P" THEN DO:      
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = "241" 
            AND utsokaonr.DELNR = 0 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
            IF AVAILABLE utsokaonr THEN DO:
               ASSIGN FILL-IN-AONR = "241" FILL-IN-DELNR = 0.
               DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
            END.         
         END.
         ELSE IF personaltemp.BEREDSKAPSAVTAL BEGINS "F" THEN DO:      
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = "245" 
            AND utsokaonr.DELNR = 0 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
            IF AVAILABLE utsokaonr THEN DO:
               ASSIGN FILL-IN-AONR = "245" FILL-IN-DELNR = 0.
               DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
            END.         
         END.
         ELSE DO:
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = "100293" AND 
            utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
            IF AVAILABLE utsokaonr THEN DO:
               ASSIGN FILL-IN-AONR = "100293" FILL-IN-DELNR = 0.
               DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
            END.         
         END.

         
      END.         
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock100 DIALOG-1 
FUNCTION klock100 RETURNS DECIMAL
   ( INPUT ber60 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

