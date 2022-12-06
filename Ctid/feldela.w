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

DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{PHMT.I}
{UPPGHMT.I}
{FLEXTAB.I}
{TIDALLT.I}
{PERBEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
{OMRTEMPW.I}  
{DIRDEF.I}
{TIDAPPDEF.I}
{TIDPERS.I}
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO. 
DEFINE SHARED VARIABLE varslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE kontrollstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE halvkvart AS INTEGER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */

DEFINE BUFFER tidbuff FOR tidallt.

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
&Scoped-define INTERNAL-TABLES utsokaonr tidallt

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


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define QUERY-STRING-DIALOG-1 FOR EACH RT8.tidallt NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-1 OPEN QUERY DIALOG-1 FOR EACH RT8.tidallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-1 tidallt
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-1 tidallt


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-AONR FILL-IN-DELNR FILL-IN-START ~
FILL-IN-PLUS FILL-IN-SLUT BTN_REG BTN_AVB CMB_OMR CMB_AVD RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN-DAG FILL-IN-AONR ~
FILL-IN-DELNR FILL-IN-START FILL-IN-PLUS FILL-IN-SLUT FILL-IN-TEXT CMB_OMR ~
FILL-IN-SKP CMB_AVD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock100 DIALOG-1  _DB-REQUIRED
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "Registrera":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avvik. fakt.bef." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

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

DEFINE VARIABLE CMB_OVERUT AS CHARACTER FORMAT "X(4)":U 
     LABEL "Övertiduttag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Komp,","Över,","Flex","Ejöv" 
     DROP-DOWN-LIST
     SIZE 8.13 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PRISTYP AS CHARACTER FORMAT "X(9)":U 
     LABEL "Debitering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TRAK AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Trakt.zon" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0","1" 
     DROP-DOWN-LIST
     SIZE 8.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-OVER AS CHARACTER FORMAT "X(1)":U 
     LABEL "Övertiduttag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PLUS AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Plus" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL ? 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     LABEL "Debitering" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets slut" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets start" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRAKT AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Trakt.zon" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTRYCK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "utryckningtemp" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALFA AS CHARACTER FORMAT "X(256)":U INITIAL "Välj fakturabefattning        Vald fakturabefattning" 
     VIEW-AS FILL-IN 
     SIZE 34.13 BY .75
     FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-VECKO AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Veckonummer" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_NODF AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Nödfall" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_VECKOVILA AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Veckovila" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_VIBEFATTNING AS CHARACTER FORMAT "x(256)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.38 BY 1.08
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY DIALOG-1 FOR 
      tidallt SCROLLING.
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 53.38 BY 11.46
         TITLE "Aktiva arbetsordernummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-PKOD AT ROW 2.92 COL 18.63 COLON-ALIGNED
     CMB_DAG AT ROW 5.5 COL 18.63 COLON-ALIGNED
     FILL-IN-DAG AT ROW 5.5 COL 32.63 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 4.25 COL 18.63 COLON-ALIGNED NO-LABEL
     FILL-IN-VECKO AT ROW 4.25 COL 18.63 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 5.58 COL 18.63 COLON-ALIGNED
     BTN_NVE AT ROW 5.29 COL 30.88
     BTN_FVE AT ROW 6.17 COL 30.88
     FILL-IN-AONR AT ROW 6.88 COL 18.63 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 8.21 COL 18.63 COLON-ALIGNED
     CMB_TRAK AT ROW 9.5 COL 18.63 COLON-ALIGNED
     CMB_OVERUT AT ROW 10.88 COL 18.63 COLON-ALIGNED
     FILL-IN-OVER AT ROW 22.42 COL 83.25 COLON-ALIGNED HELP
          "ÖVERTID = Ö, KOMP = K, INGEN ERSÄTTNING = I"
     FILL-IN-UTRYCK AT ROW 12.25 COL 18.63 COLON-ALIGNED
     FILL-IN-START AT ROW 13.58 COL 18.63 COLON-ALIGNED
     FILL-IN-PLUS AT ROW 13.33 COL 34 COLON-ALIGNED
     FILL-IN-SLUT AT ROW 14.88 COL 18.63 COLON-ALIGNED
     FILL-IN-PRISTYP AT ROW 11.75 COL 40 COLON-ALIGNED
     CMB_PRISTYP AT ROW 16.21 COL 18.63 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 17.58 COL 18.63 COLON-ALIGNED
     FILL-IN_NODF AT ROW 22.42 COL 18.5 COLON-ALIGNED
     FILL-IN_VECKOVILA AT ROW 22.42 COL 37.38 COLON-ALIGNED
     FILL-IN_RESMAL AT ROW 21.25 COL 18.63 COLON-ALIGNED
     FILL-IN-TRAKT AT ROW 21.25 COL 83.25 COLON-ALIGNED
     CMB_BEF AT ROW 20 COL 18.63 COLON-ALIGNED
     FILL-IN_VIBEFATTNING AT ROW 20 COL 37.5 COLON-ALIGNED HELP
          "BEFATTNINGSKOD" NO-LABEL
     FILL-IN-VALFA AT ROW 18.88 COL 18.63 COLON-ALIGNED NO-LABEL
     BTN_REG AT ROW 24.38 COL 67.5
     BTN_AVB AT ROW 24.38 COL 82.5
     FILL-IN-TEXT AT ROW 1.38 COL 71.75 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 3.5 COL 40.88 NO-LABEL
     CMB_OMR AT ROW 3.5 COL 72 COLON-ALIGNED NO-LABEL
     FILL-IN-SKP AT ROW 16.83 COL 42.5 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 16.83 COL 57.5 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 16.83 COL 75 COLON-ALIGNED
     BRW_AONR AT ROW 4.79 COL 43.13
     CMB_AVD AT ROW 2.38 COL 72 COLON-ALIGNED NO-LABEL
     RECT-22 AT ROW 16.71 COL 43.13
     SPACE(0.73) SKIP(7.87)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_AONR FILL-IN_ORTS DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

/* SETTINGS FOR BUTTON BTN_FVE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_BEF IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_BEF:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_DAG IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_DAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OVERUT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_OVERUT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_PRISTYP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_PRISTYP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_TRAK IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_TRAK:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-OVER IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OVER:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PRIS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-PRIS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PRISTYP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-PRISTYP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TRAKT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TRAKT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-UTRYCK IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-UTRYCK:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALFA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VALFA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VECKO IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VECKO:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NODF IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NODF:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_RESMAL IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_RESMAL:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VECKOVILA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_VECKOVILA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VIBEFATTNING IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_VIBEFATTNING:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-1           = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _TblList          = "RT8.tidallt"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra tidregistrering */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra tidregistrering */
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
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.      
      ASSIGN
      sok1 = utsokaonr.AONR      
      sok2 = utsokaonr.DELNR
      sok4 = pkod.      
      RUN nyupp_UI (INPUT 16).      
      ASSIGN
      FILL-IN-PRISTYP = sok1
      CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
      CMB_TRAK = INPUT CMB_TRAK      
      CMB_PRISTYP = FILL-IN-PRISTYP.
      IF sok3 = "1" THEN FILL-IN-UTRYCK = TRUE.
      IF sok3 = "2" THEN FILL-IN-UTRYCK = FALSE.
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR /*FILL-IN-UTRYCK*/ WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:      
         FILL-IN-PRIS = sok5.   
         DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
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
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
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
   IF MONTH(tidallt.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(tidallt.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(tidallt.DATUM) + 1),01,YEAR(tidallt.DATUM)) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Registrera */
DO:
   {muswait.i} 
   RUN btnreg_UI.
   {musarrow.i} 
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Registrera */
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
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BEF DIALOG-1
ON VALUE-CHANGED OF CMB_BEF IN FRAME DIALOG-1 /* Avvik. fakt.bef. */
DO:   
   CMB_BEF = INPUT CMB_BEF.      
   IF CMB_BEF = "Återställ bef" THEN DO:
      FIND FIRST befattningstemp WHERE befattningstemp.BEFATTNING = personaltemp.BEFATTNING
      USE-INDEX BEF NO-LOCK NO-ERROR.  
      ASSIGN
      FILL-IN_VIBEFATTNING = befattningstemp.NAMN.
      ASSIGN CMB_BEF:SCREEN-VALUE = befattningstemp.NAMN.
      CMB_BEF = INPUT CMB_BEF.
   END.
   ELSE DO:
      FIND FIRST befattningstemp WHERE befattningstemp.NAMN = CMB_BEF
      USE-INDEX BEF NO-LOCK NO-ERROR.  
      ASSIGN
      FILL-IN_VIBEFATTNING = befattningstemp.NAMN.
   END.
   DISPLAY FILL-IN_VIBEFATTNING WITH FRAME {&FRAME-NAME}.
  
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
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OVERUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OVERUT DIALOG-1
ON VALUE-CHANGED OF CMB_OVERUT IN FRAME DIALOG-1 /* Övertiduttag */
DO:
   CMB_OVERUT = INPUT CMB_OVERUT.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_PRISTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PRISTYP DIALOG-1
ON VALUE-CHANGED OF CMB_PRISTYP IN FRAME DIALOG-1 /* Debitering */
DO:                              
   ASSIGN
   CMB_PRISTYP = INPUT CMB_PRISTYP  
   FILL-IN-PRISTYP = CMB_PRISTYP. 
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
   END.
   ELSE DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = personaltemp.PERSONALKOD
      soktemp.SOKCHAR[3] = FILL-IN-PRISTYP
      soktemp.SOKCHAR[4] = personaltemp.BEFATTNING 
      soktemp.SOKDATE[1] = TODAY.
      {SOKANROP.I}
      ASSIGN
      FILL-IN-PRIS = soktemp.SOKDECI[1].
      DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
   END.  
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TRAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TRAK DIALOG-1
ON VALUE-CHANGED OF CMB_TRAK IN FRAME DIALOG-1 /* Trakt.zon */
DO:
   CMB_TRAK = INPUT CMB_TRAK.
   FILL-IN-TRAKT = CMB_TRAK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ANY-KEY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   
   ASSIGN
   musz = FALSE
   FILL-IN-SKP = "Sök på:"
   CMB_OMR:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
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
      END.
      aonrrec = RECID(utsokaonr).
      RAD_FAST = utsokaonr.FASTAAONR.
   END.
   ELSE DO:
      ASSIGN 
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      aonrrec = 0.
      RAD_FAST = FALSE.
   END.
   DISPLAY RAD_FAST FILL-IN-SKP WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN
   BRW_AONR:HIDDEN = FALSE    
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   FILL-IN-AONR = INPUT FILL-IN-AONR.
   IF FILL-IN-AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:  
   IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF AVAILABLE utsokaonr THEN DO:         
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = personaltemp.PERSONALKOD
            soktemp.SOKCHAR[3] = utsokaonr.PRISTYP
            soktemp.SOKCHAR[4] = personaltemp.BEFATTNING 
            soktemp.SOKDATE[1] = TODAY.
            {SOKANROP.I}
            ASSIGN
            FILL-IN-PRIS = soktemp.SOKDECI[1].
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.
         ASSIGN
         FILL-IN-PRISTYP = utsokaonr.PRISTYP
         CMB_TRAK = utsokaonr.TRAKTAMENTE
         FILL-IN-UTRYCK = utsokaonr.UTRYCKNING
         CMB_PRISTYP = FILL-IN-PRISTYP. 
         ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR CMB_TRAK /*FILL-IN-UTRYCK*/ 
         WITH FRAME {&FRAME-NAME}.
      END.
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
   IF MONTH(tidallt.DATUM) = 12 THEN DO:
      datkoll = DATE(12,31,YEAR(tidallt.DATUM)).
   END.
   ELSE DO:   
      datkoll = DATE((MONTH(tidallt.DATUM) + 1),01,YEAR(tidallt.DATUM)) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
   END.            
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).     
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:  
   IF INPUT FILL-IN-AONR = "" THEN DO:
      MESSAGE "Fältet " + LC(Guru.Konstanter:gaol) + " inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.    
   IF FILL-IN-AONR = INPUT FILL-IN-AONR AND 
   FILL-IN-DELNR = INPUT FILL-IN-DELNR THEN DO:
      musz = FALSE.
   END.
   ELSE DO:
      musz = TRUE.
   END.                                  
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.     
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 1
         soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
         soktemp.SOKCHAR[2] = personaltemp.PERSONALKOD
         soktemp.SOKCHAR[3] = utsokaonr.PRISTYP
         soktemp.SOKCHAR[4] = personaltemp.BEFATTNING 
         soktemp.SOKDATE[1] = TODAY.
         {SOKANROP.I}
         ASSIGN
         FILL-IN-PRIS = soktemp.SOKDECI[1].
         DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
      ASSIGN
      FILL-IN-PRISTYP = utsokaonr.PRISTYP
      CMB_TRAK = utsokaonr.TRAKTAMENTE
      FILL-IN-UTRYCK = utsokaonr.UTRYCKNING     
      CMB_PRISTYP = FILL-IN-PRISTYP. 
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR CMB_TRAK /*FILL-IN-UTRYCK */
      WITH FRAME {&FRAME-NAME}.   
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OVER DIALOG-1
ON LEAVE OF FILL-IN-OVER IN FRAME DIALOG-1 /* Övertiduttag */
DO:
  FILL-IN-OVER = INPUT FILL-IN-OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PLUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PLUS DIALOG-1
ON LEAVE OF FILL-IN-PLUS IN FRAME DIALOG-1 /* Plus */
DO:
   FILL-IN-PLUS = INPUT FILL-IN-PLUS.
   IF FILL-IN-START + FILL-IN-PLUS  > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.                                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PLUS DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-PLUS IN FRAME DIALOG-1 /* Plus */
DO:
   klocka = INPUT FILL-IN-START.  
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-START = klocka.
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT DIALOG-1
ON LEAVE OF FILL-IN-SLUT IN FRAME DIALOG-1 /* Arbetets slut */
DO:
  
   FILL-IN-SLUT = INPUT FILL-IN-SLUT.
   IF FILL-IN-SLUT > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
    IF SUBSTRING(STRING(FILL-IN-SLUT,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
   IF FILL-IN-SLUT = FILL-IN-START THEN DO:
      MESSAGE "Start och slut kan ej vara lika." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.                                  
   ASSIGN
   regstart = FILL-IN-START
   regslut = FILL-IN-SLUT.
   IF FILL-IN-START > FILL-IN-SLUT THEN DO:
      MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   regvnr = FILL-IN-VECKO.
   regdagnamn = CMB_DAG.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.  
   IF utryckningtemp.HALV = TRUE THEN DO:       
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD USE-INDEX
      PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE AND
      (CMB_OVERUT = "FLEX" OR FILL-IN-OVER = "F") THEN musz = musz. 
      ELSE IF (CMB_OVERUT = "EJÖV" OR FILL-IN-OVER = "I") THEN musz = musz.                       
      ELSE DO:
         regvnr = FILL-IN-VECKO.
         regdagnamn = CMB_DAG.
         regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
         RUN REGVEC.P.
         {SLUTARBW.I} 
         IF FILL-IN-START < regstart AND FILL-IN-SLUT > regslut THEN DO:
            ASSIGN nytid = FILL-IN-START.
            RUN TIMSEK.P.
            ASSIGN overant = sekunder
            nytid = regstart.
            RUN TIMSEK.P.
            ASSIGN overant = sekunder - overant
            nytid = FILL-IN-SLUT.
            RUN TIMSEK.P.
            ASSIGN seku = sekunder
            nytid = regslut.
            RUN TIMSEK.P.
            overant = overant + seku - sekunder.            
         END.
         ELSE DO: 
            IF FILL-IN-SLUT > regslut AND  FILL-IN-START < regslut THEN nytid = regslut.
            ELSE nytid = FILL-IN-START.
            RUN TIMSEK.P.
            overant = sekunder. 
            IF FILL-IN-START < regstart AND  FILL-IN-SLUT > regstart THEN nytid = regstart.
            ELSE nytid = FILL-IN-SLUT.
            RUN TIMSEK.P.
            overant = sekunder - overant.
         END.   
         halvkvart = 1800.         
         otim = TRUNCATE(overant / halvkvart ,0).
         otim2 = overant - (otim * halvkvart).      
         IF FILL-IN-UTRYCK = TRUE AND overant > utryckningtemp.UTRYCKNBER AND 
         otim2 > 0 AND FILL-IN-SLUT > regslut THEN DO:            
            MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END. 
         IF FILL-IN-UTRYCK = TRUE AND overant > utryckningtemp.UTRYCKNBER AND
         otim2 > 0 AND FILL-IN-START < regstart THEN DO:            
            MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
         IF FILL-IN-UTRYCK = FALSE AND otim2 > 0 AND FILL-IN-SLUT > regslut
         THEN DO:            
            MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.             
            APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
         IF FILL-IN-UTRYCK = FALSE AND otim2 > 0 AND FILL-IN-START < regstart 
         THEN DO:            
            MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.  
            APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.   
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUT IN FRAME DIALOG-1 /* Arbetets slut */
DO:
   klocka = INPUT FILL-IN-SLUT.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN-SLUT = klocka.
   DISPLAY FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-1
ON LEAVE OF FILL-IN-START IN FRAME DIALOG-1 /* Arbetets start */
DO:
   FILL-IN-START = INPUT FILL-IN-START.
   IF FILL-IN-START > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.                                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-START IN FRAME DIALOG-1 /* Arbetets start */
DO:
   klocka = INPUT FILL-IN-START.  
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-START = klocka.
   DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TRAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TRAKT DIALOG-1
ON LEAVE OF FILL-IN-TRAKT IN FRAME DIALOG-1 /* Trakt.zon */
DO:
   FILL-IN-TRAKT = INPUT  FILL-IN-TRAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTRYCK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRYCK DIALOG-1
ON LEAVE OF FILL-IN-UTRYCK IN FRAME DIALOG-1 /* utryckningtemp */
DO:
   FILL-IN-UTRYCK = INPUT  FILL-IN-UTRYCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRYCK DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-UTRYCK IN FRAME DIALOG-1 /* utryckningtemp */
DO:
   IF INPUT FILL-IN-UTRYCK = TRUE THEN FILL-IN-UTRYCK = FALSE.
   IF INPUT FILL-IN-UTRYCK = FALSE THEN FILL-IN-UTRYCK = TRUE.
   DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}. 
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
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AONRS).
   RUN fillinupdate_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_NODF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NODF DIALOG-1
ON LEAVE OF FILL-IN_NODF IN FRAME DIALOG-1 /* Nödfall */
DO:
   FILL-IN_NODF = INPUT FILL-IN_NODF. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NODF DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_NODF IN FRAME DIALOG-1 /* Nödfall */
DO:
   IF INPUT FILL-IN_NODF = TRUE THEN FILL-IN_NODF = FALSE. 
   IF INPUT FILL-IN_NODF = FALSE THEN FILL-IN_NODF = TRUE.
   DISPLAY FILL-IN_NODF WITH FRAME {&FRAME-NAME}. 
   
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
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORTS).
   RUN fillinupdate_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_RESMAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_RESMAL DIALOG-1
ON LEAVE OF FILL-IN_RESMAL IN FRAME DIALOG-1 /* Kommentar */
DO:
  FILL-IN_RESMAL = INPUT FILL-IN_RESMAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VECKOVILA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VECKOVILA DIALOG-1
ON LEAVE OF FILL-IN_VECKOVILA IN FRAME DIALOG-1 /* Veckovila */
DO:
   FILL-IN_VECKOVILA = INPUT FILL-IN_VECKOVILA. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VECKOVILA DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_VECKOVILA IN FRAME DIALOG-1 /* Veckovila */
DO:
   IF INPUT FILL-IN_VECKOVILA = TRUE THEN FILL-IN_VECKOVILA = FALSE. 
   IF INPUT FILL-IN_VECKOVILA = FALSE THEN FILL-IN_VECKOVILA = TRUE.
   DISPLAY FILL-IN_VECKOVILA WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF RAD_FAST = FALSE THEN DO:
      CMB_OMR = sparomrade.
      CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   END.  
   IF  Guru.Konstanter:globforetag = "ELPA" OR  Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF RAD_FAST = TRUE THEN DO:
         ASSIGN 
         sparomrade = CMB_OMR. 
         CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.      
      END.
   END.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF AVAILABLE utsokaonr THEN DO:   
      status-ok = BRW_AONR:DESELECT-FOCUSED-ROW() NO-ERROR.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   FILL-IN_NODF = FALSE.  
   FILL-IN_VECKOVILA = FALSE.
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   persrec = tidpers.TIDPERSREC.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.      
   FIND tidallt WHERE RECID(tidallt) = tidtabrec2 NO-LOCK NO-ERROR.   
   FOR EACH automregtemp USE-INDEX PRISTYPER NO-LOCK:
      status-ok = CMB_PRISTYP:ADD-LAST(automregtemp.PRISTYP).
   END.   
   IF  Guru.Konstanter:globforetag = "SOLE" THEN DO:
      status-ok = CMB_TRAK:ADD-LAST("02").
   END.     
   RUN anst_UI. 
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").     
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   {ANVAVDSO.I}     
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.      
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   FILL-IN-MANAD = regmannamn.
   RUN grundtid_UI.      
   RUN enable_UI.       
   DISABLE FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
   FILL-IN-UTRYCK:HIDDEN = TRUE.   
   DISABLE FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
   FILL-IN_NODF:HIDDEN = TRUE.
   DISABLE FILL-IN_VECKOVILA WITH FRAME {&FRAME-NAME}.
   FILL-IN_VECKOVILA:HIDDEN = TRUE.
   DISABLE CMB_OVERUT CMB_BEF WITH FRAME {&FRAME-NAME}.
   ASSIGN
   CMB_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_OVERUT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   DISABLE FILL-IN-DATUM BTN_FVE BTN_NVE FILL-IN-SLUT CMB_DAG WITH FRAME {&FRAME-NAME}.   
   ASSIGN    
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE.        
   {musarrow.i}
   ASSIGN
   CMB_PRISTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   utsokaonr.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok
   utsokaonr.OMRADE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gomrk
   CMB_PRISTYP:LABEL = Guru.Konstanter:gdebk    
   FILL-IN-PRISTYP:LABEL = Guru.Konstanter:gdebk
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:".
   {FRMSIZED.I}
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   {AVVBEFW.I} 
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).     
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.  
    RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE). 
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
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.     
   FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.         
   
   
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
   musz = FALSE.
   FIND tidallt WHERE RECID(tidallt) = tidtabrec2 NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-DATUM = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM.               
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN
   CMB_DAG = regdagnamn
   FILL-IN-VECKO = regvnr.                
   ASSIGN
   musz = FALSE   
   CMB_PRISTYP = INPUT CMB_PRISTYP  
   FILL-IN-PRISTYP = CMB_PRISTYP 
   FILL-IN-PRIS = INPUT FILL-IN-PRIS.    
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.     
   CMB_OVERUT = INPUT CMB_OVERUT.
   IF CMB_OVERUT = "Komp" THEN FILL-IN-OVER = "K".
   IF CMB_OVERUT = "Över" THEN FILL-IN-OVER = "Ö".
   IF CMB_OVERUT = "Flex" THEN FILL-IN-OVER = "F". 
   IF CMB_OVERUT = "Ejöv" THEN FILL-IN-OVER = "I". 
   
   ASSIGN
   FILL-IN-SLUT = INPUT FILL-IN-SLUT
   FILL-IN-START = INPUT FILL-IN-START
   FILL-IN-TRAKT = INPUT CMB_TRAK
   FILL-IN_RESMAL = INPUT FILL-IN_RESMAL 
   regdatumspar = regdatum.  
   FILL-IN_VIBEFATTNING = INPUT FILL-IN_VIBEFATTNING.
   ASSIGN FILL-IN-PLUS = INPUT FILL-IN-PLUS.
   IF FILL-IN-PLUS > 0 THEN DO:
      FIND FIRST tidallt WHERE RECID(tidallt) = tidtabrec2 NO-LOCK NO-ERROR.         
      nytid = tidallt.START.
      RUN TIMSEK.P.
      regstartsek = sekunder.
      ASSIGN nytid = FILL-IN-PLUS.
      RUN TIMSEK.P.
      RUN PLUSTIDW.P (INPUT pkod).
      ASSIGN FILL-IN-START = nytid.           
   END.       
                     
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:                          
      {AOKOLLERS.I} 
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
   END.
   RUN REGVEC.P.
   {SLUTARBW.I}
   IF FILL-IN-PRISTYP = "FRÅNVARO." THEN DO:
      IF FILL-IN-START GE regslut OR FILL-IN-SLUT LE regstart 
      OR FILL-IN-SLUT > regslut OR FILL-IN-START < regstart THEN DO:
         MESSAGE "Övertid kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
         status-mus2 = SESSION:SET-WAIT-STATE("").
          APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.    
      END.    
   END.      
   IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
      IF  FILL-IN-OVER = "I" OR FILL-IN-OVER = "F" THEN musz = musz.
      ELSE DO:
         MESSAGE "Personen har inte övertidersättning eller restidsersättning" VIEW-AS ALERT-BOX.             
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO CMB_OVERUT IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.    
      END.    
   END.
   regdatum = regdatumspar.  
   IF FILL-IN-START > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF kontrollstart >= FILL-IN-START THEN DO:
      MESSAGE "Denna registrering kan inte börja före den du delar upp." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN-START > FILL-IN-SLUT THEN DO:
      MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.     
   END.
   
   ELSE DO:         
      ASSIGN
      regstart = FILL-IN-START
      vart = "AND".
      RUN TIDSTARTW.P (INPUT pkod,INPUT tidallt.RECTIDVIS).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            FOR EACH felmeddtemp:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
               DELETE felmeddtemp.
            END.
         END.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN NO-APPLY.
      END.      
      IF FILL-IN-SLUT > 24.00 THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-SLUT IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END. 
      IF SUBSTRING(STRING(FILL-IN-SLUT,"99.99"),4 ,2) > "59" THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-SLUT IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.    
      IF FILL-IN-SLUT = FILL-IN-START THEN DO:
         MESSAGE "Start och slut kan ej vara lika." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.                       
      ASSIGN
      regstart = FILL-IN-START
      regslut = FILL-IN-SLUT.
      IF FILL-IN-START > FILL-IN-SLUT THEN DO:
         MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.      
      RUN TIDSLUTW.P (INPUT pkod,INPUT tidallt.RECTIDVIS).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            FOR EACH felmeddtemp:
               MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
               DELETE felmeddtemp.
            END.
         END.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.      
   END.
          
   FIND FIRST befattningstemp WHERE befattningstemp.NAMN = CMB_BEF NO-LOCK NO-ERROR.      
   {SOKSTART.I}
    ASSIGN
    soktemp.SOKVAL = 1
    soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
    soktemp.SOKCHAR[2] = pkod
    soktemp.SOKCHAR[4] = befattningstemp.BEFATTNING 
    soktemp.SOKDATE[1] = regdatum.
    soktemp.SOKCHAR[3] = FILL-IN-PRISTYP.    
    {SOKANROP.I}
    ASSIGN
    FILL-IN-PRIS = soktemp.SOKDECI[1].  
   IF musz = TRUE THEN DO:
      musz = FALSE. /*FIXA ÄNDRA PRIS*/
   END.     
   ELSE DO:     
   /*DEN GAMLA*/
      FIND tidallt WHERE RECID(tidallt) = tidtabrec2 EXCLUSIVE-LOCK NO-ERROR.     
      /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
      ASSIGN SUBSTRING(tidallt.PROGRAM,1,158) = "ANDTIDDEL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv 
       
      tidallt.SLUT = FILL-IN-START         
      bustart3 = tidallt.START
      regvnr = FILL-IN-VECKO     
      regdagnamn = CMB_DAG.
      ASSIGN      
      
      nytid = tidallt.START.
      RUN TIMSEK.P.
      regstartsek = sekunder.
      nytid = tidallt.SLUT.
      RUN TIMSEK.P.          
      ASSIGN
      regdatumspar = regdatum
      regslutsek = sekunder
      regdatum = tidallt.DATUM.
      RUN TOTTIDW.P (INPUT pkod).
      ASSIGN tidallt.TOTALT = klock100(nytid).                      
      regdatum = DATE((regmnr),FILL-IN-DATUM,regar).             
      FIND tidallt WHERE RECID(tidallt) = tidtabrec2 NO-LOCK NO-ERROR. 
      /*DEN NYA*/          
      regdatum = regdatumspar.
      CREATE tidbuff.            
      ASSIGN
      tidbuff.ANVANDARE = tidallt.ANVANDARE 
      tidbuff.VECKONUMMER = tidallt.VECKONUMMER 
      tidbuff.VECKOKORD = tidallt.VECKOKORD 
      tidbuff.UTRYCKNING = tidallt.UTRYCKNING 
      tidbuff.TRAKTTOT = tidallt.TRAKTTOT 
      tidbuff.TRAKTKOD = "" 
      tidbuff.TRAKTAUTO = tidallt.TRAKTAUTO 
      tidbuff.TRAKTANTAL = tidallt.TRAKTANTAL 
      tidbuff.TRAKTAMENTE = 0 
      tidbuff.TOTALT = klock100(tidallt.TOTALT) 
      tidbuff.TIDLOG = tidallt.TIDLOG 
      tidbuff.AONR = FILL-IN-AONR 
      tidbuff.BERANTAL = tidallt.BERANTAL 
      tidbuff.BERBEORD = tidallt.BERBEORD 
      tidbuff.BEREDSKAP = tidallt.BEREDSKAP 
      tidbuff.BEREDSKAPSLUT = tidallt.BEREDSKAPSLUT 
      tidbuff.BEREDSKAPSTART = tidallt.BEREDSKAPSTART 
      tidbuff.BILFORARE = tidallt.BILFORARE 
      tidbuff.DAG = tidallt.DAG 
      tidbuff.DATUM = tidallt.DATUM 
      tidbuff.DELNR = FILL-IN-DELNR 
      tidbuff.ENFLERDAGS = tidallt.ENFLERDAGS 
      tidbuff.GODKAND = tidallt.GODKAND 
      tidbuff.LAGANTAL = tidallt.LAGANTAL 
      tidbuff.LAGBAS = tidallt.LAGBAS 
      tidbuff.LONAUTO = tidallt.LONAUTO 
      tidbuff.LONTILLAGG = tidallt.LONTILLAGG 
      tidbuff.LONTILLANTAL = tidallt.LONTILLANTAL 
      tidbuff.NODF = tidallt.NODF 
      tidbuff.OANT1 = tidallt.OANT1 
      tidbuff.OANT2 = tidallt.OANT2 
      tidbuff.OANT3 = tidallt.OANT3 
      tidbuff.OKOD1 = tidallt.OKOD1 
      tidbuff.OKOD2 = tidallt.OKOD2 
      tidbuff.OKOD3 = tidallt.OKOD3 
      tidbuff.OSL1 = tidallt.OSL1 
      tidbuff.OSL2 = tidallt.OSL2 
      tidbuff.OSL3 = tidallt.OSL3 
      tidbuff.OST1 = tidallt.OST1 
      tidbuff.OST2 = tidallt.OST2 
      tidbuff.OST3 = tidallt.OST3 
      tidbuff.OVERANTAL = tidallt.OVERANTAL 
      tidbuff.OVERAUTO    = tidallt.OVERAUTO 
      tidbuff.OVERTIDTILL = tidallt.OVERTIDTILL 
      tidbuff.OVERTIDUTTAG = tidallt.OVERTIDUTTAG 
      tidbuff.PERSONALKOD = tidallt.PERSONALKOD 
      tidbuff.PRIS = FILL-IN-PRIS
      tidbuff.PRISTYP = FILL-IN-PRISTYP 
      tidbuff.VIBEFATTNING = FILL-IN_VIBEFATTNING
      tidbuff.PROGRAM = tidallt.PROGRAM 
      tidbuff.RECTIDVIS = tidallt.RECTIDVIS 
      tidbuff.RESMAL = tidallt.RESMAL 
      tidbuff.SLUT = FILL-IN-SLUT
      tidbuff.START = FILL-IN-START
      tidbuff.DEBET = FALSE
      tidbuff.SKICKA = TRUE.                      
      nytid = tidbuff.START.
      RUN TIMSEK.P.
      regstartsek = sekunder.
      nytid = tidbuff.SLUT.
      RUN TIMSEK.P.          
      ASSIGN
      regdatumspar = regdatum
      regslutsek = sekunder
      regdatum = tidbuff.DATUM.
      RUN TOTTIDW.P (INPUT pkod).
      ASSIGN tidbuff.TOTALT = klock100(nytid).      
   END.      
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
  DISPLAY FILL-IN-PKOD FILL-IN-DAG FILL-IN-AONR FILL-IN-DELNR FILL-IN-START 
          FILL-IN-PLUS FILL-IN-SLUT FILL-IN-TEXT CMB_OMR FILL-IN-SKP CMB_AVD 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-AONR FILL-IN-DELNR FILL-IN-START FILL-IN-PLUS FILL-IN-SLUT 
         BTN_REG BTN_AVB CMB_OMR CMB_AVD RECT-22 
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
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}. 
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
   ASSIGN  FILL-IN-DATUM = DAY(tidallt.DATUM).
   DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.         
   ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = tidallt.PRISTYP.           
   ASSIGN
   CMB_PRISTYP = INPUT CMB_PRISTYP
   regdatum = tidallt.DATUM.
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
   ASSIGN
   regvnr = tidallt.VECKONUMMER
   FILL-IN-VECKO = tidallt.VECKONUMMER
   FILL-IN-AONR = tidallt.AONR
   FILL-IN-DELNR = tidallt.DELNR
   CMB_TRAK:SCREEN-VALUE = STRING(tidallt.TRAKTAMENTE)   
   CMB_TRAK = INPUT CMB_TRAK
   FILL-IN-PRISTYP = tidallt.PRISTYP
   FILL-IN-PRIS = tidallt.PRIS
   kontrollstart = tidallt.START
   FILL-IN-START = tidallt.START    
   FILL-IN-SLUT = tidallt.SLUT
   FILL-IN-OVER = tidallt.OVERTIDUTTAG     
   FILL-IN-UTRYCK = tidallt.UTRYCKNING
   FILL-IN_RESMAL = tidallt.RESMAL.             
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   IF FILL-IN-OVER = "K" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Komp".
   IF FILL-IN-OVER = "Ö" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Över".
   IF FILL-IN-OVER = "F" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Flex".     
   IF FILL-IN-OVER = "I" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Ejöv".
       
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
   {NYCOL.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytolk_UI DIALOG-1 
PROCEDURE nytolk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   tidtabrecspar = tidtabrec2.   
   EMPTY TEMP-TABLE tidapptemp NO-ERROR.    
   CREATE tidapptemp.
   ASSIGN
   tidapptemp.FORETAG =  Guru.Konstanter:globforetag
   tidapptemp.ANVANDARE = Guru.Konstanter:globanv
   tidapptemp.RECPERS = persrec
   tidapptemp.RECTID = tidtabrec2
   tidapptemp.DATUM = regdatum.              
   {TIDUPPINW.I}
   {FELTEXINUT.I}
   musz = FALSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyupp_UI DIALOG-1 
PROCEDURE nyupp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock100 DIALOG-1 
FUNCTION klock100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

