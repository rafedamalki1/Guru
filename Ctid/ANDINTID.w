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
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR extratidallt.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{TIDAPPDEF.I}
&Scoped-define NEW
{PERBEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{UPPGHMT.I}
{OMRTEMPW.I}

&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
{PHMT.I}
{SOKDEF.I}
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE nyrec AS RECID NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
/* DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO. */
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE regbtn AS LOGICAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE halvkvart AS INTEGER NO-UNDO.
DEFINE VARIABLE hjdat AS INTEGER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE nydat AS INTEGER NO-UNDO.
DEFINE VARIABLE datecolh AS HANDLE NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */

DEFINE TEMP-TABLE restemp   
   FIELD DATUM AS INTEGER FORMAT ">9" 
   FIELD SLUT AS DECIMAL
   FIELD START AS DECIMAL
   INDEX DATUM IS PRIMARY DATUM START SLUT.
DEFINE BUFFER tidalltbuff FOR tidallt.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr tidallt

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.AONR utsokaonr.DELNR ~
utsokaonr.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR utsokaonr.AONR 
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


/* Definitions for BROWSE BRW_NYTID                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_NYTID DAY(tidallt.DATUM) @ nydat ~
tidallt.AONR tidallt.DELNR tidallt.START tidallt.SLUT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NYTID tidallt.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_NYTID tidallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_NYTID tidallt
&Scoped-define QUERY-STRING-BRW_NYTID FOR EACH tidallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_NYTID OPEN QUERY BRW_NYTID FOR EACH tidallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_NYTID tidallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NYTID tidallt


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-MANAD FILL-IN-DATUM FILL-IN-AONR ~
FILL-IN-DELNR CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK FILL-IN-START FILL-IN-SLUT ~
CMB_PRISTYP FILL-IN_RESMAL BTN_LAGRA BTN_REG BTN_AVB BTN_NVE BTN_FVE ~
FILL-IN_AONRS FILL-IN_ORTS RAD_FAST CMB_OMR EDITOR-INFO BRW_NYTID CMB_AVD ~
RECT-33 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN-DATUM FILL-IN-DAG ~
FILL-IN-AONR FILL-IN-DELNR CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK FILL-IN-START ~
FILL-IN-SLUT CMB_PRISTYP FILL-IN-PRIS FILL-IN_RESMAL FILL-IN_AONRS ~
FILL-IN_ORTS RAD_FAST CMB_OMR EDITOR-INFO FILL-IN-AO CMB_AVD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_LAGRA 
     LABEL "Lagra i buffert" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avvik. bef." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DAG AS CHARACTER FORMAT "X(3)":U 
     LABEL "Dag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "mån ","tis","ons","tor","fre","lör","sön" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
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

DEFINE VARIABLE EDITOR-INFO AS CHARACTER 
     VIEW-AS EDITOR
     SIZE 23.13 BY 3.25 NO-UNDO.

DEFINE VARIABLE FILL-IN-AO AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY .92 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-OVER AS CHARACTER FORMAT "X(1)":U 
     LABEL "Övertiduttag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL ? 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     LABEL "Debitering" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUT AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets slut" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets start" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRAKT AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Trakt-zon" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTRYCK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Utryckning" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALFA AS CHARACTER FORMAT "X(256)":U INITIAL "Välj befattning" 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALFA-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Vald befattning" 
     VIEW-AS FILL-IN 
     SIZE 16.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VECKO AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Veckonummer" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_NODF AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Nödfall" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 16.75 BY 1.

DEFINE VARIABLE FILL-IN_VIBEFATTNING AS CHARACTER FORMAT "x(256)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 37 BY 1.17 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 44.13 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_NYTID FOR 
      tidallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(30)":U
  ENABLE
      utsokaonr.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 44.13 BY 13
         TITLE "Aktiva arbetsordernummer".

DEFINE BROWSE BRW_NYTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NYTID DIALOG-1 _STRUCTURED
  QUERY BRW_NYTID DISPLAY
      DAY(tidallt.DATUM) @ nydat COLUMN-LABEL "Datum"
      tidallt.AONR COLUMN-LABEL "Aonr" FORMAT "X(9)":U
      tidallt.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
      tidallt.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
      tidallt.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
  ENABLE
      tidallt.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 37.25 BY 13
         TITLE "Nya tidregistreringar" TOOLTIP "Välj en registrering för ändring. Dubbel-klicka för bortag.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_AONR AT ROW 6.38 COL 36.5
     FILL-IN-PKOD AT ROW 3.25 COL 16 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 4.58 COL 16 COLON-ALIGNED NO-LABEL
     FILL-IN-VECKO AT ROW 4.58 COL 16 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 5.83 COL 16 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DAG AT ROW 5.83 COL 30.13 NO-LABEL
     FILL-IN-AONR AT ROW 7.25 COL 16 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DELNR AT ROW 8.58 COL 16 COLON-ALIGNED AUTO-RETURN 
     CMB_TRAK AT ROW 9.88 COL 16 COLON-ALIGNED
     CMB_DAG AT ROW 5.88 COL 16 COLON-ALIGNED
     CMB_OVERUT AT ROW 11.25 COL 16 COLON-ALIGNED
     FILL-IN-UTRYCK AT ROW 12.63 COL 16 COLON-ALIGNED
     FILL-IN-START AT ROW 13.96 COL 16 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-SLUT AT ROW 15.25 COL 16 COLON-ALIGNED AUTO-RETURN 
     CMB_PRISTYP AT ROW 16.58 COL 16 COLON-ALIGNED
     FILL-IN-PRISTYP AT ROW 12.71 COL 61.13 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 17.96 COL 16 COLON-ALIGNED
     FILL-IN_NODF AT ROW 23.71 COL 16.13 COLON-ALIGNED
     FILL-IN_RESMAL AT ROW 25.04 COL 16.13 COLON-ALIGNED
     FILL-IN-OVER AT ROW 23.21 COL 104.75 COLON-ALIGNED HELP
          "ÖVERTID = Ö, KOMP = K, INGEN ERSÄTTNING = I"
     FILL-IN-TRAKT AT ROW 21.83 COL 104.75 COLON-ALIGNED
     BTN_LAGRA AT ROW 26.75 COL 75.75
     BTN_REG AT ROW 26.75 COL 90.75
     BTN_AVB AT ROW 26.75 COL 105.75
     BTN_NVE AT ROW 5.63 COL 26.88
     BTN_FVE AT ROW 6.46 COL 26.88
     FILL-IN_AONRS AT ROW 19.75 COL 47 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 19.75 COL 65 COLON-ALIGNED
     RAD_FAST AT ROW 1.5 COL 43 NO-LABEL
     CMB_OMR AT ROW 5.13 COL 45.25 COLON-ALIGNED NO-LABEL
     EDITOR-INFO AT ROW 2.75 COL 90 NO-LABEL
     FILL-IN-AO AT ROW 2.83 COL 45.25 COLON-ALIGNED NO-LABEL
     BRW_NYTID AT ROW 6.38 COL 82.5
     CMB_BEF AT ROW 22.33 COL 16.13 COLON-ALIGNED
     FILL-IN_VIBEFATTNING AT ROW 22.33 COL 35.63 COLON-ALIGNED HELP
          "BEFATTNINGSKOD" NO-LABEL
     FILL-IN-VALFA AT ROW 21.29 COL 16.13 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 3.83 COL 45.25 COLON-ALIGNED NO-LABEL
     FILL-IN-VALFA-2 AT ROW 21.33 COL 35.63 COLON-ALIGNED NO-LABEL
     "Sök på:" VIEW-AS TEXT
          SIZE 7 BY .83 AT ROW 19.75 COL 37
     RECT-33 AT ROW 19.63 COL 36.5
     SPACE(39.99) SKIP(7.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: ? T "?" NO-UNDO temp-db tidallt
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_AONR 1 DIALOG-1 */
/* BROWSE-TAB BRW_NYTID FILL-IN-AO DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DIALOG-1           = TRUE.

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

/* SETTINGS FOR FILL-IN FILL-IN-AO IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-OVER IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OVER:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PRIS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PRISTYP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-PRISTYP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TRAKT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TRAKT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALFA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VALFA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VALFA-2 IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VALFA-2:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VECKO IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VECKO:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NODF IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NODF:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VIBEFATTNING IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_VIBEFATTNING:HIDDEN IN FRAME DIALOG-1           = TRUE.

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
     _FldNameList[1]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" "x(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NYTID
/* Query rebuild information for BROWSE BRW_NYTID
     _TblList          = "Temp-Tables.tidallt"
     _FldNameList[1]   > "_<CALC>"
"DAY(tidallt.DATUM) @ nydat" "Datum" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tidallt.AONR
"tidallt.AONR" "Aonr" "X(9)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tidallt.DELNR
"tidallt.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tidallt.START
"tidallt.START" "Start!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.tidallt.SLUT
"tidallt.SLUT" "Slut!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NYTID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
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
   APPLY "GO" TO FRAME {&FRAME-NAME}.
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
   RUN valao_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NYTID
&Scoped-define SELF-NAME BRW_NYTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NYTID DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_NYTID IN FRAME DIALOG-1 /* Nya tidregistreringar */
DO:   
   status-ok = BRW_NYTID:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   nyrec = RECID(tidallt).     
   IF tidallt.RECTIDVIS NE ? THEN DO:
      MESSAGE "Denna post går ej att ta bort. Använd bortag."
      VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      MESSAGE "Vill du ta bort denna registrering ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bort AS LOGICAL.
      CASE bort:
         WHEN TRUE THEN DO:
            status-ok = BRW_NYTID:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
            IF status-ok = TRUE THEN musz = musz.
            ELSE DO:          
               status-ok = BRW_NYTID:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.                  
            END.
            FIND tidallt WHERE  RECID(tidallt) = nyrec NO-ERROR. 
            DELETE tidallt.
         END.
      END CASE.
      RUN refreshbrw_UI IN brwproc[2].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NYTID DIALOG-1
ON VALUE-CHANGED OF BRW_NYTID IN FRAME DIALOG-1 /* Nya tidregistreringar */
DO:
   RUN brwnytid_UI.    
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
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.   
   RUN REGVEC.P.
   {SLUTARBW.I}
   ASSIGN
   FILL-IN-START = regstart
   FILL-IN-SLUT = regslut.
   DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LAGRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LAGRA DIALOG-1
ON CHOOSE OF BTN_LAGRA IN FRAME DIALOG-1 /* Lagra i buffert */
DO:
   regbtn = TRUE.
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   regbtn = FALSE.
   {musarrow.i} 
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
   IF regmnr = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE(regmnr + 1,01,regar) - 1.
   END.
   IF FILL-IN-DATUM > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   FILL-IN-DAG = regdagnamn.
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
   RUN REGVEC.P.
   {SLUTARBW.I}
   ASSIGN
   FILL-IN-START = regstart
   FILL-IN-SLUT = regslut.
   DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i} 
   ASSIGN
   vart = "AND"
   musz = FALSE
   regbtn = TRUE.
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   regbtn = FALSE.
   RUN nytemp_UI.           
   IF musz = TRUE THEN musz = FALSE.
   ELSE APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON LEAVE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD DIALOG-1
ON VALUE-CHANGED OF CMB_AVD IN FRAME DIALOG-1
DO:
   omravdand = 1.
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BEF DIALOG-1
ON VALUE-CHANGED OF CMB_BEF IN FRAME DIALOG-1 /* Avvik. bef. */
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
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
   USE-INDEX OMRNAMN NO-LOCK NO-ERROR.      
   omravdand = 2.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].

   RUN ngnkey_UI.
   {musarrow.i} 
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
      ASSIGN
      sok4 = pkod
      sok3 = FILL-IN-PRISTYP.
      RUN nyupp_UI (INPUT 3).      
      FILL-IN-PRIS = sok5.   
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
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   musz = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      aonrrec = RECID(utsokaonr).
      RAD_FAST = utsokaonr.FASTAAONR.
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
         IF utsokaonr.OMRADE = "" THEN DO:
            ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
         END.
      END.
   END.
   ELSE DO:
      aonrrec = 0.
      RAD_FAST = FALSE.
   END.
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
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN
   BRW_AONR:HIDDEN = FALSE    
   RAD_FAST:HIDDEN = FALSE      
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   RUN inaonr_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON ANY-KEY OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:   
   /*ccc*/
   ASSIGN
   hjdat = FILL-IN-DATUM.
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.            
   IF MONTH(regdatum) = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE((regmnr + 1),01,regar) - 1.
   END.
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
   IF hjdat NE FILL-IN-DATUM THEN DO:
       RUN REGVEC.P.
       {SLUTARBW.I}
       ASSIGN
       FILL-IN-START = regstart
       FILL-IN-SLUT = regslut.
       DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
   END.
   FILL-IN-DAG = regdagnamn.
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON ANY-KEY OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:  
   /*ccc*/
   IF INPUT FILL-IN-AONR = "" THEN DO:
      MESSAGE "Fältet aonr inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.    
   IF FILL-IN-AONR = INPUT FILL-IN-AONR AND FILL-IN-DELNR = INPUT FILL-IN-DELNR THEN DO:
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
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 47
      soktemp.SOKCHAR[1] = FILL-IN-AONR
      soktemp.SOKINT[1] = FILL-IN-DELNR.
      {SOKANROP.I}      
      IF soktemp.SOKCHAR[2] = ? THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.               
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN.      
      END.
      ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.               
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN.      
      END.
  

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
      ASSIGN
      sok1 = FILL-IN-AONR      
      sok2 = FILL-IN-DELNR
      sok4 = pkod.       
      RUN nyupp_UI (INPUT 16).      
      ASSIGN
      FILL-IN-PRISTYP = sok1
      FILL-IN-PRIS = sok5
      CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
      CMB_TRAK = INPUT CMB_TRAK      
      CMB_PRISTYP = FILL-IN-PRISTYP.
      IF sok5 = 1 THEN FILL-IN-UTRYCK = TRUE.
      IF sok5 = 2 THEN FILL-IN-UTRYCK = FALSE.
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
      WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:               
         ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT = 0.
      END.
      ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.   
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         
      END.
      ELSE DO:
         DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MANAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MANAD DIALOG-1
ON ANY-KEY OF FILL-IN-MANAD IN FRAME DIALOG-1
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OVER DIALOG-1
ON ANY-KEY OF FILL-IN-OVER IN FRAME DIALOG-1 /* Övertiduttag */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OVER DIALOG-1
ON LEAVE OF FILL-IN-OVER IN FRAME DIALOG-1 /* Övertiduttag */
DO:
  FILL-IN-OVER = INPUT FILL-IN-OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD DIALOG-1
ON LEAVE OF FILL-IN-PKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRIS DIALOG-1
ON ANY-KEY OF FILL-IN-PRIS IN FRAME DIALOG-1 /* Pris */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PRISTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PRISTYP DIALOG-1
ON ANY-KEY OF FILL-IN-PRISTYP IN FRAME DIALOG-1 /* Debitering */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT DIALOG-1
ON ANY-KEY OF FILL-IN-SLUT IN FRAME DIALOG-1 /* Arbetets slut */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT DIALOG-1
ON LEAVE OF FILL-IN-SLUT IN FRAME DIALOG-1 /* Arbetets slut */
DO: 
   /*ccc*/
   FILL-IN-SLUT = INPUT FILL-IN-SLUT.
   IF FILL-IN-SLUT > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
   IF SUBSTRING(STRING(FILL-IN-SLUT,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   ASSIGN
   regstart = FILL-IN-START
   regslut = FILL-IN-SLUT   
   regvnr = FILL-IN-VECKO
   regdagnamn = CMB_DAG.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.  
   IF utryckningtemp.HALV = TRUE THEN DO:       
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD USE-INDEX
      PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE AND CMB_OVERUT = "FLEX" THEN musz = musz.                         
      ELSE IF CMB_OVERUT = "EJÖV" THEN musz = musz.
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
            MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.            APPLY "ENTRY" TO FILL-IN-START IN FRAME {&FRAME-NAME}.
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
ON ANY-KEY OF FILL-IN-START IN FRAME DIALOG-1 /* Arbetets start */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   ASSIGN
   regstart = FILL-IN-START
   regvnr = FILL-IN-VECKO
   regdagnamn = CMB_DAG.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   IF musz = TRUE THEN DO:
      musz = FALSE.
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
ON ANY-KEY OF FILL-IN-TRAKT IN FRAME DIALOG-1 /* Trakt-zon */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TRAKT DIALOG-1
ON LEAVE OF FILL-IN-TRAKT IN FRAME DIALOG-1 /* Trakt-zon */
DO:
   FILL-IN-TRAKT = INPUT  FILL-IN-TRAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTRYCK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRYCK DIALOG-1
ON ANY-KEY OF FILL-IN-UTRYCK IN FRAME DIALOG-1 /* Utryckning */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRYCK DIALOG-1
ON LEAVE OF FILL-IN-UTRYCK IN FRAME DIALOG-1 /* Utryckning */
DO:
   FILL-IN-UTRYCK = INPUT  FILL-IN-UTRYCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-UTRYCK DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-UTRYCK IN FRAME DIALOG-1 /* Utryckning */
DO:
   IF INPUT FILL-IN-UTRYCK = TRUE THEN FILL-IN-UTRYCK = FALSE.
   IF INPUT FILL-IN-UTRYCK = FALSE THEN FILL-IN-UTRYCK = TRUE.
   DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-VECKO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-VECKO DIALOG-1
ON ANY-KEY OF FILL-IN-VECKO IN FRAME DIALOG-1 /* Veckonummer */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
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
ON ENTRY OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON LEAVE OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AONRS).
   RUN fillinupdate_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_NODF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NODF DIALOG-1
ON ANY-KEY OF FILL-IN_NODF IN FRAME DIALOG-1 /* Nödfall */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   RUN ngnkey_UI.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
ON ENTRY OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
    SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON LEAVE OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORTS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_ORTS IN FRAME DIALOG-1 /* Benämning */
DO:  
   FILL-IN_ORTS = INPUT FILL-IN_ORTS.
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


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF AVAILABLE omrtemp THEN DO:
      musz = musz.
   END.
   ELSE DO:   
      ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF RAD_FAST = FALSE THEN DO:
        IF sparomrade NE "" THEN DO:
            CMB_OMR = sparomrade.
            CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade.
            FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR
            USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
         END.
      END.
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
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   
   RUN huvud_UI. 
   RUN enable_UI.       
   {AVVBEFW.I}
   FILL-IN-VALFA-2 = "Vald befattning".
   IF Guru.Konstanter:varforetypval[3] >= 1 OR Guru.Konstanter:varforetypval[4] = 1 THEN DO: 
      FILL-IN-VALFA-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      DISPLAY FILL-IN-VALFA-2 WITH FRAME {&FRAME-NAME}.  
   END.
   ELSE FILL-IN-VALFA-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:varforetypval[3] = 4 THEN FILL-IN-VALFA-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   {FRMSIZED.I}
   RUN huvud2_UI.
   
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
   utsokaonr.AONR:READ-ONLY IN BROWSE BRW_AONR = TRUE.
   tidallt.AONR:READ-ONLY IN BROWSE BRW_NYTID = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_NYTID:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   RUN settriggerproc_UI IN brwproc[2] (INPUT 1, INPUT "nytidstartsearch_UI").   
   RUN settriggerproc_UI IN brwproc[2] (INPUT 2, INPUT "nytidanyprint_UI").
   datecolh = BRW_NYTID:GET-BROWSE-COLUMN(1) IN FRAME {&FRAME-NAME}.
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
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.     
   
      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwnytid_UI DIALOG-1 
PROCEDURE brwnytid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF AVAILABLE tidallt THEN DO:   
      ASSIGN
      FILL-IN-AONR  = tidallt.AONR 
      FILL-IN-DATUM = DAY(tidallt.DATUM) 
      FILL-IN-DELNR = tidallt.DELNR 
      FILL-IN_NODF = tidallt.NODF 
      FILL-IN-PRIS = tidallt.PRIS 
      CMB_PRISTYP = tidallt.PRISTYP 
      FILL-IN-SLUT = tidallt.SLUT 
      FILL-IN-START = tidallt.START 
      CMB_TRAK = tidallt.TRAKTAMENTE 
      FILL-IN-UTRYCK = tidallt.UTRYCK
      FILL-IN_RESMAL = tidallt.RESMAL
      tidallt.ANDRA = TRUE.
         
      RUN ladda_UI (INPUT 1).
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT = 0.
      nyrec = RECID(tidallt). 
   END.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   FILL-IN-DAG = regdagnamn.
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
   IF utryckningtemp.NODF = TRUE THEN DO:
      DISPLAY FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
      FILL-IN_NODF:HIDDEN = FALSE.
   END.
   DISPLAY FILL-IN-AONR FILL-IN-DATUM FILL-IN-DELNR CMB_OVERUT CMB_PRISTYP 
   FILL-IN_RESMAL FILL-IN-SLUT FILL-IN-START FILL-IN-UTRYCK 
   WITH FRAME {&FRAME-NAME}.      
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:     
   END.
   ELSE DO: 
      DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.      
   END.
   /*PRISFOR*/
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      DISPLAY FILL-IN_VIBEFATTNING CMB_BEF WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
   END.
   IF AVAILABLE tidallt THEN DO:   
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = tidallt.AONR AND 
      utsokaonr.DELNR = tidallt.DELNR USE-INDEX AONR NO-LOCK NO-ERROR. 
      IF AVAILABLE utsokaonr THEN DO:   
         RAD_FAST = utsokaonr.FASTAAONR.
         DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
         IF utsokaonr.AONRAVDATUM = 01/01/1991 THEN DO:
            aonrrec = RECID(utsokaonr).
            IF utsokaonr.OMRADE = "" THEN DO:
               ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
               CMB_OMR = INPUT CMB_OMR.
               DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
            END.
            IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN DO:
               RUN nycolsortprep_UI (INPUT 1).
               RUN openbdynspec_UI IN brwproc[1].
            END.      
            ELSE DO:
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
               USE-INDEX OMR NO-LOCK NO-ERROR.
               IF AVAILABLE omrtemp THEN DO:            
                  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
                  CMB_OMR = INPUT CMB_OMR.
                  DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}. 
                  RUN nycolsortprep_UI (INPUT 1).
                  RUN openbdynspec_UI IN brwproc[1].
               END.
            END.      
         END.
      END.
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
  DISPLAY FILL-IN-PKOD FILL-IN-DATUM FILL-IN-DAG FILL-IN-AONR FILL-IN-DELNR 
          CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK FILL-IN-START FILL-IN-SLUT 
          CMB_PRISTYP FILL-IN-PRIS FILL-IN_RESMAL FILL-IN_AONRS FILL-IN_ORTS 
          RAD_FAST CMB_OMR EDITOR-INFO FILL-IN-AO CMB_AVD 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-MANAD FILL-IN-DATUM FILL-IN-AONR FILL-IN-DELNR CMB_TRAK 
         CMB_OVERUT FILL-IN-UTRYCK FILL-IN-START FILL-IN-SLUT CMB_PRISTYP 
         FILL-IN_RESMAL BTN_LAGRA BTN_REG BTN_AVB BTN_NVE BTN_FVE FILL-IN_AONRS 
         FILL-IN_ORTS RAD_FAST CMB_OMR EDITOR-INFO BRW_NYTID CMB_AVD RECT-33 
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
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
      ELSE DISPLAY FILL-IN-AONR FILL-IN-DELNR CMB_TRAK WITH FRAME {&FRAME-NAME}.   
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
   IF vart = "NYA" THEN DO:
      ASSIGN
      regdagnamn = extratidallt.DAG
      FILL-IN-DATUM = DAY(extratidallt.DATUM).
      RUN REGVEC.P.
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
            FILL-IN-DATUM = DAY(tillochmeddatum + 1).
         END.
      END.
      IF extratidallt.PRISTYP = "RESTID..." THEN DO:
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = extratidallt.AONR AND 
         utsokaonr.DELNR = extratidallt.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = utsokaonr.PRISTYP.
      END.
      ELSE CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = extratidallt.PRISTYP.
      CMB_PRISTYP = INPUT CMB_PRISTYP.
      regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
      RUN REGDAG.P.   
      FILL-IN-DAG = regdagnamn.
      DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.        
      RUN REGVEC.P.
      {SLUTARBW.I}          
      ASSIGN
      FILL-IN-AONR = extratidallt.AONR
      FILL-IN-DELNR = extratidallt.DELNR
      FILL-IN-START = regstart
      FILL-IN-SLUT = regslut
      FILL-IN-UTRYCK = extratidallt.UTRYCKNING
      FILL-IN-TRAKT = extratidallt.TRAKTAMENTE   
      CMB_TRAK:SCREEN-VALUE = STRING(extratidallt.TRAKTAMENTE)   
      CMB_TRAK = INPUT CMB_TRAK
      FILL-IN-OVER = personaltemp.OVERTIDUTTAG          
      FILL-IN-PRISTYP = CMB_PRISTYP.      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT = 0.
      END.
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         ASSIGN
         sok3 = FILL-IN-PRISTYP      
         sok4 = pkod.       
         RUN nyupp_UI (INPUT 3).      
         FILL-IN-PRIS = sok5.
      END.
   END.      
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   IF FILL-IN-OVER = "K" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Komp".
   IF FILL-IN-OVER = "Ö" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Över".
   IF FILL-IN-OVER = "F" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Flex".     
   IF FILL-IN-OVER = "I" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Ejöv".        
   
   CMB_OVERUT = INPUT CMB_OVERUT.
   DISPLAY CMB_OVERUT WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtatid_UI DIALOG-1 
PROCEDURE hamtatid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
   FOR EACH tidallt WHERE tidallt.TIDLOG = TRUE NO-LOCK:
      musz = FALSE.      
      IF tidallt.GODKAND NE "" THEN musz = TRUE.
      IF tidallt.PRISTYP = "RESTID..." THEN DO:
         CREATE restemp.   
         ASSIGN             
         restemp.DATUM = INTEGER(DAY(tidallt.DATUM)) 
         restemp.SLUT = tidallt.SLUT 
         restemp.START = tidallt.START.          
      END.
      tidallt.ANDRA = FALSE.      
   END.
   tempvar = "tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE " + '"' + "RESTID..." + '"'.
   RUN setcolindex_UI IN brwproc[2] (INPUT "DATUM").
   RUN setcolsortvar_UI IN brwproc[2] (INPUT tempvar).
   RUN openbdynspec_UI IN brwproc[2].
   FIND FIRST tidallt WHERE tidallt.DATUM = regdatum AND tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE "RESTID..." AND
   tidallt.START = extratidallt.START AND tidallt.SLUT = extratidallt.SLUT
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidallt THEN FIND FIRST tidallt WHERE tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE "RESTID..." AND
   tidallt.START = extratidallt.START AND tidallt.SLUT = extratidallt.SLUT
   NO-LOCK NO-ERROR.
   IF AVAILABLE tidallt THEN DO:
      nyrec = RECID(tidallt).
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[2].
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI DIALOG-1 
PROCEDURE huvud2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod
   NO-LOCK NO-ERROR. 
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.     
   IF utryckningtemp.UTRYCK1 = FALSE THEN DO:
      DISABLE FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
      FILL-IN-UTRYCK:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ENABLE FILL-IN-UTRYCK  WITH FRAME {&FRAME-NAME}.
      FILL-IN-UTRYCK:HIDDEN = FALSE.
   END.
   DISPLAY FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
   FILL-IN_NODF:HIDDEN = TRUE.   
   IF utryckningtemp.NODF = TRUE THEN DO:
      DISPLAY FILL-IN_NODF  WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN_NODF  WITH FRAME {&FRAME-NAME}.
      FILL-IN_NODF:HIDDEN = FALSE.
   END.   
   ELSE DO:
      DISABLE FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
      FILL-IN_NODF:HIDDEN = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
      DISABLE CMB_TRAK WITH FRAME {&FRAME-NAME}.
      CMB_TRAK:HIDDEN = TRUE.
   END.   
   /*GLOBOMRADE*/
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = tidallt.AONR AND 
   utsokaonr.DELNR = tidallt.DELNR USE-INDEX AONR NO-LOCK NO-ERROR. 
   IF AVAILABLE utsokaonr THEN DO:
      RAD_FAST = utsokaonr.FASTAAONR.
      DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
      IF utsokaonr.AONRAVDATUM = 01/01/1991 THEN DO:
         aonrrec = RECID(utsokaonr).
         IF utsokaonr.OMRADE = "" THEN DO:
            ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
            CMB_OMR = INPUT CMB_OMR.
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
         END.         
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE 
         USE-INDEX OMR NO-LOCK NO-ERROR.
         IF AVAILABLE omrtemp THEN DO:            
            CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
            CMB_OMR = INPUT CMB_OMR.
            DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}. 
               RUN nycolsortprep_UI (INPUT 1).
               RUN openbdynspec_UI IN brwproc[1].
         END.
         ELSE DO:
            RUN nycolsortprep_UI (INPUT 1).
            RUN openbdynspec_UI IN brwproc[1].
         END.
      END.
   END.
   ELSE DO:
      RUN nycolsortprep_UI (INPUT 2).
      RUN openbdynspec_UI IN brwproc[1].
   END.
   DELETE extratidallt.
   DISABLE FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
   /*PRISFOR*/
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
   END.
   ELSE DO:
      ENABLE FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
   END.
   /*GLOBOMRADE*/
   EDITOR-INFO:HIDDEN = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI DIALOG-1 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN    
   FILL-IN_NODF = FALSE.  
   EDITOR-INFO = "Välj en registrering för ändring. Dubbel-klicka för bortag".
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.      
   FIND FIRST extratidallt NO-LOCK NO-ERROR.
   regdatum = extratidallt.DATUM.      
   RUN REGVEC.P.
   RUN REGDAG.P.
   FOR EACH automregtemp USE-INDEX PRISTYPER NO-LOCK:
      IF automregtemp.PRISTYP = "RESTID..." THEN musz = musz.
      ELSE 
      status-ok = CMB_PRISTYP:ADD-LAST(automregtemp.PRISTYP) IN FRAME {&FRAME-NAME}.
      
   END.  
   IF Guru.Konstanter:globforetag = "SOLE" THEN status-ok = CMB_TRAK:ADD-LAST("02").
   RUN anst_UI. 
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.  
   FILL-IN-MANAD = regmannamn.
   /*GLOBOMRADE*/
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").   
   {ANVAVDSO.I}  
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.   

   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   ASSIGN
   CMB_PRISTYP:LABEL = Guru.Konstanter:gdebk    
   FILL-IN-PRISTYP:LABEL = Guru.Konstanter:gdebk
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   utsokaonr.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok
   tidallt.AONR:LABEL IN BROWSE BRW_NYTID = Guru.Konstanter:gaok
   FILL-IN_AONRS:LABEL =  Guru.Konstanter:gaok
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok
   FILL-IN-AO = "Visa " + LC(Guru.Konstanter:gaok) + " för:".
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = personaltemp.OMRADE 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   /*GLOBOMRADE*/
   RUN grundtid_UI.
   DISPLAY RAD_FAST FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.   
   RUN hamtatid_UI.
   SESSION:DATA-ENTRY-RETURN = TRUE.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inaonr_UI DIALOG-1 
PROCEDURE inaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FILL-IN-AONR NE INPUT FRAME {&FRAME-NAME} FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF AVAILABLE utsokaonr THEN DO:        
         ASSIGN
         FILL-IN-AONR = utsokaonr.AONR
         FILL-IN-DELNR = utsokaonr.DELNR
         sok1 = utsokaonr.AONR      
         sok2 = utsokaonr.DELNR
         sok4 = pkod.       
         RUN nyupp_UI (INPUT 16).      
         ASSIGN
         FILL-IN-PRISTYP = sok1
         CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
         CMB_TRAK = INPUT CMB_TRAK      
         CMB_PRISTYP = FILL-IN-PRISTYP.
         IF sok5 = 1 THEN FILL-IN-UTRYCK = TRUE.
         IF sok5 = 2 THEN FILL-IN-UTRYCK = FALSE.
         ASSIGN CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
         WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
            ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT = 0.
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         /*PRISFOR*/
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:      
            FILL-IN-PRIS = sok5.
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.
      END.
      ELSE DO:
         ASSIGN        
         sok1 = FILL-IN-AONR      
         sok2 = FILL-IN-DELNR
         sok4 = pkod.       
         RUN nyupp_UI (INPUT 16).      
         ASSIGN
         FILL-IN-PRISTYP = sok1
         CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
         CMB_TRAK = INPUT CMB_TRAK      
         CMB_PRISTYP = FILL-IN-PRISTYP.
         IF sok5 = 1 THEN FILL-IN-UTRYCK = TRUE.
         IF sok5 = 2 THEN FILL-IN-UTRYCK = FALSE.
         ASSIGN CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
         WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
            ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT = 0.
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         /*PRISFOR*/
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:      
            FILL-IN-PRIS = sok5.
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI DIALOG-1 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   IF vadgora = 1 THEN DO:
      IF tidallt.OVERTIDUTTAG = "K" THEN DO:
         ASSIGN 
         CMB_OVERUT:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Komp". 
         CMB_OVERUT = "Komp".
      END.
      IF tidallt.OVERTIDUTTAG = "Ö" THEN DO:
         ASSIGN 
         CMB_OVERUT:SCREEN-VALUE = "Över"
         CMB_OVERUT = "Över".
      END.
      IF tidallt.OVERTIDUTTAG = "F" THEN DO:
         ASSIGN 
         CMB_OVERUT:SCREEN-VALUE = "Flex"
         CMB_OVERUT = "Flex".
      END.
      IF tidallt.OVERTIDUTTAG = "I" THEN DO:
         ASSIGN 
         CMB_OVERUT:SCREEN-VALUE = "Ejöv".        
         CMB_OVERUT = "Ejöv".
      END.
      
   END.
   IF vadgora = 2 THEN DO:
      IF CMB_OVERUT = "Komp"  THEN DO:
         ASSIGN 
         tidallt.OVERTIDUTTAG = "K".
      END.
      IF CMB_OVERUT = "Över"  THEN DO:
         ASSIGN 
         tidallt.OVERTIDUTTAG = "Ö".
      END.
      IF CMB_OVERUT:SCREEN-VALUE = "Flex"  THEN DO:
         ASSIGN 
         tidallt.OVERTIDUTTAG = "F".     
      END.
      IF CMB_OVERUT:SCREEN-VALUE = "Ejöv" THEN DO:
         ASSIGN 
         tidallt.OVERTIDUTTAG = "I".        
      END.
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ngnkey_UI DIALOG-1 
PROCEDURE ngnkey_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i} 
   {TRYCKS.I}   
   ASSIGN
   FILL-IN-SLUT = INPUT FRAME {&FRAME-NAME} FILL-IN-SLUT
   FILL-IN-START = INPUT FILL-IN-START
   regstart = FILL-IN-START
   regslut = FILL-IN-SLUT.
   IF FILL-IN-SLUT = FILL-IN-START THEN RETURN.
   IF KEYFUNCTION(LASTKEY) = ("RETURN") OR regbtn = TRUE THEN DO:      
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         FILL-IN-PRIS = INPUT FILL-IN-PRIS.   
      END.
      ASSIGN
      FILL-IN-DATUM = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM                  
      FILL-IN_RESMAL = INPUT FILL-IN_RESMAL
      CMB_PRISTYP = INPUT CMB_PRISTYP        
      FILL-IN_NODF = INPUT FILL-IN_NODF
      FILL-IN-AONR = INPUT FILL-IN-AONR
      FILL-IN-DELNR = INPUT FILL-IN-DELNR     
      CMB_OVERUT = INPUT CMB_OVERUT   
      FILL-IN-SLUT = INPUT FILL-IN-SLUT
      FILL-IN-START = INPUT FILL-IN-START
      FILL-IN-TRAKT = INPUT CMB_TRAK
      FILL-IN-UTRYCK = INPUT FILL-IN-UTRYCK.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT= 0.
      END.
      FIND tidallt WHERE RECID(tidallt) = nyrec NO-ERROR.
      IF AVAILABLE tidallt THEN DO:
         IF DAY(tidallt.DATUM) = FILL-IN-DATUM THEN DO:
            {AVVBEFREW.I}
            ASSIGN
            tidallt.ANDRA = TRUE
            tidallt.AONR = FILL-IN-AONR 
            tidallt.DATUM = DATE(regmnr,FILL-IN-DATUM,regar) 
            tidallt.DELNR = FILL-IN-DELNR 
            tidallt.NODF = FILL-IN_NODF                           
            tidallt.PRIS = FILL-IN-PRIS
            tidallt.PRISTYP = CMB_PRISTYP 
            tidallt.SLUT = FILL-IN-SLUT 
            tidallt.START = FILL-IN-START 
            tidallt.TRAKTAMENTE = CMB_TRAK 
            tidallt.UTRYCK = FILL-IN-UTRYCK
            tidallt.RESMAL = FILL-IN_RESMAL.
            RUN ladda_UI (INPUT 2).
            RUN refreshbrw_UI IN brwproc[2].
            nyrec = ?.             
            IF FILL-IN-SLUT < FILL-IN-START THEN FILL-IN-SLUT = FILL-IN-START.         
            FILL-IN_RESMAL = "".
            DISPLAY FILL-IN-START FILL-IN-SLUT FILL-IN_RESMAL WITH FRAME {&FRAME-NAME}.
            RETURN.
         END.   
      END.    
      FIND FIRST tidallt WHERE tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE "RESTID..." AND 
      DAY(tidallt.DATUM) = FILL-IN-DATUM AND tidallt.SLUT = FILL-IN-SLUT AND tidallt.START = FILL-IN-START  
      USE-INDEX PSTART NO-LOCK NO-ERROR.
      IF AVAILABLE tidallt THEN DO:         
         {AVVBEFREW.I}
         ASSIGN     
         tidallt.PERSONALKOD = pkod
         tidallt.AONR = FILL-IN-AONR 
         tidallt.DATUM = DATE(regmnr,FILL-IN-DATUM,regar)
         tidallt.DELNR = FILL-IN-DELNR 
         tidallt.NODF = FILL-IN_NODF 
         tidallt.PRIS = FILL-IN-PRIS 
         tidallt.PRISTYP = CMB_PRISTYP 
         tidallt.SLUT = FILL-IN-SLUT 
         tidallt.START = FILL-IN-START 
         tidallt.TRAKTAMENTE = CMB_TRAK 
         tidallt.UTRYCKNING = FILL-IN-UTRYCK
         tidallt.RESMAL = FILL-IN_RESMAL
         tidallt.ANDRA = TRUE.
          RUN refreshbrw_UI IN brwproc[2].
         RETURN.            
      END.
      ELSE DO:
         FIND FIRST tidallt WHERE tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE "RESTID..." AND
         DAY(tidallt.DATUM) = FILL-IN-DATUM AND          
         tidallt.START LE FILL-IN-START AND tidallt.SLUT > FILL-IN-START 
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE tidallt THEN DO:         
            {AVVBEFREW.I}
            ASSIGN     
            tidallt.PERSONALKOD = pkod
            tidallt.AONR = FILL-IN-AONR 
            tidallt.DATUM = DATE(regmnr,FILL-IN-DATUM,regar)
            tidallt.DELNR = FILL-IN-DELNR 
            tidallt.NODF = FILL-IN_NODF 
            tidallt.PRIS = FILL-IN-PRIS 
            tidallt.PRISTYP = CMB_PRISTYP 
            tidallt.SLUT = FILL-IN-SLUT 
            tidallt.START = FILL-IN-START 
            tidallt.TRAKTAMENTE = CMB_TRAK 
            tidallt.UTRYCKNING = FILL-IN-UTRYCK
            tidallt.RESMAL = FILL-IN_RESMAL
            tidallt.ANDRA = TRUE.
            RUN refreshbrw_UI IN brwproc[2].
            RETURN.            
         END.
         FIND FIRST tidallt WHERE tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE "RESTID..." AND
         DAY(tidallt.DATUM) = FILL-IN-DATUM AND          
         tidallt.START < FILL-IN-SLUT AND tidallt.SLUT >= FILL-IN-SLUT   
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE tidallt THEN DO: 
            {AVVBEFREW.I}
            ASSIGN     
            tidallt.PERSONALKOD = pkod
            tidallt.AONR = FILL-IN-AONR 
            tidallt.DATUM = DATE(regmnr,FILL-IN-DATUM,regar)
            tidallt.DELNR = FILL-IN-DELNR 
            tidallt.NODF = FILL-IN_NODF 
            tidallt.PRIS = FILL-IN-PRIS 
            tidallt.PRISTYP = CMB_PRISTYP 
            tidallt.SLUT = FILL-IN-SLUT 
            tidallt.START = FILL-IN-START 
            tidallt.TRAKTAMENTE = CMB_TRAK 
            tidallt.UTRYCKNING = FILL-IN-UTRYCK
            tidallt.RESMAL = FILL-IN_RESMAL
            tidallt.ANDRA = TRUE.
            RUN refreshbrw_UI IN brwproc[2].
            RETURN.                        
         END. 
         FIND FIRST tidallt WHERE tidallt.TIDLOG = TRUE AND tidallt.PRISTYP NE "RESTID..." AND
         DAY(tidallt.DATUM) = FILL-IN-DATUM AND          
         tidallt.START > FILL-IN-START AND tidallt.SLUT < FILL-IN-SLUT   
         USE-INDEX PSTART NO-LOCK NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            {AVVBEFREW.I}
            ASSIGN     
            tidallt.PERSONALKOD = pkod
            tidallt.AONR = FILL-IN-AONR 
            tidallt.DATUM = DATE(regmnr,FILL-IN-DATUM,regar)
            tidallt.DELNR = FILL-IN-DELNR 
            tidallt.NODF = FILL-IN_NODF 
            tidallt.PRIS = FILL-IN-PRIS 
            tidallt.PRISTYP = CMB_PRISTYP 
            tidallt.SLUT = FILL-IN-SLUT 
            tidallt.START = FILL-IN-START 
            tidallt.TRAKTAMENTE = CMB_TRAK 
            tidallt.UTRYCKNING = FILL-IN-UTRYCK
            tidallt.RESMAL = FILL-IN_RESMAL
            tidallt.ANDRA = TRUE.
            RUN refreshbrw_UI IN brwproc[2].
            RETURN.                        
         END.
      END.      
      IF FILL-IN-AONR = "" THEN RETURN.
      IF FILL-IN-SLUT = FILL-IN-START THEN RETURN.
      CREATE tidallt.
      {AVVBEFREW.I}
      ASSIGN
      tidallt.PERSONALKOD = pkod
      tidallt.AONR = FILL-IN-AONR 
      tidallt.DATUM = DATE(regmnr,FILL-IN-DATUM,regar)
      tidallt.DELNR = FILL-IN-DELNR 
      tidallt.NODF = FILL-IN_NODF 
      tidallt.PRIS = FILL-IN-PRIS 
      tidallt.PRISTYP = CMB_PRISTYP 
      tidallt.SLUT = FILL-IN-SLUT 
      tidallt.START = FILL-IN-START 
      tidallt.TRAKTAMENTE = CMB_TRAK 
      tidallt.UTRYCKNING = FILL-IN-UTRYCK
      tidallt.RESMAL = FILL-IN_RESMAL
      tidallt.ANDRA = TRUE.
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:         
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = pkod 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE flexavttemp THEN DO:
            IF flexavttemp.FLEXTID = TRUE THEN DO:
               nytid = lunchslutet.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = lunchstarten.
               RUN TIMSEK.P.
               sekunder = seku - sekunder.               
               tidallt.LAGANTAL = sekunder / 60.
            END.
         END.
      END.
      RUN ladda_UI (INPUT 2).
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tidallt)).
      RUN openbdynspec_UI IN brwproc[2].
      RUN lastselectdyn_UI IN brwproc[2].
      nyrec = ?.            
      IF FILL-IN-SLUT < FILL-IN-START THEN FILL-IN-SLUT = FILL-IN-START.
      FILL-IN_RESMAL = "".
      DISPLAY FILL-IN-START FILL-IN-SLUT FILL-IN_RESMAL WITH FRAME {&FRAME-NAME}.
   END.        
   {musarrow.i} 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytemp2_UI DIALOG-1 
PROCEDURE nytemp2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF musz = TRUE THEN DO:
      musz = FALSE. /*FIXA ÄNDRA PRIS*/
   END.     
   ELSE DO:              
      {AVVBEFREW.I}
      ASSIGN
      bustart3 = tidallt.START
      regvnr = FILL-IN-VECKO     
      regdagnamn = CMB_DAG.
      regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
       
      /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
      ASSIGN SUBSTRING(tidallt.PROGRAM,1,158) = "ANDINTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv      
      tidallt.PERSONALKOD = FILL-IN-PKOD      
      tidallt.DAG = CMB_DAG
      tidallt.VECKONUMMER = FILL-IN-VECKO
      tidallt.SLUT = FILL-IN-SLUT
      tidallt.START = FILL-IN-START 
      tidallt.TRAKTAMENTE = FILL-IN-TRAKT
      tidallt.OVERTIDUTTAG = FILL-IN-OVER 
      tidallt.UTRYCKNING = FILL-IN-UTRYCK
      tidallt.NODF = FILL-IN_NODF
      tidallt.PRISTYP = FILL-IN-PRISTYP
      tidallt.PRIS = FILL-IN-PRIS
      tidallt.RESMAL = FILL-IN_RESMAL.
      IF tidallt.AONR = FILL-IN-AONR AND tidallt.DELNR = FILL-IN-DELNR THEN DO:
         persrec = persrec.
      END.
      ELSE DO:          
         ASSIGN 
         sekunder = 0
         regdatumspar = regdatum
         regdatum = tidallt.DATUM.         
      END.
      ASSIGN 
      tidallt.DATUM = regdatum
      tidallt.AONR = FILL-IN-AONR 
      tidallt.DELNR = FILL-IN-DELNR.
      ASSIGN      
      
      nytid = tidallt.START.
      RUN TIMSEK.P.
      regstartsek = sekunder.
      nytid = tidallt.SLUT.
      RUN TIMSEK.P.
      regslutsek = sekunder.
      regdatum = tidallt.DATUM.
      RUN TOTTIDW.P (INPUT pkod).                        
      ASSIGN tidallt.TOTALT = nytid. 
      tidallt.PERSONALKOD = "".            
      RUN nytolk_UI. 
   END.                                                                       
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytemp_UI DIALOG-1 
PROCEDURE nytemp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   OPEN QUERY nyq FOR EACH tidallt WHERE tidallt.ANDRA = TRUE 
   USE-INDEX ANDRA NO-LOCK.  
   GET FIRST nyq. 
   DO WHILE AVAILABLE(tidallt):           
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[2].
/*       RUN repotid_UI (INPUT RECID(tidallt)). */
      nyrec = ?.
      ASSIGN
      FILL-IN_VIBEFATTNING = tidallt.VIBEFATTNING 
      CMB_BEF = tidallt.VIBEFATTNING 
      FILL-IN_RESMAL = tidallt.RESMAL  
      FILL-IN-AONR  = tidallt.AONR 
      FILL-IN-DATUM = DAY(tidallt.DATUM) 
      FILL-IN-DELNR = tidallt.DELNR 
      FILL-IN_NODF = tidallt.NODF 
      FILL-IN-PRIS = tidallt.PRIS 
      CMB_PRISTYP = tidallt.PRISTYP 
      FILL-IN-SLUT = tidallt.SLUT 
      FILL-IN-START = tidallt.START 
      CMB_TRAK = tidallt.TRAKTAMENTE       
      FILL-IN-UTRYCK = tidallt.UTRYCK.            
      RUN ladda_UI (INPUT 1).
      IF utryckningtemp.NODF = TRUE THEN DO:
         DISPLAY FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
         ENABLE FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
         FILL-IN_NODF:HIDDEN = FALSE.
      END.      
      DISPLAY FILL-IN-AONR FILL-IN-DATUM FILL-IN-DELNR  
      CMB_OVERUT CMB_PRISTYP FILL-IN_RESMAL  
      FILL-IN-SLUT FILL-IN-START FILL-IN-UTRYCK 
      WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      END.
      ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.          
      
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         DISPLAY FILL-IN_VIBEFATTNING CMB_BEF WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
      regdatum = DATE((regmnr),01,regar).
      IF MONTH(regdatum) = 12 THEN datkoll = DATE(12,31,YEAR(regdatum)).
      ELSE datkoll = DATE((MONTH(regdatum) + 1),01,YEAR(regdatum)) - 1.
      IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
         MESSAGE "Felaktigt angivet datum. Denna månad har bara " DAY(datkoll) "dagar."
         VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.            
      IF FILL-IN-DATUM <= 0 THEN DO:
         MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
         VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.            
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
            MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
            tillochmeddatum VIEW-AS ALERT-BOX.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END.            
      END.                              
      regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
      RUN REGVEC.P.
      RUN REGDAG.P.
      ASSIGN
      CMB_DAG = regdagnamn
      FILL-IN-VECKO = regvnr.       
      ASSIGN
      musz = FALSE   
      CMB_PRISTYP = INPUT CMB_PRISTYP  
      FILL-IN-PRISTYP = CMB_PRISTYP. 
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         FILL-IN-PRIS = INPUT FILL-IN-PRIS. 
      END.
      IF utryckningtemp.NODF = TRUE THEN FILL-IN_NODF = INPUT FILL-IN_NODF.
      ASSIGN
      FILL-IN-AONR = INPUT FILL-IN-AONR
      FILL-IN-DELNR = INPUT FILL-IN-DELNR.     
      CMB_OVERUT = INPUT CMB_OVERUT.
      IF CMB_OVERUT = "Komp" THEN FILL-IN-OVER = "K".
      IF CMB_OVERUT = "Över" THEN FILL-IN-OVER = "Ö".
      IF CMB_OVERUT = "Flex" THEN FILL-IN-OVER = "F". 
      IF CMB_OVERUT = "Ejöv" THEN FILL-IN-OVER = "I". 
      
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD USE-INDEX
      PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp THEN DO:
         IF CMB_OVERUT = "Flex" AND flexavttemp.FLEXTID = FALSE THEN DO:
            MESSAGE "Personen har INTE flextidsavtal." VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.                  
         END.
      END.
      ASSIGN
      FILL-IN-SLUT = INPUT FILL-IN-SLUT
      FILL-IN-START = INPUT FILL-IN-START
      FILL-IN-TRAKT = INPUT CMB_TRAK
      FILL-IN-UTRYCK = INPUT FILL-IN-UTRYCK
      regdatumspar = regdatum.        
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN ASSIGN CMB_TRAK = 0 FILL-IN-TRAKT = 0.
      
      IF Guru.Konstanter:globforetag = "cSUND" THEN DO:
         IF regstart = regslut AND FILL-IN-OVER = "F"  THEN DO:
            MESSAGE "Flextid skall ej registreras på helg" VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END.
         IF FILL-IN-SLUT > 18 AND FILL-IN-OVER = "F"  THEN DO:
            MESSAGE "Flextid skall ej registreras efter 18" VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END. 
         IF FILL-IN-START < 6 AND FILL-IN-OVER = "F"  THEN DO:
            MESSAGE "Flextid skall ej registreras före 6" VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END. 
         IF vart = "NYA" AND FILL-IN-OVER = "F"  THEN DO:
            MESSAGE "Flextid registreras via flexrutinen" VIEW-AS ALERT-BOX.    
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END.
      END.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR 
      USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 47
         soktemp.SOKCHAR[1] = FILL-IN-AONR
         soktemp.SOKINT[1] = FILL-IN-DELNR.
         {SOKANROP.I}      
         IF soktemp.SOKCHAR[2] = ? THEN DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.      
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.      
         END.
         ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.      
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.      
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
            musz = TRUE.
            RETURN NO-APPLY.      
         END.
         /*PRISFOR*/
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
            ASSIGN
            CMB_PRISTYP = soktemp.SOKCHAR[3]  
            FILL-IN-PRISTYP = soktemp.SOKCHAR[3].
            ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
            DISPLAY CMB_PRISTYP WITH FRAME {&FRAME-NAME}.
         END.
      
      END.
      ELSE DO:                          
         {AOKOLLERS.I}
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.          
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END.
      END.   
      IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
         IF  FILL-IN-OVER = "I" OR FILL-IN-OVER = "F" THEN musz = musz.
         ELSE DO:
            MESSAGE "Personen har inte övertidersättning eller restidsersättning" VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.
         END.    
      END.
      regdatum = regdatumspar.  
      IF FILL-IN-START > 24.00 THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.      
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.      
      IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4,2) > "59" THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.      
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.      
      IF FILL-IN-SLUT > 24.00 THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END. 
      IF SUBSTRING(STRING(FILL-IN-SLUT,"99.99"),4 ,2) > "59" THEN DO:
         MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.    
      IF FILL-IN-SLUT = FILL-IN-START THEN DO:
         MESSAGE "Start och slut kan ej vara lika." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.                       
      ASSIGN
      regstart = FILL-IN-START
      vart = "AND".
      FIND FIRST restemp WHERE          
      restemp.DATUM = DAY(regdatum) AND restemp.START LE regstart AND
      restemp.SLUT > regstart  
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE restemp THEN DO:
         musz = FALSE.
      END.   
      ELSE DO:          
         MESSAGE "Det finns en resa registrering med start "
         restemp.START " och slut " restemp.SLUT "!"  
         VIEW-AS ALERT-BOX.
         musz = TRUE.
         RETURN NO-APPLY.           
      END.                
      ASSIGN
      regstart = FILL-IN-START
      regslut = FILL-IN-SLUT.
      IF FILL-IN-START > FILL-IN-SLUT THEN DO:
         MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN NO-APPLY.
      END.
      FIND FIRST restemp WHERE restemp.DATUM = DAY(regdatum) AND restemp.START < regslut AND
      restemp.SLUT >= regslut NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE restemp THEN DO: 
         FIND FIRST restemp WHERE restemp.DATUM = DAY(regdatum) AND restemp.START > regstart AND
         restemp.SLUT < regslut NO-LOCK NO-ERROR.        
         IF NOT AVAILABLE restemp THEN musz = FALSE.    
         ELSE DO:
            MESSAGE "Det finns en resa registrering med start "
            restemp.START " och slut"    
            restemp.SLUT "!" 
            VIEW-AS ALERT-BOX.
            musz = TRUE.
            RETURN NO-APPLY.                   
         END.  
      END.
      ELSE DO:
         MESSAGE "Det finns en resa registrering med start "
         restemp.START " och slut"    
         restemp.SLUT "!" 
         VIEW-AS ALERT-BOX.
         musz = TRUE.
         RETURN NO-APPLY.             
      END.
      FIND FIRST tidalltbuff WHERE tidalltbuff.TIDLOG = TRUE AND           
      tidalltbuff.DATUM = regdatum AND tidalltbuff.START LE regstart AND
      tidalltbuff.SLUT > regstart AND tidalltbuff.RECTIDVIS NE tidallt.RECTIDVIS   
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidalltbuff THEN DO:
         musz = FALSE.
      END.   
      ELSE DO:          
         IF tidallt.RECTIDVIS = tidalltbuff.RECTIDVIS THEN musz = musz.
         ELSE DO:
            MESSAGE "Det finns redan en registrering med start "
            tidalltbuff.START " och slut " tidalltbuff.SLUT "!"  
            VIEW-AS ALERT-BOX.
            musz = TRUE.
            RETURN NO-APPLY.  
         END.    
      END.                
      IF musz = TRUE THEN DO:
         musz = FALSE.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN NO-APPLY.
      END.      
      FIND FIRST tidalltbuff WHERE tidalltbuff.TIDLOG = TRUE AND     
      tidalltbuff.DATUM = regdatum AND tidalltbuff.START < regslut AND
      tidalltbuff.SLUT >= regslut AND tidalltbuff.RECTIDVIS NE tidallt.RECTIDVIS NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE tidalltbuff THEN DO: 
         FIND FIRST tidalltbuff WHERE tidalltbuff.TIDLOG = TRUE AND        
         tidalltbuff.DATUM = regdatum AND tidalltbuff.START > regstart AND
         tidalltbuff.SLUT < regslut AND tidalltbuff.RECTIDVIS NE tidallt.RECTIDVIS NO-LOCK NO-ERROR.        
         IF NOT AVAILABLE tidalltbuff THEN DO:    
            musz = FALSE.    
         END.  
         ELSE DO:
            IF tidallt.RECTIDVIS = tidalltbuff.RECTIDVIS THEN musz = musz.
            ELSE DO:
               MESSAGE "Det finns redan en registrering med start "
               tidalltbuff.START " och slut" tidalltbuff.SLUT "!" 
               VIEW-AS ALERT-BOX.
               musz = TRUE.
               RETURN NO-APPLY.          
            END.
         END.  
      END.
      ELSE DO:
         IF tidallt.RECTIDVIS = tidalltbuff.RECTIDVIS THEN musz = musz.
         ELSE DO:
            MESSAGE "Det finns redan en registrering med start "
            tidalltbuff.START " och slut" tidalltbuff.SLUT "!" 
            VIEW-AS ALERT-BOX.
            musz = TRUE.
            RETURN NO-APPLY. 
         END.
      END.
      IF musz = TRUE THEN DO:                                
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN NO-APPLY.
      END.      
       
      RUN REGVEC.P.
      {SLUTARBW.I}
      IF FILL-IN-PRISTYP = "FRÅNVARO." THEN DO:
         IF FILL-IN-START GE regslut OR FILL-IN-SLUT LE regstart 
         OR FILL-IN-SLUT > regslut OR FILL-IN-START < regstart THEN DO:
            MESSAGE "Övertid kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            musz = TRUE.
            RETURN NO-APPLY.            
         END.    
      END.                
      IF  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR 
      Guru.Konstanter:globforetag = "GKAL"   OR Guru.Konstanter:globforetag = "LULE" THEN DO:
         IF FILL-IN-AONR = "05" OR FILL-IN-AONR = "150" THEN DO:
            nytid = FILL-IN-START.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = FILL-IN-SLUT.
            RUN TIMSEK.P.          
            ASSIGN
            regdatumspar = regdatum
            regslutsek = sekunder.        
            RUN TOTTIDW.P (INPUT pkod).
            IF nytid < regtotalt  THEN DO:           
               MESSAGE "Semester kan bara registreras hela dagar " VIEW-AS ALERT-BOX.             
               status-mus2 = SESSION:SET-WAIT-STATE("").
               musz = TRUE.
               RETURN NO-APPLY.              
            END.    
         END.
      END.   
      IF utryckningtemp.HALV = TRUE THEN DO: 
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE AND CMB_OVERUT = "FLEX" THEN musz = musz.                         
         ELSE IF CMB_OVERUT = "EJÖV" THEN musz = musz.
         ELSE DO:               
            regvnr = tidallt.VECKONUMMER.              
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
               ASSIGN 
               overant = sekunder - overant.
            END.
            halvkvart = 1800.
            
            ASSIGN   
            otim = TRUNCATE(overant / halvkvart ,0)
            otim2 = overant - (otim * halvkvart).            
            IF FILL-IN-UTRYCK = TRUE AND overant > utryckningtemp.UTRYCKNBER AND
            otim2 > 0 AND FILL-IN-SLUT > regslut THEN DO:              
               MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.         
               status-mus2 = SESSION:SET-WAIT-STATE("").
               musz = TRUE.
               RETURN NO-APPLY.             
            END.   
            IF FILL-IN-UTRYCK = TRUE AND overant > utryckningtemp.UTRYCKNBER AND
            otim2 > 0 AND FILL-IN-START < regstart THEN DO:               
               MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.         
               status-mus2 = SESSION:SET-WAIT-STATE("").
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
            IF FILL-IN-UTRYCK = FALSE AND otim2 > 0 AND FILL-IN-SLUT > regslut THEN DO:               
               MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.         
               status-mus2 = SESSION:SET-WAIT-STATE("").
               musz = TRUE.
               RETURN NO-APPLY.             
            END.
            IF FILL-IN-UTRYCK = FALSE AND otim2 > 0 AND FILL-IN-START < regstart THEN DO:               
               MESSAGE "Endast jämna halvtimmar får registreras." VIEW-AS ALERT-BOX.         
               status-mus2 = SESSION:SET-WAIT-STATE("").
               musz = TRUE.
               RETURN NO-APPLY.              
            END.
         END.             
      END.
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         ASSIGN
         sok1 = FILL-IN-AONR      
         sok2 = FILL-IN-DELNR
         sok4 = pkod.       
         RUN nyupp_UI (INPUT 16).      
         IF FILL-IN-PRISTYP = sok1 AND FILL-IN-PRIS = sok5 THEN musz = musz.
         ELSE DO:
            MESSAGE Guru.Konstanter:gdebk " och/eller priset är ändrat i"
            skip 
            "jämförelse mot " + LC(Guru.Konstanter:gaok) + " ! "
            skip
            LC(Guru.Konstanter:gaok) + " har            :" sok1 "och" sok5 
            skip
            "tidregistreringen har :" FILL-IN-PRISTYP " och" FILL-IN-PRIS
            skip
            "vill du ändra tillbaka priset till" 
            skip
            sok1 "och" sok5  "?"
   
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
            CASE val:
               WHEN TRUE THEN DO:
                  ASSIGN  CMB_PRISTYP:SCREEN-VALUE = FILL-IN-PRISTYP.             
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  musz = TRUE.
                  RETURN NO-APPLY.           
               END.
               WHEN FALSE THEN DO:
                  FILL-IN-PRISTYP = FILL-IN-PRISTYP. 
                  FILL-IN-PRIS = FILL-IN-PRIS.
               END.
            END CASE. 
         END.     
      END.
      RUN nytemp2_UI.
      RUN refreshbrw_UI IN brwproc[2].
      GET NEXT nyq. 
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytidanyprint_UI DIALOG-1 
PROCEDURE nytidanyprint_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF BRW_NYTID:CURRENT-COLUMN IN FRAME {&FRAME-NAME} = ? THEN RETURN.
   IF datecolh NE BRW_NYTID:CURRENT-COLUMN IN FRAME {&FRAME-NAME} THEN DO:
      RUN hittadyn_UI IN brwproc[2].      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytidstartsearch_UI DIALOG-1 
PROCEDURE nytidstartsearch_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF datecolh = BRW_NYTID:CURRENT-COLUMN IN FRAME {&FRAME-NAME} THEN DO:
      RUN setcolindex_UI IN brwproc[2] (INPUT "DATUM").
      RUN startsearchproc IN brwproc[2].
   END.
   ELSE DO: 
      RUN setcolindex_UI IN brwproc[2] (INPUT ?).
      RUN startsearchproc IN brwproc[2].
   END.
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
   {muswait.i} 
/*    tidtabrecspar = tidtabrec. */
   EMPTY TEMP-TABLE tidapptemp NO-ERROR. 
   
   DO TRANSACTION:
      CREATE tidapptemp.
      ASSIGN
      tidapptemp.PERSONALKOD = personaltemp.PERSONALKOD
      tidapptemp.FORETAG = Guru.Konstanter:globforetag
      tidapptemp.ANVANDARE = Guru.Konstanter:globanv
      tidapptemp.DATUM = regdatum.                 
   END.
   
   {TIDUPPINW.I}
   {musarrow.i} 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repotid_UI DIALOG-1 
PROCEDURE repotid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   BRW_NYTID:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.                        
   REPOSITION BRW_NYTID TO RECID browrec NO-ERROR.
   status-ok = BRW_NYTID:SELECT-FOCUSED-ROW() NO-ERROR.                 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valao_UI DIALOG-1 
PROCEDURE valao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF musz = FALSE THEN DO:      
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR
      sok1 = utsokaonr.AONR      
      sok2 = utsokaonr.DELNR
      sok4 = pkod.
      RUN nyupp_UI (INPUT 16).      
      ASSIGN
      FILL-IN-PRISTYP = sok1
      CMB_TRAK:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(sok2)   
      CMB_TRAK = INPUT FRAME {&FRAME-NAME} CMB_TRAK       
      CMB_PRISTYP = FILL-IN-PRISTYP.
      IF sok3 = "1" THEN FILL-IN-UTRYCK = TRUE.
      IF sok3 = "2" THEN FILL-IN-UTRYCK = FALSE.
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      END.     
      ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         FILL-IN-PRIS = sok5.
         DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

