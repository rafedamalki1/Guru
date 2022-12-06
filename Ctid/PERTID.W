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
{TIDALLT.I}
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sdatum AS DATE NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{AVDTEMP.I}
{TIDAPPDEF.I}
&Scoped-define NEW
{PERBEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{UPPGHMT.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
{PHMT.I}
{OMRTEMPW.I}
{SOKDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE pertidaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE pertiddelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE persnr AS INTEGER EXTENT 10 FORMAT "99" NO-UNDO.
DEFINE VARIABLE tal1 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal2 AS INTEGER NO-UNDO.
DEFINE VARIABLE ksiffran AS INTEGER NO-UNDO.
DEFINE VARIABLE bpnr AS DATE NO-UNDO.
DEFINE VARIABLE balder AS DECIMAL NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ftro AS LOGICAL NO-UNDO.
DEFINE VARIABLE valdkom AS CHARACTER NO-UNDO.
DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .91 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .91 NO-UNDO.

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
&Scoped-define INTERNAL-TABLES utsokaonr egnaao

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


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-DATUM FILL-IN-DATUM-2 FILL-IN-AONR ~
FILL-IN-DELNR CMB_TRAK FILL-IN_RESMAL BTN_REG BTN_AVB CMB_OMR FILL-IN-AONRS ~
FILL-IN_ORTS BTN_NVE BTN_FVE BTN_NVE-2 BTN_FVE-2 CMB_AVD RAD_FAST ~
BTN_SKAPEN RECT-33 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-DATUM FILL-IN-DATUM-2 FILL-IN-AONR ~
FILL-IN-DELNR CMB_TRAK FILL-IN_RESMAL FILL-IN-TEXT FILL-IN-PKOD ~
FILL-IN-MANAD CMB_OMR FILL-IN-AONRS FILL-IN_ORTS FILL-IN-PRIS FILL-IN-DAG ~
FILL-IN-DAG-2 CMB_AVD FILL-IN-SKP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_AONR-3 
       MENU-ITEM m_Arbetsuppgift-3 LABEL "Arbetsuppgift" .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKAPEN 
     LABEL "Spara favorit":L 
     SIZE 14.5 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avvik.bef." 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG-2 AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM-2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALFA AS CHARACTER FORMAT "X(256)":U INITIAL "Välj fakturabefattning        Vald fakturabefattning" 
     VIEW-AS FILL-IN 
     SIZE 34.13 BY .75
     FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE FILL-IN_VIBEFATTNING AS CHARACTER FORMAT "x(256)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RAD_FAST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", 1,
"Fasta aonr", 2,
"Favorit aonr", 3
     SIZE 43.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-33
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56.75 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_EAONR FOR 
      egnaao SCROLLING.
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.75 BY 12
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.75 BY 12
         TITLE "Favorit arbetsordernummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_EAONR AT ROW 5.75 COL 51
     FILL-IN-DATUM AT ROW 10.17 COL 12.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DATUM-2 AT ROW 11.75 COL 12.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-AONR AT ROW 13.38 COL 12.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DELNR AT ROW 14.96 COL 12.5 COLON-ALIGNED AUTO-RETURN 
     BRW_AONR AT ROW 5.75 COL 51
     CMB_TRAK AT ROW 16.5 COL 12.5 COLON-ALIGNED
     FILL-IN_RESMAL AT ROW 21.08 COL 12.5 COLON-ALIGNED
     BTN_REG AT ROW 21.08 COL 78.75
     BTN_AVB AT ROW 21.08 COL 93.75
     FILL-IN-TEXT AT ROW 1.75 COL 83.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PKOD AT ROW 7 COL 12.5 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 8.58 COL 12.5 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 4.33 COL 83.5 COLON-ALIGNED NO-LABEL
     FILL-IN-AONRS AT ROW 18.17 COL 65.13 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 18.17 COL 85.13 COLON-ALIGNED
     BTN_NVE AT ROW 9.75 COL 24
     BTN_FVE AT ROW 10.58 COL 24
     BTN_NVE-2 AT ROW 11.58 COL 24
     BTN_FVE-2 AT ROW 12.46 COL 24
     CMB_BEF AT ROW 19.08 COL 12.5 COLON-ALIGNED
     FILL-IN_VIBEFATTNING AT ROW 19.08 COL 32.25 COLON-ALIGNED HELP
          "BEFATTNINGSKOD" NO-LABEL
     FILL-IN-VALFA AT ROW 18 COL 12.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PRISTYP AT ROW 20.04 COL 66 COLON-ALIGNED
     CMB_PRISTYP AT ROW 19.58 COL 66 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 19.5 COL 68.5 COLON-ALIGNED
     FILL-IN-DAG AT ROW 10.17 COL 25.25 COLON-ALIGNED NO-LABEL
     FILL-IN-DAG-2 AT ROW 11.75 COL 25.25 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 3 COL 83.5 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 4.29 COL 38.5 NO-LABEL
     BTN_SKAPEN AT ROW 13.38 COL 23.5
     FILL-IN-SKP AT ROW 18.17 COL 50.13 COLON-ALIGNED NO-LABEL
     RECT-33 AT ROW 18 COL 51
     SPACE(0.24) SKIP(3.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Period tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: egnaao T "?" NO-UNDO temp-db egnaao
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_EAONR 1 DIALOG-1 */
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

/* SETTINGS FOR COMBO-BOX CMB_BEF IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_BEF:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_PRISTYP IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_PRISTYP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DAG-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PRIS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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
/* SETTINGS FOR FILL-IN FILL-IN-VALFA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VALFA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VIBEFATTNING IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_VIBEFATTNING:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
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
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EAONR
/* Query rebuild information for BROWSE BRW_EAONR
     _TblList          = "Temp-Tables.egnaao"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.egnaao.OMRADE
"egnaao.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.egnaao.AONR
"egnaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.egnaao.DELNR
"egnaao.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.egnaao.ORT
"egnaao.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_EAONR */
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
ON END-ERROR OF FRAME DIALOG-1 /* Period tidregistrering */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Period tidregistrering */
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
   RUN fillinupdate_UI.             
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
   RUN fillinupdate_UI.                
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
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.   
   FILL-IN-DATUM-2 = FILL-IN-DATUM-2 - 1.   
   IF FILL-IN-DATUM-2 = 0 THEN FILL-IN-DATUM-2 = 1.
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
   END.      
   DISPLAY FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM-2,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}. 
        
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
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}. 
   IF FILL-IN-DATUM-2 < FILL-IN-DATUM THEN DO:
      ASSIGN
      FILL-IN-DATUM-2 = FILL-IN-DATUM
      FILL-IN-DAG-2 = FILL-IN-DAG.
      DISPLAY FILL-IN-DATUM-2 FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.   
   FILL-IN-DATUM-2 = FILL-IN-DATUM-2 + 1.
   IF regmnr = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE(regmnr + 1,01,regar) - 1.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll)THEN DO:    
      FILL-IN-DATUM-2 = DAY(datkoll).
   END.      
   DISPLAY FILL-IN-DATUM-2 WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM-2,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* OK */
DO:
   {muswait.i} 
   ASSIGN
   FILL-IN-AONR = INPUT FRAME {&FRAME-NAME} FILL-IN-AONR
   FILL-IN-DELNR = INPUT FRAME {&FRAME-NAME} FILL-IN-DELNR   
   FILL-IN-DATUM = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM
   FILL-IN-DATUM-2 = INPUT FRAME {&FRAME-NAME} FILL-IN-DATUM-2
   FILL-IN_RESMAL = INPUT FRAME {&FRAME-NAME} FILL-IN_RESMAL
   CMB_BEF = INPUT CMB_BEF.
   IF regmnr = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE(regmnr + 1,01,regar) - 1.
   END.
   IF FILL-IN-DATUM-2 < FILL-IN-DATUM THEN DO:
      MESSAGE "Felaktigt angivet datum."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara " DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara " DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kann ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END. 
   IF FILL-IN-DATUM-2 <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kann ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-DATUM IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.            
   END.

   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-DATUM-2 IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.            
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SKOK" THEN DO:      
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE valdkom VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.      
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.      
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                    "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                    "Är det föräldrapenning före barnets födelse?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.
            IF val5 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF bpnr < 01/01/2014 THEN DO:
            IF balder  GE 9 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE DO:
            IF balder  > 12 THEN DO:
               MESSAGE "Barnet är äldre än 12 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.      
      END.
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).        
         IF balder  > 12 THEN DO:
            MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 132. Finns intyg?"  
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
            IF val3 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,11,50) = "Intygkrävs".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      musz = FALSE.
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.      
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF balder  > 12 THEN DO:
            MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 118. Finns intyg?"  
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
            IF val2 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Intyg krävs".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.            
         END.
      END.
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                    "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                    "Är det föräldrapenning före barnets födelse?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val6 AS LOGICAL.
            IF val6 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.         
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF bpnr < 01/01/2014 THEN DO:
            IF balder  GE 9  THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 131 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(bpnr) > 7 THEN.            
            ELSE IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
               MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 131 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         ELSE DO:
            IF balder  > 12 THEN DO:
               MESSAGE "Barnet är äldre än 12 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.      
         
      END.
      musz = FALSE.  
      RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE valdkom VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.    
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
      musz = FALSE.
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Barnets personnummer skall vara ifyllt ( format 999999-9999)." SKIP 
                    "Endast om det är frågan om föräldrapenning innan barnets födelse kan personnummer vara blankt." skip 
                    "Är det föräldrapenning före barnets födelse?"
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val7 AS LOGICAL.
            IF val7 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Innan barns födelse".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF INTEGER(SUBSTRING(FILL-IN_RESMAL,3,2)) > 12 OR INTEGER(SUBSTRING(FILL-IN_RESMAL,5,2)) > 31 THEN DO:
            MESSAGE "Felaktigt personnummer format skall vara ÅÅMMDD"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.              
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         
         IF balder  GE 9 THEN DO:
            MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 130 får inte användas."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
            MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 130 får inte användas."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
      /*IF FILL-IN-AONR = "131"  THEN musz = TRUE.      */
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF INTEGER(SUBSTRING(FILL-IN_RESMAL,3,2)) > 12 OR INTEGER(SUBSTRING(FILL-IN_RESMAL,5,2)) > 31 THEN DO:
            MESSAGE "Felaktigt personnummer format skall vara ÅÅMMDD"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.              
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
      
         IF balder  > 12 THEN DO:
            MESSAGE "Om barnet är äldre än 12 år krävs intyg för " Guru.Konstanter:gaok " 131. Finns intyg?"  
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
            IF val4 = TRUE THEN DO:
               SUBSTRING(FILL-IN_RESMAL,12,50) = "Intyg krävs".
            END.
            ELSE DO:
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
      END.
      musz = FALSE.
      RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE valdkom VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      musz = FALSE.
      RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.      
      musz = FALSE.
      IF FILL-IN-AONR = "003005" AND FILL-IN-DELNR = 1 THEN musz = TRUE.
      IF FILL-IN-AONR = "100293" AND FILL-IN-DELNR = 0 THEN musz = TRUE.
      IF FILL-IN-AONR = "241" AND FILL-IN-DELNR = 0 THEN musz = TRUE.
      IF FILL-IN-AONR = "245" AND FILL-IN-DELNR = 0 THEN musz = TRUE.         
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) " används bara för beredskap." VIEW-AS ALERT-BOX.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
         RETURN.
      END.        
   END.     
   RUN anst_UI.
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
         status-mus2 = SESSION:SET-WAIT-STATE("").         
         RETURN.
      END.
      ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
      ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
         MESSAGE  Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.      
         status-mus2 = SESSION:SET-WAIT-STATE("").         
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
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.     
   END.
   ELSE DO:                          
      {AOKOLLERS.I} 
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.       
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      IF Guru.Konstanter:globforetag = "cSUND" AND ftro = TRUE THEN DO:     
         IF utsokaonr.PRISTYP = "FRÅNVARO." THEN.
         ELSE DO:
            MESSAGE "Enbart frånvaro skall registreras på personer med förtroendetid" VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.   
   FILL-IN-STARTDAT = DATE(regmnr,FILL-IN-DATUM,regar).
   FILL-IN-STOPPDAT = DATE(regmnr,FILL-IN-DATUM-2,regar).
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PERTIDA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT Guru.Konstanter:globanv,INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,
      INPUT FILL-IN-STOPPDAT,INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
      INPUT CMB_TRAK,INPUT FILL-IN_RESMAL,INPUT CMB_BEF,
      OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN PERTIDA.P  
      (INPUT Guru.Konstanter:globanv,INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,
      INPUT FILL-IN-STOPPDAT,INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
      INPUT CMB_TRAK,INPUT FILL-IN_RESMAL,INPUT CMB_BEF,
      OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      FOR EACH felmeddtemp:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
      END.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* OK */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
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
   RUN btnskapen_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEN DIALOG-1
ON GO OF BTN_SKAPEN IN FRAME DIALOG-1 /* Spara favorit */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
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
   omravdand = 1.
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BEF DIALOG-1
ON VALUE-CHANGED OF CMB_BEF IN FRAME DIALOG-1 /* Avvik.bef. */
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


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1
DO:
   omravdand = 2.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      RUN hao_UI.
      RUN resmallabel_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONRS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONRS DIALOG-1
ON ANY-KEY OF FILL-IN-AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-AONRS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONRS DIALOG-1
ON ENTRY OF FILL-IN-AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   SESSION:DATA-ENTRY-RETURN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONRS DIALOG-1
ON LEAVE OF FILL-IN-AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN-AONRS = INPUT FILL-IN-AONRS.
   SESSION:DATA-ENTRY-RETURN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONRS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN-AONRS = INPUT FILL-IN-AONRS.
   IF RAD_FAST = 1 OR RAD_FAST = 2 THEN DO:   
      RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN-AONRS).
   END.
   ELSE DO:
      RUN sokurvaldyn_UI IN brwproc[3] (INPUT "AONR", INPUT FILL-IN-AONRS).
   END.
   RUN fillinupdate_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Start datum */
DO:  
   ASSIGN
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
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med "
         tillochmeddatum VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.            
   END.                              
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.   
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM-2 DIALOG-1
ON LEAVE OF FILL-IN-DATUM-2 IN FRAME DIALOG-1 /* Slut datum */
DO:  
   ASSIGN
   FILL-IN-DATUM-2 = INPUT FILL-IN-DATUM-2.            
   IF MONTH(regdatum) = 12 THEN DO:
      datkoll = DATE(12,31,regar).
   END.
   ELSE DO:   
      datkoll = DATE((regmnr + 1),01,regar) - 1.
   END.
   IF FILL-IN-DATUM-2 > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
   END.            
   IF FILL-IN-DATUM-2 <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
   END.            
   IF tillochmeddatum NE ? THEN DO:
     IF DAY(tillochmeddatum) >= FILL-IN-DATUM-2 THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med "
         tillochmeddatum VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.            
   END.                                                            
   regdatum = DATE((regmnr),FILL-IN-DATUM-2,regar).      
   RUN REGDAG.P.   
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:  
   IF INPUT FILL-IN-AONR = "" THEN DO:
      MESSAGE "Fältet " + LC(Guru.Konstanter:gaok) + " inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.    
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.     
   RUN hao2_UI.
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
   IF RAD_FAST = 1 OR RAD_FAST = 2 THEN DO:   
      RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORTS).      
   END.
   ELSE DO:
      RUN sokurvaldyn_UI IN brwproc[3] (INPUT "ORT", INPUT FILL-IN_ORTS).      
   END.   
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
   FILL-IN-AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok 
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   BRW_EAONR:TITLE = "Favorit " + LC(Guru.Konstanter:gaol)
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:".
   {TILLFAST2.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN FILL-IN-AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}   
   BTN_SKAPEN:TOOLTIP = "Spara valt "  + LC(Guru.Konstanter:gaok) + " som favorit ".  
   RUN REGVEC.P.
   RUN REGDAG.P.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.   
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
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").   
    /*GLOBOMRADE*/
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   {ANVAVDSO.I}      
   CMB_AVD:DELIMITER = "$".       
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".   
   IF pertidaonr = ? THEN DO:
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = personaltemp.OMRADE 
      USE-INDEX OMR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
      END.
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[24] = 1 THEN DO:            
         FIND FIRST egnaao WHERE egnaao.AONR = pertidaonr AND egnaao.DELNR = pertiddelnr AND
         egnaao.AONRAVDATUM = 01/01/1991 NO-LOCK NO-ERROR.                  
         IF AVAILABLE egnaao THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(egnaao)).
            RUN fillinupdate_UI. 
            ASSIGN RAD_FAST = 3.
         END.
         ELSE DO:
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = pertidaonr AND
            utsokaonr.DELNR = pertiddelnr NO-LOCK NO-ERROR.
            IF AVAILABLE utsokaonr THEN DO:
               IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
               IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
               RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
               RUN fillinupdate_UI. 
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE NO-LOCK NO-ERROR.               
            END.            
         END.
      END.   
      ELSE DO:            
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = pertidaonr AND
         utsokaonr.DELNR = pertiddelnr NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
            IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
            RUN fillinupdate_UI. 
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = utsokaonr.OMRADE NO-LOCK NO-ERROR.            
         END.
      END.
   END.
   IF AVAILABLE omrtemp THEN DO: 
      ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
      IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
         CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      END.
   END.
   ELSE CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
   /*GLOBOMRADE*/   
   RUN grundtid_UI.   
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.      
   SESSION:DATA-ENTRY-RETURN = TRUE.
   FILL-IN-MANAD = regmannamn.
   FILL-IN_RESMAL = "".
   RUN enable_UI.       
   /*Egna aonr*/   
   IF Guru.Konstanter:varforetypval[24] = 1 THEN DO:         
   END.
   ELSE DO:            
      ASSIGN
      BRW_EAONR:HIDDEN = TRUE
      BTN_SKAPEN:HIDDEN = TRUE.
      status-ok = RAD_FAST:DELETE("Favorit" + " " + LC(Guru.Konstanter:gaok)).      
      DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:
      utsokaonr.OMRADE:VISIBLE = FALSE.   
      egnaao.OMRADE:VISIBLE = FALSE.   
   END.
   {AVVBEFW.I}   
   {FRMSIZED.I}
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      DISABLE CMB_TRAK WITH FRAME {&FRAME-NAME}.
      CMB_TRAK:HIDDEN = TRUE.
   END.
   IF  Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:            
      /*Eva T vill släcka avvik bef i dela upp 20060420*/
      DISABLE CMB_BEF WITH FRAME {&FRAME-NAME}.
   END.   
   RUN aoladda_UI.
   
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
   egnaao.OMRADE:READ-ONLY IN BROWSE BRW_EAONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_EAONR:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   RUN sattindex_UI IN brwproc[3] (INPUT "OMRADE").
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE).
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
   END. 
   ftro = FALSE.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "Csund" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      RUN fortro IN otbeordapph (INPUT pkod, OUTPUT ftro).      
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
         /*LENA TESTAR*/
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
   ENABLE FILL-IN-AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN   
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN-AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnskapen_UI DIALOG-1 
PROCEDURE btnskapen_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
         BRW_AONR:HIDDEN  IN FRAME {&FRAME-NAME} = TRUE   
         BRW_EAONR:HIDDEN = FALSE
         CMB_OMR:HIDDEN = TRUE
         CMB_AVD:HIDDEN = TRUE
         FILL-IN-TEXT:HIDDEN = TRUE.   
         ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.
         DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN-DATUM FILL-IN-DATUM-2 FILL-IN-AONR FILL-IN-DELNR CMB_TRAK 
          FILL-IN_RESMAL FILL-IN-TEXT FILL-IN-PKOD FILL-IN-MANAD CMB_OMR 
          FILL-IN-AONRS FILL-IN_ORTS FILL-IN-PRIS FILL-IN-DAG FILL-IN-DAG-2 
          CMB_AVD FILL-IN-SKP 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-DATUM FILL-IN-DATUM-2 FILL-IN-AONR FILL-IN-DELNR CMB_TRAK 
         FILL-IN_RESMAL BTN_REG BTN_AVB CMB_OMR FILL-IN-AONRS FILL-IN_ORTS 
         BTN_NVE BTN_FVE BTN_NVE-2 BTN_FVE-2 CMB_AVD RAD_FAST BTN_SKAPEN 
         RECT-33 
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
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         ELSE DISPLAY FILL-IN-AONR FILL-IN-DELNR CMB_TRAK WITH FRAME {&FRAME-NAME}.   
      END.
      APPLY "VALUE-CHANGED" TO BRW_AONR IN FRAME {&FRAME-NAME}. 
   END.
   ELSE DO:
      IF AVAILABLE egnaao THEN DO:
         ASSIGN
         FILL-IN-AONR = egnaao.AONR
         FILL-IN-DELNR = egnaao.DELNR.
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         ELSE DISPLAY FILL-IN-AONR FILL-IN-DELNR CMB_TRAK WITH FRAME {&FRAME-NAME}.   
      END.
      APPLY "VALUE-CHANGED" TO BRW_EAONR IN FRAME {&FRAME-NAME}. 
   END.
   
   RUN resmallabel_UI.
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
   ASSIGN FILL-IN-PKOD = personaltemp.PERSONALKOD.
   IF sdatum NE ? THEN DO:   
      ASSIGN
      FILL-IN-DATUM = DAY(sdatum)
      FILL-IN-DATUM-2 = DAY(sdatum).
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-DATUM = 1
      FILL-IN-DATUM-2 = 1.
   END.   
   IF tillochmeddatum NE ? THEN DO:
      IF sdatum LE tillochmeddatum  THEN DO:      
         ASSIGN
         FILL-IN-DATUM = DAY(tillochmeddatum + 1)
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
      IF sdatum = ? THEN DO:      
         ASSIGN
         FILL-IN-DATUM = DAY(tillochmeddatum + 1)
         FILL-IN-DATUM-2 = DAY(tillochmeddatum + 1).
      END.
   END.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hao_UI DIALOG-1 
PROCEDURE hao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/          
   RUN hao2_UI.
  
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
      FILL-IN-PRISTYP = sok1.
      CMB_TRAK:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(sok2).   
      CMB_TRAK = INPUT CMB_TRAK.      
      CMB_PRISTYP = FILL-IN-PRISTYP.            
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "LULE" THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pnrkoll_UI DIALOG-1 
PROCEDURE pnrkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   DEFINE OUTPUT PARAMETER musz AS LOGICAL NO-UNDO.
   musz = FALSE.   
   IF FILL-IN_RESMAL NE "0000000000" THEN DO:  
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,1,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,2,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,3,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,4,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,5,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,6,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,7,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,8,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,9,1)) = 0 THEN musz = TRUE.
      IF INDEX("0123456789",SUBSTRING(FILL-IN_RESMAL,10,1)) = 0 THEN musz = TRUE.
      IF musz = TRUE THEN musz = musz.
      ELSE DO:      
         persnr[1] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),1,1)).
         persnr[2] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),2,1)).
         persnr[3] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),3,1)).
         persnr[4] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),4,1)).
         persnr[5] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),5,1)).
         persnr[6] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),6,1)).
         persnr[7] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),8,1)).
         persnr[8] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),9,1)).
         persnr[9] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),10,1)).
         persnr[10] = INTEGER(SUBSTRING(STRING(FILL-IN_RESMAL,"999999-9999"),11,1)).         
         persnr[1] = persnr[1] * 2.
         IF persnr[1] > 9 THEN persnr[1] = 
         INTEGER(SUBSTRING(STRING(persnr[1],"99"),1,1)) + 
         INTEGER(SUBSTRING(STRING(persnr[1],"99"),2,1)).
         persnr[3] = persnr[3] * 2.
         IF persnr[3] > 9 THEN persnr[3] =
         INTEGER(SUBSTRING(STRING(persnr[3],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[3],"99"),2,1)).
         persnr[5] = persnr[5] * 2.
         IF persnr[5] > 9 THEN persnr[5] =
         INTEGER(SUBSTRING(STRING(persnr[5],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[5],"99"),2,1)).
         persnr[7] = persnr[7] * 2.
         IF persnr[7] > 9 THEN persnr[7] =
         INTEGER(SUBSTRING(STRING(persnr[7],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[7],"99"),2,1)).
         persnr[9] = persnr[9] * 2.
         IF persnr[9] > 9 THEN persnr[9] =
         INTEGER(SUBSTRING(STRING(persnr[9],"99"),1,1)) +
         INTEGER(SUBSTRING(STRING(persnr[9],"99"),2,1)).
         tal1 = persnr[1] + persnr[2] + persnr[3] + persnr[4] + persnr[5] +
         persnr[6] + persnr[7] + persnr[8] + persnr[9].
   
         IF tal1 > 99 THEN
         tal2 = INTEGER(SUBSTRING(STRING(tal1,"999"),3,1)).
         IF tal1 < 100 THEN
         tal2 = INTEGER(SUBSTRING(STRING(tal1,"99"),2,1)).
         ksiffran = 10 - tal2.
         IF ksiffran = 10 THEN ksiffran = 0.
         IF persnr[10] = ksiffran OR (persnr[7] = 0 AND persnr[8] = 0 AND
         persnr[9] = 0 AND persnr[10] = 0) THEN ksiffran = ksiffran.
         ELSE IF (persnr[7] = 0 AND persnr[8] = 0 AND
         persnr[9] = 0 AND persnr[10] = 1) THEN ksiffran = ksiffran.
         ELSE DO:
            musz = TRUE.         
         END.   
      END.
   END.
   ELSE musz = TRUE.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resmallabel_UI DIALOG-1 
PROCEDURE resmallabel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  -------------------------------------------------------------*/    
   musz = FALSE.   
   /*vab ska inte ha krav på personnummer FILL-IN-AONR = "118"  lena 20181211*/
   IF Guru.Konstanter:globforetag = "cSUND" AND FILL-IN-AONR = "118"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SUND" AND FILL-IN-AONR = "119"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SUND" AND FILL-IN-AONR = "117"  THEN musz = TRUE.           
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "118"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "119"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "117"  THEN musz = TRUE.
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "191"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "192"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "193"  THEN musz = TRUE.
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "194"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "cMISV" AND FILL-IN-AONR = "118"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "MISV" AND FILL-IN-AONR = "119"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "MISV" AND FILL-IN-AONR = "117"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "celpa" AND FILL-IN-AONR = "118"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "elpa" AND FILL-IN-AONR = "117"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN-AONR = "131"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN-AONR = "132"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "GKAL" AND FILL-IN-AONR = "134"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "LULE" AND FILL-IN-AONR = "130"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "LULE" AND FILL-IN-AONR = "131"  THEN musz = TRUE.
   IF musz = TRUE THEN DO:
      IF Guru.Konstanter:globforetag = "LULE" THEN DO:
         /*6 siffror*/
         musz = FALSE.
         FILL-IN_RESMAL:FORMAT IN FRAME {&FRAME-NAME} = "XXXXXX" .
         FILL-IN_RESMAL:LABEL = "Barnets pnr".         
         FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR.
         IF FILL-IN_RESMAL = "" THEN ASSIGN FILL-IN_RESMAL = "000000".
      END.
      ELSE DO:
         musz = FALSE.
         FILL-IN_RESMAL:FORMAT IN FRAME {&FRAME-NAME} = "XXXXXX-XXXX" .
         FILL-IN_RESMAL:LABEL = "Barnets pnr".         
         FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR.
         IF FILL-IN_RESMAL = "" THEN ASSIGN FILL-IN_RESMAL = "0000000000".
     END.    
   END.
   ELSE DO:
      FILL-IN_RESMAL:FORMAT = "X(158)".
      FILL-IN_RESMAL:LABEL = "Kommentar".
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

