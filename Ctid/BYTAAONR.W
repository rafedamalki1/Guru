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

/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
&Scoped-define NEW
{PERBEF.I}
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
&Scoped-define SHARED SHARED
{PHMT.I}
{DIRDEF.I}
{UPPGHMT.I}
{SOKDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE varaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vardelnr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE vartrakt AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE varpris AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE varslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE varpristyp  AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE varfabef AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE varutryck AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE vartotalt AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE debitering AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE reov AS LOGICAL NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE spaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE valdkom AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS FILL-IN-AONR FILL-IN-DELNR CMB_TRAK ~
FILL-IN-UTRYCK CMB_PRISTYP BTN_REG BTN_AVS RAD_BYTA CMB_OMR FILL-IN_AONRS ~
FILL-IN_ORTS CMB_AVD RAD_FAST BTN_SKAPEN RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN-AONR FILL-IN-DELNR ~
CMB_TRAK FILL-IN-UTRYCK CMB_PRISTYP FILL-IN-PRIS RAD_BYTA FILL-IN-TEXT ~
CMB_OMR FILL-IN_AONRS FILL-IN_ORTS FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 

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
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_REG 
     LABEL "ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKAPEN 
     LABEL "Spara favorit":L 
     SIZE 14.5 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.13 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BEF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avvik. befattning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.13 BY 1 NO-UNDO.

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

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.13 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTRYCK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Utryckning" 
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
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_VIBEFATTNING AS CHARACTER FORMAT "x(256)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE RAD_BYTA AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Byta aonr", 1,
"Byta debitering", 2
     SIZE 61.5 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", 1,
"Fasta aonr", 2,
"Favorit aonr", 3
     SIZE 43.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62 BY 1.21
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 62 BY 12
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 62 BY 12
         TITLE "Favorit arbetsordernummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_EAONR AT ROW 5.5 COL 55.5
     FILL-IN-PKOD AT ROW 3.25 COL 18.5 COLON-ALIGNED
     FILL-IN-VECKO AT ROW 4.63 COL 18.5 COLON-ALIGNED
     FILL-IN-MANAD AT ROW 4.63 COL 18.5 COLON-ALIGNED NO-LABEL
     FILL-IN-AONR AT ROW 5.92 COL 18.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 7.21 COL 18.5 COLON-ALIGNED
     BRW_AONR AT ROW 5.5 COL 55.5
     CMB_TRAK AT ROW 8.63 COL 18.5 COLON-ALIGNED
     FILL-IN-UTRYCK AT ROW 9.83 COL 18.5 COLON-ALIGNED
     CMB_PRISTYP AT ROW 11.13 COL 18.5 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 12.29 COL 18.5 COLON-ALIGNED
     FILL-IN-PRISTYP AT ROW 14.04 COL 18.5 COLON-ALIGNED
     FILL-IN-VALFA AT ROW 15.54 COL 18.5 COLON-ALIGNED NO-LABEL
     CMB_BEF AT ROW 16.92 COL 18.5 COLON-ALIGNED
     FILL-IN_VIBEFATTNING AT ROW 16.92 COL 36 COLON-ALIGNED HELP
          "BEFATTNINGSKOD" NO-LABEL
     BTN_REG AT ROW 19.46 COL 88.5
     BTN_AVS AT ROW 19.46 COL 103.5
     RAD_BYTA AT ROW 1.71 COL 1.5 NO-LABEL
     FILL-IN-TEXT AT ROW 1.71 COL 93.38 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 4.33 COL 93.38 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 18.08 COL 71.38 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 18.08 COL 94 COLON-ALIGNED
     FILL-IN-SKP AT ROW 18.04 COL 54.75 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 3 COL 93.38 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 4.33 COL 50.38 NO-LABEL
     BTN_SKAPEN AT ROW 5.92 COL 29.88
     FILL-IN_FORNAMN-2 AT ROW 3.25 COL 30.13 NO-LABEL
     RECT-22 AT ROW 17.88 COL 55.5
     SPACE(0.49) SKIP(1.73)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Byta aonr på markerade tidregistreringar":L.


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

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-PRIS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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

/* SETTINGS FOR FILL-IN FILL-IN-VECKO IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VECKO:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

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
ON END-ERROR OF FRAME DIALOG-1 /* Byta aonr på markerade tidregistreringar */
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
ON ENDKEY OF FRAME DIALOG-1 /* Byta aonr på markerade tidregistreringar */
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
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      END.
      ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
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
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      END.
      ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS DIALOG-1
ON CHOOSE OF BTN_AVS IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* ok */
DO:
   {muswait.i}  
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
   END.
   ELSE DO:      
      FILL-IN-PRIS = INPUT FILL-IN-PRIS. 
      varpris = FILL-IN-PRIS.
   END.
   ASSIGN      
   musz = FALSE   
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR        
   vartrakt = INPUT CMB_TRAK
   CMB_PRISTYP = INPUT CMB_PRISTYP 
   FILL-IN-PRISTYP = CMB_PRISTYP.     
   ASSIGN
   varpristyp = FILL-IN-PRISTYP     
   varaonr = FILL-IN-AONR  
   vardelnr = FILL-IN-DELNR.  
   varutryck = INPUT FILL-IN-UTRYCK.
   FILL-IN_VIBEFATTNING = INPUT FRAME {&FRAME-NAME} FILL-IN_VIBEFATTNING.
   IF FILL-IN_VIBEFATTNING NE "" THEN DO:
      FIND FIRST befattningstemp WHERE befattningstemp.NAMN = FILL-IN_VIBEFATTNING
      NO-LOCK NO-ERROR.
      varfabef = befattningstemp.BEFATTNING.
   END.
   ELSE varfabef = FILL-IN_VIBEFATTNING.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN vartrakt = 0.
   IF RAD_BYTA = 1 THEN DO:
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
         ELSE IF soktemp.SOKDATE[1] < avdatum  THEN DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.      
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.      
         END.
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN AOVALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT avdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
             INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
             OUTPUT TABLE felmeddtemp).            
         END.
         ELSE DO:
            RUN AOVALK.P 
            (INPUT avdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
             INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
             OUTPUT TABLE felmeddtemp).            
         END.  
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            RETURN NO-APPLY.      
         END.
      END.
      ELSE DO:
         regdatum = avdatum.         
         {AOKOLLERS.I}         
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
         /* alla prnr med obligatorisk kommentar kan inte ändras i byt prnr*/
         musz = FALSE.
         RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.            
            MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.      
         RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.            
            MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.      
         musz = FALSE.
         RUN kommentaroblcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.            
            MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.      
         
         RUN kommoblkom_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz, OUTPUT valdkom ).      
         IF musz = TRUE THEN DO:
            musz = FALSE.            
            MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.         
         RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.            
            MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.      
         /* arbetstidsförkortning*/
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR  Guru.Konstanter:globforetag = "ELPA" THEN DO:
            IF FILL-IN-AONR = "160" OR FILL-IN-AONR = "161"  THEN DO:
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "Arbetstidförkortning ." 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            
            IF FILL-IN-AONR = "155"  THEN DO:
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.            
            IF spaonr = "155" AND FILL-IN-AONR NE "155" THEN DO:
               MESSAGE "Det går ej att ändra flex i tidskrivningen" VIEW-AS ALERT-BOX.                 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.               
            END.
         END.
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
            IF FILL-IN-AONR = "160" AND FILL-IN-DELNR = 0   THEN DO:
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "Arbetstidförkortning ." 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            IF FILL-IN-AONR = "130" OR FILL-IN-AONR = "131" OR FILL-IN-AONR = "143" OR FILL-IN-AONR = "250"  THEN DO:
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            musz = FALSE.
            RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.            
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.      

         END.
         IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
            IF FILL-IN-AONR = "160"   THEN DO:
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "Arbetstidförkortning ." 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            IF FILL-IN-AONR = "131" OR FILL-IN-AONR = "132" OR FILL-IN-AONR = "140" OR FILL-IN-AONR = "143" OR FILL-IN-AONR = "144" OR FILL-IN-AONR = "145"  THEN DO:
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.
            musz = FALSE.
            RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.            
               MESSAGE "Använd ändra " Guru.Konstanter:gaok "för " FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) 
               VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
               RETURN NO-APPLY.
            END.      

         END.         
      END.
      IF AVAILABLE utsokaonr THEN DO:     
         IF reov = TRUE AND utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:
            MESSAGE  "Frånvaro kan inte registreras som övertid eller restid " 
            VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
   END.
   ELSE IF RAD_BYTA = 2 THEN DO:
      ASSIGN
      sok4 = pkod
      sok3 = FILL-IN-PRISTYP.
      RUN nyupp_UI (INPUT 3).      
      IF sok5 = ? THEN DO:
         MESSAGE "Pristyp" FILL-IN-PRISTYP  "finns inte." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-PRISTYP IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.        
   END.                
   ELSE IF RAD_BYTA = 3 THEN DO:
   END. 
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* ok */
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
   CMB_AVD = INPUT CMB_AVD.       
   omravdand = 1.   
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1].
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BEF DIALOG-1
ON VALUE-CHANGED OF CMB_BEF IN FRAME DIALOG-1 /* Avvik. befattning */
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
      sok3 = utsokaonr.PRISTYP.
      RUN nyupp_UI (INPUT 3).      
      FILL-IN-PRIS = sok5.   
      DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      RUN hao2_UI.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF AVAILABLE utsokaonr THEN DO:         
         ASSIGN
         sok4 = pkod
         sok3 = utsokaonr.PRISTYP.
         RUN nyupp_UI (INPUT 3).      
         ASSIGN
         FILL-IN-PRISTYP = utsokaonr.PRISTYP
         CMB_TRAK = utsokaonr.TRAKTAMENTE
         FILL-IN-UTRYCK = utsokaonr.UTRYCKNING
         CMB_PRISTYP = FILL-IN-PRISTYP
         CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP.             
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
         WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                    
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:      
            FILL-IN-PRIS = sok5.   
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.
      END.   
      ELSE DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 47
         soktemp.SOKCHAR[1] = FILL-IN-AONR
         soktemp.SOKINT[1] = FILL-IN-DELNR.
         {SOKANROP.I}               
         ASSIGN
         sok4 = pkod
         sok3 = soktemp.SOKCHAR[3].
         RUN nyupp_UI (INPUT 3).      
         ASSIGN
         FILL-IN-PRISTYP = soktemp.SOKCHAR[3]
         CMB_TRAK = soktemp.SOKINT[3]         
         CMB_PRISTYP = FILL-IN-PRISTYP
         CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP.             
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
         WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                    
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:      
            FILL-IN-PRIS = sok5.   
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      CMB_TRAK = 0.
      CMB_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
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
         RETURN NO-APPLY.      
      END.
      ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
      ELSE IF soktemp.SOKDATE[1] < avdatum  THEN DO:
         MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.               
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.      
      END.
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN AOVALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT avdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
          INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
          OUTPUT TABLE felmeddtemp).            
      END.
      ELSE DO:
         RUN AOVALK.P 
         (INPUT avdatum,INPUT ?,INPUT personaltemp.PERSONALKOD,
          INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,
          OUTPUT TABLE felmeddtemp).            
      END.  
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.      
      END.      
   END.
   ELSE DO:
      ASSIGN               
      regvnr = FILL-IN-VECKO
      regdagnamn = "MÅN".
      RUN VECODAT.P.         
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      IF AVAILABLE utsokaonr THEN DO:               
         ASSIGN
         sok4 = pkod      
         sok3 = utsokaonr.PRISTYP.
         RUN nyupp_UI (INPUT 3).      
         ASSIGN
         FILL-IN-PRISTYP = utsokaonr.PRISTYP
         CMB_TRAK = utsokaonr.TRAKTAMENTE
         FILL-IN-UTRYCK = utsokaonr.UTRYCKNING
         CMB_PRISTYP = FILL-IN-PRISTYP
         CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP.           
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
         WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                    
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:      
            FILL-IN-PRIS = sok5.   
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.    
      END.
      ELSE DO:
         ASSIGN
         sok4 = pkod      
         sok3 = soktemp.SOKCHAR[3].
         RUN nyupp_UI (INPUT 3).      
         ASSIGN
         FILL-IN-PRISTYP = soktemp.SOKCHAR[3]
         CMB_TRAK = soktemp.SOKINT[3]
         FILL-IN-UTRYCK = FALSE
         CMB_PRISTYP = FILL-IN-PRISTYP
         CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP.           
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR FILL-IN-UTRYCK 
         WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:                    
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DO:      
            FILL-IN-PRIS = sok5.   
            DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.    
      END.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      CMB_TRAK = 0.
      CMB_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   RUN hao2_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-UTRYCK
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
   SESSION:DATA-ENTRY-RETURN = TRUE.
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


&Scoped-define SELF-NAME RAD_BYTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_BYTA DIALOG-1
ON VALUE-CHANGED OF RAD_BYTA IN FRAME DIALOG-1
DO:
   RAD_BYTA = INPUT RAD_BYTA.
   ASSIGN
   BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-SKP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   RECT-22:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_AONRS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_ORTS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   RAD_FAST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_VIBEFATTNING:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   CMB_BEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  
   DISABLE CMB_PRISTYP CMB_TRAK FILL-IN-AONR FILL-IN-DELNR
   FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.   
   IF RAD_BYTA = 1 THEN DO:
      ASSIGN 
      debitering = 1
      FILL-IN-SKP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RECT-22:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN_AONRS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN_ORTS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-AONR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-DELNR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-UTRYCK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE  
      RAD_FAST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
      ENABLE CMB_TRAK FILL-IN-AONR FILL-IN-DELNR
      FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.   
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:         
         CMB_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      END.
   END.
   ELSE IF RAD_BYTA = 2 THEN DO:
      ASSIGN debitering = 2
      FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      CMB_PRISTYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.  
      ENABLE CMB_PRISTYP FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.   
   END.
   ELSE IF RAD_BYTA = 3 THEN DO:
      ASSIGN
      FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-VALFA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE CMB_BEF WITH FRAME {&FRAME-NAME}.   
      debitering = 3.
      {AVVBEFW.I}
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
   CMB_PRISTYP:LABEL = Guru.Konstanter:gdebk    
   FILL-IN-PRISTYP:LABEL = Guru.Konstanter:gdebk
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok 
   FILL-IN-SKP = "Sök på"
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:" 
   status-ok = RAD_BYTA:DELETE("Byta aonr")
   status-ok = RAD_BYTA:DELETE("Byta debitering").
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}   
   BTN_SKAPEN:TOOLTIP = "Spara valt "  + LC(Guru.Konstanter:gaok) + " som favorit ".  
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.      
   RAD_BYTA = 1.
   RAD_BYTA:ADD-LAST("Byta " + LC(Guru.Konstanter:gaok), 1).
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
   END.
   ELSE DO:      
      RAD_BYTA:ADD-LAST("Byta " + LC(Guru.Konstanter:gdebk), 2). 
   END.
   {TILLFAST2.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   ASSIGN              
   FILL-IN-VECKO = regvnr
   FILL-IN-MANAD = regmannamn
   FILL-IN-UTRYCK = FALSE
   FILL-IN-AONR = varaonr  
   FILL-IN-DELNR = vardelnr.   
   spaonr = FILL-IN-AONR.
   FOR EACH automregtemp USE-INDEX PRISTYPER NO-LOCK:
      status-ok = CMB_PRISTYP:ADD-LAST(automregtemp.PRISTYP).
   END.  
   
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").   
   {ANVAVDSO.I}   
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.   
   
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".   
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND    
   utsokaonr.DELNR = FILL-IN-DELNR 
   USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:   
       IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
       END.
       ELSE DO:      
          ASSIGN
          sok4 = pkod
          sok3 = utsokaonr.PRISTYP.
          RUN nyupp_UI (INPUT 3).      
          FILL-IN-PRIS = sok5.   
       END.
       ASSIGN   
       FILL-IN-PRISTYP = utsokaonr.PRISTYP
       CMB_PRISTYP = utsokaonr.PRISTYP
       CMB_TRAK:SCREEN-VALUE = STRING(utsokaonr.TRAKTAMENTE).
   END.
   ELSE DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 47
      soktemp.SOKCHAR[1] = FILL-IN-AONR
      soktemp.SOKINT[1] = FILL-IN-DELNR.
      {SOKANROP.I}                     
       IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" OR (Guru.Konstanter:globforetag = "GRAN" AND regdatum >= 10/01/2002) THEN DO:
       END.
       ELSE DO:      
          ASSIGN
          sok4 = pkod
          sok3 = soktemp.SOKCHAR[3].
          RUN nyupp_UI (INPUT 3).      
          FILL-IN-PRIS = sok5.   
       END.
       ASSIGN   
       FILL-IN-PRISTYP = soktemp.SOKCHAR[3]
       CMB_PRISTYP = soktemp.SOKCHAR[3]
       CMB_TRAK:SCREEN-VALUE = STRING(soktemp.SOKINT[3]).
   END.
   ASSIGN
   FILL-IN-PKOD = pkod
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
   CMB_TRAK = INPUT CMB_TRAK.    
   DISPLAY FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:varforetypval[3] >= 1 OR Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      IF Guru.Konstanter:varforetypval[3] = 4 THEN.
      ELSE status-ok = RAD_BYTA:ADD-LAST("Byta befattning", 3). 
   END.  
   RUN enable_UI.       
   {FRMSIZED.I}   
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
   ASSIGN    
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      CMB_TRAK = 0.
      CMB_TRAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
   APPLY "VALUE-CHANGED" TO RAD_BYTA.
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
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN   
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.

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
  DISPLAY FILL-IN-PKOD FILL-IN-AONR FILL-IN-DELNR CMB_TRAK FILL-IN-UTRYCK 
          CMB_PRISTYP FILL-IN-PRIS RAD_BYTA FILL-IN-TEXT CMB_OMR FILL-IN_AONRS 
          FILL-IN_ORTS FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-AONR FILL-IN-DELNR CMB_TRAK FILL-IN-UTRYCK CMB_PRISTYP BTN_REG 
         BTN_AVS RAD_BYTA CMB_OMR FILL-IN_AONRS FILL-IN_ORTS CMB_AVD RAD_FAST 
         BTN_SKAPEN RECT-22 
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
         ASSIGN
         sok1 = utsokaonr.AONR      
         sok2 = utsokaonr.DELNR.         
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
         ASSIGN
         sok1 = egnaao.AONR      
         sok2 = egnaao.DELNR.    
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         ELSE DISPLAY FILL-IN-AONR FILL-IN-DELNR CMB_TRAK WITH FRAME {&FRAME-NAME}.   
      END.
      APPLY "VALUE-CHANGED" TO BRW_EAONR IN FRAME {&FRAME-NAME}. 
   END.
   
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

