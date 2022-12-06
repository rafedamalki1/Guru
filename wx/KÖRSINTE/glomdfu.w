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
&Scoped-define SHARED 
{FLEXTAB.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{OTIDBEORD.I}
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE frustarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE fruslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffestart AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE kaffeslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchstarten AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lunchslutet AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE regtotalt AS DECIMAL NO-UNDO.   
DEFINE SHARED VARIABLE regstart AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE regslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regstartsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regslutsek AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE valaonrrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE valdelnrlog AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE dagnr AS INTEGER NO-UNDO. 
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE status-mus2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-mus AS LOGICAL NO-UNDO.
DEFINE VARIABLE flexkvst AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvsl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexkvslspar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmostspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE flexavikapph AS HANDLE NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.   /*används i NYCOL.I*/
DEFINE VARIABLE hjtid AS DECIMAL NO-UNDO.
DEFINE VARIABLE sptid AS DECIMAL NO-UNDO.
DEFINE VARIABLE dagtotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE persnr AS INTEGER EXTENT 10 FORMAT "99" NO-UNDO.
DEFINE VARIABLE tal1 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal2 AS INTEGER NO-UNDO.
DEFINE VARIABLE ksiffran AS INTEGER NO-UNDO.
DEFINE VARIABLE bpnr AS DATE NO-UNDO.
DEFINE VARIABLE balder AS DECIMAL NO-UNDO.
DEFINE VARIABLE valdkom AS CHARACTER NO-UNDO.
{OMRTEMPW.I}
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}
{SOKDEF.I}
&Scoped-define NEW   
&Scoped-define SHARED SHARED
{DIRDEF.I}
/*{EGENBEN.I}*/
{AVDELNINGTEMPT.I}
DEFINE TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.

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
&Scoped-define INTERNAL-TABLES utsokaonr egnaao otidbeordtemp

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


/* Definitions for BROWSE BRW_OTBRD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_OTBRD otidbeordtemp.PERSONALKOD ~
otidbeordtemp.FORNAMN otidbeordtemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OTBRD 
&Scoped-define QUERY-STRING-BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_OTBRD OPEN QUERY BRW_OTBRD FOR EACH otidbeordtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_OTBRD otidbeordtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OTBRD otidbeordtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_OTBRD}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_DATUM FILL-IN_TID CMB_KNAPP ~
FILL-IN-AONR FILL-IN-DELNR BTN_REG BTN_AVB CMB_OMR BTN_NVE-2 BTN_FVE-2 ~
CMB_AVD FILL-IN_RESMAL RAD_FAST BTN_SKAPEN RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_DATUM FILL-IN_TID CMB_KNAPP ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-VISAONR FILL-IN_PERSONALKOD CMB_OMR ~
FILL-IN-STDAG FILL-IN-SKP CMB_AVD FILL-IN_RESMAL 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klock60 DIALOG-1 
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_AONR-3 
       MENU-ITEM m_Arbetsuppgift-3 LABEL "Arbetsuppgift" .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
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

DEFINE VARIABLE CMB_KNAPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Knapp" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     LIST-ITEMS "In","Ut","Lunch ut","Lunch in","Övertid ut","Övertid in","Annat in","Annat ut","Flex in","Flex ut" 
     DROP-DOWN-LIST
     SIZE 13.38 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OVERUT AS CHARACTER FORMAT "X(4)":U 
     LABEL "Övertiduttag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Komp,","Över,","Flex,","Ejöv" 
     DROP-DOWN-LIST
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OTBRD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Övertidsbeordrare" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OVER AS CHARACTER FORMAT "X(1)":U 
     LABEL "Övertiduttag" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKP AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STDAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VISAONR AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 45.5 BY 1.

DEFINE VARIABLE FILL-IN_TID AS DECIMAL FORMAT "99.99" INITIAL 0 
     LABEL "Tid" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE RAD_FAST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", 1,
"Fasta aonr", 2,
"Favorit aonr", 3
     SIZE 43.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57.5 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_EAONR FOR 
      egnaao SCROLLING.

DEFINE QUERY BRW_OTBRD FOR 
      otidbeordtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57.5 BY 11.83
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57.5 BY 11.83
         TITLE "Favorit arbetsordernummer".

DEFINE BROWSE BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OTBRD DIALOG-1 _STRUCTURED
  QUERY BRW_OTBRD NO-LOCK DISPLAY
      otidbeordtemp.PERSONALKOD COLUMN-LABEL "Enhet" FORMAT "x(5)":U
            WIDTH 7
      otidbeordtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      otidbeordtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 21.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 43 BY 6.75
         TITLE "Övertidsbeordrare" EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_OTBRD AT ROW 18.33 COL 67.5
     FILL-IN_DATUM AT ROW 6.92 COL 18.5 COLON-ALIGNED
     FILL-IN_TID AT ROW 8.58 COL 18.5 COLON-ALIGNED
     CMB_KNAPP AT ROW 10.25 COL 18.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 11.88 COL 18.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DELNR AT ROW 13.54 COL 18.5 COLON-ALIGNED AUTO-RETURN 
     BTN_REG AT ROW 25.5 COL 81.5
     BTN_AVB AT ROW 25.5 COL 96.5
     FILL-IN-VISAONR AT ROW 1.25 COL 86 COLON-ALIGNED NO-LABEL
     FILL-IN_PERSONALKOD AT ROW 5.25 COL 18.5 COLON-ALIGNED
     CMB_OMR AT ROW 3.38 COL 86.25 COLON-ALIGNED NO-LABEL
     BRW_AONR AT ROW 4.58 COL 53
     BRW_EAONR AT ROW 4.58 COL 53
     BTN_NVE-2 AT ROW 6.75 COL 31
     FILL-IN-STDAG AT ROW 6.92 COL 32.63 COLON-ALIGNED NO-LABEL
     BTN_FVE-2 AT ROW 7.58 COL 31
     FILL-IN-SKP AT ROW 16.75 COL 51.5 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 16.75 COL 66 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 16.75 COL 89.5 COLON-ALIGNED
     CMB_AVD AT ROW 2.25 COL 86.25 COLON-ALIGNED NO-LABEL
     FILL-IN_RESMAL AT ROW 18.5 COL 18.5 COLON-ALIGNED
     FILL-IN-OTBRD AT ROW 16.83 COL 18.5 COLON-ALIGNED
     CMB_OVERUT AT ROW 15.21 COL 18.5 COLON-ALIGNED
     FILL-IN-OVER AT ROW 12.75 COL 46.5 COLON-ALIGNED HELP
          "ÖVERTID = Ö, KOMP = K, INGEN ERSÄTTNING = I"
     RAD_FAST AT ROW 3 COL 43 NO-LABEL
     BTN_SKAPEN AT ROW 11.88 COL 31.5
     RECT-22 AT ROW 16.58 COL 53
     SPACE(0.49) SKIP(8.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera flextid":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: egnaao T "?" NO-UNDO temp-db egnaao
      TABLE: otidbeordtemp T "?" NO-UNDO temp-db otidbeordtemp
      TABLE: utsokaonr T "?" NO-UNDO TEMP-DB utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB BRW_OTBRD 1 DIALOG-1 */
/* BROWSE-TAB BRW_AONR CMB_OMR DIALOG-1 */
/* BROWSE-TAB BRW_EAONR BRW_AONR DIALOG-1 */
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

/* SETTINGS FOR BROWSE BRW_OTBRD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_OTBRD:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OVERUT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_OVERUT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OTBRD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OTBRD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OVER IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OVER:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-STDAG IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-VISAONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OTBRD
/* Query rebuild information for BROWSE BRW_OTBRD
     _TblList          = "Temp-Tables.otidbeordtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.otidbeordtemp.PERSONALKOD
"PERSONALKOD" "Enhet" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.otidbeordtemp.FORNAMN
"FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.otidbeordtemp.EFTERNAMN
"EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "21.25" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OTBRD */
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
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera flextid */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera flextid */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
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


&Scoped-define BROWSE-NAME BRW_OTBRD
&Scoped-define SELF-NAME BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OTBRD DIALOG-1
ON VALUE-CHANGED OF BRW_OTBRD IN FRAME DIALOG-1 /* Övertidsbeordrare */
DO:  
  
   IF AVAILABLE otidbeordtemp THEN DO:
      ASSIGN FILL-IN-OTBRD = otidbeordtemp.PERSONALKOD.
      DISPLAY FILL-IN-OTBRD WITH FRAME {&FRAME-NAME}.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.
   
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 DIALOG-1
ON CHOOSE OF BTN_FVE-2 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM - 1.     
   REPEAT:
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(FILL-IN_DATUM).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.      
      IF sok2 = 0 THEN dagnr = WEEKDAY(FILL-IN_DATUM).
      ELSE dagnr = sok2.
      IF dagnr = 1 OR dagnr = 7 THEN DO:
         FILL-IN_DATUM = FILL-IN_DATUM - 1. 
      END.
      ELSE LEAVE.         
   END.
   RUN dag_UI.       
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 DIALOG-1
ON CHOOSE OF BTN_NVE-2 IN FRAME DIALOG-1 /* + */
DO:   
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM.   
   FILL-IN_DATUM = FILL-IN_DATUM + 1.  
   REPEAT:
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(FILL-IN_DATUM).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END. 
      IF sok2 = 0 THEN dagnr = WEEKDAY(FILL-IN_DATUM).
      ELSE dagnr = sok2.
      IF dagnr = 1 OR dagnr = 7 THEN DO:
         FILL-IN_DATUM = FILL-IN_DATUM + 1. 
      END.
      ELSE LEAVE.         
   END.
   RUN dag_UI.      
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:   
   ASSIGN
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   FILL-IN_TID = INPUT FILL-IN_TID  
   CMB_KNAPP = INPUT CMB_KNAPP
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR
   FILL-IN_RESMAL = INPUT FILL-IN_RESMAL
   FILL-IN-OTBRD = INPUT FILL-IN-OTBRD
   regdatum = FILL-IN_DATUM.   
   EMPTY TEMP-TABLE felmeddtemp  NO-ERROR. 
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE anvandartemp THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 3,INPUT personaltemp.PERSONALKOD,INPUT anvandartemp.ANVANDARE,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 3,INPUT personaltemp.PERSONALKOD,INPUT anvandartemp.ANVANDARE,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.         
         APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      SUBSTRING(FILL-IN_RESMAL,159,6) = SUBSTRING(FILL-IN-OTBRD,1,6).
   END.            

   IF CMB_OVERUT = "Komp" THEN FILL-IN-OVER = "K".
   IF CMB_OVERUT = "Över" THEN FILL-IN-OVER = "Ö".
   IF CMB_OVERUT = "Flex" THEN FILL-IN-OVER = "F". 
   IF CMB_OVERUT = "Ejöv" THEN FILL-IN-OVER = "I". 
   IF CMB_OVERUT = "Ledi" THEN FILL-IN-OVER = "L".
   IF CMB_OVERUT = "Komb" THEN FILL-IN-OVER = "L".


   IF FILL-IN_TID > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.      
   IF FILL-IN_TID = 00.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END. 
   IF SUBSTRING(STRING(FILL-IN_TID,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF FILL-IN_DATUM > TODAY THEN DO:
      MESSAGE "Inga registreringar framåt i tiden." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RUN REGVEC.P.
   RUN REGDAG.P.
   pkod = personaltemp.PERSONALKOD.
   {SLFLARBW.I}
   sok4 = STRING(regdatum).
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 9,INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 9,INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5). 
   END.
   IF sok4 = "" THEN sok4 = sok4.
   ELSE IF regdatum LE DATE(sok4) THEN DO:
      MESSAGE "Tidsedeln är godkänd/färdigrapporterad tom"  DATE(sok4) VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.      
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      IF CMB_KNAPP = "Övertid in" OR CMB_KNAPP = "Övertid ut" THEN DO:      
         IF regstart = regslut THEN musz = musz.
         ELSE DO:         
            IF FILL-IN_TID GE regstart AND FILL-IN_TID < regslut  THEN DO:
               MESSAGE "Det går inte att registrera övertid under ordinarie arbetstid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
         END.
         IF FILL-IN-OTBRD NE ""  THEN DO:      
            FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = FILL-IN-OTBRD AND otidbeordtemp.AKTIV = TRUE  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE otidbeordtemp  THEN DO:
               MESSAGE "Angiven övertidsbeordrare finns inte"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.
            IF FILL-IN-OTBRD = personaltemp.PERSONALKOD THEN DO:                  
               MESSAGE "Man kan inte berodra sig själv"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
               
            END.
         END.
         IF CMB_OVERUT = "Flex" THEN DO:
            MESSAGE "Övertid kan inte läggas som flex." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         /*IF kollfl = 1 THEN musz = musz.*/
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Kommentar är obligatorik vid övertid" VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.                             
         /*IF FILL-IN-OVER = "F" OR personaltemp.OVERTIDUTTAG = "I" THEN DO:
            IF FILL-IN-OTBRD NE "" THEN DO:
               MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.      
         END.         */
         IF personaltemp.OVERTIDUTTAG = "I" THEN DO:
            IF FILL-IN-OTBRD NE "" THEN DO:
               MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
               status-mus2 = SESSION:SET-WAIT-STATE("").
               RETURN.
            END.      
         END.
         ELSE DO:      
            /*IF kollfl = 1 THEN musz = musz.
            ELSE DO:            */
               IF FILL-IN_TID GE regslut  THEN DO:                       
                  IF FILL-IN-OTBRD = "" THEN DO:
                     MESSAGE "Övertidsbeordrare måste anges vid övertid. Välj från vallista!"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.      
                  /*IF FILL-IN_ORSAK = "" THEN DO:
                     MESSAGE "Kommentar är obligatorik vid övertid"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.*/
               END.
               ELSE IF FILL-IN_TID < regslut AND FILL-IN_TID GE regstart  THEN DO:                       
                  IF FILL-IN-OTBRD NE "" THEN DO:
                     MESSAGE "Övertidsbeordrare skall skall bara användas för övertid"  VIEW-AS ALERT-BOX. 
                     status-mus2 = SESSION:SET-WAIT-STATE("").
                     RETURN.
                  END.      
               END.
            /*END.*/
         END.    
         
      END.
      
      sok2 = 0.
      /*IF personaltemp.OVERTIDUTTAG = "I" THEN musz = musz.
      /*ELSE IF FILL-IN-START GE regstart AND FILL-IN-SLUT LE regslut  THEN musz = musz.*/
      ELSE DO:                        
         ASSIGN
         sok1 = personaltemp.PERSONALKOD
         sok4 = STRING(FILL-IN_DATUM)
         sok5 = regstart.

         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 47,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 47,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
      END.           
      
      hjtid = klock60(klock100(sok2 / 100) + klock100(FILL-IN_TID) - klock100(sptid)).     
      
      IF hjtid < 0 THEN DO:
         MESSAGE "Full arbetstid är inte uppnådd med denna sluttid, alltså är detta ingen övertid" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.*/

      IF CMB_KNAPP = "Övertid ut" THEN DO:      
         EMPTY TEMP-TABLE flexdagtemp NO-ERROR. 
         EMPTY TEMP-TABLE flextemp NO-ERROR. 
         RUN nydag_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN_DATUM,OUTPUT TABLE flexdagtemp).   
         RUN nyflextemp_UI IN flexavikapph (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN_DATUM,OUTPUT TABLE flextemp).    
         
         FIND FIRST flextemp WHERE flextemp.KOM = TRUE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flextemp  THEN DO:
            MESSAGE "In-registrering måste finnas innan man kan lägga in en övertid ut-registrering." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         FIND FIRST flexdagtemp NO-LOCK NO-ERROR.
         RUN dagtot_UI (INPUT flexdagtemp.START, INPUT lunchstarten , INPUT lunchslutet, INPUT FILL-IN_TID, INPUT flexdagtemp.FLARB, OUTPUT dagtotal).                                                      
   
         /*hjtid = klock60( klock100(sok2 / 100) + klock100(FILL-IN_TID) - klock100(sptid)).     */                  
         IF ( dagtotal - regtotalt)  < 0 THEN DO:
            MESSAGE "Full arbetstid är inte uppnådd med denna sluttid, alltså är detta ingen övertid" VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
      END.
   END.   

   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      IF regdatum LE flexregtemp.SALDOKORD THEN DO:
          MESSAGE "Flextid är låst till och med" flexregtemp.SALDOKORD VIEW-AS ALERT-BOX.
          status-mus2 = SESSION:SET-WAIT-STATE("").
          APPLY "ENTRY" TO FILL-IN_DATUM IN FRAME {&FRAME-NAME}.
          APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.        
      ASSIGN
      flexkvst = flexregtemp.KVSTART
      flexkvsl = flexregtemp.KVSLUT
      flexmost = flexregtemp.MOSTART
      flexmosl = flexregtemp.MOSLUT.
      /*hårdkodning för att slippa ändra flexreg.MOSLUT efter sommartid OBS- måste ändras till vintertid en gång till*/
      
      IF MONTH(regdatum) > MONTH(flexregtemp.SOMMARST) AND 
      MONTH(regdatum) < MONTH(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.               
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/
      END.
      ELSE IF MONTH(regdatum) = MONTH(flexregtemp.SOMMARSL) AND 
      DAY(regdatum) <= DAY(flexregtemp.SOMMARSL) THEN DO:
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/
      END.
      ELSE IF MONTH(regdatum) = MONTH(flexregtemp.SOMMARST) AND 
       DAY(regdatum) >= DAY(flexregtemp.SOMMARST) THEN DO: 
         flexkvst = flexregtemp.KVSOST.
         flexkvsl = flexregtemp.KVSOSL.
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 8 THEN ASSIGN flexmosl = 7.30.*/
      END.       
      ELSE DO:
         IF flexavttemp.FLEXKOD = "K" THEN regdatum = regdatum.
         ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                       
         /*ELSE IF flexavttemp.FLEXKOD = "KV" AND flexmosl = 7.3 THEN ASSIGN flexmosl = 8.*/  
      END.      
      IF personaltemp.DELTID = TRUE THEN DO:         
         sok1 = personaltemp.ANSTALLNING.
         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 6,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.                     
         IF sok2 = 1 THEN sok2 = sok2.
         ELSE DO: 
            musz = FALSE.
            FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
            IF AVAILABLE flexregtemp THEN DO:            
               IF MONTH(TODAY) > MONTH(flexregtemp.SOMMARST) AND 
               MONTH(TODAY) < MONTH(flexregtemp.SOMMARSL) THEN musz = TRUE.
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARSL) AND 
               DAY(TODAY) <= DAY(flexregtemp.SOMMARSL) THEN musz = TRUE.               
               ELSE IF MONTH(TODAY) = MONTH(flexregtemp.SOMMARST) AND 
               DAY(TODAY) >= DAY(flexregtemp.SOMMARST) THEN musz = TRUE.
            END.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               IF sok4 NE "" THEN DO:
                  nytid = DECIMAL(sok4).
               END.
               ELSE DO:
                  sekunder = sok5.
                  RUN SEKTIM.P.
               END.
            END.
            ELSE DO:         
               sekunder = sok5.
               RUN SEKTIM.P.
            END.
            
            IF nytid > regslut THEN DO:                               
               flexkvslspar = flexkvsl.
               nytid = flexkvsl.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - sok5 + sekunder.
               RUN SEKTIM.P.
               flexkvsl = nytid.
               nytid = flexkvst.
               RUN TIMSEK.P.
               seku = sekunder.
               nytid = regslut.
               RUN TIMSEK.P.
               sekunder = seku - sok5 + sekunder.
               RUN SEKTIM.P.
               flexkvst = nytid.               
               IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
                  flexkvsl = flexkvslspar.
                  /*IF personaltemp.PERSONALKOD = "LMN" THEN DO:
                     flexkvsl = flexkvslspar.
                  END.
                  IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                     flexkvsl = flexkvslspar.
                  END.
                  IF personaltemp.PERSONALKOD = "SEABN" THEN DO:
                     flexkvsl = flexkvslspar.
                  END.*/
               END.
            END.            
            sok1 = personaltemp.ANSTALLNING.            
            IF Guru.Konstanter:appcon THEN DO: 
               RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.
            ELSE DO:
               RUN FLEXTIDH.P 
               (INPUT 5,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
               INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
            END.               
            IF sok2 = 1 THEN sok2 = sok2.
            ELSE DO:
               sekunder = sok5.
               RUN SEKTIM.P.                              
               IF nytid < regstart THEN DO:    
                  flexmostspar = flexmost.
                  nytid = flexmosl.
                  RUN TIMSEK.P.
                  seku = sekunder.
                  nytid = regstart.
                  RUN TIMSEK.P.
                  sekunder = seku - sok5 + sekunder.
                  RUN SEKTIM.P.
                  flexmosl = nytid.
                  nytid = flexmost.
                  RUN TIMSEK.P.
                  seku = sekunder.
                  nytid = regstart.
                  RUN TIMSEK.P.
                  sekunder = seku - sok5 + sekunder.
                  RUN SEKTIM.P.
                  flexmost = nytid.
                  IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:               
                     IF personaltemp.PERSONALKOD = "SEMGN" THEN DO:
                        flexmost = flexmostspar.
                     END.
                  END.
               END.                              
            END.         
         END.        
      END.
   END.
   ASSIGN
   sok1 = ansttemp.KOD
   sok4 = STRING(regdatum).
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 7,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   IF sok2 = 1 THEN DO:
      flexkvst = regslut.
   END. 
   /*slippa dubletter Lena 20160912*/
   ASSIGN 
   sok3 = personaltemp.PERSONALKOD
   sok4 = STRING(FILL-IN_DATUM) 
   sok5 = FILL-IN_TID.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 50,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 50,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   IF sok1 = "X" THEN DO:
      sok1 = "".
      MESSAGE "Det finns redan en registrering detta klockslag denna dag!" VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   IF CMB_KNAPP = "IN" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:         
         IF FILL-IN_TID > flexmosl THEN DO:                       
            MESSAGE "Du är utanför flexramen använd knappen Flex in istället" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.     
         IF FILL-IN_TID < flexmost THEN DO:                       
            MESSAGE "Du är inte inom flexramen! Vill du registrera ändå?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
            CASE val3:             
               WHEN FALSE THEN DO:
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
               END.   
            END CASE.
         END.   
         
      END.   
   END.      
   IF CMB_KNAPP = "UT" THEN DO: 
      FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
      IF AVAILABLE flexregtemp THEN DO:
         IF FILL-IN_TID < flexkvst THEN DO:                       
            MESSAGE "Du är inte inom flexramen, använd knappen Flex ut istället! " VIEW-AS ALERT-BOX.            
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.              
         END.            
      END.   
   END.   
   IF CMB_KNAPP = "Flex in" OR CMB_KNAPP = "Annat in" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:        
         IF FILL-IN_TID < regstart THEN DO:                                                         
            MESSAGE "Före ordinarie arbetstid använd knappen In istället" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.                   
      END.   
   END.      
   
   IF CMB_KNAPP = "Flex ut" OR CMB_KNAPP = "Annat ut" THEN DO:       
      IF AVAILABLE flexregtemp THEN DO:        
        /* borttaget eftersom de vill kunna stäpmpla "in" och sedan "flex ut" INNAN ordinarie arbetstid tex In 6.15 Flex ut 6.50
        IF FILL-IN_TID < regstart THEN DO:                                                         
            MESSAGE "Före ordinarie arbetstid kan inte knappen flex ut användas" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.        */
         IF FILL-IN_TID GE regslut THEN DO:                                                         
            MESSAGE "Efter ordinarie arbetstid använd knappen Ut istället" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_TID IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.                  
      END.   
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
      IF CMB_KNAPP = "FLEX UT" THEN ASSIGN FILL-IN-AONR = "155" FILL-IN-DELNR = 0. 
   END.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF CMB_KNAPP = "LUNCH UT" THEN ASSIGN FILL-IN-AONR = "940" FILL-IN-DELNR = 0. 
   END.
   IF CMB_KNAPP = "UT" OR CMB_KNAPP = "LUNCH UT" THEN DO:       
      RELEASE utsokaonr.
   END.
   ELSE DO:   
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE utsokaonr THEN DO:
         &Scoped-define FORMATNAMN FILL-IN-AONR
         {AOFORMAT2.I}
         MESSAGE Guru.Konstanter:gaok formataonr STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:      
         {AOKOLLERS.I} 
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            RETURN NO-APPLY.      
         END.            
           
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            &Scoped-define FORMATNAMN FILL-IN-AONR
            {AOFORMAT2E.I}
            MESSAGE Guru.Konstanter:gaok formataonr STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.     
      END.
      IF AVAILABLE utsokaonr THEN DO:
         IF (globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV") AND utsokaonr.AONR = "150" THEN DO:
            MESSAGE "Semester skall bara registreras hela dagar. Använd periodregistrering." VIEW-AS ALERT-BOX.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:               
            IF CMB_KNAPP = "IN" OR CMB_KNAPP = "Annat in"  THEN DO:         
               IF utsokaonr.AONR = "155" THEN DO:
                  MESSAGE "Använd knapparna flex in eller flex ut istället. För hel dag flex använd periodregistrering." VIEW-AS ALERT-BOX.         
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
               END.
            END.
            IF CMB_KNAPP = "FLEX IN" THEN DO:         
               IF utsokaonr.AONR = "155" THEN DO:
                  MESSAGE "Ange det projekt som skall gälla efter inflexning." VIEW-AS ALERT-BOX.         
                  status-mus2 = SESSION:SET-WAIT-STATE("").
                  APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
                  APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
               END.
            END.
         END.
         IF Guru.Konstanter:globforetag = "LULE" AND utsokaonr.AONR = "150" THEN DO:
            MESSAGE "Semester skall bara registreras hela dagar. Använd periodregistrering." VIEW-AS ALERT-BOX.         
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      musz = FALSE.
      RUN kommentarutbcheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            IF Guru.Konstanter:globforetag = "MISV"  THEN DO:      
               MESSAGE "Det är obligatoriskt att ange kursens namn."  VIEW-AS ALERT-BOX. 
            END.
            ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:      
               MESSAGE "Ange utbildningens och utbildningsföretagets namn."  VIEW-AS ALERT-BOX. 
            END.
            ELSE DO:            
               MESSAGE "Kommentar om vad utbildningen avser är obligatorisk."  VIEW-AS ALERT-BOX. 
            END.
            status-mus2 = SESSION:SET-WAIT-STATE("").            
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.            
         END.
      END.      
      FILL-IN_RESMAL:FORMAT = "X(158)".
      
      musz = FALSE.      
      RUN tvbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
      /*IF FILL-IN-AONR = "118"  THEN musz = TRUE.      */
      IF musz = TRUE THEN DO:
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
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
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.
         END.
      END.            
      musz = FALSE.
      RUN fpbarncheck_UI IN otbeordapph (INPUT FILL-IN-AONR,INPUT FILL-IN-DELNR,OUTPUT musz).
      /*IF FILL-IN-AONR = "117"  THEN musz = TRUE.      */
      /*IF FILL-IN-AONR = "119"  THEN musz = TRUE.      */
      IF musz = ? THEN DO:
         /*ok utan personnummer 60 dagar nnnan barns födelse*/
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
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
               APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
               APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
            END.                         
         END.
         ELSE musz = TRUE.                  
      END.
      IF musz = TRUE THEN DO:         
         FILL-IN_RESMAL:FORMAT = "XXXXXX-XXXX".
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Fyll i barnets personnummer format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         musz = FALSE.
         RUN pnrkoll_UI (OUTPUT musz).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            MESSAGE "Barnets personnummer felaktigt angivet. Skall vara format 999999-9999"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         bpnr = DATE(SUBSTRING(FILL-IN_RESMAL,1,6)).
         balder = (( regdatum - bpnr ) / 365 ).
         IF balder  GE 9 THEN DO:
            MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF balder  > 8 AND balder  < 9 AND MONTH(bpnr) > 7 THEN.            
         ELSE IF balder  > 8 AND balder  < 9 AND MONTH(regdatum) > 7 THEN DO:
            MESSAGE "Barnet är äldre än 8 år " Guru.Konstanter:gaok " 119 eller 117 får inte användas."  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
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
      /*IF FILL-IN-AONR = "135"  THEN musz = TRUE.      */
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF FILL-IN_RESMAL = "" THEN DO:
            MESSAGE "Obligatorisk kommentar"  VIEW-AS ALERT-BOX. 
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN_RESMAL IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.
      END.
      musz = FALSE.
      
   END.   
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN sok1 = utsokaonr.AONR
      sok2 = utsokaonr.DELNR.
   END.
   ELSE ASSIGN sok1 = "" sok2 = 0.
   status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GLOMDNUFAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vart,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,
      INPUT flexkvst,INPUT flexkvsl,INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr,INPUT FILL-IN_RESMAL,INPUT FILL-IN-OVER ,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GLOMDNUFAPP.P 
      (INPUT vart,INPUT personaltemp.PERSONALKOD,INPUT regdatum,INPUT CMB_KNAPP,
      INPUT sok1,INPUT sok2,INPUT FILL-IN_TID,
      INPUT flexkvst,INPUT flexkvsl,INPUT Guru.Konstanter:globanv,INPUT regstart,INPUT regslut,
      INPUT regdagnamn, INPUT regvnr ,INPUT FILL-IN_RESMAL,INPUT FILL-IN-OVER , OUTPUT TABLE felmeddtemp).
   END.   
   status-mus2 = SESSION:SET-WAIT-STATE("").
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
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
   IF VALID-HANDLE(flexavikapph) THEN DELETE PROCEDURE flexavikapph NO-ERROR.
   flexavikapph = ?.
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


&Scoped-define SELF-NAME CMB_KNAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KNAPP DIALOG-1
ON VALUE-CHANGED OF CMB_KNAPP IN FRAME DIALOG-1 /* Knapp */
DO:
   CMB_KNAPP = INPUT CMB_KNAPP.
   IF CMB_KNAPP = "FLEX UT"  OR CMB_KNAPP = "LUNCH UT" OR CMB_KNAPP = "UT" THEN DO:
      ASSIGN      
      FILL-IN-AONR:HIDDEN = TRUE
      FILL-IN-DELNR:HIDDEN = TRUE
      BRW_AONR:HIDDEN = TRUE    
      RAD_FAST:HIDDEN = TRUE   
      FILL-IN_AONRS:HIDDEN = TRUE 
      FILL-IN_ORTS:HIDDEN = TRUE.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-AONR:HIDDEN = FALSE
      FILL-IN-DELNR:HIDDEN = FALSE
      BRW_AONR:HIDDEN = FALSE    
      RAD_FAST:HIDDEN = FALSE   
      FILL-IN_AONRS:HIDDEN = FALSE 
      FILL-IN_ORTS:HIDDEN = FALSE.
   END.
   IF CMB_KNAPP = "ÖVERTID IN"  OR CMB_KNAPP = "ÖVERTID UT" THEN DO:
     ASSIGN
     BRW_OTBRD:HIDDEN = FALSE
     FILL-IN-OTBRD:HIDDEN = FALSE
     CMB_OVERUT:HIDDEN = FALSE.
     ENABLE BRW_OTBRD FILL-IN-OTBRD CMB_OVERUT WITH FRAME {&FRAME-NAME}.       
  END.
  ELSE DO:
     ASSIGN
     BRW_OTBRD:HIDDEN = TRUE
     FILL-IN-OTBRD:HIDDEN = TRUE
     CMB_OVERUT:HIDDEN = TRUE.
  END.
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
ON ENTRY OF CMB_OVERUT IN FRAME DIALOG-1 /* Övertiduttag */
DO:
   /*DISABLE FILL-IN-PRIS CMB_PRISTYP.
   ASSIGN
   FILL-IN-PRIS:HIDDEN = TRUE   
   CMB_PRISTYP:HIDDEN = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OVERUT DIALOG-1
ON VALUE-CHANGED OF CMB_OVERUT IN FRAME DIALOG-1 /* Övertiduttag */
DO:
   CMB_OVERUT = INPUT CMB_OVERUT.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   RUN resmallabel_UI.
   /*RUN entryaonr_UI.*/
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
   /*IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      
   END.*/
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
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.   
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUM DIALOG-1
ON LEAVE OF FILL-IN_DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   regdatumspar = regdatum
   FILL-IN_DATUM = INPUT FILL-IN_DATUM
   regdatum = FILL-IN_DATUM. 
   RUN dag_UI.
   DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.   
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   NO-LOCK NO-ERROR.   
   ASSIGN
   sok1 = ansttemp.KOD
   sok4 = STRING(FILL-IN_DATUM).
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.      
   
   IF sok2 = 0 THEN dagnr = WEEKDAY(FILL-IN_DATUM).
   ELSE dagnr = sok2.
   IF dagnr = 1 OR dagnr = 7 THEN DO:
       MESSAGE "Du kan inte registrera flex helgdagar."
      VIEW-AS ALERT-BOX.
      FILL-IN_DATUM = regdatumspar.
      RUN dag_UI.
      DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.   
   END. 
   IF FILL-IN_DATUM > TODAY THEN DO:     
      MESSAGE "Du kan inte registrera flex framåt i tiden."
      VIEW-AS ALERT-BOX.
      FILL-IN_DATUM = regdatumspar.
      RUN dag_UI.
      DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.   
   END.
   FIND FIRST flexregtemp WHERE flexregtemp.KOD = flexavttemp.FLEXKOD USE-INDEX FLEXREG NO-LOCK NO-ERROR.
   IF AVAILABLE flexregtemp THEN DO:
      IF FILL-IN_DATUM LE flexregtemp.SALDOKORD THEN DO:
         MESSAGE "Flextid är låst till och med" flexregtemp.SALDOKORD VIEW-AS ALERT-BOX.
         FILL-IN_DATUM = regdatumspar.
         RUN dag_UI.
         DISPLAY FILL-IN_DATUM WITH FRAME {&FRAME-NAME}.   
      END.          
   END. 
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


&Scoped-define SELF-NAME FILL-IN_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TID DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN_TID IN FRAME DIALOG-1 /* Tid */
DO:
   klocka = INPUT FILL-IN_TID.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN_TID = klocka.
   DISPLAY FILL-IN_TID WITH FRAME {&FRAME-NAME}.
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
      FILL-IN-VISAONR:HIDDEN = TRUE.   
      ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:         
      RUN openbdynspec_UI IN brwproc[1].
      ASSIGN
      BRW_AONR:HIDDEN = FALSE   
      BRW_EAONR:HIDDEN = TRUE
      CMB_OMR:HIDDEN = FALSE
      CMB_AVD:HIDDEN = FALSE
      FILL-IN-VISAONR:HIDDEN = FALSE.   
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
   RUN main_UI.
   
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
   IF vart = "AND" THEN DISABLE CMB_KNAPP WITH FRAME {&FRAME-NAME}.  
   IF Guru.Konstanter:globforetag = "elpa" THEN .
   ELSE DO:   
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.    
   END.       
   IF Guru.Konstanter:globforetag = "SUFL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch in") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("Lunch ut") IN FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      status-ok = CMB_KNAPP:DELETE("ÖVERTID IN") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_KNAPP:DELETE("ÖVERTID UT") IN FRAME {&FRAME-NAME}.      
   END.
   status-mus = SESSION:SET-WAIT-STATE(""). 
   IF FILL-IN-OVER = "K" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Komp".
   IF FILL-IN-OVER = "Ö" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Över".
   IF FILL-IN-OVER = "F" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Flex".     
   IF FILL-IN-OVER = "I" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Ejöv".
   /*RUN entryaonr_UI.*/
   RUN aoladda_UI.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.       
   END.
   ELSE DO:
      RUN FLEXAVIKAPP.P PERSISTENT SET flexavikapph.
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
   FILL-IN-VISAONR:HIDDEN = FALSE.       
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.   
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "cELPA" )  AND Guru.Konstanter:globallao = TRUE THEN DO:
      IF AVAILABLE utsokaonr THEN DO:
         aonrrec = RECID(utsokaonr).
         IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
         IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
         /*RAD_FAST = utsokaonr.FASTAAONR.*/
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
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
         ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
                     FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
                  FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
            /*RAD_FAST = utsokaonr.FASTAAONR.*/
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
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
         
         /*RAD_FAST = FALSE.*/
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
      FILL-IN-VISAONR:HIDDEN = TRUE.       
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
      FILL-IN-VISAONR:HIDDEN = FALSE.       
   END.
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.
   ASSIGN   
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dagtot_UI DIALOG-1 
PROCEDURE dagtot_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER dagstart  AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER lstarten  AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER lslutet AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER dagslut  AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER vflarb AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER dagtotal AS DECIMAL NO-UNDO.
   dagtotal = klock60(klock100(dagslut) - klock100(dagstart) - klock100(lslutet) + klock100(lstarten) + klock100(vflarb)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dag_UI DIALOG-1 
PROCEDURE dag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = FILL-IN_DATUM.
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-STDAG = regdagnamn + "dag".
   DISPLAY FILL-IN-STDAG WITH FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN_DATUM FILL-IN_TID CMB_KNAPP FILL-IN-AONR FILL-IN-DELNR 
          FILL-IN-VISAONR FILL-IN_PERSONALKOD CMB_OMR FILL-IN-STDAG FILL-IN-SKP 
          CMB_AVD FILL-IN_RESMAL 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN_DATUM FILL-IN_TID CMB_KNAPP FILL-IN-AONR FILL-IN-DELNR BTN_REG 
         BTN_AVB CMB_OMR BTN_NVE-2 BTN_FVE-2 CMB_AVD FILL-IN_RESMAL RAD_FAST 
         BTN_SKAPEN RECT-22 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryaonr_UI DIALOG-1 
PROCEDURE entryaonr_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   ASSIGN
   musz = FALSE
   FILL-IN-SKP = "Sök på:" 
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   FILL-IN-VISAONR:HIDDEN = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
                  FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
      IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
      IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
      /*RAD_FAST = utsokaonr.FASTAAONR.*/
   END.
   ELSE DO:
      IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
         sparomrade = Guru.Konstanter:gomrk + " : alla".
         ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.
         DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
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
      /*RAD_FAST = FALSE.*/
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
   IF FILL-IN-AONR NE "" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
      utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(utsokaonr)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.     
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
   ASSIGN
   sok1 = "TID"
   sok3 = personaltemp.PERSONALKOD.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 1,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
   END.
   IF sok1 = "FINNS EJ" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR  = TRUE AND
      utsokaonr.OMRADE = "" AND utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN FILL-IN-AONR = utsokaonr.AONR FILL-IN-DELNR = utsokaonr.DELNR.
         valaonrrec = RECID(utsokaonr).
         IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
         IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
         /*RAD_FAST = utsokaonr.FASTAAONR.*/
      END.
      ELSE DO:
         FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR  = FALSE AND
         utsokaonr.OMRADE = "" AND utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = utsokaonr.AONR FILL-IN-DELNR = utsokaonr.DELNR.
            valaonrrec = RECID(utsokaonr).
            IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
            IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
         END.
      END.
   END.
   ELSE DO:
      ASSIGN FILL-IN-AONR = sok1 
      FILL-IN-DELNR = sok2.
      FIND FIRST utsokaonr WHERE utsokaonr.AONR  = sok1 AND
      utsokaonr.DELNR = sok2 USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         valaonrrec = RECID(utsokaonr).
         IF utsokaonr.FASTAAONR = TRUE THEN RAD_FAST = 2.
         IF utsokaonr.FASTAAONR = FALSE THEN RAD_FAST = 1.
         /*RAD_FAST = utsokaonr.FASTAAONR.*/
      END.    
   END.
  /* FILL-IN_DATUM = bdatum. */
   /*ASSIGN 
   sok1 = ""
   sok2 = 0.
   IF Guru.Konstanter:appcon THEN DO: 
      RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 10,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok5).            
   END.
   ELSE DO:
      RUN FLEXTIDH.P 
      (INPUT 10,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
      INPUT-OUTPUT personaltemp.PERSONALKOD,INPUT-OUTPUT sok5).            
   END.
   IF sok1 NE "" THEN DO: 
      ASSIGN
      FILL-IN-AONR = sok1
      FILL-IN-DELNR = sok2.
   END.*/
   IF vart = "NYA" THEN DO:
      ASSIGN
      /*bdatum = FLEXTID.DATUM*/
      FILL-IN_TID = 00.00
      CMB_KNAPP = "IN".
   END.   
   FILL-IN_PERSONALKOD = personaltemp.PERSONALKOD.  
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
         FILL-IN-VISAONR:HIDDEN = TRUE.   
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
         FILL-IN-VISAONR:HIDDEN = FALSE.   
         ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
      END.                  
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtegna_UI DIALOG-1 
PROCEDURE hmtegna_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   /*Egna aonr*/   
   IF Guru.Konstanter:varforetypval[24] = 1 THEN DO:   
      EMPTY TEMP-TABLE egnaao NO-ERROR. 
      tthandle = TEMP-TABLE egnaao:HANDLE.
      FIND FIRST sparaladdatemp NO-ERROR.
      IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
      ASSIGN
      sparaladdatemp.GLOBANV = pkod /*Tidredovisningens aonr spara på personalkod istället för användare**/
      sparaladdatemp.BENAMNING = "AONRE" /*Benämnings sufix, i detta fall ELPAO$STOR*/
      sparaladdatemp.TABVAL = "AONRTAB" /*Tabellnamn*/
      sparaladdatemp.FALTVALAO = "AONR" /*CHARACTER field*/
      sparaladdatemp.FALTVALDEL = "DELNR" /*Integer field*/
      sparaladdatemp.FALTVALDATE = "AONRAVDATUM".   /*DATE field*/
      
      RUN laddabrw_UI IN brwproc[3] 
      (INPUT TABLE-HANDLE tthandle, INPUT TABLE sparaladdatemp).          
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main_UI DIALOG-1 
PROCEDURE main_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
  CMB_AVD:DELIMITER IN FRAME {&FRAME-NAME} = "$".
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").
   
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").
   {OMRHMT.I}
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      RUN othmtbolag_UI IN otbeordapph (INPUT pkod,OUTPUT TABLE otidbeordtemp).
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.    
   &Scoped-define FORMATNAMN utsokaonr.AONR
   &Scoped-define FORMATNAMNOMR utsokaonr.OMRADE
   &Scoped-define BROWSE-NAME BRW_AONR
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 3,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   /*{OMRAOFORMAT.I}*/ 
   {ANVAVDSOF.I}
      
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".


   FIND FIRST omrtemp WHERE omrtemp.OMRADE = globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
   IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
   END.
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.    
   /*IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 2,INPUT pkod,INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.*/
   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLEXTAB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).     
   END.
   ELSE DO:
      RUN FLEXTAB.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE ansttemp,INPUT-OUTPUT TABLE flexregtemp,
      INPUT-OUTPUT TABLE flexavttemp,INPUT-OUTPUT TABLE flexsaldotemp,INPUT-OUTPUT TABLE utryckningtemp).      
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.    
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   NO-LOCK NO-ERROR.   
   FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   globomr = personaltemp.OMRADE. 
   FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE anvandartemp  THEN Guru.Konstanter:globallao = anvandartemp.ALLAONR.
   ELSE  Guru.Konstanter:globallao = TRUE.  
   FILL-IN-OVER = personaltemp.OVERTIDUTTAG.          
   ASSIGN
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   BRW_EAONR:TITLE = "Favorit " + LC(Guru.Konstanter:gaol)
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok  
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok   
   regdatum = TODAY  
   FILL-IN_DATUM = TODAY - 1  
   FILL-IN-VISAONR = "Visa " + LC(Guru.Konstanter:gaok) + " för:"
   {TILLFAST2.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   &Scoped-define FORMATNAMN FILL-IN-AONR      
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   /*BTN_SKAPEN:LABEL = "Spara favorit " + LC(Guru.Konstanter:gaok) .    */
   BTN_SKAPEN:TOOLTIP = "Spara valt "  + LC(Guru.Konstanter:gaok) + " som favorit ".  
   REPEAT:                                    
      ASSIGN
      sok1 = ansttemp.KOD
      sok4 = STRING(FILL-IN_DATUM).
      IF Guru.Konstanter:appcon THEN DO: 
         RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.
      ELSE DO:
         RUN FLEXTIDH.P 
         (INPUT 8,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
         INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
      END.      
      IF sok2 = 0 THEN dagnr = WEEKDAY(FILL-IN_DATUM).
      ELSE dagnr = sok2.
      IF dagnr = 1 OR dagnr = 7 THEN DO:
         FILL-IN_DATUM = FILL-IN_DATUM - 1.
      END.
      ELSE LEAVE.
   END.
   RUN dag_UI.
   RUN grundtid_UI. 
   RUN hmtegna_UI.
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
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
      /*vab ska inte ha krav på personnummer FILL-IN-AONR = "118"  lena 20181211*/
      IF  FILL-IN-AONR = "119" OR FILL-IN-AONR = "117"  THEN DO:                
         FILL-IN_RESMAL:FORMAT IN FRAME {&FRAME-NAME} = "XXXXXX-XXXX" .
         FILL-IN_RESMAL:LABEL = "Barnets pnr".         
         FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR.
         IF FILL-IN_RESMAL = "" THEN ASSIGN FILL-IN_RESMAL = "0000000000".
      END.
      ELSE DO:
         FILL-IN_RESMAL:FORMAT = "X(158)".
         FILL-IN_RESMAL:LABEL = "Kommentar".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klock60 DIALOG-1 
FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

