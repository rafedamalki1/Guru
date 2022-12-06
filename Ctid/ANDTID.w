&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */



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
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{DIRDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.
{PHMT.I}
{OMRTEMPW.I}

{SOKDEF.I}
{OTIDBEORD.I}
{EXTRATAB.I}

DEFINE SHARED VARIABLE allaandh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE varslut AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE bytaonrvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE datkoll AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE hjdat AS INTEGER NO-UNDO.
DEFINE VARIABLE kontrollstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE halvkvart AS INTEGER NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE flexkvsl AS DECIMAL NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE persnr AS INTEGER EXTENT 10 FORMAT "99" NO-UNDO.
DEFINE VARIABLE tal1 AS INTEGER NO-UNDO.
DEFINE VARIABLE tal2 AS INTEGER NO-UNDO.
DEFINE VARIABLE ksiffran AS INTEGER NO-UNDO.
DEFINE VARIABLE bpnr AS DATE NO-UNDO.
DEFINE VARIABLE balder AS DECIMAL NO-UNDO.
DEFINE VARIABLE spaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE spdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE mellanslutvar AS DECIMAL NO-UNDO.

DEFINE VARIABLE spregdatum AS DATE NO-UNDO.
DEFINE VARIABLE spregstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE spregslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE splunchstarten  AS DECIMAL NO-UNDO.
DEFINE VARIABLE splunchslutet AS DECIMAL NO-UNDO.
DEFINE VARIABLE skregdatum AS DATE NO-UNDO.
DEFINE VARIABLE skregstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE skregslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE sklunchstarten  AS DECIMAL NO-UNDO.
DEFINE VARIABLE sklunchslutet AS DECIMAL NO-UNDO.

DEFINE VARIABLE hjrstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjrslut AS DECIMAL NO-UNDO.
DEFINE VARIABLE ograns AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE hjdelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE pcheck AS LOGICAL NO-UNDO.
DEFINE VARIABLE ftro AS LOGICAL NO-UNDO.
DEFINE VARIABLE tillit AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE starttidwh AS HANDLE NO-UNDO.
DEFINE VARIABLE sluttidwh  AS HANDLE NO-UNDO.
DEFINE VARIABLE maxovcheck AS DECIMAL NO-UNDO.
DEFINE VARIABLE d48datum AS DATE  NO-UNDO.
DEFINE VARIABLE d48godk AS CHARACTER NO-UNDO.   
DEFINE VARIABLE dispens48 AS LOGICAL NO-UNDO.
DEFINE VARIABLE asfaktapph AS HANDLE NO-UNDO. 
DEFINE VARIABLE valdkom AS CHARACTER NO-UNDO.
DEFINE VARIABLE vilach AS LOGICAL NO-UNDO.
DEFINE VARIABLE nofall AS LOGICAL NO-UNDO.
DEFINE VARIABLE nodatum AS DATE NO-UNDO.
DEFINE VARIABLE nogodk AS CHARACTER NO-UNDO.


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
&Scoped-define INTERNAL-TABLES utsokaonr egnaao otidbeordtemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT utsokaonr.AVDELNINGNR 
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
&Scoped-Define ENABLED-OBJECTS TOG_FORSTA FILL-IN-START FILL-IN-PLUS ~
FILL-IN-SLUT FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL BTN_SNABB BTN_REG ~
BTN_SLUT BTN_START BTN_AVB CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK CMB_PRISTYP ~
CMB_OMR CMB_AVD BTN_SKAPEN BTN_DAG RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ARBETSTID TOG_FORSTA FILL-IN-START ~
FILL-IN-PLUS FILL-IN-SLUT FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL ~
FILL-IN-PKOD FILL-IN-DAG CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK CMB_PRISTYP ~
FILL-IN-PRIS FILL-IN-TEXT CMB_OMR FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 

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
DEFINE MENU POPUP-MENU-BRW_AONR 
       MENU-ITEM m_Arbetsuppgift LABEL "Arbetsuppgift" .

DEFINE MENU POPUP-MENU-BRW_AONR-3 
       MENU-ITEM m_Arbetsuppgift-3 LABEL "Arbetsuppgift" .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_DAG 
     LABEL "Kommentar till dagbok":L 
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

DEFINE BUTTON BTN_SLUT 
     LABEL "Tid nu" 
     SIZE 8 BY 1.

DEFINE BUTTON BTN_SNABB 
     LABEL "Spara":L 
     SIZE 14 BY 1 TOOLTIP "Sparar registrering men står kvar i bilden för ytterligare registrering".

DEFINE BUTTON BTN_START 
     LABEL "Tid nu" 
     SIZE 8 BY 1.

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

DEFINE VARIABLE FILL-IN-ARBETSTID AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetstid" 
     VIEW-AS FILL-IN 
     SIZE 36.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVDRA AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Avdrag bered" 
     VIEW-AS FILL-IN 
     SIZE 4.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LUNCH AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Lunch i minuter" 
     VIEW-AS FILL-IN 
     SIZE 6.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OTBRD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ö-tidsbeordrare" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

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
     SIZE 6.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Arbetets start" 
     VIEW-AS FILL-IN 
     SIZE 6.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.75 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRAKT AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Trakt.zon" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-UTRYCK AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Utryckning" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALFA AS CHARACTER FORMAT "X(256)":U INITIAL "Välj fakturabefattning        Vald fakturabefattning" 
     VIEW-AS FILL-IN 
     SIZE 34.13 BY .75
     FONT 11 NO-UNDO.

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

DEFINE VARIABLE FILL-IN_NODF AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Nödfall" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Gäller storstörningsavtal".

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_RESMAL AS CHARACTER FORMAT "X(158)" 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 81.13 BY 1.

DEFINE VARIABLE FILL-IN_VECKOVILA AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Veckovila" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

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

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.38 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_FORSTA AS LOGICAL INITIAL no 
     LABEL "Första tid" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .79 TOOLTIP "Det valda projektet läggs på första tidregistreringen" NO-UNDO.

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
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U WIDTH 1.75
      utsokaonr.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
      utsokaonr.AVDELNINGNR FORMAT "->,>>>,>>9":U
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.38 BY 11.46
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.38 BY 11.46
         TITLE "Egna arbetsordernummer".

DEFINE BROWSE BRW_OTBRD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OTBRD DIALOG-1 _STRUCTURED
  QUERY BRW_OTBRD NO-LOCK DISPLAY
      otidbeordtemp.PERSONALKOD COLUMN-LABEL "Enhet" FORMAT "x(5)":U
            WIDTH 7
      otidbeordtemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      otidbeordtemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 24.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 43 BY 6
         TITLE "Övertidsbeordrare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-ARBETSTID AT ROW 26.71 COL 18 COLON-ALIGNED WIDGET-ID 2
     TOG_FORSTA AT ROW 11.25 COL 1.5
     FILL-IN-DATUM AT ROW 5.5 COL 17.75 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-START AT ROW 6.92 COL 17.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-PLUS AT ROW 6.92 COL 30.25 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-SLUT AT ROW 8.33 COL 17.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-LUNCH AT ROW 9.75 COL 17.38 COLON-ALIGNED
     FILL-IN-AONR AT ROW 11.17 COL 17.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-DELNR AT ROW 12.58 COL 17.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN-OTBRD AT ROW 23.25 COL 17.38 COLON-ALIGNED
     FILL-IN_RESMAL AT ROW 24.17 COL 17.38 COLON-ALIGNED
     BTN_SNABB AT ROW 27.38 COL 56.88
     BTN_REG AT ROW 27.38 COL 71.88
     BRW_EAONR AT ROW 5.04 COL 44.5
     BRW_OTBRD AT ROW 18 COL 57.88
     BTN_SLUT AT ROW 8.33 COL 1.5
     BTN_START AT ROW 6.92 COL 1.5
     BTN_AVB AT ROW 27.38 COL 86.88
     FILL-IN-PKOD AT ROW 2.67 COL 17.38 COLON-ALIGNED
     CMB_DAG AT ROW 5.5 COL 17.38 COLON-ALIGNED
     FILL-IN-DAG AT ROW 5.5 COL 30 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 4.08 COL 17.38 COLON-ALIGNED NO-LABEL
     FILL-IN-VECKO AT ROW 4.08 COL 17.38 COLON-ALIGNED
     BRW_AONR AT ROW 5.13 COL 44.38
     CMB_TRAK AT ROW 14 COL 17.38 COLON-ALIGNED
     CMB_OVERUT AT ROW 15.38 COL 17.38 COLON-ALIGNED
     FILL-IN-OVER AT ROW 22.75 COL 83.5 COLON-ALIGNED HELP
          "ÖVERTID = Ö, KOMP = K, INGEN ERSÄTTNING = I"
     FILL-IN-UTRYCK AT ROW 16.75 COL 17.38 COLON-ALIGNED
     FILL-IN-PRISTYP AT ROW 12.08 COL 40 COLON-ALIGNED
     CMB_PRISTYP AT ROW 18.17 COL 17.38 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 19.54 COL 17.38 COLON-ALIGNED
     FILL-IN-AVDRA AT ROW 19.5 COL 44.5 COLON-ALIGNED
     FILL-IN_NODF AT ROW 16.75 COL 17.5 COLON-ALIGNED
     FILL-IN_VECKOVILA AT ROW 19.5 COL 67.5 COLON-ALIGNED
     FILL-IN-TRAKT AT ROW 21.58 COL 83.25 COLON-ALIGNED
     CMB_BEF AT ROW 22.13 COL 17.38 COLON-ALIGNED
     FILL-IN_VIBEFATTNING AT ROW 22.13 COL 36.75 COLON-ALIGNED HELP
          "BEFATTNINGSKOD" NO-LABEL
     FILL-IN-VALFA AT ROW 20.96 COL 17.38 COLON-ALIGNED NO-LABEL
     FILL-IN-TEXT AT ROW 1 COL 76.13 COLON-ALIGNED NO-LABEL
     RAD_FAST AT ROW 3.79 COL 34 NO-LABEL
     CMB_OMR AT ROW 3.58 COL 76.38 COLON-ALIGNED NO-LABEL
     FILL-IN-SKP AT ROW 16.83 COL 43.38 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 16.83 COL 59.38 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 16.83 COL 79.5 COLON-ALIGNED
     BTN_NVE AT ROW 5.25 COL 28.88
     BTN_FVE AT ROW 6.13 COL 28.88
     CMB_AVD AT ROW 2.38 COL 76.38 COLON-ALIGNED NO-LABEL
     BTN_SKAPEN AT ROW 11.17 COL 28.5
     FILL-IN_FORNAMN-2 AT ROW 2.67 COL 28.38 NO-LABEL
     BTN_DAG AT ROW 25.33 COL 19.63
     RECT-22 AT ROW 16.67 COL 44.5
     SPACE(0.86) SKIP(11.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra tidregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: egnaao T "?" NO-UNDO temp-db egnaao
      TABLE: otidbeordtemp T "?" NO-UNDO temp-db otidbeordtemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_EAONR BTN_REG DIALOG-1 */
/* BROWSE-TAB BRW_OTBRD BRW_EAONR DIALOG-1 */
/* BROWSE-TAB BRW_AONR FILL-IN-VECKO DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:POPUP-MENU IN FRAME DIALOG-1             = MENU POPUP-MENU-BRW_AONR:HANDLE
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

/* SETTINGS FOR FILL-IN FILL-IN-ARBETSTID IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-AVDRA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AVDRA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DATUM IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-DATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LUNCH IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-LUNCH:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-OTBRD IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OTBRD:HIDDEN IN FRAME DIALOG-1           = TRUE.

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

/* SETTINGS FOR FILL-IN FILL-IN-SKP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-TRAKT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TRAKT:HIDDEN IN FRAME DIALOG-1           = TRUE.

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

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_NODF IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NODF:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

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
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no "1.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.utsokaonr.AVDELNINGNR
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OTBRD
/* Query rebuild information for BROWSE BRW_OTBRD
     _TblList          = "Temp-Tables.otidbeordtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.otidbeordtemp.PERSONALKOD
"otidbeordtemp.PERSONALKOD" "Enhet" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.otidbeordtemp.FORNAMN
"otidbeordtemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.otidbeordtemp.EFTERNAMN
"otidbeordtemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "24.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON END-ERROR OF FRAME DIALOG-1 /* Ändra tidregistrering */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   RUN borthandle_UI IN otbeordapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
   /*status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().*/     
   IF musz = FALSE THEN DO: 
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
         FILL-IN-PRIS = sok5
         CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
         CMB_TRAK = INPUT CMB_TRAK      
         CMB_PRISTYP = FILL-IN-PRISTYP.
         IF sok3 = "1" THEN FILL-IN-UTRYCK = TRUE.
         IF sok3 = "2" THEN FILL-IN-UTRYCK = FALSE.
         ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR /*FILL-IN-UTRYCK*/ WITH FRAME {&FRAME-NAME}.
         IF utryckningtemp.UTRYCK1 = TRUE THEN DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.           
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.         
         RUN resmallabel_UI.
      END.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_EAONR
&Scoped-define SELF-NAME BRW_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EAONR DIALOG-1
ON MOUSE-MENU-CLICK OF BRW_EAONR IN FRAME DIALOG-1 /* Egna arbetsordernummer */
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
ON VALUE-CHANGED OF BRW_EAONR IN FRAME DIALOG-1 /* Egna arbetsordernummer */
DO:   
   IF musz = FALSE THEN DO: 
      IF AVAILABLE egnaao THEN DO:      
         ASSIGN
         FILL-IN-AONR = egnaao.AONR
         FILL-IN-DELNR = egnaao.DELNR
         sok1 = egnaao.AONR      
         sok2 = egnaao.DELNR
         sok4 = pkod.
         RUN nyupp_UI (INPUT 16).      
         ASSIGN
         FILL-IN-PRISTYP = sok1
         FILL-IN-PRIS = sok5
         CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
         CMB_TRAK = INPUT CMB_TRAK      
         CMB_PRISTYP = FILL-IN-PRISTYP.
         IF sok3 = "1" THEN FILL-IN-UTRYCK = TRUE.
         IF sok3 = "2" THEN FILL-IN-UTRYCK = FALSE.
         ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR /*FILL-IN-UTRYCK*/ WITH FRAME {&FRAME-NAME}.
         IF utryckningtemp.UTRYCK1 = TRUE THEN DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.        
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN DO:            
         END.
         ELSE DISPLAY CMB_TRAK WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         END.
         ELSE DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.         
         RUN resmallabel_UI.
      END.
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
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DAG DIALOG-1
ON CHOOSE OF BTN_DAG IN FRAME DIALOG-1 /* Kommentar till dagbok */
DO:  
   RUN btndag_UI.
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
   IF tillochmeddatum NE ? THEN IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN FILL-IN-DATUM = DAY(tillochmeddatum + 1).         
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar). 
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.    
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.   
   RUN REGVEC.P.
   {SLUTARBW.I}
   FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
   DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.
   ASSIGN
   FILL-IN-START = regstart
   FILL-IN-SLUT = regslut.
   DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.   
   RUN lucheck_UI.
   RUN skiftcheck_UI.
   RUN otbrd_UI.
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
   /*BEHÖVS OM TIDSTART SAGT ATT DEN REDAN FINNS*/
   IF MONTH(extratidallt.DATUM) = 12 THEN datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).   
   ELSE datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.   
   IF FILL-IN-DATUM > DAY(datkoll)THEN FILL-IN-DATUM = DAY(datkoll).   
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.   
   RUN REGVEC.P.
   {SLUTARBW.I}
   FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
   DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.
   ASSIGN
   FILL-IN-START = regstart
   FILL-IN-SLUT = regslut.
   DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
   RUN lucheck_UI.
   RUN skiftcheck_UI.
   RUN otbrd_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:  
   RUN btnreg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON GO OF BTN_REG IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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


&Scoped-define SELF-NAME BTN_SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SLUT DIALOG-1
ON CHOOSE OF BTN_SLUT IN FRAME DIALOG-1 /* Tid nu */
DO:
   IF sluttidwh:READ-ONLY = FALSE THEN DO:
      RUN tidtime_UI IN otbeordapph (OUTPUT FILL-IN-SLUT).
      DISPLAY FILL-IN-SLUT WITH FRAME {&FRAME-NAME}. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SNABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SNABB DIALOG-1
ON CHOOSE OF BTN_SNABB IN FRAME DIALOG-1 /* Spara */
DO:  
   RUN btnregs_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START DIALOG-1
ON CHOOSE OF BTN_START IN FRAME DIALOG-1 /* Tid nu */
DO:
   IF starttidwh:READ-ONLY = FALSE THEN DO:
      RUN tidtime_UI IN otbeordapph (OUTPUT FILL-IN-START).
      DISPLAY FILL-IN-START WITH FRAME {&FRAME-NAME}. 
   END.
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
   omravdand = 2.
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
   IF CMB_OVERUT = "Komp" THEN FILL-IN-OVER = "K".
   IF CMB_OVERUT = "Över" THEN FILL-IN-OVER = "Ö".
   IF CMB_OVERUT = "Flex" THEN FILL-IN-OVER = "F". 
   IF CMB_OVERUT = "Ejöv" THEN FILL-IN-OVER = "I". 
   
   IF CMB_OVERUT = "MerK" THEN FILL-IN-OVER = "N".
   IF CMB_OVERUT = "MerÖ" THEN FILL-IN-OVER = "M".
   RUN otbrd_UI.   
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
      RUN hao_UI.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVDRA DIALOG-1
ON LEAVE OF FILL-IN-AVDRA IN FRAME DIALOG-1 /* Avdrag bered */
DO:
   FILL-IN-AVDRA = INPUT  FILL-IN-AVDRA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVDRA DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-AVDRA IN FRAME DIALOG-1 /* Avdrag bered */
DO:
   IF INPUT FILL-IN-AVDRA = TRUE THEN FILL-IN-AVDRA = FALSE.
   IF INPUT FILL-IN-AVDRA = FALSE THEN FILL-IN-AVDRA = TRUE.
   DISPLAY FILL-IN-AVDRA WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM DIALOG-1
ON LEAVE OF FILL-IN-DATUM IN FRAME DIALOG-1 /* Datum */
DO:
   ASSIGN
   hjdat = FILL-IN-DATUM.
   FILL-IN-DATUM = INPUT FILL-IN-DATUM.         
   IF MONTH(extratidallt.DATUM) = 12 THEN datkoll = DATE(12,31,YEAR(extratidallt.DATUM)).   
   ELSE datkoll = DATE((MONTH(extratidallt.DATUM) + 1),01,YEAR(extratidallt.DATUM)) - 1.   
   IF FILL-IN-DATUM > DAY(datkoll) THEN DO:
      MESSAGE "Felaktigt angivet datum. Denna månad har bara" DAY(datkoll) "dagar."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.            
   IF FILL-IN-DATUM <= 0 THEN DO:
      MESSAGE "Felaktigt angivet datum. Datum kan ej vara mindre än 1." 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.            
   IF tillochmeddatum NE ? THEN DO:
      IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.            
   END.                              
   regdatum = DATE((regmnr),FILL-IN-DATUM,regar).     
   RUN REGDAG.P.   
   IF hjdat NE FILL-IN-DATUM THEN DO:
       RUN REGVEC.P.
       {SLUTARBW.I}
       FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
       DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.
       ASSIGN
       FILL-IN-START = regstart
       FILL-IN-SLUT = regslut.
       DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.
   END.
   RUN lucheck_UI.
   RUN skiftcheck_UI.
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
   bytaonrvar = FALSE.
   IF FILL-IN-AONR = INPUT FILL-IN-AONR AND 
   FILL-IN-DELNR = INPUT FILL-IN-DELNR THEN bytaonrvar = FALSE.   
   ELSE bytaonrvar = TRUE.   
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   RUN hao2_UI.
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
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.      
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
   IF bytaonrvar = TRUE THEN DO:
      bytaonrvar = FALSE.
      IF AVAILABLE utsokaonr THEN DO:      
         ASSIGN
         FILL-IN-AONR = utsokaonr.AONR
         FILL-IN-DELNR = utsokaonr.DELNR
         sok1 = utsokaonr.AONR      
         sok2 = utsokaonr.DELNR.
      END.
      ELSE DO:
         ASSIGN
         sok1 = FILL-IN-AONR      
         sok2 = FILL-IN-DELNR.
      END.
      sok4 = pkod.       
      RUN nyupp_UI (INPUT 16).      
      ASSIGN
      FILL-IN-PRISTYP = sok1
      CMB_TRAK:SCREEN-VALUE = STRING(sok2)   
      CMB_TRAK = INPUT CMB_TRAK      
      CMB_PRISTYP = FILL-IN-PRISTYP.
      IF sok5 = 1 THEN FILL-IN-UTRYCK = TRUE.
      IF sok5 = 2 THEN FILL-IN-UTRYCK = FALSE.
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR /*FILL-IN-UTRYCK*/    WITH FRAME {&FRAME-NAME}.   
      IF utryckningtemp.UTRYCK1 = TRUE THEN DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
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
   IF vart = "DEL" THEN musz = musz.
   ELSE DO:
      ASSIGN
      regstart = FILL-IN-START
      regvnr = FILL-IN-VECKO
      regdagnamn = CMB_DAG.
      IF gvisatidpermanad = FALSE THEN RUN VECODAT.P.
      ELSE regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
      IF musz = TRUE THEN DO:
        musz = FALSE.
        RETURN NO-APPLY.
     END.
  END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PLUS DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-PLUS IN FRAME DIALOG-1 /* Plus */
DO:   
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
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN musz = musz.
   ELSE DO:   
      IF FILL-IN-START > FILL-IN-SLUT THEN DO:
         MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ASSIGN
   regvnr = FILL-IN-VECKO
   regdagnamn = CMB_DAG.
   IF gvisatidpermanad = FALSE THEN RUN VECODAT.P.
   ELSE regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
      IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
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
   IF FILL-IN-START = 24.00 THEN DO:
      MESSAGE "Ange starttid 00.00 för start vid midnatt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.                               
   IF vart = "DEL" THEN musz = musz.
   ELSE DO:
      ASSIGN
      regstart = FILL-IN-START
      regvnr = FILL-IN-VECKO
      regdagnamn = CMB_DAG.
      IF gvisatidpermanad = FALSE THEN RUN VECODAT.P.
      ELSE regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
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
   SESSION:DATA-ENTRY-RETURN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AONRS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_AONRS IN FRAME DIALOG-1 /* Aonr */
DO:
   FILL-IN_AONRS = INPUT FILL-IN_AONRS.
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
   FILL-IN_RESMAL = INPUT FILL-IN_RESMAL NO-ERROR.
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


&Scoped-define SELF-NAME m_Arbetsuppgift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Arbetsuppgift DIALOG-1
ON CHOOSE OF MENU-ITEM m_Arbetsuppgift /* Arbetsuppgift */
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


&Scoped-define SELF-NAME m_Arbetsuppgift-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Arbetsuppgift-3 DIALOG-1
ON CHOOSE OF MENU-ITEM m_Arbetsuppgift-3 /* Arbetsuppgift */
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
   &Scoped-define FORMATNAMN FILL-IN_AONRS   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN huvud_UI.    
   ASSIGN
   starttidwh = FILL-IN-START:HANDLE
   sluttidwh = FILL-IN-SLUT:HANDLE.  
   BTN_SKAPEN:TOOLTIP = "Spara valt "  + LC(Guru.Konstanter:gaok) + " som favorit ".
     
   RUN enable_UI.       
   /*Egna aonr*/   
   IF Guru.Konstanter:varforetypval[24] = 1 THEN DO:         
   END.
   ELSE DO:            
      BRW_EAONR:HIDDEN = TRUE.      
      BTN_SKAPEN:HIDDEN = TRUE.      
      status-ok = RAD_FAST:DELETE("Favorit" + " " + LC(Guru.Konstanter:gaok)).      
      DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   END.
   {AVVBEFW.I}   
   {FRMSIZED.I}        
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:      
      IF vart = "DEL" THEN DO:
         /*Ingrid vill släcka avvik bef i dela upp 20060420*/
         DISABLE CMB_BEF WITH FRAME {&FRAME-NAME}.
      END.
   END.   
   IF  Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:            
      /*Eva T vill släcka avvik bef i dela upp 20060420*/
      DISABLE CMB_BEF WITH FRAME {&FRAME-NAME}.
   END.   
   IF Guru.Konstanter:globforetag = "MISV" THEN DO:
      utsokaonr.OMRADE:VISIBLE = FALSE.   
      egnaao.OMRADE:VISIBLE = FALSE.   
   END.
   IF vart = "DEL" THEN TOG_FORSTA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   ELSE TOG_FORSTA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   IF vart = "NYA" THEN BTN_SNABB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   ELSE BTN_SNABB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   RUN huvud2_UI.   
   RUN aoladda_UI.
   ASSIGN
   spaonr = FILL-IN-AONR
   spdelnr = FILL-IN-DELNR.
   RUN resmallabel_UI.
  
   {musarrow.i}
   {DIA_M_SLUT.I}
   mellanslutvar = FILL-IN-SLUT.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_OTBRD:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_EAONR:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "AKTIV = TRUE ").   
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
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph.
   END.    
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      RUN othmtbolag_UI IN otbeordapph (INPUT pkod,OUTPUT TABLE otidbeordtemp).      
   END.
   ftro = FALSE.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      RUN fortro IN otbeordapph (INPUT pkod, OUTPUT ftro).      
   END.
   tillit = FALSE.
   IF Guru.Konstanter:globforetag = "GKAL"  THEN DO:   
      RUN tillit IN otbeordapph (INPUT pkod, OUTPUT tillit).      
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
   IF Guru.Konstanter:globforetag = "GKAL" THEN DO:                                               
      status-ok = CMB_OVERUT:ADD-LAST("MerK") IN FRAME {&FRAME-NAME}.     
      status-ok = CMB_OVERUT:ADD-LAST("MerÖ") IN FRAME {&FRAME-NAME}.        
      DISPLAY CMB_OVERUT WITH FRAME {&FRAME-NAME}.    
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
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" )  AND Guru.Konstanter:globallao = TRUE THEN DO:
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
      BRW_AONR:HIDDEN = TRUE
      BRW_EAONR:HIDDEN = FALSE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btndag_UI DIALOG-1 
PROCEDURE btndag_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF FILL-IN-AONR = "" THEN DO:
      MESSAGE Guru.Konstanter:gaok "får inte vara blankt" VIEW-AS ALERT-BOX.
      RETURN.      
   END.
   ASSIGN 
   nytid = FILL-IN-START.
   RUN TIMSEK.P.
   ASSIGN
   regstartsek = sekunder.
   IF FILL-IN-PLUS > 0 THEN DO:
      nytid = FILL-IN-START.
      RUN TIMSEK.P.
      ASSIGN 
      regstartsek = sekunder
      nytid = FILL-IN-PLUS.
      RUN TIMSEK.P.
      RUN PLUSTIDW.P (INPUT pkod).
      ASSIGN FILL-IN-SLUT = nytid.
   END.     
   nytid = FILL-IN-SLUT.
   RUN TIMSEK.P.
   ASSIGN
   regslutsek = sekunder.
   RUN TOTTIDW.P (INPUT pkod).                        
   MESSAGE FILL-IN_RESMAL "och " nytid " timmar förs över till dagboken för " FILL-IN-AONR FILL-IN-DELNR 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         IF NOT VALID-HANDLE(edataapph) THEN DO:
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN EXTRATABHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
            END.
            ELSE DO:
               RUN EXTRATABHMT.P PERSISTENT SET edataapph.
            END.
         END.
         EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.   
         CREATE inextrakopptemp.          
         ASSIGN
         inextrakopptemp.PROGRAM = "AODAGBOK"                   
         inextrakopptemp.KOPPLACHAR1 = FILL-IN-AONR               
         inextrakopptemp.KOPPLAINT1 = FILL-IN-DELNR
         inextrakopptemp.KOPPLACHAR2 = ?            
         inextrakopptemp.KOPPLAINT2 =  ?
         SUBSTRING(inextrakopptemp.SOKCHAR[1],1,15) = Guru.Konstanter:globanv
         SUBSTRING(inextrakopptemp.SOKCHAR[1],20)   = FILL-IN_RESMAL 
         inextrakopptemp.SOKDAT[1]                  = regdatum
         inextrakopptemp.SOKDEC[1]                  = nytid. 
         /*skapa spara */
         RUN sparaextra_UI IN edataapph (INPUT TABLE inextrakopptemp).           
         EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
      END.
      WHEN FALSE THEN DO:
         musz = TRUE. 
      END.      
   END CASE.      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnregs_UI DIALOG-1 
PROCEDURE btnregs_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}    
   RUN resmallabel_UI.
   {ANDTIDBTNR.I }   
   RUN ok_UI. 
   RUN nystart_UI IN allaandh.
   RUN btnnyex_UI IN allaandh (OUTPUT TABLE extratidallt).   
   FIND FIRST extratidallt WHERE NO-LOCK NO-ERROR.

   RUN grundtid_UI.      
   RUN huvud2_UI. 
   FILL-IN-PLUS = 0.
   ASSIGN
   spaonr = FILL-IN-AONR
   spdelnr = FILL-IN-DELNR.
   RUN resmallabel_UI.
   DISPLAY FILL-IN-PKOD FILL-IN-DAG FILL-IN-START FILL-IN-PLUS FILL-IN-SLUT 
          FILL-IN-AONR FILL-IN-DELNR CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK 
          CMB_PRISTYP FILL-IN-PRIS FILL-IN_RESMAL FILL-IN-TEXT CMB_OMR 
          FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnreg_UI DIALOG-1 
PROCEDURE btnreg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}    
   RUN resmallabel_UI.
   {ANDTIDBTNR.I}
   
   RUN ok_UI.   
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN-ARBETSTID TOG_FORSTA FILL-IN-START FILL-IN-PLUS FILL-IN-SLUT 
          FILL-IN-AONR FILL-IN-DELNR FILL-IN_RESMAL FILL-IN-PKOD FILL-IN-DAG 
          CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK CMB_PRISTYP FILL-IN-PRIS 
          FILL-IN-TEXT CMB_OMR FILL-IN-SKP CMB_AVD FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
  ENABLE TOG_FORSTA FILL-IN-START FILL-IN-PLUS FILL-IN-SLUT FILL-IN-AONR 
         FILL-IN-DELNR FILL-IN_RESMAL BTN_SNABB BTN_REG BTN_SLUT BTN_START 
         BTN_AVB CMB_TRAK CMB_OVERUT FILL-IN-UTRYCK CMB_PRISTYP CMB_OMR CMB_AVD 
         BTN_SKAPEN BTN_DAG RECT-22 
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
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN
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
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN
         DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         ELSE DISPLAY FILL-IN-AONR FILL-IN-DELNR CMB_TRAK WITH FRAME {&FRAME-NAME}.   
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
   IF vart = "NYA" THEN DO:
      regdagnamn = extratidallt.DAG.
      ASSIGN  FILL-IN-DATUM = DAY(extratidallt.DATUM).
      IF tillochmeddatum NE ? THEN DO:
         IF DAY(tillochmeddatum) >= FILL-IN-DATUM THEN DO:
            FILL-IN-DATUM = DAY(tillochmeddatum + 1).
         END.
      END.
      DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN-DATUM BTN_FVE BTN_NVE WITH FRAME {&FRAME-NAME}.      
      IF extratidallt.PRISTYP = "RESTID..." THEN DO:
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = extratidallt.AONR AND 
         utsokaonr.DELNR = extratidallt.DELNR USE-INDEX AONR NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = utsokaonr.PRISTYP.
      END.
      ELSE CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = extratidallt.PRISTYP.
      ASSIGN  
      CMB_PRISTYP = INPUT CMB_PRISTYP.
      regdatum = DATE((regmnr),FILL-IN-DATUM,regar).      
      RUN REGDAG.P.
      IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
      FILL-IN-DAG = regdagnamn + "dag".
      DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
      RUN REGVEC.P.
      {SLUTARBW.I}      
      FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
      DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-AONR = extratidallt.AONR
      FILL-IN-DELNR = extratidallt.DELNR
      FILL-IN-START = regstart    
      FILL-IN-SLUT = regslut   
      FILL-IN-UTRYCK = extratidallt.UTRYCKNING
      FILL-IN-TRAKT = extratidallt.TRAKTAMENTE   
      FILL-IN_RESMAL = ""
      CMB_TRAK:SCREEN-VALUE = STRING(extratidallt.TRAKTAMENTE)   
      CMB_TRAK = INPUT CMB_TRAK
      FILL-IN-OVER = personaltemp.OVERTIDUTTAG          
      FILL-IN-PRISTYP = CMB_PRISTYP.
      RUN lucheck_UI.
      RUN skiftcheck_UI.      
      /*PRISFOR*/
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      END.
      ELSE DO:
         ASSIGN
         sok3 = FILL-IN-PRISTYP
         sok4 = personaltemp.PERSONALKOD. 
         IF Guru.Konstanter:appcon THEN DO: 
            RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT 3,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         ELSE DO:
            RUN FLEXTIDH.P 
            (INPUT 3,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
            INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
         END.
         FILL-IN-PRIS = sok5.
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR  Guru.Konstanter:globforetag = "ELPA" THEN musz = musz.
      ELSE DO:      
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF AVAILABLE flexavttemp AND flexavttemp.FLEXTID = TRUE AND FILL-IN-OVER = "I" 
         THEN FILL-IN-OVER = "F". 
      END.            
      IF hjrstart = 0 AND hjrslut = 0 THEN DO:      
         /*inget skift över dygnet*/
         IF regstart < varslut AND varslut < regslut THEN FILL-IN-START = varslut. 
      END.
      ELSE DO:                  
         IF hjrstart < varslut AND varslut < 24 THEN FILL-IN-START = varslut. 
         ELSE IF regstart = 0 AND regslut = 24 THEN DO:
            /*SKIFT 0-24  0-6 2-24 föreslå rätt arbetstid*/
            IF varslut < hjrstart THEN DO:
               IF splunchstarten > varslut  THEN DO:                                 
                  /* 0-6 18-24 arbetstid och det hitentill bara finns ex 0-3 så skall 3-6 föreslås*/
                  ASSIGN FILL-IN-START = varslut
                  FILL-IN-SLUT = splunchstarten.                                    
               END.
            END.
         END.
      END.
   END.   
   ELSE DO:
      
      IF gvisatidpermanad = TRUE THEN DO:
         ASSIGN  FILL-IN-DATUM = DAY(extratidallt.DATUM).
         DISPLAY FILL-IN-DATUM FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.         
      END.
      ELSE DO:
         ASSIGN  CMB_DAG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = extratidallt.DAG.
         CMB_DAG = INPUT CMB_DAG.
         DISPLAY CMB_DAG FILL-IN-VECKO WITH FRAME {&FRAME-NAME}.
         ENABLE CMB_DAG WITH FRAME {&FRAME-NAME}.
      END.
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = extratidallt.PRISTYP.           
      ASSIGN
      CMB_PRISTYP = INPUT CMB_PRISTYP
      regdatum = extratidallt.DATUM.
      RUN REGDAG.P.
      IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
      FILL-IN-DAG = regdagnamn + "dag".
      DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
      RUN REGVEC.P.
      {SLUTARBW.I}      
      FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
      DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.
      ASSIGN
      regvnr = extratidallt.VECKONUMMER
      FILL-IN-VECKO = extratidallt.VECKONUMMER
      FILL-IN-AONR = extratidallt.AONR
      FILL-IN-DELNR = extratidallt.DELNR
      CMB_TRAK:SCREEN-VALUE = STRING(extratidallt.TRAKTAMENTE)   
      CMB_TRAK = INPUT CMB_TRAK
      FILL-IN-PRISTYP = extratidallt.PRISTYP
      FILL-IN-PRIS = extratidallt.PRIS
      kontrollstart = extratidallt.START
      FILL-IN-START = extratidallt.START    
      FILL-IN-SLUT = extratidallt.SLUT
      FILL-IN-OVER = extratidallt.OVERTIDUTTAG     
      FILL-IN-UTRYCK = extratidallt.UTRYCKNING
      FILL-IN_RESMAL = SUBSTRING(extratidallt.RESMAL,1,158).
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "Celpa"  THEN DO:
         FILL-IN-OTBRD = SUBSTRING(extratidallt.RESMAL,159,6).                
      END.       
      FILL-IN_NODF = extratidallt.NODF.             
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
        FILL-IN-LUNCH = extratidallt.LAGANTAL.
      END.
            
   END.  
   ASSIGN
   FILL-IN-PKOD = personaltemp.PERSONALKOD
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      IF FILL-IN-AONR = "165" THEN FILL-IN-OVER = "I".
   END.
   IF FILL-IN-OVER = "K" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Komp".
   IF FILL-IN-OVER = "Ö" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Över".
   IF FILL-IN-OVER = "F" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Flex".     
   IF FILL-IN-OVER = "I" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Ejöv".   
   
   IF FILL-IN-OVER = "N" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "MerK".
   IF FILL-IN-OVER = "M" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "MerÖ".    
  
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
      IF sok5 = 1 THEN FILL-IN-UTRYCK = TRUE.
      IF sok5 = 2 THEN FILL-IN-UTRYCK = FALSE.
      ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
      DISPLAY CMB_PRISTYP FILL-IN-AONR FILL-IN-DELNR /*FILL-IN-UTRYCK */ WITH FRAME {&FRAME-NAME}.
      IF utryckningtemp.UTRYCK1 = TRUE THEN DISPLAY FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
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
      RUN resmallabel_UI.
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
   IF AVAILABLE utryckningtemp AND  utryckningtemp.UTRYCK1 = FALSE THEN DO:
      DISABLE FILL-IN-UTRYCK WITH FRAME {&FRAME-NAME}.
      FILL-IN-UTRYCK:HIDDEN = TRUE.
   END.   
   ELSE DO:
      ENABLE FILL-IN-UTRYCK  WITH FRAME {&FRAME-NAME}.
      FILL-IN-UTRYCK:HIDDEN = FALSE.
   END.  
  
   IF AVAILABLE utryckningtemp AND utryckningtemp.NODF = TRUE THEN DO:
      DISPLAY FILL-IN_NODF  WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN_NODF  WITH FRAME {&FRAME-NAME}.
      FILL-IN_NODF:HIDDEN = FALSE.
   END.   
   ELSE IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:         
      RUN htillatnodf IN asfaktapph (INPUT pkod, OUTPUT nofall, OUTPUT nodatum, OUTPUT nogodk).
      IF nofall = TRUE THEN DO:
         DISPLAY FILL-IN_NODF  WITH FRAME {&FRAME-NAME}.
         ENABLE FILL-IN_NODF  WITH FRAME {&FRAME-NAME}.
         FILL-IN_NODF:HIDDEN = FALSE.
      END.                                              
   END.
   ELSE DO:
      DISABLE FILL-IN_NODF WITH FRAME {&FRAME-NAME}.
      FILL-IN_NODF:HIDDEN = TRUE.
   END.   
   IF Guru.Konstanter:globforetag = "cLULE"  THEN DO:
      DISPLAY FILL-IN_VECKOVILA  WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN_VECKOVILA  WITH FRAME {&FRAME-NAME}.
      FILL-IN_VECKOVILA:HIDDEN = FALSE.
   END.   
   ELSE DO:
      DISABLE FILL-IN_VECKOVILA WITH FRAME {&FRAME-NAME}.
      FILL-IN_VECKOVILA:HIDDEN = TRUE.
   END. 
   musz = FALSE.
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:            
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp THEN DO:
         IF flexavttemp.FLEXTID = TRUE THEN DO:
            musz = TRUE.
         END.
      END.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      DISPLAY FILL-IN-LUNCH  WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN-LUNCH  WITH FRAME {&FRAME-NAME}.
      FILL-IN-LUNCH:HIDDEN = FALSE.      
   END.
   ELSE DO:
      DISABLE FILL-IN-LUNCH WITH FRAME {&FRAME-NAME}.
      FILL-IN-LUNCH:HIDDEN = TRUE.
   END.
   musz = FALSE.   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
      DISABLE CMB_TRAK WITH FRAME {&FRAME-NAME}.
      CMB_TRAK:HIDDEN = TRUE.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      IF vart = "AND" THEN DO:
         DISABLE FILL-IN-PLUS WITH FRAME {&FRAME-NAME}.
         FILL-IN-PLUS:HIDDEN = TRUE.
      END.      
   END.
   IF vart = "DEL" THEN DO:
      DISABLE FILL-IN-DATUM BTN_FVE BTN_NVE /*FILL-IN-SLUT*/ CMB_DAG WITH FRAME {&FRAME-NAME}.
      FILL-IN_RESMAL = "".
      DISPLAY FILL-IN_RESMAL WITH FRAME {&FRAME-NAME}.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
      RUN openbdynspec_UI IN brwproc[2].
      RUN title_UI IN brwproc[2].
      ENABLE FILL-IN-OTBRD BRW_OTBRD WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-OTBRD WITH FRAME {&FRAME-NAME}.      
   END.
   ASSIGN    
   CMB_OMR:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE.   
   IF vart = "AND" THEN DISABLE CMB_DAG WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA"  THEN DO:
      IF vart = "AND" AND extratidallt.OVERTIDUTTAG = "F" THEN DO:
         FIND FIRST tidallt WHERE tidallt.PERSONALKOD = extratidallt.PERSONALKOD AND 
         tidallt.DATUM = extratidallt.DATUM AND tidallt.SLUT LE extratidallt.START AND 
         tidallt.OVERTIDUTTAG = "F" USE-INDEX PKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidallt  THEN DO:         
            DISABLE FILL-IN-START FILL-IN-PLUS WITH FRAME {&FRAME-NAME}.         
            starttidwh:READ-ONLY = TRUE.
         END.
         ELSE starttidwh:READ-ONLY = FALSE.
         FIND FIRST tidallt WHERE tidallt.PERSONALKOD = extratidallt.PERSONALKOD AND 
         tidallt.DATUM = extratidallt.DATUM AND tidallt.START GE extratidallt.SLUT AND 
         tidallt.OVERTIDUTTAG = "F" USE-INDEX PKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidallt  THEN DO:                        
            DISABLE FILL-IN-SLUT FILL-IN-PLUS WITH FRAME {&FRAME-NAME}.         
            sluttidwh:READ-ONLY = TRUE. 

         END.
         ELSE sluttidwh:READ-ONLY = FALSE. 
         IF extratidallt.AONR = "155" THEN DO:
            DISABLE FILL-IN-START FILL-IN-PLUS WITH FRAME {&FRAME-NAME}.         
            starttidwh:READ-ONLY = TRUE.
            DISABLE FILL-IN-SLUT FILL-IN-PLUS WITH FRAME {&FRAME-NAME}.         
            sluttidwh:READ-ONLY = TRUE. 
         END.
      END.
   END.
   /*PRISFOR*/
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
      utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN
         CMB_PRISTYP = utsokaonr.PRISTYP  
         FILL-IN-PRISTYP = utsokaonr.PRISTYP.
         ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
         DISPLAY CMB_PRISTYP WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE DO:
      IF vart = "NYA" THEN DO:
         /*PRISFOR*/
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
         utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN
            sok1 = utsokaonr.AONR      
            sok2 = utsokaonr.DELNR
            sok4 = pkod.       
            RUN nyupp_UI (INPUT 16).      
            ASSIGN
            FILL-IN-PRISTYP = sok1
            FILL-IN-PRIS = sok5
            CMB_PRISTYP = FILL-IN-PRISTYP.
            ASSIGN  CMB_PRISTYP:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FILL-IN-PRISTYP. 
            DISPLAY CMB_PRISTYP FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
         END.
      END.
      ENABLE FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.
   END.   
   RUN otbrd_UI.   
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "Celpa"  THEN DO:         
      IF FILL-IN-OTBRD NE "" THEN DO:      
         FIND FIRST otidbeordtemp WHERE otidbeordtemp.PERSONALKOD = FILL-IN-OTBRD NO-LOCK NO-ERROR.
         IF AVAILABLE otidbeordtemp  THEN DO:            
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(otidbeordtemp)).
            RUN lastselectdyn_UI IN brwproc[2].             
         END.
      END.
   END.      
   
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
   FILL-IN_AONRS = INPUT  FRAME {&FRAME-NAME} FILL-IN_AONRS.
   ASSIGN
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   BRW_EAONR:TITLE = "Favorit " + LC(Guru.Konstanter:gaol)
   CMB_PRISTYP:LABEL = Guru.Konstanter:gdebk
   FILL-IN-PRISTYP:LABEL = Guru.Konstanter:gdebk
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:".
   {TILLFAST2.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   FILL-IN_NODF = FALSE.  
   FILL-IN_VECKOVILA = FALSE.      
   FILL-IN-AVDRA = TRUE.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.   
   FIND FIRST extratidallt NO-LOCK NO-ERROR.   
   FOR EACH automregtemp USE-INDEX PRISTYPER NO-LOCK:
      IF automregtemp.PRISTYP = "RESTID..." THEN musz = musz.
      ELSE status-ok = CMB_PRISTYP:ADD-LAST(automregtemp.PRISTYP).
   END.  
   IF Guru.Konstanter:globforetag = "SOLE" THEN DO:
      status-ok = CMB_TRAK:ADD-LAST("02").
   END.  
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
   FILL-IN-MANAD = regmannamn.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DELETE felmeddtemp.      
   RUN grundtid_UI.      
   SESSION:DATA-ENTRY-RETURN = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lucheck_UI DIALOG-1 
PROCEDURE lucheck_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN DO:
      FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = personaltemp.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE flexavttemp THEN DO:
         IF flexavttemp.FLEXTID = TRUE THEN DO:
            IF regstart NE regslut THEN DO:         
               /*CHECKA OM LUNCH REDAN FINNS*/
               ASSIGN
               sok1 = personaltemp.PERSONALKOD
               sok2 = 0
               sok4 = STRING(regdatum).         
               IF Guru.Konstanter:appcon THEN DO: 
                  RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                  (INPUT 45,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                  INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
               END.
               ELSE DO:
                  RUN FLEXTIDH.P 
                  (INPUT 45,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                  INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
               END.
               IF sok2 > 0  THEN FILL-IN-LUNCH = sok2.               
               ELSE IF lunchslutet NE lunchstarten THEN DO:                     
                  nytid = lunchslutet.
                  RUN TIMSEK.P.
                  ASSIGN
                  seku = sekunder
                  nytid = lunchstarten.
                  RUN TIMSEK.P.
                  ASSIGN
                  sekunder = seku - sekunder
                  FILL-IN-LUNCH = sekunder / 60.
               END.
               ELSE FILL-IN-LUNCH =  0.
            END.
            ELSE FILL-IN-LUNCH =  0.
            DISPLAY FILL-IN-LUNCH WITH FRAME {&FRAME-NAME}.
         END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytolk_UI DIALOG-1 
PROCEDURE nytolk_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   EMPTY TEMP-TABLE tidapptemp NO-ERROR.    
   CREATE tidapptemp.
   ASSIGN
   tidapptemp.PERSONALKOD = personaltemp.PERSONALKOD
   tidapptemp.FORETAG = Guru.Konstanter:globforetag
   tidapptemp.ANVANDARE = Guru.Konstanter:globanv
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok_UI DIALOG-1 
PROCEDURE ok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF musz = TRUE THEN DO: 
      musz = FALSE. /*FIXA ÄNDRA PRIS*/
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
      END.
      IF vart = "DEL" THEN DO:
         ASSIGN
         hjaonr = tidallt.AONR
         hjdelnr = tidallt.DELNR.
         EMPTY TEMP-TABLE etidallt NO-ERROR. 
         CREATE etidallt.
         BUFFER-COPY tidallt TO etidallt.
         DO TRANSACTION:                                       
            /*DEN GAMLA*/
            /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
            ASSIGN 
            SUBSTRING(tidallt.PROGRAM,1,158) = "ANDTIDDEL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv 
            /*obs*/
            tidallt.PERSONALKOD = ""      
            tidallt.SLUT = FILL-IN-START         
            regvnr = FILL-IN-VECKO     
            regdagnamn = CMB_DAG
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
            ASSIGN tidallt.TOTALT = nytid.          
            IF Guru.Konstanter:globforetag = "cLULE"  THEN DO:
               IF tidallt.LONTILLAGG = "" OR tidallt.LONTILLAGG = "108" THEN DO:
                   /*VECKOVILA SKA HA LART 108 EJ 100 TILL LÖN*/
                  sok4 = "108".
                  sok1 = ansttemp.KOD.
                  IF FILL-IN_VECKOVILA = TRUE THEN DO:
                     IF Guru.Konstanter:appcon THEN DO: 
                        RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                        (INPUT 19,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                        INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                     END.
                     ELSE DO:
                        RUN FLEXTIDH.P 
                        (INPUT 19,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                        INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                     END.
                     IF sok2 = 1 THEN DO:
                        ASSIGN tidallt.LONTILLAGG = "108"
                        tidallt.LONTILLANTAL = nytid.
                     END.   
                  END.
                  ELSE ASSIGN tidallt.LONTILLAGG = "" tidallt.LONTILLANTAL = 0.                  
               END.
            END.
            IF TOG_FORSTA = TRUE THEN DO:
               ASSIGN
               tidallt.AONR = FILL-IN-AONR
               tidallt.DELNR = FILL-IN-DELNR.               
               /*Bugg Första tid följde inte resmål pris pristyp med Lena 20171124*/
               {AVVBEFREW.I}
               ASSIGN 
               tidallt.PRISTYP = FILL-IN-PRISTYP
               tidallt.PRIS = FILL-IN-PRIS
               tidallt.RESMAL = FILL-IN_RESMAL
               tidallt.TRAKTAMENTE = FILL-IN-TRAKT
               tidallt.OVERTIDUTTAG = FILL-IN-OVER.
                 
            END.            
            IF gvisatidpermanad = FALSE THEN RUN VECODAT.P.
            ELSE regdatum = DATE((regmnr),FILL-IN-DATUM,regar).             
            EMPTY TEMP-TABLE mellansluttemp NO-ERROR. 
            CREATE mellansluttemp.
            BUFFER-COPY tidallt TO mellansluttemp.
            mellansluttemp.RECTIDVIS = ?.
         END.        
         RUN nytolk_UI.          
         DO TRANSACTION:
            /*DEN NYA*/          
            regdatum = regdatumspar.
            CREATE tidallt.
            {AVVBEFREW.I}
            /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
            ASSIGN SUBSTRING(tidallt.PROGRAM,1,158) = "ANDTIDDEL" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            /*obs*/
            tidallt.PERSONALKOD = ""                 
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
            tidallt.DATUM = regdatum
            tidallt.AONR = FILL-IN-AONR 
            tidallt.DELNR = FILL-IN-DELNR
            tidallt.RESMAL = SUBSTRING(FILL-IN_RESMAL,1,158). 
            IF TOG_FORSTA = TRUE THEN DO:
               ASSIGN
               tidallt.AONR = hjaonr
               tidallt.DELNR = hjdelnr.
               /*Bugg Första tid följde inte resmål pris pristyp med Lena 20171124*/
               FIND FIRST etidallt NO-LOCK NO-ERROR.
               ASSIGN
               tidallt.PRISTYP = etidallt.PRISTYP
               tidallt.PRIS = etidallt.PRIS
               tidallt.RESMAL = etidallt.RESMAL
               tidallt.TRAKTAMENTE = etidallt.TRAKTAMENTE
               tidallt.OVERTIDUTTAG = etidallt.OVERTIDUTTAG.                
            END.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
               SUBSTRING(tidallt.RESMAL,159,6) = SUBSTRING(FILL-IN-OTBRD,1,6).
            END.            
            IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
               tidallt.LAGANTAL = FILL-IN-LUNCH.
            END.
         END.         
         RUN nytolk_UI.                 
         IF FILL-IN-SLUT = mellanslutvar  THEN.
         ELSE DO:
            ASSIGN
            mellansluttemp.START = FILL-IN-SLUT
            mellansluttemp.SLUT = mellanslutvar.
            CREATE tidallt.
            BUFFER-COPY mellansluttemp TO tidallt.
            RUN nytolk_UI.                 
            EMPTY TEMP-TABLE mellansluttemp NO-ERROR. 
         END.
      END.
      ELSE DO:
         DO TRANSACTION:                                               
            ASSIGN
            regvnr = FILL-IN-VECKO     
            regdagnamn = CMB_DAG.
            IF gvisatidpermanad = FALSE THEN RUN VECODAT.P.
            ELSE regdatum = DATE((regmnr),FILL-IN-DATUM,regar).
            ASSIGN SUBSTRING(tidallt.PROGRAM,1,158) = "ANDTID" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
            /*obs*/
            tidallt.PERSONALKOD = ""      
            tidallt.DAG = CMB_DAG
            tidallt.VECKONUMMER = FILL-IN-VECKO
            tidallt.SLUT = FILL-IN-SLUT
            tidallt.START = FILL-IN-START 
            tidallt.TRAKTAMENTE = FILL-IN-TRAKT
            tidallt.OVERTIDUTTAG = FILL-IN-OVER 
            tidallt.UTRYCKNING = FILL-IN-UTRYCK
            tidallt.NODF = FILL-IN_NODF
            tidallt.PRISTYP = FILL-IN-PRISTYP
            tidallt.PRIS = FILL-IN-PRIS.            
            tidallt.RESMAL = SUBSTRING(FILL-IN_RESMAL,1,158). 
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:
               SUBSTRING(tidallt.RESMAL,159,6) = SUBSTRING(FILL-IN-OTBRD,1,6).
            END.            
            IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:
               tidallt.LAGANTAL = FILL-IN-LUNCH.
            END.
            IF tidallt.AONR = FILL-IN-AONR AND
            tidallt.DELNR = FILL-IN-DELNR THEN persrec = persrec.            
            ELSE DO:          
               ASSIGN 
               sekunder = 0
               regdatumspar = regdatum
               regdatum = tidallt.DATUM.
               regdatum = regdatumspar.
            END.
            ASSIGN 
            tidallt.DATUM = regdatum
            tidallt.AONR = FILL-IN-AONR 
            tidallt.DELNR = FILL-IN-DELNR.
            {AVVBEFREW.I}                
            nytid = tidallt.START.
            RUN TIMSEK.P.
            ASSIGN
            regstartsek = sekunder
            nytid = tidallt.SLUT.
            RUN TIMSEK.P.
            ASSIGN
            regslutsek = sekunder
            regdatum = tidallt.DATUM.
            RUN TOTTIDW.P (INPUT pkod).                        
            ASSIGN tidallt.TOTALT = nytid.             
            IF Guru.Konstanter:globforetag = "cLULE"  THEN DO:
               IF tidallt.LONTILLAGG = "" OR tidallt.LONTILLAGG = "108" THEN DO:
               /*VECKOVILA SKA HA LART 108 EJ 100 TILL LÖN*/
                  IF FILL-IN_VECKOVILA = TRUE THEN DO:
                     sok4 = "108".
                     sok1 = ansttemp.KOD.
                     IF Guru.Konstanter:appcon THEN DO: 
                        RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                        (INPUT 19,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                        INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                     END.
                     ELSE DO:
                        RUN FLEXTIDH.P 
                        (INPUT 19,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                        INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                     END.
                     IF sok2 = 1 THEN DO:
                        ASSIGN tidallt.LONTILLAGG = "108"
                        tidallt.LONTILLANTAL = nytid.
                     END.                     
                  END.   
                  ELSE DO:
                     ASSIGN tidallt.LONTILLAGG = "" 
                     tidallt.LONTILLANTAL = 0.                  
                  END.
               END.
            END.                  
            
         END.                                   
         RUN nytolk_UI. 
      END.                                    
      
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE otbrd_UI DIALOG-1 
PROCEDURE otbrd_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:               
      IF FILL-IN-OVER = "F" THEN DO:            
         ASSIGN FILL-IN-OTBRD = "".         
         FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.         
      END.
      ELSE IF personaltemp.OVERTIDUTTAG = "I" THEN DO:         
         IF Guru.Konstanter:globforetag = "SNAT" THEN DO:            
            IF FILL-IN-OVER = "Ö" OR FILL-IN-OVER = "K" THEN DO:
               FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
               BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.               
            END.   
            ELSE DO:
               ASSIGN FILL-IN-OTBRD = "".         
               FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
               BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            END.   
         END.
         ELSE DO:   
            ASSIGN FILL-IN-OTBRD = "".         
            FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
            BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.            
      END.
      ELSE DO:
         FILL-IN-OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         BRW_OTBRD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                  
      END.
   END.   
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
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "117"  THEN musz = TRUE.
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "118"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SNAT" AND FILL-IN-AONR = "119"  THEN musz = TRUE.            
   
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
   IF Guru.Konstanter:globforetag = "SKOK" AND FILL-IN-AONR = "131"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SKOK" AND FILL-IN-AONR = "132"  THEN musz = TRUE.            
   IF Guru.Konstanter:globforetag = "SKOK" AND FILL-IN-AONR = "134"  THEN musz = TRUE.              
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skiftcheck_UI DIALOG-1 
PROCEDURE skiftcheck_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/        
   ASSIGN
   hjrstart = 0
   hjrslut = 0.
   IF Guru.Konstanter:globforetag = "elpa" OR  Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:                     
      IF regstart = 0 OR regslut = 24 THEN DO:
         ASSIGN
         spregdatum = regdatum
         spregstart = regstart
         spregslut = regslut
         splunchstarten = lunchstarten
         splunchslutet = lunchslutet                       
         regdatum = regdatum + 1.
         RUN REGVEC.P.
         {SLUTARBW.I}   
         FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
         DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.   
         ASSIGN
         skregdatum = regdatum
         skregstart = regstart
         skregslut = regslut
         sklunchstarten = lunchstarten
         sklunchslutet = lunchslutet            
         regdatum = spregdatum.
         RUN REGVEC.P.
         {SLUTARBW.I} 
         FILL-IN-ARBETSTID = STRING(regstart,">9.99") + "-" + STRING(regslut,">9.99") + "  lunch: " + STRING(lunchstarten,">9.99") + "-" + STRING(lunchslutet,">9.99").
         DISPLAY FILL-IN-ARBETSTID WITH FRAME {&FRAME-NAME}.
         IF regstart = 0 AND regslut = 24 THEN DO: 
            ASSIGN
            FILL-IN-START = splunchslutet.    
            IF skregslut = 24 THEN ASSIGN FILL-IN-SLUT = sklunchstarten. 
            ELSE ASSIGN FILL-IN-SLUT = skregslut.                
            ASSIGN
            hjrstart = FILL-IN-START
            hjrslut = FILL-IN-SLUT.
         END.
         ELSE IF regslut = 24 AND skregstart = 00 THEN DO:
            IF skregslut = 24 THEN ASSIGN FILL-IN-SLUT = sklunchstarten. 
            ELSE ASSIGN FILL-IN-SLUT = skregslut. 
            ASSIGN
            hjrstart = FILL-IN-START
            hjrslut = FILL-IN-SLUT.
         END.
         IF FILL-IN-SLUT = 00 AND FILL-IN-START NE 00 THEN FILL-IN-SLUT = 24.
         DISPLAY FILL-IN-START FILL-IN-SLUT WITH FRAME {&FRAME-NAME}.   
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

