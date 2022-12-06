&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/08/97 -  8:47 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER allatider AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW NEW
{TIDALLT.I}
{PERBEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{FORETEMP.I}
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}

{DIRDEF.I}
{UPPGHMT.I}
&Scoped-define SHARED SHARED
{FLEXTAB.I}
{PHMT.I}
{SOKDEF.I}
{WHANDLTEMP.I}

&Scoped-define NEW NEW
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.

DEFINE NEW SHARED VARIABLE brwbdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwavdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE varaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE vardelnr AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE vartrakt AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE varpris AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE varslut AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE varpristyp  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varfabef AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varutryck AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE vartotalt AS DECIMAL NO-UNDO.
DEFINE NEW SHARED VARIABLE korda AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE debitering AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE gamlakoden  AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE reov AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE placerarec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE pertidaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE pertiddelnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE allaandh AS HANDLE NO-UNDO.

DEFINE SHARED VARIABLE dirtidoveraknare AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE dirtid AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE dirtidrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE allatidvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE flexav AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartgamla AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bilforare AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.

DEFINE VARIABLE tidalltrec AS RECID NO-UNDO.
DEFINE VARIABLE tidalltrec2 AS RECID NO-UNDO.
DEFINE VARIABLE extrarec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE openqtid AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE varlon AS CHARACTER NO-UNDO.
DEFINE VARIABLE varlantal AS DECIMAL NO-UNDO.
DEFINE VARIABLE nydatum AS DATE NO-UNDO.
DEFINE VARIABLE nyaonr AS CHARACTER NO-UNDO.
DEFINE VARIABLE nydelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE valdbil AS CHARACTER NO-UNDO.
DEFINE VARIABLE energiavt AS LOGICAL NO-UNDO.
DEFINE VARIABLE varifran AS INTEGER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE xhop AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.   
DEFINE VARIABLE colsortlog AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-stopp AS LOGICAL NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO. 
DEFINE VARIABLE projvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE bortvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE pbkollsv AS LOGICAL NO-UNDO.
DEFINE VARIABLE entrepejtid AS LOGICAL NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ptraff AS LOGICAL NO-UNDO.
DEFINE VARIABLE lasanv AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE byttemp NO-UNDO
   FIELD VADGORA AS INTEGER
   FIELD BEFATTNING AS CHARACTER
   FIELD PRIS AS DECIMAL
   FIELD PRISTYP AS CHARACTER 
   FIELD UTRYCKNING AS LOGICAL
   FIELD AONR AS CHARACTER 
   FIELD DELNR AS INTEGER
   FIELD TRAKTAMENTE AS INTEGER 
   FIELD ANVANDARE AS CHARACTER
   FIELD RECTIDVIS AS RECID.
DEFINE TEMP-TABLE ebyttemp NO-UNDO LIKE byttemp
   FIELD DATUM AS DATE
   FIELD START AS DECIMAL.
DEFINE TEMP-TABLE spardatum
   FIELD sregvnr AS INTEGER FORMAT "999" 
   FIELD sregdagnamn AS CHARACTER FORMAT "X(3)"   
   FIELD sregdatum AS DATE 
   FIELD sbdatum AS DATE 
   FIELD savdatum AS DATE. 

DEFINE BUFFER tidbuff FOR tidallt.

{TIDUTTTNEW.I}

DEFINE NEW SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar för perioden", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 31.5 BY 2
     BGCOLOR 8  NO-UNDO.
DEFINE NEW SHARED VARIABLE TOGGLE-MONTH AS LOGICAL INITIAL ? 
     LABEL "Två tidsedlar vid månadasskifte":L 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY 1 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_EGNAAONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES egnaao tidallt overtemp

/* Definitions for BROWSE BRW_EGNAAONR                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_EGNAAONR egnaao.AONR egnaao.DELNR ~
egnaao.OMRADE egnaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_EGNAAONR 
&Scoped-define QUERY-STRING-BRW_EGNAAONR FOR EACH egnaao NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_EGNAAONR OPEN QUERY BRW_EGNAAONR FOR EACH egnaao NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_EGNAAONR egnaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_EGNAAONR egnaao


/* Definitions for BROWSE BRW_TIDANDG                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDANDG tidallt.DATUM tidallt.DAG ~
tidallt.START tidallt.SLUT tidallt.TOTALT tidallt.VILART ~
tidallt.BEREDSKAPSTART tidallt.BEREDSKAPSLUT tidallt.BERANTAL tidallt.AONR ~
tidallt.DELNR tidallt.BERBEORD tidallt.LONTILLANTAL tidallt.LONAUTO ~
tidallt.TRAKTANTAL tidallt.TRAKTAUTO tidallt.ANVANDARE tidallt.TRAKTAMENTE ~
tidallt.VIBEFATTNING tidallt.PRISTYP tidallt.PRIS tidallt.GODKAND ~
SUBSTRING (tidallt.VECKOKORD,4,7) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDANDG tidallt.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TIDANDG tidallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TIDANDG tidallt
&Scoped-define QUERY-STRING-BRW_TIDANDG FOR EACH tidallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TIDANDG OPEN QUERY BRW_TIDANDG FOR EACH tidallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TIDANDG tidallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDANDG tidallt


/* Definitions for BROWSE BRW_TIDOVRG                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDOVRG overtemp.DATUM overtemp.DAG ~
overtemp.AONR overtemp.DELNR overtemp.VILART overtemp.OVERANTAL ~
overtemp.OVERAUTO overtemp.GODKAND 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDOVRG overtemp.DATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TIDOVRG overtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TIDOVRG overtemp
&Scoped-define QUERY-STRING-BRW_TIDOVRG FOR EACH overtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TIDOVRG OPEN QUERY BRW_TIDOVRG FOR EACH overtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TIDOVRG overtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDOVRG overtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-MENY MBTN_BER MBTN_LON FBTN_VPERS ~
MBTN_OVER FBTN_NAAVB FBTN_VISA BTN_NY BTN_UPP BTN_BORT BTN_NYALLA ~
BTN_SKAPEGNA MBTN_TID BTN_PREG BTN_DELA BTN_KOP BTN_BYTA BTN_NYLON BTN_RES ~
MBTN_TRAKT BTN_AVB BTN_AVBH 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS FILL-IN-PKOD ~
FILL-IN_FORNAMN-2 FILL-IN-RUBRIK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVBH AUTO-END-KEY 
     LABEL "Stäng Guru":L 
     SIZE 14 BY 1 TOOLTIP "Stänger ner hela Guru".

DEFINE BUTTON BTN_BERED 
     LABEL "Registrera beredskap":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_BYTA 
     LABEL "Byta aonr":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_DELA 
     LABEL "Dela upp":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_FORDON 
     LABEL "Fordon":L 
     SIZE 12 BY 1.5 TOOLTIP "Registrera tex skylift".

DEFINE BUTTON BTN_KOP 
     LABEL "Kopiera dag":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_NYALLA 
     LABEL "  Buffert registrering":L 
     SIZE 12 BY 1.5 TOOLTIP "Gör flera tidreg. på en gång och lagra dessa i en buffert för senare tolkning.".

DEFINE BUTTON BTN_NYLON 
     LABEL "   Nytt lönetillägg":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_PREG 
     LABEL "Period":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_RES 
     LABEL "Endagsrestid":L 
     SIZE 12 BY 1.5 TOOLTIP "Enstaka registrering antingen före eller efter ordinarie arbetstid, tolkar enbart restid".

DEFINE BUTTON BTN_SKAPEGNA 
     LABEL "Lägg upp/ta bort favoriter":L 
     SIZE 12 BY 1.5 TOOLTIP "Lägg upp/ta bort favoritprojektnr".

DEFINE BUTTON BTN_TJRESA 
     LABEL "Registrera Tjänsteresa":L 
     SIZE 12 BY 1.5 TOOLTIP "Flerdygnstjänsteresa med tolkning av t.ex traktamente, resetillägg, måltidsavdrag och restid (både inrikes och utrikes) eller endagsrestid för flera dagar på en gång".

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.5.

DEFINE BUTTON FBTN_FABEF 
     LABEL "Visa avvikande bef.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_NAAVB 
     LABEL "Nästa person":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa tidsedel":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPERS 
     LABEL "Välj person":L 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_BER  NO-FOCUS
     LABEL "Bered" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_LON  NO-FOCUS
     LABEL "Lön" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_OVER  NO-FOCUS
     LABEL "Övertid" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_TID  NO-FOCUS
     LABEL "Tid" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_TRAKT  NO-FOCUS
     LABEL "Trakt" 
     SIZE 7.25 BY 2.33.

DEFINE VARIABLE FILL-IN-INGA AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 63 BY 2.88
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RUBRIK AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 61.63 BY 1.33
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_AR AS INTEGER FORMAT "9999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_VECKONUMMER AS INTEGER FORMAT "->>>>>>9" INITIAL 0 
     LABEL "Veckonr" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 1.5 BY 1.04 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 63.5 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124.88 BY .08.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_EGNAAONR FOR 
      egnaao SCROLLING.

DEFINE QUERY BRW_TIDANDG FOR 
      tidallt SCROLLING.

DEFINE QUERY BRW_TIDOVRG FOR 
      overtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_EGNAAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EGNAAONR WINDOW-1 _STRUCTURED
  QUERY BRW_EGNAAONR NO-LOCK DISPLAY
      egnaao.AONR FORMAT "X(6)":U
      egnaao.DELNR FORMAT "999":U
      egnaao.OMRADE FORMAT "x(6)":U
      egnaao.ORT FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING NO-SCROLLBAR-VERTICAL SIZE 33.5 BY 4.5
         TITLE "Egna aonr".

DEFINE BROWSE BRW_TIDANDG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDANDG WINDOW-1 _STRUCTURED
  QUERY BRW_TIDANDG NO-LOCK DISPLAY
      tidallt.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      tidallt.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      tidallt.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U COLUMN-FGCOLOR 1
            LABEL-FGCOLOR 1
      tidallt.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U COLUMN-FGCOLOR 3
            LABEL-FGCOLOR 3
      tidallt.TOTALT COLUMN-LABEL "Tid" FORMAT ">9.99":U COLUMN-FGCOLOR 4
            LABEL-FGCOLOR 4
      tidallt.VILART COLUMN-LABEL "Lart" FORMAT "X(5)":U WIDTH 6
      tidallt.BEREDSKAPSTART COLUMN-LABEL "Start" FORMAT "99.99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      tidallt.BEREDSKAPSLUT COLUMN-LABEL "Slut" FORMAT "99.99":U
            COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      tidallt.BERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U COLUMN-FGCOLOR 4
            LABEL-FGCOLOR 4
      tidallt.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      tidallt.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      tidallt.BERBEORD COLUMN-LABEL "Beordrad" FORMAT "Ja/Nej":U
      tidallt.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>>9.<<":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      tidallt.LONAUTO COLUMN-LABEL "Auto/!Manuell" FORMAT "Auto/Manuell":U
            WIDTH 8
      tidallt.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "-99.9":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      tidallt.TRAKTAUTO COLUMN-LABEL "Auto/!Manuell" FORMAT "Auto/Manuell":U
            WIDTH 8
      tidallt.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 17
      tidallt.TRAKTAMENTE COLUMN-LABEL "Trakt.!zon" FORMAT "99":U
      tidallt.VIBEFATTNING COLUMN-LABEL "Befattning" FORMAT "X(25)":U
      tidallt.PRISTYP COLUMN-LABEL "Debitering" FORMAT "X(9)":U
            WIDTH 11
      tidallt.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9":U
      tidallt.GODKAND COLUMN-LABEL "Godkänd" FORMAT "x(256)":U
            WIDTH 10
      SUBSTRING (tidallt.VECKOKORD,4,7) COLUMN-LABEL "Till lön!eko.sys"
  ENABLE
      tidallt.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 108 BY 17.

DEFINE BROWSE BRW_TIDOVRG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDOVRG WINDOW-1 _STRUCTURED
  QUERY BRW_TIDOVRG NO-LOCK DISPLAY
      overtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      overtemp.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      overtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      overtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      overtemp.VILART COLUMN-LABEL "Lart" FORMAT "X(4)":U WIDTH 5
      overtemp.OVERANTAL COLUMN-LABEL "Antal" FORMAT "-99.99":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      overtemp.OVERAUTO COLUMN-LABEL "Auto/Man" FORMAT "Auto/Manuell":U
      overtemp.GODKAND COLUMN-LABEL "Godkänd!tidsedel" FORMAT "x(256)":U
            WIDTH 10
  ENABLE
      overtemp.DATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57 BY 17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     MBTN_BER AT ROW 1 COL 26
     RAD_ALLTID AT ROW 1.67 COL 1.5 NO-LABEL
     MBTN_LON AT ROW 1 COL 15.5
     BRW_EGNAAONR AT ROW 3.58 COL 75.5
     FILL-IN_NAMN AT ROW 4.5 COL 22.25 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN-REGIS AT ROW 4.92 COL 3.25 COLON-ALIGNED NO-LABEL
     FILL-IN_FORNAMN AT ROW 4.92 COL 22.25 COLON-ALIGNED NO-LABEL
     FILL-IN-PKOD AT ROW 6 COL 13.25 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 6 COL 22.25 COLON-ALIGNED NO-LABEL
     FILL-IN_AR AT ROW 7.04 COL 22.25 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 7.04 COL 31.63 COLON-ALIGNED NO-LABEL
     FILL-IN_VECKONUMMER AT ROW 7.04 COL 31.88 COLON-ALIGNED
     FBTN_VPERS AT ROW 8 COL 111
     BRW_TIDANDG AT ROW 8.25 COL 1.5
     MBTN_OVER AT ROW 1 COL 52.5
     BRW_TIDOVRG AT ROW 8.25 COL 20.25
     FBTN_NAAVB AT ROW 9.08 COL 111
     FBTN_VISA AT ROW 10.21 COL 111
     FBTN_FABEF AT ROW 11.29 COL 111
     FILL-IN-INGA AT ROW 13.42 COL 10.25 COLON-ALIGNED NO-LABEL
     RAD_ALLVAL AT ROW 15.54 COL 3.13 NO-LABEL
     BTN_NY AT ROW 25.67 COL 24.88
     BTN_UPP AT ROW 25.67 COL 39
     BTN_BORT AT ROW 25.67 COL 53.13
     BTN_NYALLA AT ROW 25.67 COL 67.25
     BTN_SKAPEGNA AT ROW 25.67 COL 81.5
     MBTN_TID AT ROW 1 COL 1
     BTN_PREG AT ROW 27.17 COL 2.5
     BTN_DELA AT ROW 27.17 COL 15.13
     BTN_KOP AT ROW 27.17 COL 28.63
     BTN_BYTA AT ROW 27.17 COL 42.13
     BTN_BERED AT ROW 27.17 COL 42.13
     BTN_NYLON AT ROW 27.17 COL 55.63
     BTN_RES AT ROW 27.17 COL 69.13
     BTN_TJRESA AT ROW 27.17 COL 82.63
     MBTN_TRAKT AT ROW 1 COL 38
     BTN_FORDON AT ROW 27.17 COL 96
     BTN_AVB AT ROW 27.17 COL 111
     BTN_AVBH AT ROW 28.25 COL 111
     FILL-IN-RUBRIK AT ROW 3.54 COL 1.5 NO-LABEL
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: egnaao T "?" NO-UNDO temp-db egnaao
      TABLE: tidallt T "?" NO-UNDO temp-db tidallt
      TABLE: ? T "?" NO-UNDO temp-db overtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Registrera / ändra tidregistrering"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,                                                          */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_EGNAAONR MBTN_LON FRAME-A */
/* BROWSE-TAB BRW_TIDANDG FBTN_VPERS FRAME-A */
/* BROWSE-TAB BRW_TIDOVRG MBTN_OVER FRAME-A */
/* SETTINGS FOR BROWSE BRW_EGNAAONR IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_EGNAAONR:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR BROWSE BRW_TIDANDG IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TIDANDG:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR BROWSE BRW_TIDOVRG IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TIDOVRG:HIDDEN  IN FRAME FRAME-A                = TRUE.

/* SETTINGS FOR BUTTON BTN_BERED IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BERED:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_FORDON IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FORDON:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_TJRESA IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_TJRESA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON FBTN_FABEF IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_FABEF:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-INGA IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-INGA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-RUBRIK IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_AR IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_VECKONUMMER IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_VECKONUMMER:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLTID IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLTID:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EGNAAONR
/* Query rebuild information for BROWSE BRW_EGNAAONR
     _TblList          = "Temp-Tables.egnaao"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.egnaao.AONR
     _FldNameList[2]   = Temp-Tables.egnaao.DELNR
     _FldNameList[3]   = Temp-Tables.egnaao.OMRADE
     _FldNameList[4]   = Temp-Tables.egnaao.ORT
     _Query            is NOT OPENED
*/  /* BROWSE BRW_EGNAAONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDANDG
/* Query rebuild information for BROWSE BRW_TIDANDG
     _TblList          = "Temp-Tables.tidallt"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.tidallt.DATUM
"tidallt.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidallt.DAG
"tidallt.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidallt.START
"tidallt.START" "Start!tid" ? "decimal" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tidallt.SLUT
"tidallt.SLUT" "Slut!tid" ? "decimal" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tidallt.TOTALT
"tidallt.TOTALT" "Tid" ">9.99" "decimal" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tidallt.VILART
"tidallt.VILART" "Lart" "X(5)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tidallt.BEREDSKAPSTART
"tidallt.BEREDSKAPSTART" "Start" ? "decimal" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tidallt.BEREDSKAPSLUT
"tidallt.BEREDSKAPSLUT" "Slut" ? "decimal" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tidallt.BERANTAL
"tidallt.BERANTAL" "Antal" ? "decimal" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tidallt.AONR
"tidallt.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tidallt.DELNR
"tidallt.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tidallt.BERBEORD
"tidallt.BERBEORD" "Beordrad" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.tidallt.LONTILLANTAL
"tidallt.LONTILLANTAL" "Antal" ">>>>9.<<" "decimal" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.tidallt.LONAUTO
"tidallt.LONAUTO" "Auto/!Manuell" "Auto/Manuell" "logical" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.tidallt.TRAKTANTAL
"tidallt.TRAKTANTAL" "Antal" ? "decimal" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.tidallt.TRAKTAUTO
"tidallt.TRAKTAUTO" "Auto/!Manuell" "Auto/Manuell" "logical" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.tidallt.ANVANDARE
"tidallt.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.tidallt.TRAKTAMENTE
"tidallt.TRAKTAMENTE" "Trakt.!zon" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.tidallt.VIBEFATTNING
"tidallt.VIBEFATTNING" "Befattning" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > Temp-Tables.tidallt.PRISTYP
"tidallt.PRISTYP" "Debitering" ? "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > Temp-Tables.tidallt.PRIS
"tidallt.PRIS" "Pris" ">>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > Temp-Tables.tidallt.GODKAND
"tidallt.GODKAND" "Godkänd" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"SUBSTRING (tidallt.VECKOKORD,4,7)" "Till lön!eko.sys" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TIDANDG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDOVRG
/* Query rebuild information for BROWSE BRW_TIDOVRG
     _TblList          = "Temp-Tables.overtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.overtemp.DATUM
"overtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.overtemp.DAG
"overtemp.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.overtemp.AONR
"overtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.overtemp.DELNR
"overtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.overtemp.VILART
"overtemp.VILART" "Lart" "X(4)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.overtemp.OVERANTAL
"overtemp.OVERANTAL" "Antal" ? "decimal" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.overtemp.OVERAUTO
"overtemp.OVERAUTO" "Auto/Man" "Auto/Manuell" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.overtemp.GODKAND
"overtemp.GODKAND" "Godkänd!tidsedel" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TIDOVRG */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_TIDANDG
&Scoped-define SELF-NAME BRW_TIDANDG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TIDANDG WINDOW-1
ON VALUE-CHANGED OF BRW_TIDANDG IN FRAME FRAME-A
DO:   
   RUN btidandg_UI.  
   /*Lena  Olsson Elpool i Umeå AB  4 okt 2017 09:16:42 
   lägga in tjänsteresa på veckokörd en lönekörd tidsedel mittsverige  
   */
         
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TIDOVRG
&Scoped-define SELF-NAME BRW_TIDOVRG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TIDOVRG WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_TIDOVRG IN FRAME FRAME-A
DO:
   RUN upptid_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TIDOVRG WINDOW-1
ON VALUE-CHANGED OF BRW_TIDOVRG IN FRAME FRAME-A
DO:   
   RUN brwand_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO: 
   {AVBGOM.I}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.   
   ASSIGN
   flexav = FALSE.  /*obs lena flex*/
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBH WINDOW-1
ON CHOOSE OF BTN_AVBH IN FRAME FRAME-A /* Stäng Guru */
DO: 
   
      
   MESSAGE "Vill du verkligen stänga ner hela Guru?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
   IF val = TRUE THEN DO:
      {AVBGOM.I}
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.        
      ASSIGN
      flexav = FALSE.  /*obs lena flex*/
      IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
      DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
      Guru.Konstanter:apphand = ?.
      Guru.Konstanter:appcon = FALSE.
      {AVBFRAM.I}
      QUIT.
   END.
   ELSE RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BERED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BERED WINDOW-1
ON CHOOSE OF BTN_BERED IN FRAME FRAME-A /* Registrera beredskap */
DO:
   {muswait.i}      
   FIND FIRST spardatum NO-ERROR.
   IF NOT AVAILABLE spardatum THEN CREATE spardatum.
   ASSIGN
   spardatum.sregvnr = regvnr  
   spardatum.sregdagnamn = regdagnamn    
   spardatum.sregdatum = regdatum  
   spardatum.sbdatum = bdatum  
   spardatum.savdatum = avdatum.  
   IF AVAILABLE tidallt THEN regdatum = tidallt.DATUM.
   {AVBGOMD.I}
   RUN BERREG.W (INPUT 2,INPUT pkod).    
   ASSIGN 
   regvnr =     spardatum.sregvnr  
   regdagnamn = spardatum.sregdagnamn    
   regdatum =   spardatum.sregdatum  
   bdatum =     spardatum.sbdatum  
   avdatum =    spardatum.savdatum.
    
   IF AVAILABLE tidpers THEN APPLY "CHOOSE" TO MBTN_BER.      
   musz = FALSE.

   
   {AVBFRAMD.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO: 
   {muswait.i}
   RUN bort_UI.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTA WINDOW-1
ON CHOOSE OF BTN_BYTA IN FRAME FRAME-A /* Byta aonr */
DO:
   
   RUN valdbyt_UI.

   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DELA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DELA WINDOW-1
ON CHOOSE OF BTN_DELA IN FRAME FRAME-A /* Dela upp */
DO:
   RUN btndela_UI. 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FORDON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FORDON WINDOW-1
ON CHOOSE OF BTN_FORDON IN FRAME FRAME-A /* Fordon */
DO:   
   antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   ELSE RUN bilar_UI.              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP WINDOW-1
ON CHOOSE OF BTN_KOP IN FRAME FRAME-A /* Kopiera dag */
DO:
   {muswait.i}    
   antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   ELSE DO:
      {AVBGOMD.I}
      IF AVAILABLE tidallt THEN DO:                  
         RUN KOPIDAG.W (INPUT pkod,INPUT tidallt.DATUM ).
      END.
      ELSE DO:
         RUN KOPIDAG.W (INPUT pkod,INPUT ?).
      END.
      {AVBFRAMD.I}.
      {musarrow.i}       
      IF musz = FALSE THEN DO:
         BRW_TIDANDG:HIDDEN = FALSE.        
         ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
         FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}.               
         IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
            IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
         END.
         IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
            IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
            DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
         END.         
         IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.         
         RUN open_UI.   
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
         RUN lastselectdyn_UI IN brwproc[1].                                          
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
      END.   
   END.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP WINDOW-1
ON GO OF BTN_KOP IN FRAME FRAME-A /* Kopiera dag */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOP WINDOW-1
ON LEAVE OF BTN_KOP IN FRAME FRAME-A /* Kopiera dag */
DO:
   musz = musz.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON MOUSE-MENU-CLICK OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:   
   APPLY "CHOOSE" TO BTN_NYALLA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYALLA WINDOW-1
ON CHOOSE OF BTN_NYALLA IN FRAME FRAME-A /*   Buffert registrering */
DO:   
   IF RAD_ALLTID = 1 THEN DO:   
      {BUFFTID.I}
      {muswait.i} 
      vart = "NYA".        
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI. 
      
      regmannamn = FILL-IN-MANAD.
      {AVBGOMD.I}
      RUN ANDINTID.W (INPUT pkod, INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      {musarrow.i}     
      IF musz = FALSE THEN DO:
         BRW_TIDANDG:HIDDEN = FALSE.
         ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
         FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}. 
         RUN open_UI.
         FIND tidallt WHERE RECID(tidallt) = tidalltrec NO-LOCK NO-ERROR.  
         IF NOT AVAILABLE tidallt THEN tidalltrec = tidalltrec2.
         ELSE DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].            
         END.      
      END.
   END.
   IF RAD_ALLTID = 2 THEN DO:            
      IF AVAILABLE tidallt THEN DO:
         EMPTY TEMP-TABLE etidallt NO-ERROR.                  
         antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.            
         IF antal_valda = 0 THEN RETURN.
         IF NOT AVAILABLE tidallt  THEN RETURN.         
         antal_raknare = 1.
         KOLL:      
         DO WHILE antal_raknare LE antal_valda :               
            status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.         
            CREATE etidallt.
            BUFFER-COPY tidallt TO etidallt.
            antal_raknare = antal_raknare + 1.
         END.   
         
         RUN LONBLANKETT.P (INPUT TABLE etidallt).       
      END.
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYLON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYLON WINDOW-1
ON CHOOSE OF BTN_NYLON IN FRAME FRAME-A /*    Nytt lönetillägg */
DO:
   {muswait.i}
   antal_raknare = 1.            
   status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME} NO-ERROR.               
   IF AVAILABLE tidallt THEN DO:
      ASSIGN
      nydatum = tidallt.DATUM 
      nyaonr = tidallt.AONR
      nydelnr = tidallt.DELNR.
   END.
   IF allatider = 1 THEN DO:
      IF AVAILABLE tidallt THEN DO:            
         ASSIGN
         regdatum = tidallt.DATUM
         regdagnamn = tidallt.DAG
         tidalltrec = RECID(tidallt)
         tidalltrec2 = tidalltrec.    
      END.
      ELSE regdagnamn = "MÅN".     
      ASSIGN
      vart = "NYA" 
      bdatum = brwbdatum
      avdatum = brwavdatum
      allatider = 2.
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      allatider = 1.
      {AVBGOMD.I}
      RUN ANDLON.W (INPUT 1 , INPUT 1,INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      musz = FALSE.
   END.
   
   {musarrow.i}
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PREG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PREG WINDOW-1
ON CHOOSE OF BTN_PREG IN FRAME FRAME-A /* Period */
DO:
   {muswait.i} 
   antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      pertidaonr = ?.
      pertiddelnr = ?.
   END.
   ELSE DO:
      IF AVAILABLE tidallt THEN DO:      
          BRW_TIDANDG:FETCH-SELECTED-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
          pertidaonr = tidallt.AONR.
          pertiddelnr = tidallt.DELNR.
      END.
      ELSE DO:
         pertidaonr = ?.
         pertiddelnr = ?.
      END.
   END.
   {AVBGOMD.I} 
   IF AVAILABLE tidallt THEN DO:                  
      RUN PERTID.W (INPUT pkod,INPUT tidallt.DATUM ).
   END.
   ELSE DO:
      RUN PERTID.W (INPUT pkod,INPUT ?).
   END.   
   {AVBFRAMD.I}.
   {musarrow.i}      
   
   /* det kan vara felmeddelande för en rad av flera*/
   BRW_TIDANDG:HIDDEN = FALSE.        
   ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
   FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   
   IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
      IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
      IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
   END.   
   IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
   DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.   
   RUN open_UI.   
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
   RUN lastselectdyn_UI IN brwproc[1].                                          

   IF musz = TRUE THEN DO:
      musz = FALSE.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PREG WINDOW-1
ON GO OF BTN_PREG IN FRAME FRAME-A /* Period */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PREG WINDOW-1
ON LEAVE OF BTN_PREG IN FRAME FRAME-A /* Period */
DO:
   musz = musz.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_RES
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_RES WINDOW-1
ON CHOOSE OF BTN_RES IN FRAME FRAME-A /* Endagsrestid */
DO:
   vart = "NYA".
   RUN valdny_UI (INPUT TRUE).
   RUN nytid_UI.
   energiavt = FALSE.        
   IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.
   
   
   FIND FIRST extratidallt NO-ERROR.
   IF AVAILABLE extratidallt THEN DO :            
      extratidallt.ENFLERDAGS = "Endag".
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 1
      soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
      soktemp.SOKCHAR[2] = pkod
      soktemp.SOKCHAR[3] = "RESTID..."
      soktemp.SOKCHAR[4] = personaltemp.BEFATTNING 
      soktemp.SOKDATE[1] = extratidallt.DATUM.
      {SOKANROP.I}
      ASSIGN         
      extratidallt.PRISTYP = "RESTID..." 
      extratidallt.PRIS = soktemp.SOKDECI[1]. 
      IF energiavt = TRUE THEN ASSIGN extratidallt.TRAKTAMENTE = 00.         
      ELSE extratidallt.TRAKTAMENTE = 01.
      IF Guru.Konstanter:globforetag = "GKAL" THEN extratidallt.TRAKTAMENTE = 01.
   END.   
   RUN resstart_UI.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKAPEGNA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEGNA WINDOW-1
ON CHOOSE OF BTN_SKAPEGNA IN FRAME FRAME-A /* Lägg upp/ta bort favoriter */
DO:  
   {AVBGOMD.I}   
   RUN EGNAAONR.W (INPUT pkod).
   {AVBFRAMD.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEGNA WINDOW-1
ON GO OF BTN_SKAPEGNA IN FRAME FRAME-A /* Lägg upp/ta bort favoriter */
DO:
   /**/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TJRESA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TJRESA WINDOW-1
ON CHOOSE OF BTN_TJRESA IN FRAME FRAME-A /* Registrera Tjänsteresa */
DO:
   {muswait.i}         
   {AVBGOMD.I}
   FIND FIRST spardatum NO-ERROR.
   IF NOT AVAILABLE spardatum THEN CREATE spardatum.
   ASSIGN
   spardatum.sregvnr = regvnr  
   spardatum.sregdagnamn = regdagnamn    
   spardatum.sregdatum = regdatum  
   spardatum.sbdatum = bdatum  
   spardatum.savdatum = avdatum.  
   RUN RESVAL.W (OUTPUT varifran).
   
   IF varifran = 1 THEN DO:      
      IF AVAILABLE tidallt THEN DO:            
         RUN RESREGN.W 
         (INPUT tidallt.DATUM, INPUT tidallt.AONR, INPUT tidallt.DELNR, 
          INPUT TRUE, INPUT 2,INPUT pkod).            
      END.
      ELSE DO:         
         RUN RESREGN.W 
         (INPUT regdatum, INPUT "", INPUT 0, INPUT TRUE, INPUT 2,INPUT pkod).
            
      END.            
   END.
   ELSE IF varifran = 2 THEN DO:
      IF AVAILABLE tidallt THEN DO:
         RUN RESBORT.W
         (INPUT tidallt.DATUM, INPUT 2,INPUT pkod).
      END.
      ELSE DO:
         RUN RESBORT.W 
         (INPUT regdatum, INPUT 2,INPUT pkod).
      END.
   END.   
   ELSE IF varifran = 3 THEN DO:
      IF AVAILABLE tidallt THEN DO:
         RUN TJANAST.W
         (INPUT tidallt.DATUM, INPUT 2,INPUT pkod, INPUT tidallt.AONR, INPUT tidallt.DELNR).
      END.
      ELSE DO:
         RUN TJANAST.W 
        (INPUT regdatum, INPUT 2,INPUT pkod, INPUT "", INPUT 0).
      END.
   END.   
   ELSE IF varifran = 4 THEN DO:      
      IF AVAILABLE tidallt THEN DO:
         RUN UTLRESR.W 
         (INPUT tidallt.DATUM, INPUT 2,INPUT pkod, INPUT tidallt.AONR, INPUT tidallt.DELNR).
      END.
      ELSE DO:
         RUN UTLRESR.W 
         (INPUT regdatum, INPUT 2,INPUT pkod, INPUT "", INPUT 0).
      END.      
   END.    
   musz = FALSE.
   ASSIGN 
   regvnr =     spardatum.sregvnr  
   regdagnamn = spardatum.sregdagnamn    
   regdatum =   spardatum.sregdatum  
   bdatum =     spardatum.sbdatum  
   avdatum =    spardatum.savdatum.
   IF musz = FALSE THEN RUN open_UI.      

   /*FÖNSTERPROBLEM*/
   /*
   APPLY "ENTRY" TO RAD_ALLTID IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
   */   
   
   APPLY "CHOOSE" TO MBTN_TID.  
   {AVBFRAMD.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Ändra */
DO:  
   {muswait.i}
   RUN upptid_UI.   
   {musarrow.i}
   musz = FALSE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FABEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FABEF WINDOW-1
ON CHOOSE OF FBTN_FABEF IN FRAME FRAME-A /* Visa avvikande bef. */
DO:
   RUN btnfabef_UI.
   status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW()  NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FABEF WINDOW-1
ON GO OF FBTN_FABEF IN FRAME FRAME-A /* Visa avvikande bef. */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NAAVB WINDOW-1
ON CHOOSE OF FBTN_NAAVB IN FRAME FRAME-A /* Nästa person */
DO:    
   IF NOT AVAILABLE tidpers THEN DO:           
     APPLY "WINDOW-CLOSE":U TO WINDOW-1. 
   END.
   ELSE FIND NEXT tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   {muswait.i}    
   IF NOT AVAILABLE tidpers THEN DO:    
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE":U TO WINDOW-1.    
   END.   
   ELSE DO:
      EMPTY TEMP-TABLE overtemp NO-ERROR. 
      EMPTY TEMP-TABLE tidallt NO-ERROR.       
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ASSIGN
      status-stopp = FALSE
      pkod = tidpers.PERSONALKOD.      
      RUN huvud_UI.
      RUN hmtegna_UI.
      RUN starthelp_UI.
      /*sista raden redan förvald. Gör koll om knappar skall släckas om raden är godkänd*/
      RUN btidandg_UI.      
      musz = FALSE.
   END.
   IF VALID-HANDLE(aonrapph) THEN DO:   
      RUN bilkoll_UI IN aonrapph (INPUT pkod,OUTPUT pbkollsv).
      IF pbkollsv = FALSE THEN BTN_FORDON:HIDDEN = TRUE.
   END.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_NAAVB WINDOW-1
ON GO OF FBTN_NAAVB IN FRAME FRAME-A /* Nästa person */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa tidsedel */
DO:
   RUN btnvisa_UI.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON GO OF FBTN_VISA IN FRAME FRAME-A /* Visa tidsedel */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPERS WINDOW-1
ON CHOOSE OF FBTN_VPERS IN FRAME FRAME-A /* Välj person */
DO:
   {AVBGOMD.I}
   RUN VPERVAL.W.
   {AVBFRAMD.I}     
   {muswait.i} 
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ASSIGN
      status-stopp = FALSE.
      RUN vpersval_UI.
      RUN starthelp_UI.      
      /*sista raden redan förvald. Gör koll om knappar skall släckas om raden är godkänd*/
      RUN btidandg_UI.      
      musz = FALSE.
   END.
   {musarrow.i}   
   musz = FALSE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_BER WINDOW-1
ON CHOOSE OF MBTN_BER IN FRAME FRAME-A /* Bered */
DO:
   RAD_ALLTID = 3.
   RUN btnvit_UI (INPUT MBTN_BER:HANDLE).
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_LON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_LON WINDOW-1
ON CHOOSE OF MBTN_LON IN FRAME FRAME-A /* Lön */
DO:
   RAD_ALLTID = 2.
   RUN btnvit_UI (INPUT MBTN_LON:HANDLE).
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_OVER WINDOW-1
ON CHOOSE OF MBTN_OVER IN FRAME FRAME-A /* Övertid */
DO:
   RAD_ALLTID = 5.
   RUN btnvit_UI (INPUT MBTN_OVER:HANDLE).
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_TID WINDOW-1
ON CHOOSE OF MBTN_TID IN FRAME FRAME-A /* Tid */
DO:
   RAD_ALLTID = 1.
   RUN btnvit_UI (INPUT MBTN_TID:HANDLE).
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_TRAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_TRAKT WINDOW-1
ON CHOOSE OF MBTN_TRAKT IN FRAME FRAME-A /* Trakt */
DO:
   RAD_ALLTID = 4.
   RUN btnvit_UI (INPUT MBTN_TRAKT:HANDLE).
   APPLY "VALUE-CHANGED" TO RAD_ALLTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ALLTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ALLTID WINDOW-1
ON VALUE-CHANGED OF RAD_ALLTID IN FRAME FRAME-A
DO:   
      
   IF RAD_ALLTID = 1 THEN DO:      
      ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_FORDON WITH FRAME {&FRAME-NAME}.
      IF Guru.Konstanter:globforetag = "SUND"  OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:            
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flexavttemp THEN DO:
            BTN_NY:LOAD-IMAGE("bilder\xbtn_overtid.gif") NO-ERROR.               
         END.
         ELSE IF flexavttemp.FLEXTID = FALSE THEN DO:
            BTN_NY:LOAD-IMAGE("bilder\xbtn_ny.gif") NO-ERROR.               
         END.
         ELSE DO:
            BTN_NY:LOAD-IMAGE("bilder\xbtn_overtid.gif") NO-ERROR.               
         END.
      END.
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         BTN_RES:HIDDEN = TRUE.
      END.
      IF Guru.Konstanter:globforetag = "MISV" THEN DO:
         BTN_NYLON:HIDDEN = TRUE.
      END.
      IF pbkollsv = FALSE THEN BTN_FORDON:HIDDEN = TRUE.
   END.
   ELSE DO: 
      BTN_FORDON:HIDDEN = TRUE.
      BTN_NY:LOAD-IMAGE("bilder\xbtn_ny.gif") NO-ERROR.         
   END.      
   IF RAD_ALLTID = 3 THEN DO:
      /*CCCC*/
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.      
   allatider = RAD_ALLTID.
   tempcolh = ?.
   {BUFFTID.I}
   RUN radandtid_UI.      
   RUN visknapp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_EGNAAONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(allaandh) THEN DELETE PROCEDURE allaandh NO-ERROR.   
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.     
   IF VALID-HANDLE(otbeordapph) THEN DELETE PROCEDURE otbeordapph NO-ERROR.
   IF VALID-HANDLE(persapph) THEN DELETE PROCEDURE persapph NO-ERROR.
   
  
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 2,INPUT pkod,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   regar = YEAR(regdatum).  
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.
      LEAVE MAIN-BLOCK.
   END.
   IF personaltemp.AKTIV = FALSE THEN DO:
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.
      LEAVE MAIN-BLOCK.
   END.   
   RUN huvud_UI.
   RUN hmtegna_UI.
   IF allatidvar = TRUE THEN DO:      
      APPLY "MOUSE-MENU-CLICK" TO BTN_NY IN FRAME {&FRAME-NAME}.
      allatidvar = FALSE.      
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.
      LEAVE MAIN-BLOCK.
   END.
   ELSE DO:
      RUN enable_UI.
      IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
         ENABLE BTN_TJRESA WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_TJRESA WITH FRAME {&FRAME-NAME}.
      END.
      {FRMSIZE.I} 
   END.                
   ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}.
   DISPLAY BTN_FORDON WITH FRAME {&FRAME-NAME}. 
   BTN_FORDON:HIDDEN = TRUE.      
   IF allatider = 1 THEN DO:      
      BTN_FORDON:HIDDEN = FALSE.
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         BTN_RES:HIDDEN = TRUE.
      END.
      IF Guru.Konstanter:globforetag = "MISV" THEN DO:
         BTN_NYLON:HIDDEN = TRUE.
      END.   
   END.
   ELSE BTN_FORDON:HIDDEN = TRUE.      
   DISPLAY FILL-IN_AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}. 
   
   nydatum = brwbdatum.  
   status-ok = RAD_ALLTID:DELETE("").  
   ASSIGN
   MBTN_TID:COLUMN = 1
   MBTN_BER:COLUMN = 1
   MBTN_LON:COLUMN = 1 
   MBTN_TRAKT:COLUMN = 1
   MBTN_OVER:COLUMN = 1
   MBTN_TID:HIDDEN = TRUE
   MBTN_BER:HIDDEN = TRUE
   MBTN_LON:HIDDEN = TRUE
   MBTN_TRAKT:HIDDEN = TRUE
   MBTN_OVER:HIDDEN = TRUE.
   MBTN_TID:LOAD-IMAGE("BILDER\xbtn_tidreg.gif").
   MBTN_BER:LOAD-IMAGE("BILDER\xbtn_beredskap.gif").
   MBTN_LON:LOAD-IMAGE("BILDER\xbtn_lon.gif").
   MBTN_TRAKT:LOAD-IMAGE("BILDER\xbtn_trakt.gif").
   MBTN_OVER:LOAD-IMAGE("BILDER\xbtn_overtidm.gif").
   Guru.GlobalaVariabler:collefth = ?.
   IF Guru.Konstanter:tidasekvar[1] = TRUE THEN DO:
      status-ok = RAD_ALLTID:ADD-LAST("Tidregistrering", 1).      
   END.
   IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO: 
      status-ok = RAD_ALLTID:ADD-LAST("Lönetillägg", 2). 
      
   END.
   ELSE BTN_NYLON:HIDDEN = TRUE.
   IF Guru.Konstanter:tidasekvar[3] = TRUE THEN DO:
      status-ok = RAD_ALLTID:ADD-LAST("Beredskap", 3).              
      
   END.
   IF Guru.Konstanter:tidasekvar[4] = TRUE THEN DO:
      status-ok = RAD_ALLTID:ADD-LAST("Traktamente", 4).              
      
   END.
   IF Guru.Konstanter:tidasekvar[5] = TRUE THEN DO:
      status-ok = RAD_ALLTID:ADD-LAST("Övertidstillägg", 5).           
      
   END.
   RUN buttplace_UI.
   ON 'VALUE-CHANGED' OF FRAME FRAME-A PERSISTENT RUN buttplace_UI IN THIS-PROCEDURE.
   RUN setapplyvc IN framesizeh (INPUT FRAME FRAME-A:HANDLE ).
   IF Guru.Konstanter:tidtsekvar[1] = FALSE THEN DO:
      BTN_TJRESA:HIDDEN = TRUE.
      BTN_RES:HIDDEN = TRUE.
   END.   
   RAD_ALLTID = allatider.   
   IF RAD_ALLTID = 1 THEN DO:   
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:            
         FIND FIRST flexavttemp WHERE flexavttemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flexavttemp THEN DO:
            BTN_NY:LOAD-IMAGE("bilder\xbtn_overtid.gif") NO-ERROR.            
         END.
         ELSE IF flexavttemp.FLEXTID = FALSE THEN DO:
            BTN_NY:LOAD-IMAGE("bilder\xbtn_ny.gif") NO-ERROR.            
         END.
         ELSE DO:
            BTN_NY:LOAD-IMAGE("bilder\xbtn_overtid.gif") NO-ERROR.            
         END.
      END.
      ELSE DO:       
         BTN_NY:LOAD-IMAGE("bilder\xbtn_ny.gif") NO-ERROR.         
      END.    
   END.        
   {BUFFTID.I}
   IF vaxla = TRUE THEN FBTN_VISA:HIDDEN = TRUE.   
   RUN sek_UI.    
   RUN VISMEDDU.W.
   ASSIGN
   BTN_FORDON:LABEL IN FRAME {&FRAME-NAME} = "Registrera~n  bilar"
   BTN_NYLON:LABEL IN FRAME {&FRAME-NAME} = "   Nytt~nlönetillägg"
   BTN_NYALLA:LABEL IN FRAME {&FRAME-NAME} = "  Buffert~nregistrering"
   BTN_RES:LABEL IN FRAME {&FRAME-NAME} = "Endags~nrestid"
   BTN_TJRESA:LABEL IN FRAME {&FRAME-NAME} = "Registrera~nTjänsteresa"   
   BTN_BERED:LABEL IN FRAME {&FRAME-NAME} = "Registrera~nberedskap".
   BTN_SKAPEGNA:LABEL IN FRAME {&FRAME-NAME} = "Lägg upp/ta bort~nfavorit "  + LC(Guru.Konstanter:gaok).
   BTN_BERED:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   FBTN_FABEF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.  
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_BERED:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_BERED:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).   
   BTN_BERED:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   FBTN_FABEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_NYALLA:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_NYALLA:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_RES:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_RES:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   
   IF BTN_TJRESA:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:
      RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_TJRESA:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).   
      RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_TJRESA:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   END.   
   IF BTN_NYLON:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:
      RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_NYLON:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
      RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_NYLON:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   END.   
   
   
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_SKAPEGNA:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_SKAPEGNA:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   RUN SetWindowLongA IN Guru.Konstanter:hpApi (BTN_FORDON:HWND IN FRAME {&FRAME-NAME},-16,1409294336,OUTPUT ReturnValue).
   RUN SendMessageA IN Guru.Konstanter:hpApi (BTN_FORDON:HWND IN FRAME {&FRAME-NAME},244,1409294336,1,OUTPUT ReturnValue).
   BTN_SKAPEGNA:TOOLTIP = "Lägg upp/ta bort favorit "  + LC(Guru.Konstanter:gaok).     
   RUN starthelp_UI.
   /*sista raden redan förvald. Gör koll om knappar skall släckas om raden är godkänd*/
   RUN btidandg_UI.         
   IF Guru.Konstanter:varforetypval[24] = 1 THEN.   
   ELSE DO:
      BTN_SKAPEGNA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.               
   END.
   musz = FALSE.
   RUN visknapp_UI.
 
   IF RAD_ALLTID = 1 THEN RUN btnvit_UI (INPUT MBTN_TID:HANDLE).
   IF RAD_ALLTID = 2 THEN RUN btnvit_UI (INPUT MBTN_LON:HANDLE).
   IF RAD_ALLTID = 3 THEN RUN btnvit_UI (INPUT MBTN_BER:HANDLE).
   IF RAD_ALLTID = 4 THEN RUN btnvit_UI (INPUT MBTN_TRAKT:HANDLE).
   IF RAD_ALLTID = 5 THEN RUN btnvit_UI (INPUT MBTN_OVER:HANDLE).
   RUN bilkoll_UI IN aonrapph (INPUT pkod,OUTPUT pbkollsv).
   IF pbkollsv = FALSE THEN BTN_FORDON:HIDDEN = TRUE.
   
   /*/*Lena  Olsson Elpool i Umeå AB  4 okt 2017 09:16:42 
   lägga in tjänsteresa på veckokörd en lönekörd tidsedel mittsverige  
   */
   ENABLE BTN_TJRESA WITH FRAME {&FRAME-NAME}.
   DISPLAY BTN_TJRESA WITH FRAME {&FRAME-NAME}.
   ENABLE BTN_UPP WITH FRAME {&FRAME-NAME}.
   DISPLAY BTN_UPP WITH FRAME {&FRAME-NAME}.
   */
   
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   ASSIGN
   tidallt.DATUM:READ-ONLY IN BROWSE BRW_TIDANDG = TRUE
   overtemp.DATUM:READ-ONLY IN BROWSE BRW_TIDOVRG = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TIDANDG:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_TIDOVRG:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_EGNAAONR:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.       
   END.
   ELSE DO:
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN PHMTAPP.P PERSISTENT SET persapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
      RUN PHMTAPP.P PERSISTENT SET persapph.
   END. 
   
   ON START-SEARCH OF BRW_TIDANDG IN FRAME {&FRAME-NAME} PERSISTENT RUN colproc_UI IN THIS-PROCEDURE.
   CREATE whandltemp.
   ordningnr = 1.   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,WINDOW-1:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NY:HANDLE IN FRAME {&FRAME-NAME}).        
   RUN ALLANDM.P PERSISTENT SET allaandh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokomp_UI WINDOW-1 
PROCEDURE aokomp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FOR EACH egnaao NO-LOCK:
         RUN komplettaonr IN aonrapph 
         (INPUT egnaao.AONR,INPUT egnaao.DELNR,
         OUTPUT aonrrec,OUTPUT projvar,OUTPUT bortvar).
         ASSIGN
         egnaao.PROJEKTOR = projvar
         egnaao.AONRREC = aonrrec
         egnaao.TABORT = bortvar.      
      END.
      FOR EACH egnaao,
      EACH omrtemp WHERE omrtemp.OMRADE = egnaao.OMRADE:
         egnaao.AVDELNINGNR = omrtemp.AVDELNINGNR.
      END.
   END.
   ELSE IF vad = 2 THEN DO:
      RUN komplettaonr IN aonrapph 
         (INPUT egnaao.AONR,INPUT egnaao.DELNR,
         OUTPUT aonrrec,OUTPUT projvar,OUTPUT bortvar).
      ASSIGN
      egnaao.PROJEKTOR = projvar
      egnaao.AONRREC = aonrrec
      egnaao.TABORT = bortvar.      
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = egnaao.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         egnaao.AVDELNINGNR = omrtemp.AVDELNINGNR.
      END.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE berand_UI WINDOW-1 
PROCEDURE berand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN open_UI.    
   DISPLAY FILL-IN_AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.
   FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
   YEAR(tidallt.DATUM) = YEAR(brwbdatum) AND 
   MONTH(tidallt.DATUM) = MONTH(brwbdatum) AND tidallt.BEREDSKAP NE ''
   USE-INDEX PKOD NO-LOCK NO-ERROR.
  
   IF tidsedlog = TRUE THEN DO:
      FBTN_NAAVB:HIDDEN = TRUE. 
      tidsedlog = FALSE.
   END.  
   IF NOT AVAILABLE tidallt THEN DO:
      DISABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}. 
      regdatum = brwbdatum.
      RUN kontroll_UI.
      musz = FALSE.      
   END.
   ELSE DO :
      regvnr = tidallt.VECKONUMMER.   
      regdatum = tidallt.DATUM.
      RUN kontroll_UI.
      musz = FALSE. 
      tidalltrec = RECID(tidallt).
      RUN open_UI.    
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
      IF dirtid = TRUE THEN DO:         
         dirtid = FALSE.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = dirtidrec NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
            IF tillochmeddatum NE ? THEN DO:
               IF tidallt.DATUM > tillochmeddatum THEN DO:                  
                  IF status-stopp = FALSE THEN ENABLE BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.                                 
               END.
            END.
            ELSE DO:               
               IF status-stopp = FALSE THEN ENABLE BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.                                       
            END.
         END.                 
      END.         
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bilar_UI WINDOW-1 
PROCEDURE bilar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   varpristyp = "TOT.PRIS.".   
   antal_raknare = 1.            
   status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME} NO-ERROR.     
   IF AVAILABLE tidallt THEN DO:
      ASSIGN
      nydatum = tidallt.DATUM 
      nyaonr = tidallt.AONR
      nydelnr = tidallt.DELNR.
   END.
   IF allatider = 1 THEN DO:
      IF AVAILABLE tidallt THEN DO:            
         ASSIGN
         regdatum = tidallt.DATUM
         regdagnamn = tidallt.DAG
         tidalltrec = RECID(tidallt)
         tidalltrec2 = tidalltrec.    
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = tidallt.AONR AND 
         utsokaonr.DELNR = tidallt.DELNR NO-LOCK NO-ERROR.                  
         IF AVAILABLE utsokaonr THEN DO:         
            varpristyp = utsokaonr.PRISTYP.   
         END.
      END.
      ELSE regdagnamn = "MÅN".     
      ASSIGN
      vart = "NYA" 
      bdatum = brwbdatum
      avdatum = brwavdatum.               
      allatider = 2.                     
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      allatider = 1.
      ASSIGN 
      extratidallt.LONTILLANTAL = varlantal 
      extratidallt.PRISTYP = varpristyp.      
      antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.      
      {AVBGOMD.I}
      RUN ANDLON.W (INPUT 2,INPUT antal_valda,INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      EMPTY TEMP-TABLE extratidallt NO-ERROR.       
      IF musz = TRUE THEN DO:                 
         musz = FALSE. 
      END.
      ELSE DO:
         antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         IF antal_valda > 1 THEN DO:           
            /*DEN SOM HAR FÅTT LÖNETILL*/
            FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec NO-ERROR.
            IF AVAILABLE tidallt THEN DO:
               ASSIGN
               valdbil = tidallt.LONTILLAGG
               varaonr = tidallt.AONR 
               vardelnr = tidallt.DELNR              
               antal_raknare = 2.
               DO WHILE antal_raknare LE antal_valda :                           
                  status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.                                
                  IF AVAILABLE tidallt THEN DO:
                     ASSIGN
                     nydatum = tidallt.DATUM 
                     nyaonr = tidallt.AONR
                     nydelnr = tidallt.DELNR.
                  END.
                  IF AVAILABLE tidallt THEN DO:            
                     ASSIGN
                     regdatum = tidallt.DATUM
                     regdagnamn = tidallt.DAG
                     tidalltrec = RECID(tidallt)
                     tidalltrec2 = tidalltrec.    
                  END.
                  ELSE regdagnamn = "MÅN".     
                  ASSIGN
                  vart = "NYA" 
                  bdatum = brwbdatum
                  avdatum = brwavdatum.                                        
                  musz = FALSE.
                  FIND FIRST utsokaonr WHERE utsokaonr.AONR = nyaonr AND 
                  utsokaonr.DELNR = nydelnr NO-LOCK NO-ERROR.                  
                  IF AVAILABLE utsokaonr THEN DO: 
                     IF utsokaonr.PRISTYP NE "FRÅNVARO." THEN musz = TRUE.
                  END.
                  ELSE musz = TRUE.
                  IF musz = TRUE THEN DO:
                     musz = FALSE.                  
                     allatider = 2.
                     RUN valdny_UI (INPUT FALSE).
                     RUN nytid_UI.
                     allatider = 1.
                     ASSIGN                     
                     /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
                     SUBSTRING(extratidallt.PROGRAM,1,158) = "LONREG" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv                     
                     extratidallt.LONAUTO = FALSE 
                     extratidallt.LONTILLAGG = valdbil
                     extratidallt.LONTILLANTAL = varlantal                    
                     extratidallt.PRISTYP = utsokaonr.PRISTYP.      
                     ASSIGN 
                     tidallt.LONTILLANTAL = varlantal
                     tidallt.LONTILLAGG = valdbil.                                                         
                  END.
                  antal_raknare = antal_raknare + 1.
                  antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.                  
               END.
               avdatum = brwavdatum.
            END.
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN LONREG.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT Guru.Konstanter:globanv,INPUT 1,INPUT pkod,INPUT "" ,INPUT TABLE extratidallt,
               OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
            END.
            ELSE DO:
               RUN LONREG.P 
               (INPUT Guru.Konstanter:globanv,INPUT 1,INPUT pkod,INPUT "",INPUT TABLE extratidallt,
               OUTPUT placerarec,OUTPUT TABLE tidallt APPEND).
            END.                         
         END.
      END.
   END.
   {musarrow.i}
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF allatider = 5 THEN DO:
      IF overtemp.OVERAUTO = TRUE THEN DO:
         MESSAGE 
         "Det går inte att ta bort ett automatiskt övertidstillägg. Sätt antal lika med 0."
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
      IF overtemp.GODKAND NE "" THEN RETURN.           
      RUN valdabort_UI.
      IF openqtid = TRUE THEN DO:
         RUN open_UI.
         FIND FIRST overtemp WHERE overtemp.RECTIDVIS = placerarec NO-ERROR.
         placerarec = RECID(overtemp).
         IF AVAILABLE overtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(overtemp)).
            RUN lastselectdyn_UI IN brwproc[2].                          
            IF overtemp.GODKAND NE "" THEN 
            DISABLE BTN_BORT BTN_UPP BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}.         
         END.
         ELSE DO:
            BRW_TIDOVRG:HIDDEN = TRUE.            
            DISABLE BTN_BORT BTN_UPP BTN_UPP BTN_DELA  BTN_BYTA BTN_NYLON BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}. 
            DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}.      
         END.
      END.      
      ELSE DO:
         FIND FIRST overtemp WHERE overtemp.RECTIDVIS = placerarec NO-ERROR.
         placerarec = RECID(overtemp).
         IF AVAILABLE overtemp THEN DO:    
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(overtemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.
      END.
      musz = FALSE.
      RETURN.
   END.
   ELSE DO:
      status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() NO-ERROR.
      ASSIGN
      tidalltrec = RECID(tidallt)
      tidalltrec2 = tidalltrec
      bdatum = brwbdatum
      avdatum = brwavdatum.
      IF tidallt.GODKAND NE "" THEN RETURN.
      IF tidallt.ENFLERDAGS = "FLERDAG" THEN DO:
         MESSAGE "Flerdygnsförättning tas bort i restids rutinen!"
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
      RUN valdabort_UI.
      IF openqtid = TRUE THEN DO:
         RUN open_UI.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec NO-ERROR.
         tidalltrec = RECID(tidallt).
         IF AVAILABLE tidallt THEN DO:    
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].
            IF tidallt.GODKAND NE "" THEN 
            DISABLE BTN_BORT BTN_UPP BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}.         
         END.
         ELSE DO:
            BRW_TIDANDG:HIDDEN = TRUE.            
            DISABLE BTN_BORT BTN_UPP BTN_UPP BTN_DELA BTN_KOP BTN_BYTA BTN_NYLON BTN_RES  WITH FRAME {&FRAME-NAME}. 
            DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}.      
         END.
      END.
      ELSE DO:
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec NO-ERROR.
         tidalltrec = RECID(tidallt).
         IF AVAILABLE tidallt THEN DO:    
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
         END.
      END.
      musz = FALSE.
      RETURN.        
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwand_UI WINDOW-1 
PROCEDURE brwand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_ALLTID = 2 THEN DO:  
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
         ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.
      END.

   END.
   IF RAD_ALLTID NE 5 THEN DO:  
      status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
      IF AVAILABLE tidallt THEN DO:   
         regvnr = tidallt.VECKONUMMER.   
         IF tidallt.GODKAND NE "" THEN DO:
            DISABLE  BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
         END.  
         ELSE DO:
            IF RAD_ALLTID = 3 THEN DO:
               IF status-stopp = FALSE THEN ENABLE BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
            END.   
            ELSE IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
         END.   
      END.
   END.   
   IF RAD_ALLTID = 5 THEN DO:  
      status-ok = BRW_TIDOVRG:SELECT-FOCUSED-ROW() NO-ERROR. 
      IF AVAILABLE overtemp THEN DO:
         IF overtemp.GODKAND = "" THEN 
         IF status-stopp = FALSE THEN ENABLE BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
         ELSE DISABLE BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btidandg_UI WINDOW-1 
PROCEDURE btidandg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_ALLTID NE 1 THEN RUN brwand_UI.
   ELSE DO:
      antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.   
      musz = FALSE.
      IF antal_valda = 0 THEN RETURN.
      IF NOT AVAILABLE tidallt  THEN RETURN.      
      antal_raknare = 1.
      KOLL:      
      DO WHILE antal_raknare LE antal_valda :               
         status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.         
         IF tidallt.GODKAND NE "" THEN DO:
            musz = TRUE.
            LEAVE KOLL.
         END.
         antal_raknare = antal_raknare + 1.
      END.   
      IF antal_valda = 1 THEN DO:
         IF tidallt.GODKAND NE "" THEN musz = TRUE.
         ELSE musz = FALSE.      
      END.
      IF musz = TRUE THEN DO:
         IF tillochmeddatum = ? THEN musz = musz.
         ELSE
         DISABLE BTN_UPP BTN_DELA  BTN_BYTA BTN_BORT BTN_NYLON BTN_RES WITH FRAME {&FRAME-NAME}.                          
      END.
      ELSE DO:
         IF status-stopp = FALSE THEN ENABLE BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
         IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
            IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
            DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
         END.
         IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
            IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
            DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
         END.         
         IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.         
      END.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btndela_UI WINDOW-1 
PROCEDURE btndela_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.   
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN NO-APPLY.                
   END.
   IF tidallt.PRISTYP = "RESTID..." THEN DO:
      MESSAGE "Detta är en restidsregistrering." SKIP
      "Du kan inte dela upp en restidsregistrering." SKIP         
      "Ta bort din tjänsteresa och gör om tjänsterese-registreringen." VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa"  THEN DO:
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN.
      ELSE IF tidallt.aonr = "150" THEN DO:
         MESSAGE "Semester registreras enbart hela dagar" VIEW-AS ALERT-BOX.             
         RETURN NO-APPLY.                
      END.
   END.
   ASSIGN
   tidalltrec = RECID(tidallt).   
   {muswait.i}
   ASSIGN
   regdatum = tidallt.DATUM
   vart = "DEL".
   
   RUN valdny_UI (INPUT TRUE).   
   {AVBGOMD.I}
   /*IF Guru.Konstanter:globforetag = "elpa" THEN RUN ANDTIDA.W (INPUT pkod,INPUT TABLE extratidallt).*/
   RUN ANDTID.W (INPUT pkod,INPUT TABLE extratidallt).
   {AVBFRAMD.I}
   IF musz = FALSE THEN DO:
      RUN open_UI.      
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
      tidallt.TIDLOG = TRUE NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].                         
   END.
   {musarrow.i}
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfabef_UI WINDOW-1 
PROCEDURE btnfabef_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   EMPTY TEMP-TABLE tidut NO-ERROR.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN AVBEF.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT pkod,INPUT brwbdatum,INPUT brwavdatum,OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut). 
   END.
   ELSE DO:
      RUN AVBEF.P 
      (INPUT pkod,INPUT brwbdatum,INPUT brwavdatum,OUTPUT TABLE felmeddtemp,OUTPUT TABLE tidut). 
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.     
      RETURN.
   END.
   {AVBGOM.I}
   RUN UTRAPP.W (INPUT "Befattningar").
   {AVBFRAM.I}  
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   {musarrow.i}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnyex_UI WINDOW-1 
PROCEDURE btnnyex_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER TABLE FOR extratidallt.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnysl_UI WINDOW-1 
PROCEDURE btnnysl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF musz = FALSE THEN DO:         
      BRW_TIDANDG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}.       
      IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
      END.
      IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
      END.      
      IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.                                
      ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_FORDON WITH FRAME {&FRAME-NAME}.          
      IF pbkollsv = FALSE THEN BTN_FORDON:HIDDEN = TRUE.                     
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         BTN_RES:HIDDEN = TRUE.
      END.
      IF Guru.Konstanter:globforetag = "MISV" THEN DO:
         BTN_NYLON:HIDDEN = TRUE.
      END.
      RUN open_UI.            
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
      tidallt.TIDLOG = TRUE NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].               
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnyst_UI WINDOW-1 
PROCEDURE btnnyst_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    vart = "NYA".        
    RUN valdny_UI (INPUT TRUE).      
    RUN nytid_UI.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnny_UI WINDOW-1 
PROCEDURE btnny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   {muswait.i}         
   IF allatider = 1 THEN DO: 
      RUN btnnyst_UI.      
      {AVBGOMD.I}      
      RUN ANDTID.W (INPUT pkod,INPUT TABLE extratidallt).      
      {AVBFRAMD.I}
      RUN btnnysl_UI.      
   END.
   IF allatider = 2 THEN DO:
      vart = "NYA".
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.      
      {AVBGOMD.I}
      RUN ANDLON.W (INPUT 1 , INPUT 1,INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = FALSE THEN DO:        
         status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         BRW_TIDANDG:HIDDEN = FALSE.
         ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
         FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
         RUN open_UI.    
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
         tidallt.LONTILLAGG NE ""
         NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].            
         END.         
      END.
      IF musz = TRUE THEN DO:                 
         musz = FALSE.
      END.
   END.
   IF allatider = 3 THEN DO:       
      /* ny beredskap- bara för admin pers annars veckoberedskap Lena 20040303*/
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         IF Guru.Konstanter:globniv = 0 OR Guru.Konstanter:globniv = 1 OR Guru.Konstanter:globniv = 30  THEN musz = musz.
         ELSE DO:      
            MESSAGE "Använd istället Registrera beredskap" VIEW-AS ALERT-BOX.            
            RETURN.
         END.
      END.
      IF Guru.Konstanter:globforetag = "LULE"  OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         IF Guru.Konstanter:globniv = 0  OR Guru.Konstanter:globniv = 10  THEN musz = musz.
         ELSE DO:      
            MESSAGE "Använd istället Registrera beredskap" VIEW-AS ALERT-BOX.            
            RETURN.
         END.
      END.
      ASSIGN
      vart = "NYA"         
      bdatum = brwbdatum
      avdatum = brwavdatum. 
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      {AVBGOMD.I}
      RUN ANDBER.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = FALSE THEN DO:
         BRW_TIDANDG:HIDDEN = FALSE.
         ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
         FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
         RUN open_UI.    
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec NO-ERROR.
         IF tidallt.DATUM >= brwbdatum THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].            
         END.
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.         
      END.      
      ASSIGN
      bdatum = brwbdatum
      avdatum = brwavdatum.
   END.
   IF allatider = 4 THEN DO:
      FIND tidallt WHERE RECID(tidallt) = tidalltrec NO-LOCK NO-ERROR.
      IF AVAILABLE tidallt THEN DO:
         status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() NO-ERROR.
         APPLY "VALUE-CHANGED" TO BRW_TIDANDG.         
         ASSIGN
         regdatum = tidallt.DATUM
         regdagnamn = tidallt.DAG
         tidalltrec = RECID(tidallt).
         tidalltrec2 = tidalltrec.    
      END.   
      ELSE regdagnamn = "MÅN".     
      ASSIGN
      vart = "NYA"
      bdatum = brwbdatum
      avdatum = brwavdatum. 
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      {AVBGOMD.I}
      RUN ANDTRAKT.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = FALSE THEN DO:
         status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         BRW_TIDANDG:HIDDEN = FALSE.
         ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
         FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
         RUN open_UI.    
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
         tidallt.TRAKTKOD NE ""
         NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
             RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
             RUN lastselectdyn_UI IN brwproc[1].            
         END.         
      END.
      IF musz = TRUE THEN DO:                 
         musz = FALSE.
      END.      
      bdatum = brwbdatum.
      avdatum = brwavdatum.      
   END.
   IF allatider = 5 THEN DO:
      IF AVAILABLE overtemp THEN DO:
         status-ok = BRW_TIDOVRG:SELECT-FOCUSED-ROW() NO-ERROR.
         APPLY "VALUE-CHANGED" TO BRW_TIDOVRG.
         ASSIGN
         regdatum = overtemp.DATUM
         regdagnamn = overtemp.DAG
         tidalltrec = RECID(overtemp)
         tidalltrec2 = tidalltrec.      
      END.  
      ELSE regdagnamn = "MÅN".             
      vart = "NYA". 
      bdatum = brwbdatum.
      avdatum = brwavdatum. 
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      {AVBGOMD.I}
      RUN ANDOVER.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = FALSE THEN DO:
         BRW_TIDOVRG:HIDDEN = FALSE.
         ENABLE BRW_TIDOVRG WITH FRAME {&FRAME-NAME}.
         FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
         RUN open_UI.      
         FIND FIRST overtemp WHERE overtemp.RECTIDVIS = placerarec NO-ERROR.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(overtemp)).
         RUN lastselectdyn_UI IN brwproc[2].            
      END.
      IF musz = TRUE THEN DO:    
         musz = FALSE.
      END.
   END.
   
   {musarrow.i}
   musz = FALSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvisa_UI WINDOW-1 
PROCEDURE btnvisa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   ASSIGN    
   vaxla = TRUE.
   RAD_TIDSVAL = 1.             
   ASSIGN
   bdatum = brwbdatum      
   avdatum = brwavdatum.
   {AVBGOM.I}
   IF AVAILABLE tidallt THEN DO:
      FIND CURRENT tidallt NO-LOCK NO-ERROR. 
   END.
   {AMERICANEUROPEAN.I}
   RUN TIDSEDL.W.
   {EUROPEANAMERICAN.I}
   {AVBFRAM.I}   
   vaxla = FALSE.
   extrarec = ?.
   IF AVAILABLE tidallt THEN extrarec = RECID(tidallt).   
   APPLY "VALUE-CHANGED" TO RAD_ALLTID IN FRAME {&FRAME-NAME}.
   IF extrarec NE ? THEN DO: 
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].                         
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT tidpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT tidpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DELETE felmeddtemp.
      IF brwavdatum = tillochmeddatum THEN 
      DISABLE BTN_BERED BTN_BORT BTN_BYTA BTN_DELA BTN_KOP BTN_NY  BTN_NYLON BTN_PREG 
      BTN_RES BTN_TJRESA BTN_UPP BTN_UPP BTN_FORDON WITH FRAME {&FRAME-NAME}.
   END.   
   IF allatider = 1 THEN DO:             
      RUN opensok_UI.
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.         
   END.      
   IF allatider = 2 THEN DO:      
      RUN opensok_UI.    
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.       
   END.
   IF allatider = 3 THEN DO:      
      RUN opensok_UI.   
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.        
   END.
   IF allatider = 4 THEN DO:      
      RUN opensok_UI.   
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.
      
   END.
   IF allatider = 5 THEN DO:
      BRW_TIDANDG:SELECT-ROW(1) NO-ERROR.    
      RUN refreshbrw_UI IN brwproc[2].
      APPLY "VALUE-CHANGED" TO BRW_TIDOVRG.      
   END.        
   {musarrow.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvit_UI WINDOW-1 
PROCEDURE btnvit_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buttplace_UI WINDOW-1 
PROCEDURE buttplace_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:collefth = ?.
   IF Guru.Konstanter:tidasekvar[1] = TRUE THEN DO:
      
      Guru.GlobalaVariabler:colrighth = MBTN_TID:HANDLE IN FRAME FRAME-A.           
      Guru.GlobalaVariabler:colrighth:HIDDEN  = FALSE.
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO: 
       
      Guru.GlobalaVariabler:colrighth = MBTN_LON:HANDLE IN FRAME FRAME-A.           
      Guru.GlobalaVariabler:colrighth:HIDDEN  = FALSE.
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   ELSE BTN_NYLON:HIDDEN = TRUE.
   IF Guru.Konstanter:tidasekvar[3] = TRUE THEN DO:
                    
      Guru.GlobalaVariabler:colrighth = MBTN_BER:HANDLE IN FRAME FRAME-A.           
      Guru.GlobalaVariabler:colrighth:HIDDEN  = FALSE.
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:tidasekvar[4] = TRUE THEN DO:
                    
      Guru.GlobalaVariabler:colrighth = MBTN_TRAKT:HANDLE IN FRAME FRAME-A.           
      Guru.GlobalaVariabler:colrighth:HIDDEN  = FALSE.
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF Guru.Konstanter:tidasekvar[5] = TRUE THEN DO:
                 
      Guru.GlobalaVariabler:colrighth = MBTN_OVER:HANDLE .           
      Guru.GlobalaVariabler:colrighth:HIDDEN  = FALSE.
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE colproc_UI WINDOW-1 
PROCEDURE colproc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   tempcolh = BRW_TIDANDG:CURRENT-COLUMN IN FRAME {&FRAME-NAME}.
   colsortlog = TRUE.
   RUN opensok_UI.
   colsortlog = FALSE.
   RUN startsearchproc IN brwproc[1].      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-REGIS FILL-IN-PKOD FILL-IN_FORNAMN-2 FILL-IN-RUBRIK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-MENY MBTN_BER MBTN_LON FBTN_VPERS MBTN_OVER FBTN_NAAVB FBTN_VISA 
         BTN_NY BTN_UPP BTN_BORT BTN_NYALLA BTN_SKAPEGNA MBTN_TID BTN_PREG 
         BTN_DELA BTN_KOP BTN_BYTA BTN_NYLON BTN_RES MBTN_TRAKT BTN_AVB 
         BTN_AVBH 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtegna_UI WINDOW-1 
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
      RUN aokomp_UI (INPUT 1).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmttid_UI WINDOW-1 
PROCEDURE hmttid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vadgora,INPUT pkod,INPUT brwbdatum,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE overtemp,INPUT-OUTPUT TABLE tidallt).
   END.
   ELSE DO:
      RUN TIDHMT.P  
      (INPUT vadgora,INPUT pkod,INPUT brwbdatum,INPUT brwavdatum, 
      INPUT-OUTPUT TABLE overtemp,INPUT-OUTPUT TABLE tidallt).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-1 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN   
   korda = 0
   tidalltrec = 0
   bilforare = FALSE
   brwbdatum = bdatum.
   brwbdatum = DATE(MONTH(bdatum),01,YEAR(bdatum)).   
   brwavdatum = avdatum.    
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   persrec = tidpers.TIDPERSREC.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.   
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = personaltemp.ANSTALLNING
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   IF RAD_ALLVAL = 1 THEN DO:
      FILL-IN-REGIS = "Ansvarig:".
      FIND FIRST ansvarigtemp WHERE ansvarigtemp.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,1,5) NO-LOCK NO-ERROR.
      FILL-IN_FORNAMN = ansvarigtemp.FORNAMN + " " + ansvarigtemp.EFTERNAMN.
      FILL-IN_FORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      DISPLAY FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 2 THEN DO:
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
      FILL-IN_NAMN:HIDDEN = FALSE.
      FILL-IN_NAMN = omrtemp.NAMN.
      DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      FILL-IN-REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      FILL-IN-REGIS = "".
   END. 
   ASSIGN   
   tidallt.PRISTYP:LABEL IN BROWSE BRW_TIDANDG = Guru.Konstanter:gdebk
   BTN_BYTA:LABEL = "Byta " + LC(Guru.Konstanter:gaok).
   IF RAD_ALLVAL = 5 THEN FILL-IN-REGIS = "Markerade enheter". 
   regmnr = MONTH(bdatum).
   RUN MANNAMN.P.
   ASSIGN
   vart = "AND"  
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN      
   FILL-IN-PKOD = personaltemp.PERSONALKOD
   FILL-IN_AR = YEAR(bdatum)
   FILL-IN-MANAD = regmannamn
   FILL-IN_VECKONUMMER = regvnr.        
   RUN hmttid_UI (INPUT 2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontroll_UI WINDOW-1 
PROCEDURE kontroll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   regdatum = brwbdatum.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT tidpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT tidpers.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         IF tillochmeddatum < regdatum THEN DELETE felmeddtemp.         
         ELSE DO:      
            musz = TRUE.
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.         
         END.     
      END.
      /*även för dem som inte har felmeddelande tex kalmar*/
      IF tillochmeddatum < regdatum THEN musz = musz.
      ELSE musz = TRUE.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      DISABLE BTN_BERED BTN_BORT BTN_BYTA BTN_DELA BTN_KOP BTN_NY BTN_NYALLA BTN_NYLON BTN_PREG 
      BTN_RES BTN_TJRESA BTN_UPP BTN_UPP BTN_FORDON WITH FRAME {&FRAME-NAME}.          
      IF tillochmeddatum NE ? AND brwavdatum = tillochmeddatum THEN DISABLE BTN_NY BTN_NYLON BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
      ELSE DO:         
         IF status-stopp = FALSE THEN ENABLE BTN_NY  WITH FRAME {&FRAME-NAME}.                  
         IF RAD_ALLTID = 1 THEN DO:
            IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
               IF status-stopp = FALSE THEN ENABLE BTN_TJRESA WITH FRAME {&FRAME-NAME}.
            END.               
            IF status-stopp = FALSE THEN ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}.            
         END.
         {BUFFTID.I}         
         IF allatider = 2 THEN DO:            
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
               BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.               
               ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.               
            END.      
         END.         
         IF RAD_ALLTID = 3 THEN DO:
            IF status-stopp = FALSE THEN ENABLE BTN_BERED WITH FRAME {&FRAME-NAME}.
         END.
         IF RAD_ALLTID = 1 THEN DO:
            IF status-stopp = FALSE THEN ENABLE BTN_PREG WITH FRAME {&FRAME-NAME}.
            IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
               IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
            END.
            IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
               IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.               
               DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
            END.              
            IF status-stopp = FALSE THEN ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}.
            DISPLAY BTN_FORDON WITH FRAME {&FRAME-NAME}.                       
            IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
            DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.                       
         END.
      END.         
   END.
   IF Guru.Konstanter:varforetypchar[9] = "1" THEN DO:
      entrepejtid = FALSE.
      RUN kollentrep_UI IN otbeordapph (INPUT pkod, OUTPUT entrepejtid).
      IF entrepejtid = TRUE THEN DO:
         DISABLE BTN_BERED BTN_BORT BTN_BYTA BTN_DELA BTN_KOP BTN_NY BTN_NYALLA BTN_NYLON BTN_PREG 
         BTN_RES BTN_TJRESA BTN_UPP BTN_UPP BTN_FORDON WITH FRAME {&FRAME-NAME}.
      END.   
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lonand_UI WINDOW-1 
PROCEDURE lonand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
   tidallt.DATUM >= brwbdatum AND tidallt.DATUM <= brwavdatum AND 
   tidallt.LONTILLAGG NE ''
   USE-INDEX PSTART NO-LOCK NO-ERROR. 
   IF tidsedlog = TRUE THEN DO:
      FBTN_NAAVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      tidsedlog = FALSE.
   END.  
   IF NOT AVAILABLE tidallt THEN DO:
      BRW_TIDANDG:HIDDEN = TRUE.      
      DISABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}.   
      RUN kontroll_UI.
      musz = FALSE.
   END.
   ELSE DO :
      regvnr = tidallt.VECKONUMMER.   
      RUN kontroll_UI.
      musz = FALSE.                    
      tidalltrec = RECID(tidallt).      
      RUN open_UI.
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}. 
      BRW_TIDANDG:HIDDEN = FALSE.      
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
      IF dirtid = TRUE THEN DO:
         dirtid = FALSE.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = dirtidrec NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
            IF tillochmeddatum NE ? THEN DO:
               IF tidallt.DATUM > tillochmeddatum THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               END.
            END.
            ELSE DO:
               IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.                          
            END.
         END.
      END.        
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextguru_UI WINDOW-1 
PROCEDURE nextguru_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytid_UI WINDOW-1 
PROCEDURE nytid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   RUN REGDAG.P.
   RUN REGVEC.P.
   regar = YEAR(regdatum).
   IF allatider = 1 THEN DO:
      vart = "NYA".      
   END.
   IF allatider = 2 THEN DO:
      IF vart = "NYA" THEN DO:                
      END.     
      IF vart = "ANN" THEN DO:         
      END.
   END.
   IF allatider = 3 THEN DO:
   END.
   IF allatider = 4 THEN DO:
      IF vart = "NYA" THEN DO:
      END.
      IF vart = "ANN" THEN DO:      
      END.
   END.
   IF allatider = 5 THEN DO:
      IF vart = "NYA" THEN DO:
      END.
      IF vart = "ANN" THEN DO:      
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opensok_UI WINDOW-1 
PROCEDURE opensok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}     
   IF VALID-HANDLE(tempcolh) = FALSE THEN DO:
      tempcolh = BRW_TIDANDG:GET-BROWSE-COLUMN(1) IN FRAME {&FRAME-NAME}.
   END.    
   /*ANSVARIG*/    
   IF NOT VALID-HANDLE(tempcolh) THEN RETURN.
   CASE tempcolh:NAME:   
      WHEN "DATUM" THEN DO:
         IF allatider = 1 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
         ELSE IF allatider = 2 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
         ELSE IF allatider = 3 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
         ELSE IF allatider = 4 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """"").              
      END.
      WHEN "DAG" THEN DO:
         IF allatider = 1 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
         ELSE IF allatider = 2 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
         ELSE IF allatider = 3 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
         ELSE IF allatider = 4 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """"").           
      END.
      WHEN "START" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").         
      END.
      WHEN "SLUT" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").         
      END.
      WHEN "VILART" THEN DO:            
         IF allatider = 2 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
         ELSE IF allatider = 3 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
         ELSE IF allatider = 4 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """"").          
      END.
      WHEN "BEREDSKAPSTART" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
      END.
      WHEN "BEREDSKAPSLUT" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
      END.
      WHEN "BERANTAL" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
      END.
      WHEN "AONR" THEN DO:            
         IF allatider = 1 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
         ELSE IF allatider = 2 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
         ELSE IF allatider = 3 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
         ELSE IF allatider = 4 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """"").           
      END.
      WHEN "BERBEORD" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
      END.
      WHEN "LONTILLANTAL" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
      END.
      WHEN "LONAUTO" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
         LEAVE.
      END.
      WHEN "TRAKTANTAL" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """""). 
      END.
      WHEN "TRAKTAUTO" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """""). 
      END.
      WHEN "ANVANDARE" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
      END.
      WHEN "TRAKTAMENTE" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
      END.
      WHEN "GODKAND" THEN DO:            
         IF allatider = 1 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
         IF allatider = 2 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.LONTILLAGG NE """"").
         IF allatider = 3 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TYP = ""BER""").
         IF allatider = 4 THEN RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TRAKTKOD NE """""). 
      END.
      WHEN "PRISTYP" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
      END.
      WHEN "PRIS" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
      END.
      WHEN "VIBEFATTNING" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
      END.
      WHEN "TOTALT" THEN DO:
         RUN setcolsortvar_UI IN brwproc[1] (INPUT "tidallt.TIDLOG = TRUE").
      END.
   END CASE.
   IF allatider = 3 THEN RUN sattindex_UI IN brwproc[1] (INPUT "PKOD").
   ELSE RUN sattindex_UI IN brwproc[1] (INPUT "").
   IF colsortlog = FALSE THEN RUN openbdynspec_UI IN brwproc[1].
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opent_UI WINDOW-1 
PROCEDURE opent_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
  RUN hmttid_UI (INPUT 1).
  EMPTY TEMP-TABLE overtemp NO-ERROR.   
  FOR EACH tidallt WHERE tidallt.OKOD1 NE "":
      IF tidallt.OKOD1 NE "" THEN DO:
         RUN over_UI (INPUT 1).     
      END.
      IF tidallt.OKOD2 NE "" THEN DO:
         RUN over_UI (INPUT 2).     
      END.
      IF tidallt.OKOD3 NE "" THEN DO:
         RUN over_UI (INPUT 3).     
      END.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI WINDOW-1 
PROCEDURE open_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   RUN hmttid_UI (INPUT 2).
   IF allatider = 5 THEN DO:
      RUN openbdyn_UI IN brwproc[2] (INPUT "").
      GET FIRST BRW_TIDOVRG NO-LOCK.
      IF AVAILABLE overtemp THEN DO:
         ENABLE BRW_TIDOVRG WITH FRAME {&FRAME-NAME}.
         BRW_TIDOVRG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
      ELSE BRW_TIDOVRG:HIDDEN = TRUE.
      RETURN.
   END.
   ASSIGN
   tidallt.START:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.SLUT:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.TOTALT:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BERANTAL:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.BERBEORD:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.LONTILLANTAL:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.LONAUTO:VISIBLE IN BROWSE BRW_TIDANDG = FALSE          
   tidallt.TRAKTANTAL:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.TRAKTAUTO:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.TRAKTAMENTE:VISIBLE IN BROWSE BRW_TIDANDG = FALSE      
   tidallt.PRISTYP:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.VIBEFATTNING:VISIBLE IN BROWSE BRW_TIDANDG = FALSE
   tidallt.PRIS:VISIBLE IN BROWSE BRW_TIDANDG = FALSE.
   IF allatider = 1 THEN DO:      
      RUN opensok_UI.
      ASSIGN
      tidallt.START:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.SLUT:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.TOTALT:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.TRAKTAMENTE:VISIBLE IN BROWSE BRW_TIDANDG = TRUE      
      tidallt.PRISTYP:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.VIBEFATTNING:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.PRIS:VISIBLE IN BROWSE BRW_TIDANDG = TRUE.     
      tidallt.ANVANDARE:LABEL IN BROWSE BRW_TIDANDG = Guru.Konstanter:gaonamnk.
      ASSIGN
      BRW_TIDANDG:COLUMN = 1.5
      BRW_TIDANDG:WIDTH-CHARS = 108.
   END.
   IF allatider = 2 THEN DO:
      RUN opensok_UI.
      BRW_TIDANDG:COLUMN = 1.5.
      ASSIGN   
      tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.LONTILLANTAL:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.LONAUTO:VISIBLE IN BROWSE BRW_TIDANDG = TRUE.                
      ASSIGN
      BRW_TIDANDG:WIDTH-CHARS = 60     
      BRW_TIDANDG:COLUMN = 22.
      
   END.
   IF allatider = 3 THEN DO:  
      /*BEREDSKAP*/
      RUN opensok_UI.
      BRW_TIDANDG:COLUMN = 1.5.
      ASSIGN
      tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BEREDSKAPSTART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BEREDSKAPSLUT:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BERANTAL:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.BERBEORD:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.ANVANDARE:VISIBLE IN BROWSE BRW_TIDANDG = TRUE.      
      tidallt.ANVANDARE:LABEL IN BROWSE BRW_TIDANDG = "Användare".
      ASSIGN
      BRW_TIDANDG:WIDTH-CHARS = 70
      BRW_TIDANDG:COLUMN = 15.
      /*CCCC*/
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      
      
   END.
   IF allatider = 4 THEN DO:
      RUN opensok_UI.
      BRW_TIDANDG:COLUMN = 1.5.
      ASSIGN
      tidallt.VILART:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.AONR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.DELNR:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.TRAKTANTAL:VISIBLE IN BROWSE BRW_TIDANDG = TRUE
      tidallt.TRAKTAUTO:VISIBLE IN BROWSE BRW_TIDANDG = TRUE.  
      ASSIGN
      BRW_TIDANDG:WIDTH-CHARS = 65
      BRW_TIDANDG:COLUMN = 15.
   END.   
   GET FIRST BRW_TIDANDG NO-LOCK.
   IF AVAILABLE tidallt THEN DO:
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
      BRW_TIDANDG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE BRW_TIDANDG:HIDDEN = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE overand_UI WINDOW-1 
PROCEDURE overand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* RUN opent_UI.      */
   FIND FIRST overtemp NO-LOCK NO-ERROR. 
   IF tidsedlog = TRUE THEN DO:
      FBTN_NAAVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      tidsedlog = FALSE.
   END.  
   IF NOT AVAILABLE overtemp THEN DO:      
      BRW_TIDOVRG:HIDDEN = TRUE.      
      DISABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}. 
      RUN kontroll_UI.
      musz = FALSE.
   END.
   ELSE DO :           
      regvnr = overtemp.VECKONUMMER.   
      RUN kontroll_UI.
      musz = FALSE.   
      FIND FIRST overtemp USE-INDEX PSTART NO-LOCK NO-ERROR.
      tidalltrec = RECID(overtemp).  
      RUN open_UI.
      ENABLE BRW_TIDOVRG WITH FRAME {&FRAME-NAME}.          
      BRW_TIDOVRG:HIDDEN = FALSE.      
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF dirtid = TRUE THEN DO:
         dirtid = FALSE.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = dirtidrec NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
            IF tillochmeddatum NE ? THEN DO:
               IF tidallt.DATUM > tillochmeddatum THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               END.
            END.
            ELSE DO:
               IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.                          
            END.
         END.         
      END.
                 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vadok AS INTEGER NO-UNDO.
   CREATE overtemp.
   ASSIGN 
   overtemp.AONR = tidallt.AONR 
   overtemp.DAG = tidallt.DAG 
   overtemp.DELNR = tidallt.DELNR 
   overtemp.OVERANTAL = tidallt.OANT1 
   overtemp.OVERAUTO = tidallt.OVERAUTO 
   overtemp.DATUM = tidallt.DATUM
   overtemp.GODKAND = tidallt.GODKAND
   overtemp.VECKONUMMER = tidallt.VECKONUMMER
   overtemp.RECTIDVIS = tidallt.RECTIDVIS.
   IF vadok = 1 THEN overtemp.OVERTIDTILL = tidallt.OKOD1.
   IF vadok = 2 THEN overtemp.OVERTIDTILL = tidallt.OKOD2.
   IF vadok = 3 THEN overtemp.OVERTIDTILL = tidallt.OKOD3.
   FIND FIRST overkodtemp WHERE overkodtemp.KOD = ansttemp.KOD AND
   overkodtemp.OVERTIDTILL = overtemp.OVERTIDTILL NO-LOCK NO-ERROR.                  
   IF AVAILABLE overkodtemp THEN overtemp.VILART = overkodtemp.VILART.
   ELSE overtemp.VILART = overtemp.OVERTIDTILL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE radandtid_UI WINDOW-1 
PROCEDURE radandtid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   IF AVAILABLE tidallt THEN DO:
      ASSIGN
      nydatum = tidallt.DATUM 
      nyaonr = tidallt.AONR
      nydelnr = tidallt.DELNR.
   END.
   ASSIGN   
   BRW_TIDANDG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   BRW_TIDOVRG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_BYTA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_DELA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_KOP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_PREG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_NYLON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_RES:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_TJRESA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_FORDON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_BERED:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_FABEF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF allatider = 1 THEN DO:
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN.
         ELSE DO:      
            IF status-stopp = FALSE THEN ENABLE FBTN_FABEF WITH FRAME {&FRAME-NAME}.
            DISPLAY FBTN_FABEF WITH FRAME {&FRAME-NAME}.
         END.   
      END.   
      IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT  WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT  WITH FRAME {&FRAME-NAME}.
      {BUFFTID.I}      
      IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
      END.
          
      IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.         
         DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
      END.      
      
      ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_FORDON WITH FRAME {&FRAME-NAME}.
         
      IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.           
      ASSIGN
      FILL-IN-INGA = "Det finns inga tidregistreringar"      
      WINDOW-1:TITLE = "Registrera / ändra tidregistrering"      
      FILL-IN-RUBRIK = "Registrera / ändra tidregistrering för:".           
      RUN tidand_UI.
      RUN bilkoll_UI IN aonrapph (INPUT pkod,OUTPUT pbkollsv).
      IF pbkollsv = FALSE THEN BTN_FORDON:HIDDEN = TRUE.
   END.
   IF allatider = 2 THEN DO:
      IF Guru.Konstanter:globanv = "CELPAO" THEN DO:
         BTN_NYALLA:LABEL IN FRAME {&FRAME-NAME} = "  Personligt~nutlägg mall".
         BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         IF status-stopp = FALSE THEN ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.
      END.      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         BTN_NYALLA:LABEL IN FRAME {&FRAME-NAME} = "  Personligt~nutlägg mall".
         BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.         
         ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.
      END.
      IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         BTN_NYALLA:LABEL IN FRAME {&FRAME-NAME} = "  Utlägg~nlöneavdrag mall".
         BTN_NYALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.         
         ENABLE BTN_NYALLA WITH FRAME {&FRAME-NAME}.
      END.
      IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.      
      ASSIGN
      FILL-IN-INGA = "Det finns inga lönetillägg"
      WINDOW-1:TITLE = "Ändra eller registrera lönetillägg"
      FILL-IN-RUBRIK = "Ändra eller registrera lönetillägg för:".      
      RUN lonand_UI.
   END.
   IF allatider = 3 THEN DO:      
      IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BERED BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_NY BTN_UPP BTN_BERED BTN_BORT WITH FRAME {&FRAME-NAME}.      
      ASSIGN
      FILL-IN-INGA = "Det finns ingen beredskap"
      WINDOW-1:TITLE = "Ändra eller registrera beredskap"
      FILL-IN-RUBRIK = "Ändra eller registrera beredskap för:".      
      RUN berand_UI.
   END.
   IF allatider = 4 THEN DO:
      IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-INGA = "Det finns inga traktamenten"
      WINDOW-1:TITLE = "Ändra eller registrera traktamente"
      FILL-IN-RUBRIK = "Ändra eller registrera traktamente för:".      
      RUN traand_UI.
   END. 
   IF allatider = 5 THEN DO:
      IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-INGA = "Det finns inga övertidstillägg"
      WINDOW-1:TITLE = "Ändra eller registrera övertidstillägg"
      FILL-IN-RUBRIK = "Ändra eller registrera övertidstillägg för:".      
      RUN overand_UI.       
   END.
   DISPLAY FILL-IN-RUBRIK  WITH FRAME {&FRAME-NAME}.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resstart_UI WINDOW-1 
PROCEDURE resstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   {muswait.i}
   {AVBGOMD.I}   
   RUN RESANDAW.W (INPUT pkod,INPUT TABLE extratidallt).
   {AVBFRAMD.I}         
   IF musz = FALSE THEN DO:
      BRW_TIDANDG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.        
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}.
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      
      IF status-stopp = FALSE THEN ENABLE BTN_BORT BTN_UPP BTN_DELA BTN_BYTA WITH FRAME {&FRAME-NAME}.       
      IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
      END.     
      IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
         IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
         DISPLAY BTN_RES WITH FRAME {&FRAME-NAME}.
      END.      
      IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
      DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.           
   END.
   {musarrow.i}
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   RUN open_UI.
   FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
   tidallt.TIDLOG = TRUE NO-ERROR.
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
   RUN lastselectdyn_UI IN brwproc[1].                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sek_UI WINDOW-1 
PROCEDURE sek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ENABLE BTN_PREG WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:collefth = BTN_PREG:HANDLE.   
   ENABLE BTN_DELA WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = BTN_DELA:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
   ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = BTN_KOP:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
   ENABLE BTN_BYTA WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = BTN_BYTA:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   
   IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
      ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = BTN_NYLON:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.         
   IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
      ENABLE BTN_RES WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = BTN_RES:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      ENABLE BTN_TJRESA WITH FRAME {&FRAME-NAME}. 
      Guru.GlobalaVariabler:colrighth = BTN_TJRESA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
   END.         
   ENABLE BTN_FORDON WITH FRAME {&FRAME-NAME}. 
   Guru.GlobalaVariabler:colrighth = BTN_FORDON:HANDLE.           
   RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE starthelp_UI WINDOW-1 
PROCEDURE starthelp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN_FORNAMN-2 = tidpers.FORNAMN + " " + tidpers.EFTERNAMN.  
   FILL-IN-PKOD = tidpers.PERSONALKOD.
   RUN radandtid_UI.           
   DISPLAY FILL-IN_FORNAMN-2 FILL-IN-PKOD 
   WITH FRAME {&FRAME-NAME}.  
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 1,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.

   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF Guru.Konstanter:globanv = lasanv THEN DO:    
         MESSAGE felmeddtemp.FELMEDD  VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL UPDATE val4 AS LOGICAL.
         DELETE felmeddtemp.
         CASE val4:                  
            WHEN TRUE THEN DO:           
               RUN lasupptid_UI IN persapph (INPUT  tidpers.PERSONALKOD, OUTPUT ptraff).
               IF ptraff = TRUE THEN DO:
                  MESSAGE tidpers.PERSONALKOD + " är nu upplåst!."
                  VIEW-AS ALERT-BOX.
               END.                         
            END.                  
         END CASE.
      END.   
      ELSE DO:
         status-stopp = TRUE.
         DISABLE BTN_NY BTN_NYALLA BTN_NYLON BTN_RES BTN_TJRESA BTN_UPP 
                  BTN_DELA BTN_KOP BTN_BYTA BTN_BORT BTN_BERED BTN_PREG BTN_UPP BTN_FORDON BTN_SKAPEGNA
         WITH FRAME {&FRAME-NAME}.
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.     
      END.
   END.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN PERBEFPRIS2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT pkod,INPUT brwbdatum,OUTPUT TABLE perspristemp,OUTPUT TABLE befvaltemp).   
   END.
   ELSE DO:
      RUN PERBEFPRIS2.P 
      (INPUT pkod,INPUT brwbdatum,OUTPUT TABLE perspristemp,OUTPUT TABLE befvaltemp).   
   END. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidand_UI WINDOW-1 
PROCEDURE tidand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   
   ASSIGN
   BTN_BYTA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   BTN_DELA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   FIND LAST tidallt WHERE 
   tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
   tidallt.DATUM >= brwbdatum AND tidallt.DATUM <= brwavdatum AND 
   tidallt.TIDLOG = TRUE
   USE-INDEX PSTART NO-LOCK NO-ERROR. 
   IF tidsedlog = TRUE THEN DO:
      FBTN_NAAVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      tidsedlog = FALSE.
   END.   
   IF flexav = TRUE THEN DO:   /*obs lena flex*/
      FBTN_NAAVB:HIDDEN = TRUE. 
      FBTN_VPERS:HIDDEN = TRUE. 
   END.     
   IF NOT AVAILABLE tidallt THEN DO:
      BRW_TIDANDG:HIDDEN = TRUE.      
      DISABLE BTN_BORT BTN_UPP BTN_DELA BTN_KOP BTN_BYTA BTN_NYLON BTN_RES BTN_FORDON WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}.
      RUN kontroll_UI.      
   END.
   ELSE DO :            
      ASSIGN
      regvnr = tidallt.VECKONUMMER.
      RUN kontroll_UI.
      tidalltrec = RECID(tidallt).
      RUN open_UI.      
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}. 
      BRW_TIDANDG:HIDDEN = FALSE.                       
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF dirtid = TRUE THEN DO:
         dirtid = FALSE.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = dirtidrec NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
            IF tillochmeddatum NE ? THEN DO:
               IF tidallt.DATUM > tillochmeddatum THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
                  {BUFFTID.I}
                  IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
                     IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
                     DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
                  END.            
                  IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
                     IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
                     DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
                  END.                  
                  IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.                                
               END.
            END.
            ELSE DO:
               IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
               {BUFFTID.I}
               IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
               END.
               IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
               END.               
               IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.                             
            END.
         END.
      END.
      ELSE DO:
         GET LAST BRW_TIDANDG NO-LOCK.
         IF AVAILABLE tidallt THEN DO:
            regvnr = tidallt.VECKONUMMER.
            tidalltrec = RECID(tidallt).
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
            IF tillochmeddatum NE ? THEN DO:
               IF tidallt.DATUM > tillochmeddatum THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA  BTN_BORT WITH FRAME {&FRAME-NAME}.
                  {BUFFTID.I}
                  IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
                     IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
                     DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
                  END.
                  IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
                     IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
                     DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
                  END.                  
                  IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.                                   
               END.
            END.
            ELSE DO:
               IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA BTN_BORT WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_NY BTN_PREG BTN_UPP BTN_DELA BTN_BYTA  BTN_BORT WITH FRAME {&FRAME-NAME}.
               {BUFFTID.I}
               IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NYLON WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NYLON WITH FRAME {&FRAME-NAME}.
               END.
               IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_RES BTN_TJRESA WITH FRAME {&FRAME-NAME}.
               END.               
               IF status-stopp = FALSE THEN ENABLE BTN_KOP WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_KOP WITH FRAME {&FRAME-NAME}.                             
            END.   
         END.
      END.   
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidbuff_UI WINDOW-1 
PROCEDURE tidbuff_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*LENA  10/14/2002 kopiera alla fält om ändring*/
   IF vart = "AND" THEN DO:
      BUFFER-COPY tidallt 
      TO extratidallt.
   END.
   ELSE IF vart = "DEL" THEN DO:
      BUFFER-COPY tidallt 
      TO extratidallt.
   END.
   ELSE DO:
      BUFFER-COPY tidallt 
      EXCEPT 
      tidallt.PRIS
      tidallt.PRISTYP
      tidallt.TRAKTAMENTE
      tidallt.TRAKTANTAL
      tidallt.TRAKTAUTO
      tidallt.TRAKTKOD
      tidallt.OKOD1
      tidallt.OKOD2
      tidallt.OKOD3
      tidallt.OANT1
      tidallt.OANT2
      tidallt.OANT3
      tidallt.OST1
      tidallt.OST2
      tidallt.OST3
      tidallt.OSL1
      tidallt.OSL2
      tidallt.OSL3
      tidallt.TOTALT
      tidallt.UTRYCKNING 
      tidallt.LAGBAS 
      tidallt.ENFLERDAGS 
      tidallt.RESMAL 
      tidallt.LONAUTO 
      tidallt.LONTILLAGG 
      tidallt.LONTILLANTAL 
      tidallt.VECKOKORD         
      tidallt.GODKAND 
      TO extratidallt.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE traand_UI WINDOW-1 
PROCEDURE traand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST tidallt WHERE tidallt.PERSONALKOD = personaltemp.PERSONALKOD AND
   tidallt.DATUM >= brwbdatum AND tidallt.DATUM <= brwavdatum AND 
   tidallt.TRAKTKOD NE ''
   USE-INDEX PSTART NO-LOCK NO-ERROR.
   IF tidsedlog = TRUE THEN DO:
      FBTN_NAAVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
      tidsedlog = FALSE.
   END.   
   IF NOT AVAILABLE tidallt THEN DO:      
      BRW_TIDANDG:HIDDEN = TRUE.
      DISABLE BTN_BORT BTN_UPP WITH FRAME {&FRAME-NAME}. 
      DISPLAY FILL-IN-INGA WITH FRAME {&FRAME-NAME}.
      RUN kontroll_UI.
      musz = FALSE.
   END.
   ELSE DO :
      regvnr = tidallt.VECKONUMMER.   
      RUN kontroll_UI.
      musz = FALSE.           
      tidalltrec = RECID(tidallt).
      RUN open_UI.    
      ENABLE BRW_TIDANDG WITH FRAME {&FRAME-NAME}. 
      BRW_TIDANDG:HIDDEN = FALSE.      
      FILL-IN-INGA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      
      IF dirtid = TRUE THEN DO:
         dirtid = FALSE.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = dirtidrec NO-ERROR.
         IF AVAILABLE tidallt THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
            RUN lastselectdyn_UI IN brwproc[1].                         
            IF tillochmeddatum NE ? THEN DO:
               IF tidallt.DATUM > tillochmeddatum THEN DO:
                  IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
                  DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               END.
            END.
            ELSE DO:
               IF status-stopp = FALSE THEN ENABLE BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.
               DISPLAY BTN_NY BTN_UPP BTN_BORT WITH FRAME {&FRAME-NAME}.                          
            END.
         END.      
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upptid_UI WINDOW-1 
PROCEDURE upptid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF allatider = 1 THEN DO:
      antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.     
      IF antal_valda = 0 THEN DO:      
         MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
         musz = TRUE.
         RETURN.                
      END.
      tidalltrec = RECID(tidallt).    
      IF tidallt.PRISTYP = "RESTID..." AND tidallt.ENFLERDAGS BEGINS "Flerdag" THEN DO:
         MESSAGE "Detta är en restidsregistrering för en flerdygnförrättning." SKIP
         "Om du bara ska byta " + LC(Guru.Konstanter:gaok) + " använd knappen Byta " + LC(Guru.Konstanter:gaok) + "." SKIP         
         "Annars - ta bort din tjänsteresa och gör om tjänsterese-registreringen." VIEW-AS ALERT-BOX.
         RETURN.
      END.
      ELSE IF tidallt.PRISTYP = "RESTID..." AND tidallt.ENFLERDAGS = "Endag" THEN DO:
         IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
            vart = "AND".
            RUN valdny_UI (INPUT TRUE).            
            RUN resstart_UI.
            RETURN.         
         END.   
         ELSE DO:
            MESSAGE "Detta är en restidsregistrering." SKIP
            "Om du bara ska byta " + LC(Guru.Konstanter:gaok) + " använd knappen Byta " + LC(Guru.Konstanter:gaok) + "." SKIP         
            "Annars - ta bort din tjänsteresa och gör om tjänsterese-registreringen." VIEW-AS ALERT-BOX.
         END.
      END.      
      ASSIGN
      regdatum = tidallt.DATUM
      vart = "AND".
     
      RUN valdny_UI (INPUT TRUE).       
      {AVBGOMD.I}
      /*IF Guru.Konstanter:globforetag = "elpa" THEN RUN ANDTIDA.W (INPUT pkod,INPUT TABLE extratidallt).*/
      RUN ANDTID.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = TRUE THEN RETURN.
      RUN open_UI.
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND 
      tidallt.TIDLOG = TRUE NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.
   IF allatider = 2 THEN DO:
      status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.     
      ASSIGN
      tidalltrec = RECID(tidallt)
      tidalltrec2 = tidalltrec   
      vart = "AND"
      bdatum = brwbdatum
      avdatum = brwavdatum.
      IF tidallt.LONAUTO = TRUE THEN vart = "ANN".
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      {AVBGOMD.I}
      RUN ANDLON.W (INPUT 1 , INPUT 1,INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.
      
      RUN open_UI.     
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND 
      tidallt.LONTILLAGG NE "" NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   IF allatider = 3 THEN DO:
      IF  Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "CSNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         IF Guru.Konstanter:globniv = 0 OR Guru.Konstanter:globniv = 1 OR Guru.Konstanter:globniv = 30  THEN musz = musz.
         ELSE DO:      
            MESSAGE "Använd istället Ta bort och Registrera beredskap"
            VIEW-AS ALERT-BOX.         
            RETURN.
         END.
      END.
      
      status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.        
      ASSIGN
      tidalltrec = RECID(tidallt)
      tidalltrec2 = tidalltrec   
      vart = "AND"
      bdatum = brwbdatum
      avdatum = brwavdatum.
      RUN valdny_UI (INPUT TRUE).
      {AVBGOMD.I}
      RUN ANDBER.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = TRUE THEN RETURN.
      RUN open_UI.
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].
      ASSIGN
      bdatum = brwbdatum
      avdatum = brwavdatum.   
   END.
   IF allatider = 4 THEN DO:
      status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_TIDANDG.      
      ASSIGN
      tidalltrec = RECID(tidallt)
      tidalltrec2 = tidalltrec   
      vart = "AND"
      bdatum = brwbdatum
      avdatum = brwavdatum.
      IF tidallt.TRAKTAUTO = TRUE THEN vart = "ANN".
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      {AVBGOMD.I}
      RUN ANDTRAKT.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.
      RUN open_UI.     
      FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
      tidallt.TRAKTKOD NE "" 
      NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
      RUN lastselectdyn_UI IN brwproc[1].            
      ASSIGN
      bdatum = brwbdatum
      avdatum = brwavdatum.   
   END.
   IF allatider = 5 THEN DO:
      status-ok = BRW_TIDOVRG:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_TIDOVRG.      
      ASSIGN
      tidalltrec = RECID(overtemp)
      tidalltrec2 = tidalltrec     
      vart = "AND"
      bdatum = brwbdatum
      avdatum = brwavdatum.
      IF overtemp.OVERAUTO = TRUE THEN vart = "ANN".
      RUN valdny_UI (INPUT TRUE).
      RUN nytid_UI.
      {AVBGOMD.I}
      RUN ANDOVER.W (INPUT pkod,INPUT TABLE extratidallt).
      {AVBFRAMD.I}
      IF musz = TRUE THEN DO :
         musz = FALSE.
         RETURN.                    
      END.
      RUN open_UI. 
      FIND FIRST overtemp WHERE overtemp.RECTIDVIS = placerarec NO-ERROR.
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(overtemp)).
      RUN lastselectdyn_UI IN brwproc[2].
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdabort_UI WINDOW-1 
PROCEDURE valdabort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF allatider = 5 THEN DO:
      antal_valda = BRW_TIDOVRG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.   
      IF antal_valda = 0 THEN DO:      
         MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
         musz = TRUE.
         RETURN.                
      END.
   END.
   ELSE DO:
      antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.   
      IF antal_valda = 0 THEN DO:      
         MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
         musz = TRUE.
         RETURN.                
      END.
   END.
   {muswait.i}
   {AVBGOMD.I} 
   IF allatider = 5 THEN DO:
      ASSIGN       
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :         
         status-ok = BRW_TIDOVRG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME} NO-ERROR.                
         tidalltrec = RECID(overtemp).
         tidalltrec2 = tidalltrec.         
         musz = FALSE.
         IF overtemp.GODKAND NE "" THEN DO:
            musz = musz.
         END.
         ELSE DO:
            IF overtemp.OVERAUTO = TRUE THEN DO:
               MESSAGE 
               "Det går inte att ta bort ett automatiskt övertidstillägg. Sätt antal lika med 0."
               VIEW-AS ALERT-BOX.
               musz = TRUE.
            END.
            ELSE DO:    
               placerarec = overtemp.RECTIDVIS.
               RUN BORTOVER.W (INPUT tidalltrec,INPUT overtemp.PERSONALKOD).     
            END.           
         END.         
         IF musz = FALSE AND openqtid = FALSE THEN openqtid = TRUE.                
         musz = FALSE.
         antal_raknare = antal_raknare + 1.
      END.  
      status-ok = BRW_TIDOVRG:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.                             
   END.
   ELSE DO:          
      ASSIGN       
      antal_raknare = 1.      
      DO WHILE antal_raknare LE antal_valda :         
         status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME} NO-ERROR.                
         tidalltrec = RECID(tidallt).
         tidalltrec2 = tidalltrec.         
         musz = FALSE.
         IF tidallt.GODKAND NE "" THEN DO:
            musz = musz.
         END.
         ELSE IF tidallt.PRISTYP = "RESTID..." AND tidallt.ENFLERDAGS = "FLERDAG" THEN DO:
            MESSAGE "Flerdygnsförättning tas bort i restids rutinen!"
            VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
            IF allatider = 1 THEN DO:
               placerarec = tidallt.RECTIDVIS.
               RUN BORTTID.W (INPUT tidalltrec,INPUT tidallt.PERSONALKOD).
            END.
            IF allatider = 2 THEN DO:
               IF tidallt.LONAUTO = TRUE THEN DO:
                  MESSAGE "Det går inte att ta bort ett automatiskt lönnetillägg. Sätt antal lika med 0."
                  VIEW-AS ALERT-BOX.                 
                  musz = TRUE.
               END.
               ELSE DO:
                  placerarec = tidallt.RECTIDVIS.
                  RUN BORTLON.W (INPUT tidalltrec,INPUT tidallt.PERSONALKOD).                  
               END.
            END.
            IF allatider = 3 THEN DO:
               placerarec = tidallt.RECTIDVIS.
               RUN BORTBER.W (INPUT tidalltrec,INPUT tidallt.PERSONALKOD).
               antal_raknare = antal_valda.
            END.
            IF allatider = 4 THEN DO:
               IF tidallt.TRAKTAUTO = TRUE THEN DO:
                  MESSAGE "Det går inte att ta bort ett automatiskt traktamente. Sätt antal lika med 0."
                  VIEW-AS ALERT-BOX.
                  musz = TRUE.
               END.
               ELSE DO:               
                  placerarec = tidallt.RECTIDVIS.
                  RUN BORTTRAK.W (INPUT tidalltrec,INPUT tidallt.PERSONALKOD).     
               END.
            END. 
         END.         
         IF musz = FALSE AND openqtid = FALSE THEN DO:
            ASSIGN
            openqtid = TRUE.                
         END.
         musz = FALSE.
         antal_raknare = antal_raknare + 1.
      END.  
      status-ok = BRW_TIDANDG:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.                        
     
   END.
   {AVBFRAMD.I}
   {musarrow.i}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdbyt_UI WINDOW-1 
PROCEDURE valdbyt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   antal_valda = BRW_TIDANDG:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen tidregistrering är markerad." VIEW-AS ALERT-BOX.    
      musz = TRUE.
      RETURN.                
   END.
   ELSE DO:  
      antal_raknare = 1.
      avdatum = brwbdatum.
      DO WHILE antal_raknare LE antal_valda :         
         status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.         
         IF avdatum < tidallt.DATUM THEN avdatum = tidallt.DATUM.
         antal_raknare = antal_raknare + 1.
      END.          
      ASSIGN 
      varaonr = tidallt.AONR 
      vardelnr = tidallt.DELNR.
      debitering = 1.      
      reov = FALSE.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :         
         status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.                             
         tidalltrec = RECID(tidallt).
         IF debitering = 1 THEN DO:       
            IF tidallt.LONTILLAGG NE "" THEN ASSIGN reov = TRUE.
           /* semtillägget plockas bara bort på sista dagen felaktigt*/
           IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
               IF tidallt.AONR = "150" AND tidallt.LONTILLAGG = "5089" THEN reov = FALSE.
               IF tidallt.AONR = "171" AND tidallt.LONTILLAGG = "4574" THEN reov = FALSE.
            END.
            IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:               
               IF tidallt.AONR = "171" AND tidallt.LONTILLAGG = "4574" THEN reov = FALSE.
            END.
            IF tidallt.OKOD1 NE "" THEN ASSIGN reov = TRUE.
            IF tidallt.OKOD2 NE "" THEN ASSIGN reov = TRUE.
            IF tidallt.OKOD3 NE "" THEN ASSIGN reov = TRUE.            
         END.                                
         antal_raknare = antal_raknare + 1.
      END.
      
      {AVBGOMD.I}      
      RUN BYTAAONR.W (INPUT pkod).            
      {AVBFRAMD.I}
      {muswait.i}
      IF musz = FALSE THEN DO:
         antal_raknare = 1.         
         EMPTY TEMP-TABLE ebyttemp NO-ERROR.
         DO WHILE antal_raknare LE antal_valda :         
            status-ok = BRW_TIDANDG:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.                                
            tidalltrec = RECID(tidallt).
            IF Guru.Konstanter:globforetag = "sund"  AND tidallt.AONR = "155" THEN antal_raknare = antal_raknare.
            ELSE IF Guru.Konstanter:globforetag = "SNAT" AND tidallt.AONR = "155" THEN antal_raknare = antal_raknare.
            ELSE IF Guru.Konstanter:globforetag = "MISV" AND tidallt.AONR = "155" THEN antal_raknare = antal_raknare.
            ELSE IF Guru.Konstanter:globforetag = "elpa" AND tidallt.AONR = "155" THEN antal_raknare = antal_raknare.
            ELSE DO:                                
               CREATE ebyttemp.           
               ASSIGN 
               ebyttemp.DATUM         = tidallt.DATUM  
               ebyttemp.START         = tidallt.START
               ebyttemp.VADGORA       = debitering
               ebyttemp.BEFATTNING    = varfabef
               ebyttemp.PRIS          = varpris   
               ebyttemp.PRISTYP       = varpristyp
               ebyttemp.UTRYCKNING    = varutryck 
               ebyttemp.AONR          = varaonr   
               ebyttemp.DELNR         = vardelnr  
               ebyttemp.TRAKTAMENTE   = vartrakt  
               ebyttemp.ANVANDARE     = Guru.Konstanter:globanv   
               ebyttemp.RECTIDVIS     = tidallt.RECTIDVIS.                                                          
            END.
            antal_raknare = antal_raknare + 1.
         END.
         FOR EACH ebyttemp BY ebyttemp.DATUM BY ebyttemp.START:
            EMPTY TEMP-TABLE byttemp NO-ERROR.
            CREATE byttemp.
            BUFFER-COPY ebyttemp TO byttemp.
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN BYTTOLKW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT TABLE byttemp,OUTPUT placerarec).
            END.
            ELSE DO:
               RUN BYTTOLKW.P 
               (INPUT TABLE byttemp,OUTPUT placerarec).
            END.
         END.
         EMPTY TEMP-TABLE byttemp NO-ERROR.
         EMPTY TEMP-TABLE ebyttemp NO-ERROR.
         RUN open_UI.
         FIND FIRST tidallt WHERE tidallt.RECTIDVIS = placerarec AND
         tidallt.TIDLOG = TRUE NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidallt)).
         RUN lastselectdyn_UI IN brwproc[1].            
      END.
      avdatum = brwavdatum.
   END.    
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdny_UI WINDOW-1 
PROCEDURE valdny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tabort AS LOGICAL NO-UNDO.
   antal_valda = 0. 
   IF tabort = TRUE THEN DO:
      EMPTY TEMP-TABLE extratidallt NO-ERROR.       
   END.
   CREATE extratidallt.
   IF allatider = 1 THEN DO:
      /*Tidregistreringar*/
      IF AVAILABLE tidallt THEN DO:
         ASSIGN
         varaonr = tidallt.AONR
         vardelnr = tidallt.DELNR
         regdatum = tidallt.DATUM
         varpristyp = tidallt.PRISTYP
         varpris = tidallt.PRIS
         varslut = tidallt.SLUT
         vartrakt = tidallt.TRAKTAMENTE
         regdagnamn = tidallt.DAG
         tidalltrec = RECID(tidallt)
         tidalltrec2 = tidalltrec.
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = tidallt.AONR AND 
         utsokaonr.DELNR = tidallt.DELNR NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN
            varpristyp = utsokaonr.PRISTYP
            varutryck = utsokaonr.UTRYCKNING.
         END.           
         IF tillochmeddatum NE ? THEN DO:
            IF tillochmeddatum >= regdatum THEN DO:
               regdatum = tillochmeddatum + 1.      
            END.         
         END.        
         RUN tidbuff_UI.
         ASSIGN
         extratidallt.VECKOKORD = ""
         extratidallt.GODKAND = "".
      END.  
      ELSE DO:
         regdagnamn = "MÅN".     
         varpristyp = "TOT.PRIS.".
         ASSIGN
         extratidallt.PRISTYP = varpristyp
         extratidallt.DAG = regdagnamn
         extratidallt.DATUM = regdatum.      
      END.
      IF vart = "NYA" THEN DO:
         extratidallt.RECTIDVIS = ?.
      END.
      extratidallt.TIDLOG = TRUE.
   END.
   IF allatider = 2 THEN DO:
      /*Lönetillägg*/
      IF vart = "ANN" THEN DO:
         /*AUTO*/
         RUN tidbuff_UI.
         ASSIGN
         extratidallt.LONAUTO     = tidallt.LONAUTO 
         extratidallt.LONTILLAGG  = tidallt.LONTILLAGG 
         extratidallt.LONTILLANTAL = tidallt.LONTILLANTAL 
         extratidallt.ENFLERDAGS     = tidallt.ENFLERDAGS 
         extratidallt.PRISTYP = "".
      END.
      ELSE DO:
         IF AVAILABLE tidallt THEN DO:
            ASSIGN
            varaonr = tidallt.AONR 
            vardelnr = tidallt.DELNR
            varlon = tidallt.LONTILLAGG
            varlantal = tidallt.TOTALT.
            RUN tidbuff_UI.
            ASSIGN
            extratidallt.LONAUTO     = tidallt.LONAUTO 
            extratidallt.LONTILLAGG  = tidallt.LONTILLAGG 
            extratidallt.LONTILLANTAL = tidallt.LONTILLANTAL 
            extratidallt.PRISTYP = "".
            ASSIGN
            extratidallt.VECKOKORD = ""
            extratidallt.GODKAND = "".
            IF tidallt.AONR = "" THEN DO:
               ASSIGN  
               extratidallt.AONR = nyaonr
               extratidallt.DELNR = nydelnr.           
            END.         
         END.
         ELSE DO:
            regdagnamn = "MÅN".     
            ASSIGN
            extratidallt.DAG = regdagnamn
            extratidallt.DATUM = regdatum.      
         END.
      END.
      IF vart = "NYA" THEN DO:
         ASSIGN
         extratidallt.LONTILLANTAL = 0
         extratidallt.RECTIDVIS = ?
         extratidallt.TIDLOG = FALSE
         extratidallt.START = 7
         extratidallt.SLUT = 7.
      END.
   END.
   IF allatider = 3 THEN DO:
      /*Beredskap*/
      IF AVAILABLE tidallt THEN DO:
         status-ok = BRW_TIDANDG:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         APPLY "VALUE-CHANGED" TO BRW_TIDANDG.
         ASSIGN 
         regdatum = tidallt.DATUM
         regvnr = tidallt.VECKONUMMER
         regdagnamn = tidallt.DAG.      
         ASSIGN
         tidalltrec = RECID(tidallt)
         tidalltrec2 = tidalltrec. 
         RUN tidbuff_UI.
         ASSIGN
         extratidallt.PRISTYP = ""
         extratidallt.VECKOKORD = ""
         extratidallt.GODKAND = "".
      END.
      ELSE DO:
         regdagnamn = "MÅN".     
         ASSIGN
         extratidallt.TIDLOG = FALSE
         extratidallt.DAG = regdagnamn
         extratidallt.DATUM = regdatum
         extratidallt.VECKONUMMER = regvnr
         extratidallt.START = 7.00 
         extratidallt.SLUT = 7.00 
         /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
         SUBSTRING(extratidallt.PROGRAM,1,158) = "BERAND" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv         
         extratidallt.DATUM = regdatum
         extratidallt.DAG = regdagnamn.
      END.     
      IF vart = "NYA" THEN DO:
         ASSIGN
         extratidallt.RECTIDVIS = ?
         extratidallt.TIDLOG = FALSE
         extratidallt.START = 7
         extratidallt.SLUT = 7.
      END.
   END.
   IF allatider = 4 THEN DO:
      /*Traktamente*/
      IF vart = "ANN" THEN DO:
         /*AUTO*/
         RUN tidbuff_UI.
         extratidallt.PRISTYP = "".
      END.
      ELSE DO:
         IF AVAILABLE tidallt THEN DO:
            ASSIGN
            varaonr = tidallt.AONR 
            vardelnr = tidallt.DELNR.
            RUN tidbuff_UI.
            extratidallt.PRISTYP = "".
            ASSIGN
            extratidallt.VECKOKORD = ""
            extratidallt.GODKAND = "".
            IF tidallt.AONR = "" THEN DO:
               ASSIGN  
               extratidallt.AONR = nyaonr
               extratidallt.DELNR = nydelnr.           
            END.         
         END.
         ELSE DO:
            regdagnamn = "MÅN".     
            ASSIGN
            extratidallt.DAG = regdagnamn
            extratidallt.DATUM = regdatum.      
         END.
      END.
      IF vart = "NYA" THEN DO:
         ASSIGN
         extratidallt.TRAKTANTAL = 0
         extratidallt.RECTIDVIS = ?
         extratidallt.TIDLOG = FALSE
         extratidallt.START = 7
         extratidallt.SLUT = 7.
      END.
   END.  
   IF allatider = 5 THEN DO:
      /*Övertidstillägg*/ 
      gamlakoden = "".
      IF vart = "ANN" THEN DO:
         /*AUTO*/
         BUFFER-COPY overtemp TO extratidallt.               
         extratidallt.PRISTYP = "".
         gamlakoden = overtemp.OVERTIDTILL.
      END.
      ELSE DO:
         IF AVAILABLE overtemp THEN DO:
            gamlakoden = overtemp.OVERTIDTILL.
            ASSIGN
            varaonr = overtemp.AONR 
            vardelnr = overtemp.DELNR.
            BUFFER-COPY overtemp TO extratidallt.      
            extratidallt.PRISTYP = "".
            ASSIGN
            extratidallt.VECKOKORD = ""
            extratidallt.GODKAND = "".
            IF overtemp.AONR = "" THEN DO:
               ASSIGN  
               extratidallt.AONR = nyaonr
               extratidallt.DELNR = nydelnr.           
            END.         
         END.
         ELSE DO:
            regdagnamn = "MÅN".     
            ASSIGN
            extratidallt.DAG = regdagnamn
            extratidallt.DATUM = regdatum.      
         END.
      END.
      IF vart = "NYA" THEN DO:
         ASSIGN
         extratidallt.RECTIDVIS = ?
         extratidallt.TIDLOG = FALSE
         extratidallt.START = 7
         extratidallt.SLUT = 7.
      END.    
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visknapp_UI WINDOW-1 
PROCEDURE visknapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:collefth = ?.
   IF Guru.GlobalaVariabler:collefth = ? AND BTN_NY:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:
      Guru.GlobalaVariabler:collefth = BTN_NY:HANDLE.
   END.
   IF BTN_UPP:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_UPP:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_UPP:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF BTN_BORT:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_BORT:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_BORT:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_NYALLA:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_NYALLA:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_NYALLA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_SKAPEGNA:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_SKAPEGNA:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_SKAPEGNA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   /*andra*/
   Guru.GlobalaVariabler:collefth = ?.
   IF Guru.GlobalaVariabler:collefth = ? AND BTN_PREG:VISIBLE = TRUE  THEN DO:
      Guru.GlobalaVariabler:collefth = BTN_PREG:HANDLE.
   END.
   IF BTN_DELA:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_DELA:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_DELA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   
   IF BTN_KOP:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_KOP:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_KOP:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   IF BTN_BYTA:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_BYTA:HANDLE.   
      Guru.GlobalaVariabler:colrighth = BTN_BYTA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   IF Guru.Konstanter:tidasekvar[2] = TRUE THEN DO:
      IF Guru.Konstanter:globforetag = "MISV" THEN.
      ELSE DO:
         IF BTN_NYLON:VISIBLE = TRUE  THEN DO:
            IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_NYLON:HANDLE.   
            Guru.GlobalaVariabler:colrighth = BTN_NYLON:HANDLE.           
            RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
         END.
      END.   
   END.
   IF Guru.Konstanter:tidtsekvar[1] = TRUE THEN DO:
      IF Guru.Konstanter:globforetag = "GKAL" THEN.
      ELSE DO:      
         IF BTN_RES:VISIBLE = TRUE  THEN DO:
            IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_RES:HANDLE.   
            Guru.GlobalaVariabler:colrighth = BTN_RES:HANDLE.           
            RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
         END.
      END.
      IF BTN_TJRESA:VISIBLE = TRUE  THEN DO:
         IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_TJRESA:HANDLE.   
         Guru.GlobalaVariabler:colrighth = BTN_TJRESA:HANDLE.           
         RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
      END.
   END.
  
     
   IF BTN_FORDON:VISIBLE = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_FORDON:HANDLE.   
      Guru.GlobalaVariabler:colrighth = BTN_FORDON:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vpersval_UI WINDOW-1 
PROCEDURE vpersval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      FIND tidpers WHERE RECID(tidpers) = perssokrec NO-LOCK NO-ERROR.                             
      ASSIGN
      pkod = tidpers.PERSONALKOD      
      tidalltrec = 0
      persrec = tidpers.TIDPERSREC.      
      RUN huvud_UI.
      RUN hmtegna_UI.
      FILL-IN_FORNAMN-2 = tidpers.FORNAMN + " " + tidpers.EFTERNAMN.  
      FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
      NO-LOCK NO-ERROR. 
      FILL-IN-PKOD = personaltemp.PERSONALKOD.
      RUN radandtid_UI.      
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI WINDOW-1 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordnr = ordnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

