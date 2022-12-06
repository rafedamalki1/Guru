&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE anlaggtemp NO-UNDO LIKE anlaggtemp.
DEFINE TEMP-TABLE ansvaraotemp NO-UNDO LIKE ansvaraotemp.
DEFINE TEMP-TABLE anvandartemp NO-UNDO LIKE anvandartemp.
DEFINE TEMP-TABLE aonrkonttemp NO-UNDO LIKE aonrkonttemp.
DEFINE TEMP-TABLE aonrtidperstemp NO-UNDO LIKE aonrtidperstemp.
DEFINE TEMP-TABLE aotidslagtemp NO-UNDO LIKE aotidslagtemp.
DEFINE TEMP-TABLE arbarttemp NO-UNDO LIKE arbarttemp.
DEFINE TEMP-TABLE avtalaonrtemp NO-UNDO LIKE avtalaonrtemp.
DEFINE TEMP-TABLE avtaodattemp NO-UNDO LIKE avtaodattemp.
DEFINE TEMP-TABLE beratemp NO-UNDO LIKE beratemp.
DEFINE TEMP-TABLE bestkundallt NO-UNDO LIKE bestkundallt.
DEFINE TEMP-TABLE bestkundextra NO-UNDO LIKE bestkundextra.
DEFINE TEMP-TABLE dagboktemp NO-UNDO LIKE dagboktemp.
DEFINE TEMP-TABLE gatill NO-UNDO LIKE gatill.
DEFINE TEMP-TABLE omranvandartemp NO-UNDO LIKE omranvandartemp.
DEFINE TEMP-TABLE personaltemp NO-UNDO LIKE personaltemp.
DEFINE TEMP-TABLE priotemp NO-UNDO LIKE priotemp.
DEFINE TEMP-TABLE projtemp NO-UNDO LIKE projtemp.



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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER nyttaovarin AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER kopiavar AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER aonrvar2 AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER delnrvar2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER plannrvar2 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar2 AS INTEGER NO-UNDO.



DEFINE NEW SHARED VARIABLE omrpersanvok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nyttaovar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrvar   AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE plannrvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE artalvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE slutaejnu AS LOGICAL NO-UNDO.
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

ASSIGN
plannrvar = plannrvar2
artalvar =  artalvar2 
aonrvar =  aonrvar2 
delnrvar = delnrvar2.
nyttaovar = nyttaovarin.
/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE btnberh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE btnkalkh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE Guru.SharedVariable:btnfakth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE Guru.SharedVariable:btnmarkh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1 NO-UNDO.


DEFINE NEW SHARED VARIABLE bortaoapph AS HANDLE NO-UNDO.                     /*BORTAOAPP.P*/
DEFINE NEW SHARED VARIABLE nyttaoapph AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE NEW SHARED VARIABLE nyttaoejapph AS HANDLE NO-UNDO.                              /* dokhant.P */
DEFINE NEW SHARED VARIABLE fbestapph AS HANDLE NO-UNDO.                      /*EXTRATABHMT*/
DEFINE NEW SHARED VARIABLE visaaonrh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE avtaodatapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE anvapph AS HANDLE NO-UNDO.
DEFINE VARIABLE Guru.SharedVariable:btnaonrh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE omrbildvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE regvnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE tidbgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidagamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE tidpgamla AS CHARACTER NO-UNDO.
DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrvalvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE aoomradenr AS LOGICAL NO-UNDO.
DEFINE VARIABLE nrserierec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE NEW SHARED VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE mappvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE avtrec AS RECID NO-UNDO.
DEFINE VARIABLE sokmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE varfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE Okvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE openqkoll AS LOGICAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE sldatum AS DATE NO-UNDO.
DEFINE VARIABLE stdatum AS DATE NO-UNDO.
DEFINE VARIABLE projrapp AS CHARACTER FORMAT "X(101)" NO-UNDO.  
DEFINE VARIABLE omradespar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE bestspar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE aonrrecsp AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecsp2 AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel AS RECID NO-UNDO.
DEFINE VARIABLE aonrrecdel2 AS RECID NO-UNDO.
DEFINE VARIABLE stat AS INTEGER NO-UNDO.
DEFINE VARIABLE akoden AS INTEGER NO-UNDO.

DEFINE VARIABLE rowgatill AS ROWID NO-UNDO.


DEFINE VARIABLE editanm AS CHARACTER FORMAT "x(68)" NO-UNDO.
DEFINE VARIABLE flagga AS INTEGER NO-UNDO.
DEFINE VARIABLE aokopprec AS RECID NO-UNDO.
DEFINE VARIABLE aotidrec AS RECID NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bestvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE radfast AS LOGICAL NO-UNDO.
DEFINE VARIABLE autoregvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_BERKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KALKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_KOPPKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_TIDLKOP AS LOGICAL NO-UNDO.
DEFINE VARIABLE reslog AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ordnvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE idvar AS INTEGER NO-UNDO.

DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE refvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE extraomr AS CHARACTER NO-UNDO EXTENT 2.
DEFINE NEW SHARED VARIABLE sparavdat AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE brwkoll AS INTEGER NO-UNDO EXTENT 10.
DEFINE NEW SHARED VARIABLE gfastkh AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE gtillkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE mainvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE arbbspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbfspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE kollref AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE fbaonrvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttsparatao AS LOGICAL NO-UNDO. 
DEFINE VARIABLE gamomrnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE gambestnamn AS CHARACTER NO-UNDO.
{WHANDLTEMP.I}
   FIELD WF AS HANDLE EXTENT 200.
{EXTRATAB.I}  
{ALLDEF.I}
{SOKDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{FAKTTYPDEF.I}
{ANSPROJBER.I}
{OMRTEMPW.I}

{HOPPSEK2W.I}
{AVTPLANTEMP.I}

&Scoped-define SHARED SHARED 
{ANVTEMPS.I}
   /*
DEFINE TEMP-TABLE exanvandartemp NO-UNDO LIKE anvandartemp.
*/
DEFINE NEW SHARED TEMP-TABLE omranvandartemp NO-UNDO LIKE anvandartemp.
{PERSONALTEMP.I}
{AVDTEMP.I}
{DIRDEF.I}
{AONRDEF.I}
{BESTKUNDALLT.I}
{JURPERST.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{GATILL.I}
{FAKTIN.I}
{KALKIN.I}
{BERIN.I}
{MARKVARDIN.I}
{AUTOMREGTEMP.I}
{AVTAONRTEMP.I}
{AONRTIDPERS.I}
{DAGBOKTEMP.I}
{BERATEMP.I}

DEFINE NEW SHARED TEMP-TABLE aonrdeltemp
   FIELD AONR   AS CHARACTER 
   FIELD DELNR      AS INTEGER
   FIELD OMRADE  AS CHARACTER 
   FIELD BESTID  AS CHARACTER 
   FIELD FASTAAONR AS LOGICAL
   FIELD FASTKALK AS LOGICAL.

/*{PLANNRTEMP.I} */






DEFINE BUFFER gabuff FOR gatill.
DEFINE BUFFER aonrbuff FOR uppaonrtemp.
DEFINE QUERY abq FOR aonrbuff FIELDS (aonrbuff.FASTAAONR).
DEBUGGER:SET-BREAK().

RUN pstart_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-AONR
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr egnaao tidpers

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


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS tidpers.PERSONALKOD tidpers.FORNAMN ~
tidpers.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS tidpers.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS tidpers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS tidpers
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH tidpers NO-LOCK ~
    BY tidpers.PERSONALKOD
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH tidpers NO-LOCK ~
    BY tidpers.PERSONALKOD.
&Scoped-define TABLES-IN-QUERY-BRW_PERS tidpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS tidpers


/* Definitions for FRAME FRAME-BARABREDD                                */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-MENY MBTN_AOPERSSEK MBTN_KOPPL BTN_OK ~
BTN_AVB MBTN_ANM MBTN_AVT MBTN_DOK MBTN_FAK MBTN_KONT MBTN_OVR MBTN_TIDL ~
MBTN_TIDPERS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_AONR-3 
       MENU-ITEM m_Arbetsuppgift-3 LABEL "Arbetsuppgift" .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_ANM  NO-FOCUS
     LABEL "Anmärkningar" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_AOPERSSEK  NO-FOCUS
     LABEL "Arbetandepersonal" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_AVT  NO-FOCUS
     LABEL "Tillstånd/Avtal" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_DOK  NO-FOCUS
     LABEL "Dokument" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_FAK  NO-FOCUS
     LABEL "Faktura" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_KONT  NO-FOCUS
     LABEL "Konton" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_KOPPL  NO-FOCUS
     LABEL "Kopplingar" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_OVR  NO-FOCUS
     LABEL "Övrigt" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_TIDL  NO-FOCUS
     LABEL "Tidlägen" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_TIDPERS  NO-FOCUS
     LABEL "Tidskrivandepersonal" 
     SIZE 7.25 BY 2.33.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 124.88 BY .08.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_SKAPEN 
     LABEL "Spara favorit":L 
     SIZE 14.5 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-DAG-2 AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 8 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENFLE AS LOGICAL FORMAT "Endag/Flerdygn":U INITIAL NO 
     LABEL "Endag/Flerdygn" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN-3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTAR AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Sluttid" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTAR AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Starttid" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr/personal för:" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN-2 AS CHARACTER FORMAT "X(40)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", 1,
"Fasta aonr", 2,
"Favorit aonr", 3
     SIZE 39.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_EAONR FOR 
      egnaao SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      tidpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR C-Win _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område"
      utsokaonr.AONR COLUMN-LABEL "Aonr"
      utsokaonr.DELNR COLUMN-LABEL "Del!nr"
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning"
  ENABLE
      utsokaonr.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 55.38 BY 10.83
         FONT 4
         TITLE "Aktiva arbetsordernummer" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE BRW_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EAONR C-Win _STRUCTURED
  QUERY BRW_EAONR NO-LOCK DISPLAY
      egnaao.OMRADE COLUMN-LABEL "Område"
      egnaao.AONR COLUMN-LABEL "Aonr"
      egnaao.DELNR COLUMN-LABEL "Del!nr"
      egnaao.ORT COLUMN-LABEL "Ort/Benämning"
  ENABLE
      egnaao.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57.38 BY 9.42
         FONT 4
         TITLE "Favorit arbetsordernummer".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      tidpers.PERSONALKOD COLUMN-LABEL "Enhet/Sign"
      tidpers.FORNAMN COLUMN-LABEL "Förnamn"
      tidpers.EFTERNAMN COLUMN-LABEL "Efternamn"
  ENABLE
      tidpers.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 39.25 BY 8.42
         FONT 4.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-AONR
     MBTN_AOPERSSEK AT ROW 1 COL 107.13
     MBTN_KOPPL AT ROW 1 COL 1
     BTN_OK AT ROW 28.29 COL 95.75
     BTN_AVB AT ROW 28.29 COL 111
     MBTN_ANM AT ROW 1 COL 28.5
     MBTN_AVT AT ROW 1 COL 78.5
     MBTN_DOK AT ROW 1 COL 66.5
     MBTN_FAK AT ROW 1 COL 54.5
     MBTN_KONT AT ROW 1 COL 42.5
     MBTN_OVR AT ROW 1 COL 89
     MBTN_TIDL AT ROW 1 COL 16.5
     MBTN_TIDPERS AT ROW 1 COL 98.38
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.

DEFINE FRAME FRAME-BERPROJ
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-BEST
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-DOK
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-FAKT
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-KONTO
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-KOPP
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-OVRIGT
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-TIDL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-TIDPERS
     "Tidskrivande personal" VIEW-AS TEXT
          SIZE 30 BY 1.21 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 16.75
         SIZE 124.5 BY 11.5.

DEFINE FRAME FRAME-BARABREDD
     FILL-IN-TEXT AT ROW 1.13 COL 86.25 COLON-ALIGNED NO-LABEL WIDGET-ID 38
     CMB_AVD AT ROW 1.13 COL 100.63 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     RAD_FAST AT ROW 1.96 COL 62 NO-LABEL WIDGET-ID 42
     FILL-IN-PKOD AT ROW 2 COL 19 COLON-ALIGNED WIDGET-ID 28
     FILL-IN_FORNAMN-2 AT ROW 2 COL 30.5 NO-LABEL WIDGET-ID 40
     CMB_OMR AT ROW 2.17 COL 100.63 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     FILL-IN-ENFLE AT ROW 3.21 COL 19 COLON-ALIGNED WIDGET-ID 26
     BRW_AONR AT ROW 3.29 COL 70 WIDGET-ID 400
     BTN_NVE-3 AT ROW 4 COL 31.38 WIDGET-ID 8
     FILL-IN-STARTDAT AT ROW 4.42 COL 19 COLON-ALIGNED WIDGET-ID 36
     FILL-IN-DAG AT ROW 4.42 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     BRW_EAONR AT ROW 4.5 COL 67.5 WIDGET-ID 300
     BRW_PERS AT ROW 4.5 COL 86 WIDGET-ID 200
     BTN_FVE-3 AT ROW 4.88 COL 31.38 WIDGET-ID 4
     FILL-IN-STARTAR AT ROW 5.54 COL 19 COLON-ALIGNED WIDGET-ID 34
     BTN_NVE-4 AT ROW 6.5 COL 31.5 WIDGET-ID 10
     FILL-IN-SLUTDAT AT ROW 6.75 COL 19 COLON-ALIGNED WIDGET-ID 32
     FILL-IN-DAG-2 AT ROW 6.75 COL 33 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     BTN_FVE-4 AT ROW 7.38 COL 31.5 WIDGET-ID 6
     FILL-IN-SLUTAR AT ROW 7.88 COL 19 COLON-ALIGNED WIDGET-ID 30
     FILL-IN-AONR AT ROW 9.08 COL 19 COLON-ALIGNED WIDGET-ID 18
     BTN_SKAPEN AT ROW 9.08 COL 31 WIDGET-ID 12
     FILL-IN-DELNR AT ROW 10 COL 19 COLON-ALIGNED WIDGET-ID 24
     FILL-IN-NAMN-1 AT ROW 6.63 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN-NAMN-2 AT ROW 7.71 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN-NAMN-3 AT ROW 8.83 COL 20.13 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 124.88 BY 13.25
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: anlaggtemp T "?" NO-UNDO temp-db anlaggtemp
      TABLE: ansvaraotemp T "?" NO-UNDO temp-db ansvaraotemp
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: aonrkonttemp T "?" NO-UNDO temp-db aonrkonttemp
      TABLE: aonrtidperstemp T "?" NO-UNDO temp-db aonrtidperstemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: arbarttemp T "?" NO-UNDO temp-db arbarttemp
      TABLE: avtalaonrtemp T "?" NO-UNDO temp-db avtalaonrtemp
      TABLE: avtaodattemp T "?" NO-UNDO temp-db avtaodattemp
      TABLE: beratemp T "?" NO-UNDO temp-db beratemp
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: bestkundextra T "?" NO-UNDO temp-db bestkundextra
      TABLE: dagboktemp T "?" NO-UNDO temp-db dagboktemp
      TABLE: gatill T "?" NO-UNDO temp-db gatill
      TABLE: omranvandartemp T "?" NO-UNDO temp-db omranvandartemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
      TABLE: projtemp T "?" NO-UNDO temp-db projtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Guru-Projekthuvud"
         HEIGHT             = 28.33
         WIDTH              = 125
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 240
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 240
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-BARABREDD:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-BERPROJ:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-BEST:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-DOK:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-FAKT:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-KONTO:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-KOPP:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-OVRIGT:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-TIDL:FRAME = FRAME FRAME-AONR:HANDLE
       FRAME FRAME-TIDPERS:FRAME = FRAME FRAME-AONR:HANDLE.

/* SETTINGS FOR FRAME FRAME-AONR
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-AONR:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-BARABREDD
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_AONR FILL-IN-ENFLE FRAME-BARABREDD */
/* BROWSE-TAB BRW_EAONR FILL-IN-DAG FRAME-BARABREDD */
/* BROWSE-TAB BRW_PERS BRW_EAONR FRAME-BARABREDD */
ASSIGN 
       FRAME FRAME-BARABREDD:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME FRAME-BARABREDD                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME FRAME-BARABREDD         = 1000.

/* SETTINGS FOR BROWSE BRW_EAONR IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BRW_EAONR:HIDDEN  IN FRAME FRAME-BARABREDD                = TRUE
       BRW_EAONR:POPUP-MENU IN FRAME FRAME-BARABREDD             = MENU POPUP-MENU-BRW_AONR-3:HANDLE
       BRW_EAONR:MAX-DATA-GUESS IN FRAME FRAME-BARABREDD         = 1000.

/* SETTINGS FOR BROWSE BRW_PERS IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME FRAME-BARABREDD                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-BARABREDD         = 300.

/* SETTINGS FOR BUTTON BTN_FVE-3 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-4 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-3 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-4 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DAG-2 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-NAMN-1 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-NAMN-1:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NAMN-2 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-NAMN-2:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-NAMN-3 IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-NAMN-3:READ-ONLY IN FRAME FRAME-BARABREDD        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SLUTDAT IN FRAME FRAME-BARABREDD
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SLUTDAT:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME FRAME-BARABREDD
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME FRAME-BARABREDD
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-BARABREDD
   NO-DISPLAY                                                           */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FRAME FRAME-BERPROJ
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-BERPROJ:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-BEST
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-BEST:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-DOK
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-DOK:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-FAKT
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-FAKT:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-KONTO
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-KONTO:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-KOPP
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-KOPP:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-OVRIGT
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-OVRIGT:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-TIDL
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-TIDL:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-TIDPERS
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-TIDPERS:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.tidpers"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tidpers.PERSONALKOD|yes"
     _FldNameList[1]   > Temp-Tables.tidpers.PERSONALKOD
"tidpers.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidpers.FORNAMN
"tidpers.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidpers.EFTERNAMN
"tidpers.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-AONR
/* Query rebuild information for FRAME FRAME-AONR
     _Query            is NOT OPENED
*/  /* FRAME FRAME-AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BERPROJ
/* Query rebuild information for FRAME FRAME-BERPROJ
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BERPROJ */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BEST
/* Query rebuild information for FRAME FRAME-BEST
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-DOK
/* Query rebuild information for FRAME FRAME-DOK
     _Query            is NOT OPENED
*/  /* FRAME FRAME-DOK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-FAKT
/* Query rebuild information for FRAME FRAME-FAKT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-FAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KONTO
/* Query rebuild information for FRAME FRAME-KONTO
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KONTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-KOPP
/* Query rebuild information for FRAME FRAME-KOPP
     _Query            is NOT OPENED
*/  /* FRAME FRAME-KOPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-OVRIGT
/* Query rebuild information for FRAME FRAME-OVRIGT
     _Query            is NOT OPENED
*/  /* FRAME FRAME-OVRIGT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDL
/* Query rebuild information for FRAME FRAME-TIDL
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDPERS
/* Query rebuild information for FRAME FRAME-TIDPERS
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDPERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Guru-Projekthuvud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   slutaejnu = FALSE.
   RUN btnavb_UI IN Guru.SharedVariable:btnaonrh.
   IF slutaejnu = TRUE THEN DO:
      slutaejnu = FALSE.
      RETURN NO-APPLY.
   END.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Guru-Projekthuvud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR C-Win
ON MOUSE-MENU-CLICK OF BRW_AONR IN FRAME FRAME-BARABREDD /* Aktiva arbetsordernummer */
DO:
   sok4 = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR C-Win
ON VALUE-CHANGED OF BRW_AONR IN FRAME FRAME-BARABREDD /* Aktiva arbetsordernummer */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EAONR C-Win
ON MOUSE-MENU-CLICK OF BRW_EAONR IN FRAME FRAME-BARABREDD /* Favorit arbetsordernummer */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_EAONR C-Win
ON VALUE-CHANGED OF BRW_EAONR IN FRAME FRAME-BARABREDD /* Favorit arbetsordernummer */
DO:
   IF musz = FALSE THEN DO:      
      RUN fillinupdate_UI.                
   END.
   musz = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS C-Win
ON LEAVE OF BRW_PERS IN FRAME FRAME-BARABREDD
DO:   
   IF INPUT FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blank." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD AND 
   personaltemp.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
   ELSE DO:         
      RUN nastapers_UI.      
      musz = FALSE.
   END.
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.   
   DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS C-Win
ON VALUE-CHANGED OF BRW_PERS IN FRAME FRAME-BARABREDD
DO:
   FILL-IN-PKOD = tidpers.PERSONALKOD.      
   persrec = tidpers.TIDPERSREC.
   valgubbe = TRUE.
   RUN nasta_UI.
   RUN nastapers_UI.
   valgubbe = FALSE.
   musz = FALSE.      
   {musarrow.i}  
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
   DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 WITH FRAME {&FRAME-NAME}.   
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 C-Win
ON CHOOSE OF BTN_FVE-3 IN FRAME FRAME-BARABREDD /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   RUN btnfve3_UI.
   
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 C-Win
ON CHOOSE OF BTN_FVE-4 IN FRAME FRAME-BARABREDD /* - */
DO: 
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   RUN btnfve4_UI.
   
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 C-Win
ON CHOOSE OF BTN_NVE-3 IN FRAME FRAME-BARABREDD /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
   RUN btnnve3_UI.
   
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.   
   IF FILL-IN-SLUTDAT < FILL-IN-STARTDAT THEN DO:
      FILL-IN-SLUTDAT = FILL-IN-STARTDAT.
      RUN fislutdat_UI.
      DISPLAY FILL-IN-SLUTDAT FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 C-Win
ON CHOOSE OF BTN_NVE-4 IN FRAME FRAME-BARABREDD /* + */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT + 1.        
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   RUN btnnve4_UI.  
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKAPEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEN C-Win
ON CHOOSE OF BTN_SKAPEN IN FRAME FRAME-BARABREDD /* Spara favorit */
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
         FILL-IN-SOKPA:HIDDEN = TRUE.   
         ENABLE BRW_EAONR WITH FRAME {&FRAME-NAME}.
         DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPEN C-Win
ON GO OF BTN_SKAPEN IN FRAME FRAME-BARABREDD /* Spara favorit */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME FRAME-BARABREDD
DO:
   CMB_AVD = INPUT CMB_AVD.   
   omravdand = 1.    
   RUN nycolsortprep_UI (INPUT 2).
   RUN openbdynspec_UI IN brwproc[1]. 
   {CMB_AVDB2.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR C-Win
ON VALUE-CHANGED OF CMB_OMR IN FRAME FRAME-BARABREDD
DO:
   CMB_OMR = INPUT CMB_OMR.            
   omravdand = 2.    
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR C-Win
ON ENTRY OF FILL-IN-AONR IN FRAME FRAME-BARABREDD /* Aonr */
DO:   
   RUN fiaonr_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR C-Win
ON LEAVE OF FILL-IN-AONR IN FRAME FRAME-BARABREDD /* Aonr */
DO:
  IF FILL-IN-AONR NE INPUT FILL-IN-AONR THEN DO:
      FILL-IN-AONR = INPUT FILL-IN-AONR.
      RUN hao2_UI.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR C-Win
ON MOUSE-SELECT-CLICK OF FILL-IN-AONR IN FRAME FRAME-BARABREDD /* Aonr */
DO:  
   RUN fiaonr_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR C-Win
ON LEAVE OF FILL-IN-DELNR IN FRAME FRAME-BARABREDD /* Delnr */
DO:  
   
   IF INPUT FILL-IN-AONR = "" THEN DO:
      MESSAGE "Fältet " + LC(Guru.Konstanter:gaok) + " kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
       RETURN NO-APPLY.
   END.    
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND 
   utsokaonr.DELNR = FILL-IN-DELNR USE-INDEX AONR NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE utsokaonr THEN DO:
      MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "finns inte." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      regdatum = FILL-IN-SLUTDAT.
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-AONR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   RUN hao2_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENFLE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENFLE C-Win
ON LEAVE OF FILL-IN-ENFLE IN FRAME FRAME-BARABREDD /* Endag/Flerdygn */
DO:
   FILL-IN-ENFLE = INPUT FILL-IN-ENFLE.   
   RUN fienfle_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENFLE C-Win
ON MOUSE-SELECT-CLICK OF FILL-IN-ENFLE IN FRAME FRAME-BARABREDD /* Endag/Flerdygn */
DO:
   IF INPUT FILL-IN-ENFLE = TRUE THEN FILL-IN-ENFLE = FALSE.  
   IF INPUT FILL-IN-ENFLE = FALSE THEN FILL-IN-ENFLE = TRUE.
   ASSIGN
   FILL-IN-FSTART = FALSE
   FILL-IN-FSLUT = FALSE
   FILL-IN-FSTART:HIDDEN = TRUE 
   FILL-IN-FSLUT:HIDDEN = TRUE
   FILL-IN-RESTID:HIDDEN = TRUE
   FILL-IN-TRORD:HIDDEN = TRUE.
   IF FILL-IN-ENFLE = TRUE THEN DO:
      ASSIGN
      FILL-IN-NATT = FALSE      
      FILL-IN-FRIMAT = FALSE
      FILL-IN-3MAN = FALSE  
      FILL-IN-REDTRAKT = FALSE      
      FILL-IN-NATT:HIDDEN = TRUE       
      FILL-IN-FRIMAT:HIDDEN = TRUE
      FILL-IN-3MAN:HIDDEN = TRUE
      FILL-IN-REDTRAKT:HIDDEN = TRUE
      FILL-IN-REDDATUM:HIDDEN = TRUE.                         
   END.   
   ELSE DO:
      ENABLE FILL-IN-NATT  WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-NATT:HIDDEN = FALSE      
      FILL-IN-FRIMAT:HIDDEN = FALSE
      FILL-IN-3MAN:HIDDEN = FALSE
      FILL-IN-REDTRAKT:HIDDEN = FALSE
      FILL-IN-REDDATUM:HIDDEN = TRUE.      
      IF Guru.Konstanter:globforetag = "celpa" THEN DO:
         /* mer än 3 mån*/
         ASSIGN
         FILL-IN-FSTART:HIDDEN = FALSE  
         FILL-IN-FSLUT:HIDDEN = FALSE.         
      END.
   END.
   ASSIGN
   FILL-IN-MAT = FALSE
   FILL-IN-FRIMAT = FALSE
   FILL-IN-3MAN = FALSE
   FILL-IN-REDTRAKT = FALSE.      
   RUN endag_UI.
   DISPLAY FILL-IN-ENFLE WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTAR C-Win
ON LEAVE OF FILL-IN-SLUTAR IN FRAME FRAME-BARABREDD /* Sluttid */
DO:
  FILL-IN-SLUTAR = INPUT FILL-IN-SLUTAR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTAR C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTAR IN FRAME FRAME-BARABREDD /* Sluttid */
DO:
  klocka = INPUT FILL-IN-SLUTAR.  
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-SLUTAR = klocka.
   DISPLAY FILL-IN-SLUTAR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT C-Win
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME FRAME-BARABREDD /* Slut datum */
DO:
  FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
  RUN fislutdat_UI.
  regdatum = FILL-IN-SLUTDAT. 
  RUN REGDAG.P.
  IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
  FILL-IN-DAG-2 = regdagnamn + "dag".
  IF FILL-IN-FSLUT = FALSE /*AND FILL-IN-ENFLE = FALSE*/ THEN DO:   
     RUN REGVEC.P.
     {SLUTARBW.I}
     ASSIGN
     FILL-IN-SLUTAR = regslut.
     DISPLAY FILL-IN-SLUTAR WITH FRAME {&FRAME-NAME}.
  END.
  DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME FRAME-BARABREDD /* Slut datum */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   regdatum = INPUT FILL-IN-SLUTDAT.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = regdatum.
   RUN REGVEC.P.
   {SLUTARBW.I}
   ASSIGN
   FILL-IN-SLUTAR = regslut.
   IF FILL-IN-ENFLE = FALSE THEN
   DISPLAY FILL-IN-SLUTAR WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTAR C-Win
ON LEAVE OF FILL-IN-STARTAR IN FRAME FRAME-BARABREDD /* Starttid */
DO:
  FILL-IN-STARTAR = INPUT FILL-IN-STARTAR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTAR C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTAR IN FRAME FRAME-BARABREDD /* Starttid */
DO:
   klocka = INPUT FILL-IN-STARTAR.  
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   FILL-IN-STARTAR = klocka.
   DISPLAY FILL-IN-STARTAR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME FRAME-BARABREDD /* Start datum */
DO:
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   RUN fistartdat_UI. 
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.       
   IF FILL-IN-SLUTDAT < FILL-IN-STARTDAT THEN DO:
      FILL-IN-SLUTDAT = FILL-IN-STARTDAT.
      RUN fislutdat_UI.
      DISPLAY FILL-IN-SLUTDAT FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME FRAME-BARABREDD /* Start datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = regdatum.
   RUN REGVEC.P.
   {SLUTARBW.I}
   ASSIGN
   FILL-IN-STARTAR = regstart.
   IF FILL-IN-ENFLE = FALSE THEN
   DISPLAY FILL-IN-STARTAR WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Arbetsuppgift-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Arbetsuppgift-3 C-Win
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST C-Win
ON VALUE-CHANGED OF RAD_FAST IN FRAME FRAME-BARABREDD
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


&Scoped-define FRAME-NAME FRAME-AONR
&Scoped-define BROWSE-NAME BRW_AONR
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
   RUN avsluta_UI.   
   RUN disable_UI.
END.   
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   mainvar = TRUE.
   &Scoped-define FRAME-NAME FRAME-AONR
   {WIN_M_START.I}
    
   
   {muswait.i}
   {ALLSTARTDYN.I} 
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk.   
   CMB_BESTAVD = "Alla".
   nyttsparatao = FALSE.   
   EMPTY TEMP-TABLE uppaonrtemp NO-ERROR. 
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE valdaao THEN DO:
      CREATE uppaonrtemp.
      BUFFER-COPY valdaao TO uppaonrtemp.
   END.
   IF NOT AVAILABLE uppaonrtemp THEN DO:
      IF aonrvar = "" OR aonrvar = ? THEN musz = musz.
      ELSE DO:
         RUN hamtuppaonr IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE uppaonrtemp).
         FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
         uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.       
      END.
   END.
   RUN laddaao IN nyttaoapph (INPUT aonrvar,
                              INPUT delnrvar,
                              INPUT Guru.Konstanter:globforetag,
                              INPUT nyttaovar,
                              INPUT kalkrec,
                              INPUT Guru.Konstanter:globanv,
                              OUTPUT TABLE anlaggtemp,
                              OUTPUT TABLE aonrkonttemp,
                              OUTPUT TABLE ansvaraotemp,
                              OUTPUT TABLE aotidslagtemp,
                              OUTPUT TABLE aotidkopp,
                              OUTPUT TABLE arbarttemp,
                              OUTPUT TABLE avtalaonrtemp,
                              OUTPUT TABLE priotemp,
                              OUTPUT TABLE automregtemp,
                              OUTPUT TABLE beredningtemp,                               
                              OUTPUT TABLE varderingtemp,
                              OUTPUT TABLE fastspectemp,
                              OUTPUT TABLE kbenamntemp,
                              OUTPUT TABLE anvandartemp,
                              OUTPUT TABLE beratemp,
                              INPUT-OUTPUT TABLE uppaonrtemp).
   FILL-IN_ARBARTKOD:LABEL = Guru.Konstanter:gartk.
   FILL-IN_ARBARTKODOV:LABEL = Guru.Konstanter:gartk.
   BRW_ARBARTOV:TITLE = "Välj " + Guru.Konstanter:gartk.
   BRW_ARBART:TITLE = "Välj " + Guru.Konstanter:gartk.
   FILL-IN_PKODOV:LABEL = Guru.Konstanter:gpriok.
   FILL-IN_PKOD:LABEL = Guru.Konstanter:gpriok.  
   priotemp.PKOD:LABEL IN BROWSE BRW_PRIOOV = Guru.Konstanter:gpriok.
   priotemp.PKOD:LABEL IN BROWSE BRW_PRIO = Guru.Konstanter:gpriok.
   BRW_PRIOOV:TITLE = "Välj " + Guru.Konstanter:gpriok.
   BRW_PRIO:TITLE = "Välj " + Guru.Konstanter:gpriok.
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   ASSIGN
   aotidslagtemp.TIDLAGE:LABEL IN BROWSE BRW_AOTID = Guru.Konstanter:gtidlk.
   &Scoped-define BROWSE-NAME BRW_AOTID
   BRW_AOTID:TITLE = "Valda " + LC(Guru.Konstanter:gtidlk).
   FILL-IN_UTFARDAT:LABEL = CAPS(SUBSTRING(Guru.Konstanter:gutfardk,1,1)) + LC(SUBSTRING(Guru.Konstanter:gutfardk,2)).
   &Scoped-define FRAME-NAME FRAME-BARABREDD
   &Scoped-define FORMATNAMN FILL-IN_AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   
   RUN huvud_UI.                                               
   IF musz = TRUE THEN DO:
      LEAVE MAIN-BLOCK.
   END.
   ASSIGN
   arbbspar = BRW_ARBART:COLUMN   
   arbfspar = FILL-IN_ARBARTKOD:COLUMN.
   IF vartpro = "NAOPLAN" THEN DO:
      IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
         IF AVAILABLE uppaonrtemp THEN DO:
            RUN plankontohmt_UI IN nyttaoapph (INPUT plannrvar,INPUT artalvar,INPUT aonrvar,
                                            INPUT delnrvar,OUTPUT TABLE aonrkonttemp,
                                            INPUT-OUTPUT TABLE uppaonrtemp).
            FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
            uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF AVAILABLE uppaonrtemp THEN DO:
               IF uppaonrtemp.ANM[1] NE "" THEN DO:
                  ASSIGN EDITOR_ANM = uppaonrtemp.ANM[1].
                  DISPLAY EDITOR_ANM WITH FRAME FRAME-ANM.
               END.
            END.
         END.         
      END.
   END.    
   
   IF musz = TRUE THEN DO:
      LEAVE MAIN-BLOCK.
   END.
   RUN dagbokhmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,OUTPUT TABLE dagboktemp).
   RUN enable_UI.     
   {FRMSIZEF.I}  
   RUN menyrad_UI.
   RUN main2_UI.
   /*aonrsund*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:  
      IF plannrvar = ? AND nyttaovar = TRUE  THEN DO:
         IF aonrvar BEGINS "38" OR aonrvar BEGINS "36" OR aonrvar BEGINS "46" OR aonrvar BEGINS "63" OR aonrvar BEGINS "79"  THEN DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = "1611" USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               ASSIGN  CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = omrtemp.NAMN.
               FILL-IN_BEST = omrtemp.NAMN.
            END. 
            RUN autokonto_UI IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR).                        
            EMPTY TEMP-TABLE aonrkonttemp NO-ERROR. 
            RUN konthmt IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE aonrkonttemp).
         END.
         ELSE DO:                  
            status-ok = CMB_OMR:ADD-FIRST(" ") IN FRAME FRAME-BARABREDD.
            ASSIGN 
            CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = " "
            FILL-IN_BEST = " " .
         END.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.         
         APPLY "ENTRY" TO CMB_OMR.
      END.        
   END.
   IF  Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "cELPA" THEN DO:  
      IF plannrvar = ? AND nyttaovar = TRUE  THEN DO:        
         status-ok = CMB_OMR:ADD-FIRST(" ") IN FRAME FRAME-BARABREDD.
         ASSIGN 
         CMB_OMR:SCREEN-VALUE IN FRAME FRAME-BARABREDD = " "
         FILL-IN_BEST = " " .
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.         
         APPLY "ENTRY" TO CMB_OMR.
      END.        
   END.
   IF  Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:      
      FILL-IN_BEST = " " .
      FILL-IN_BEST:HIDDEN = TRUE.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR  NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
         IF AVAILABLE judavdtemp THEN DO:
            FILL-IN_AVDELNING = judavdtemp.AVDELNINGNAMN.
            DISPLAY FILL-IN_AVDELNING WITH FRAME FRAME-BARABREDD.
         END.
      END.      
   END.   
   IF nyttaovar = FALSE THEN DO:
      IF CMB_OMR = "" AND FILL-IN_BEST = " " THEN
      DISABLE FILL-IN_BEST CMB_OMR WITH FRAME {&FRAME-NAME}.       
   END.
      
   RUN btnstart_UI.
   IF varforetypchar[4] NE "" THEN DO:
      IF omrpersanvok = TRUE OR Guru.Konstanter:globniv = 0 THEN DO:
         MBTN_AOPERSSEK:HIDDEN IN FRAME FRAME-AONR = FALSE. 
         Guru.GlobalaVariabler:colrighth = MBTN_AOPERSSEK:HANDLE. 
         RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      END.
      ELSE MBTN_AOPERSSEK:HIDDEN IN FRAME FRAME-AONR = TRUE.
   END.
   ELSE MBTN_AOPERSSEK:HIDDEN IN FRAME FRAME-AONR = TRUE. 
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
   IF Guru.Konstanter:globforetag = "snat" AND Guru.Konstanter:globanv = "elpao" THEN  ENABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
   ELSE IF Guru.Konstanter:globanv = "elpao" THEN  ENABLE FILL-IN_BEST CMB_OMR WITH FRAME FRAME-BARABREDD.
   
  IF Guru.Konstanter:globforetag = "ORBI" THEN DO:
      ASSIGN
      FILL-IN-NAMN-1:HIDDEN = TRUE
      FILL-IN-NAMN-3:HIDDEN = TRUE
      FILL-IN_STARTDAG:HIDDEN = TRUE
      FILL-IN_ARBANSVARIG:HIDDEN = TRUE.
   END.     
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN BORTAOAPP.P PERSISTENT SET bortaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BORTAOAPP.P PERSISTENT SET bortaoapph. 
   END.
   IF Guru.Konstanter:appcon THEN DO:
      /*FAKTFOR*/
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      /*FAKTFOR*/
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   
   IF varforetypval[21] = 1 THEN DO:
      RUN DOKHANT.P PERSISTENT SET nyttaoejapph.
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN DOKHANTAPP.P PERSISTENT SET nyttaoejapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN DOKHANTAPP.P PERSISTENT SET nyttaoejapph.
      END.   
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVTAODATAPP.P PERSISTENT SET avtaodatapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AVTAODATAPP.P PERSISTENT SET avtaodatapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN VISAAPP.P PERSISTENT SET visaaonrh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VISAAPP.P PERSISTENT SET visaaonrh.
   END.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aokoll_UI C-Win 
PROCEDURE aokoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FILL-IN_AONR = ? THEN DO:
      MESSAGE Guru.Konstanter:gaok + " kan inte vara okänt!" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN NO-APPLY.
   END.
   IF FILL-IN_AONR = "" THEN DO:
      MESSAGE Guru.Konstanter:gaol + " kan inte vara blankt!" VIEW-AS ALERT-BOX.
      musz = TRUE.
      RETURN NO-APPLY. 
   END.
   IF nyttaovar = TRUE THEN DO:
      RUN aokoll IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT aonrrec,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      musz = musz.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         musz = TRUE.
         RETURN NO-APPLY.
      END.
      MESSAGE  "Du har valt " Guru.Konstanter:gaok  fill-in_aonr  
      SKIP     "är detta rätt ?      "
      SKIP     "  "
      SKIP     "Obs !! Detta går ej att ändra senare."
      SKIP     "  "   
      SKIP     "Om det ändå blir fel kontakta Elpool tel 090/184540"    
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
      CASE val1:
         WHEN TRUE THEN DO:
            musz = FALSE.              
            RETURN.
         END.
         WHEN FALSE THEN DO:
            musz = TRUE. 
         END.      
      END CASE.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI C-Win 
PROCEDURE avb_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                     
   IF nyttaovar = TRUE AND nyttsparatao = TRUE THEN DO: 
      APPLY "CHOOSE" TO BTN_OK IN FRAME FRAME-AONR.
   END.
   ELSE DO:
       
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:      
         APPLY "CHOOSE" TO BTN_OK IN FRAME FRAME-AONR.
      END.
      ELSE IF NOT svar THEN DO:  
         ASSIGN
         aonrvar2 =  aonrvar 
         delnrvar2 = delnrvar.
         {muswait.i}            
         IF nyttaovar = TRUE THEN DO:
            ASSIGN
            mappvar = ""
            mappvarhj = "".
            mappvar = uppaonrtemp.AONR + "000".                       
            IF grundmappvar NE "" THEN DO:
               mappvarhj = grundmappvar + mappvar.
               RUN delmappvar IN nyttaoejapph (INPUT mappvarhj).               
            END.
         END.
         musz = TRUE.
         {BESTALLTTEMPBORT.I}     
         RUN avsluta_UI.
         {musarrow.i}
         APPLY "END-ERROR":U TO SELF.         
      END.
      ELSE DO:
         slutaejnu = TRUE.
      END.
   END.                          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluta_UI C-Win 
PROCEDURE avsluta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
      RUN borthand_UI IN Guru.SharedVariable:btnaonrh.
      DELETE PROCEDURE Guru.SharedVariable:btnaonrh NO-ERROR.      
   END.   
   IF VALID-HANDLE(bortaoapph) THEN DELETE PROCEDURE bortaoapph.      
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.      
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph NO-ERROR.
      nyttaoapph = ?.
   END.      
   IF VALID-HANDLE(visaaonrh) THEN DELETE PROCEDURE visaaonrh.
   IF VALID-HANDLE(nyttaoejapph) THEN DELETE PROCEDURE nyttaoejapph NO-ERROR.
   APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btngatill_UI C-Win 
PROCEDURE btngatill_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   MESSAGE "fixa" VIEW-AS ALERT-BOX.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnya_UI C-Win 
PROCEDURE btnnya_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "fix2" VIEW-AS ALERT-BOX.
   /*
   IF RAD_VAL = 3 THEN DO:
      EMPTY TEMP-TABLE edagboktemp NO-ERROR. 
      CREATE edagboktemp.
      edagboktemp.DATUM = TODAY.
      RUN DAGBOKREG.W (OUTPUT  musz,INPUT-OUTPUT TABLE edagboktemp).
      IF musz = FALSE THEN DO:
         FIND FIRST edagboktemp NO-ERROR.
         CREATE dagboktemp.
         BUFFER-COPY edagboktemp TO dagboktemp.
         RUN setlastrowid_UI IN brwproc[15] (INPUT ROWID(dagboktemp)).         
         RUN openbdynspec_UI IN brwproc[15]. 
         RUN lastselectdyn_UI IN brwproc[15]. 
      END.
      EMPTY TEMP-TABLE edagboktemp NO-ERROR. 
      musz = FALSE.     
   END.
   IF RAD_VAL = 6 THEN DO:
      musz = FALSE.
      ordnvar = ?.
      idvar = ?.
      ASSIGN
      mappvarhj = uppaonrtemp.AONR + STRING(uppaonrtemp.DELNR) + "\".          
      mappvar = grundmappvar +  mappvarhj.                                         
      IF grundmappvar NE "" THEN DO:
         FILE-INFO:FILE-NAME = mappvar.     
         IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
            MESSAGE "Mappen " mappvar " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool." VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.  
      ELSE DO:
         MESSAGE "Ingen mappen är upplagd! Kontakta Elpool." VIEW-AS ALERT-BOX.
         RETURN.
      END.
      {AVBGOM.I}
      RUN NYAVTALAO.W (INPUT uppaonrtemp.AONR,INPUT uppaonrtemp.DELNR,INPUT TRUE,
                             INPUT-OUTPUT ordnvar,INPUT-OUTPUT idvar,INPUT TRUE,
                             OUTPUT mappvar).
      {AVBFRAM.I}
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[6]. 
         FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = uppaonrtemp.AONR AND 
         avtalaonrtemp.DELNR = uppaonrtemp.DELNR AND avtalaonrtemp.ORDN = ordnvar AND 
         avtalaonrtemp.AVTID = idvar AND avtalaonrtemp.LNK = TRUE NO-LOCK NO-ERROR.         
         IF AVAILABLE avtalaonrtemp THEN DO:
            RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(avtalaonrtemp)).         
         END.
         RUN openbdynspec_UI IN brwproc[7]. 
         RUN lastselectdyn_UI IN brwproc[7]. 
         IF AVAILABLE avtalaonrtemp THEN DO:
            IF avtalaonrtemp.FIL NE "" THEN DO:
               RUN OPENDOC.P (avtalaonrtemp.FIL,"","",NO).
            END.         
            ELSE DO:
               MESSAGE "Det finns ingen fil kopplad till dokumentet."
               VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
         END.
      END.
      ELSE musz = FALSE.
   END.
   IF RAD_VAL = 7 THEN DO:
      musz = FALSE.
      ordnvar = ?.
      idvar = ?.
      ASSIGN
      mappvarhj = uppaonrtemp.AONR + STRING(uppaonrtemp.DELNR) + "\".          
      mappvar = grundmappvar +  mappvarhj.                                         
      IF grundmappvar NE "" THEN DO:
         FILE-INFO:FILE-NAME = mappvar.
         IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
            MESSAGE "Mappen " mappvar " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool." VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.      
      {AVBGOM.I}
      RUN NYAVTALAO.W (INPUT uppaonrtemp.AONR,INPUT uppaonrtemp.DELNR,INPUT TRUE,
                             INPUT-OUTPUT ordnvar,INPUT-OUTPUT idvar,INPUT FALSE,
                             OUTPUT mappvar).
      {AVBFRAM.I}
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[7]. 
         FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = uppaonrtemp.AONR AND 
         avtalaonrtemp.DELNR = uppaonrtemp.DELNR AND avtalaonrtemp.ORDN = ordnvar AND 
         avtalaonrtemp.AVTID = idvar AND avtalaonrtemp.LNK = FALSE NO-LOCK NO-ERROR.                  
         RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(avtalaonrtemp)).
         RUN openbdynspec_UI IN brwproc[6]. 
         RUN lastselectdyn_UI IN brwproc[6].         
         IF avtalaonrtemp.FIL NE "" THEN DO:         
            RUN OPENDOC.P (avtalaonrtemp.FIL,"","",NO).
         END.         
         ELSE DO:
            MESSAGE "Det finns ingen fil kopplad till Tillståndet/Avtalet."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
      END.
      ELSE musz = FALSE.
   END.
   */  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
   
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE uppaonrtemp  THEN DO:   
      ASSIGN
      omradespar = uppaonrtemp.OMRADE
      bestspar = uppaonrtemp.BESTID.   
   END.
   ELSE DO:
      ASSIGN
      omradespar = ""
      bestspar = "".   
   END.
   IF AVAILABLE uppaonrtemp THEN DO:
      ASSIGN
      musz = FALSE
      FILL-IN_AONRAVDATUM = INPUT FRAME FRAME-BARABREDD FILL-IN_AONRAVDATUM
      FILL-IN_BEST = INPUT FILL-IN_BEST
      CMB_DEBPR = INPUT CMB_DEBPR
      CMB_OMR = INPUT CMB_OMR
      CMB_FAK = INPUT FRAME FRAME-FAKT CMB_FAK
      FILL-IN_FASTAAONR = INPUT FILL-IN_FASTAAONR
      FILL-IN_ANLNR = INPUT FRAME FRAME-OVRIGT FILL-IN_ANLNR 
      FILL-IN_AONR = INPUT FILL-IN_AONR 
      FILL-IN_ARBANSVARIG = INPUT FILL-IN_ARBANSVARIG
      FILL-IN_ARBARTKOD = INPUT FRAME FRAME-KONTO FILL-IN_ARBARTKOD
      FILL-IN_PKOD = INPUT FRAME FRAME-KONTO FILL-IN_PKOD
      EDITOR_AUPP = INPUT EDITOR_AUPP 
      EDITOR_ANM = INPUT FRAME FRAME-ANM EDITOR_ANM
      FILL-IN_BEREDARE = INPUT FILL-IN_BEREDARE
      FILL-IN_STARTDAG = INPUT FILL-IN_STARTDAG
      FILL-IN_DELNR = INPUT FILL-IN_DELNR
      FILL-IN_ORT = INPUT FILL-IN_ORT
      FILL-IN_PKODOV = INPUT FILL-IN_PKODOV 
      FILL-IN_TRAKTAMENTE = INPUT CMB_TRAK
      FILL-IN_UTRYCKNING = INPUT FILL-IN_UTRYCKNING
      FILL-IN_UTFARDAT = INPUT FILL-IN_UTFARDAT
      FILL-IN-KONAKT = INPUT FILL-IN-KONAKT
      FILL-IN-REF = INPUT FILL-IN-REF.      
      FILL-IN_ANVANDARE = INPUT FILL-IN_ANVANDARE.      
      IF FILL-IN_AONRAVDATUM:HIDDEN = TRUE THEN FILL-IN_AONRAVDATUM = 01/01/1991.
      IF uppaonrtemp.OMRADE = "" THEN .
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
         NO-LOCK NO-ERROR.
         FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bestkundallt THEN DO:
            FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_OMR 
            NO-LOCK NO-ERROR.
         END. 
      END.
      /*aonrsund*/
      /*FAKTFOR*/
      musz = FALSE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                       
         IF CMB_FAK = "Faktureras ej" THEN musz = musz.
         ELSE IF fbaonrvar  = FALSE THEN fbaonrvar = FALSE.
         ELSE DO:
            fbaonrvar = FALSE.
            FOR EACH gatill WHERE gatill.TYPCHAR = "FBAONR":
               IF gatill.F2 NE ? THEN musz = TRUE.
            END.
            IF musz = FALSE THEN DO:
            END.
         END.
         musz = FALSE.
      END.
      IF FILL-IN_BEST = "" THEN DO:
         IF CMB_OMR NE "" THEN DO:
            MESSAGE Guru.Konstanter:gbestk "får inte vara blank!" VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.         
         END.
      END.      
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
         IF FILL-IN_ORT = "" THEN DO:
            MESSAGE "Benämning är obligatorisk!" 
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.
         END.         
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
         NO-LOCK NO-ERROR.         
         IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "16" THEN DO:
            IF FILL-IN_ARBARTKOD = 1 THEN DO:
               MESSAGE Guru.Konstanter:gartk " får inte vara 1"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
            FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.   
            IF FILL-IN_ARBARTKOD = 11 THEN DO:
               FIND FIRST aonrkonttemp WHERE aonrkonttemp.K1 NE "1622" NO-LOCK NO-ERROR.
               IF AVAILABLE aonrkonttemp THEN DO:
                  MESSAGE Guru.Konstanter:gartk " 11 måste ha " + kbenamntemp.K1 + " 1622"
                  VIEW-AS ALERT-BOX.            
                  musz = TRUE.
                  RETURN NO-APPLY.            
               END.
            END.
            IF FILL-IN_ARBARTKOD = 12 THEN DO:
               FIND FIRST aonrkonttemp WHERE aonrkonttemp.K1 NE "1623" NO-LOCK NO-ERROR.
               IF AVAILABLE aonrkonttemp THEN DO:
                  MESSAGE Guru.Konstanter:gartk " 12 måste ha " + kbenamntemp.K1 + " 1623"
                  VIEW-AS ALERT-BOX.            
                  musz = TRUE.
                  RETURN NO-APPLY.            
               END.
            END.
         END.         
         IF AVAILABLE omrtemp THEN DO:
            FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
            IF AVAILABLE judavdtemp THEN DO:
               IF judavdtemp.JUDID = "SEAB" THEN DO:              
                  IF SUBSTRING(FILL-IN_AONR,1,1) NE "S" THEN DO:
                     MESSAGE Guru.Konstanter:gaok + " ska börja med ett- S" VIEW-AS ALERT-BOX.            
                     musz = TRUE.
                     RETURN NO-APPLY.            
                 END.
              END.
            END.         
         END.
      END.
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:         
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
         NO-LOCK NO-ERROR.       

         IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "10" THEN DO:
            IF FILL-IN_ARBARTKOD = 1 THEN DO:
               MESSAGE Guru.Konstanter:gartk " får inte vara 1"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
            FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.   
            IF FILL-IN_ARBARTKOD = 32 THEN DO:
               FIND FIRST aonrkonttemp WHERE aonrkonttemp.K1 NE "016" NO-LOCK NO-ERROR.
               IF AVAILABLE aonrkonttemp THEN DO:
                  MESSAGE Guru.Konstanter:gartk " 32 måste ha " + kbenamntemp.K1 + " 016"
                  VIEW-AS ALERT-BOX.            
                  musz = TRUE.
                  RETURN NO-APPLY.            
               END.            
            END.
         END.         
         IF AVAILABLE omrtemp THEN DO:
            FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
            IF AVAILABLE judavdtemp THEN DO:
               IF judavdtemp.JUDID = "BYGG" THEN DO:              
                  IF SUBSTRING(FILL-IN_AONR,1,1) NE "S" THEN DO:
                     MESSAGE Guru.Konstanter:gaok + " ska börja med ett- S" VIEW-AS ALERT-BOX.            
                     musz = TRUE.
                     RETURN NO-APPLY.            
                 END.
              END.
            END.         
         END.
      END.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:       
         kollref = INTEGER(FILL-IN-REF) NO-ERROR.
         IF FILL-IN-REF NE ""  THEN DO:
            IF LENGTH(FILL-IN-REF) NE 6 THEN DO:
               MESSAGE Guru.Konstanter:grefbefk " skall vara numeriskt 6 positioner!" 
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.         
            END.                     
         END.
         ELSE IF FILL-IN-REF NE "" AND kollref = 0 THEN DO:            
            MESSAGE Guru.Konstanter:grefbefk " skall vara numeriskt 6 positioner!" 
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.         
         END.
      END.
      IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
         IF uppaonrtemp.FAKTTYP = "Faktureras ej" THEN DO:
            MESSAGE "Faktureringstyp måste vara löpande räkning eller fast pris"
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.            
         END.
      END.
      RUN fbaonrkoll_UI IN Guru.SharedVariable:btnaonrh (OUTPUT musz). 
      IF musz = TRUE THEN RETURN.     
      IF CMB_DEBPR NE "TOT.PRIS." THEN DO:
         DEFINE VARIABLE val2 AS LOGICAL NO-UNDO.
         MESSAGE "Vill du att " LC(Guru.Konstanter:gdebk) " skall vara ? "  CMB_DEBPR skip
         "normalt brukar den vara 'TOT.PRIS.' !"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2.
         CASE val2:
            WHEN TRUE THEN DO:
               musz = musz.
            END.
            WHEN FALSE THEN DO:
               musz = TRUE.
               RETURN NO-APPLY.
            END.
         END CASE.
      END.      
      IF uppaonrtemp.DELNR = 0 THEN DO:   
         FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST NO-LOCK NO-ERROR.
         IF AVAILABLE bestkundallt THEN DO:
            IF varforetypval[11] = 0 THEN DO:
               IF nyttaovar = FALSE THEN DO:               
                  IF bestkundallt.BESTID NE bestspar THEN DO:
                     MESSAGE "Du har även ändrat på Beställare/Kund" SKIP 
                     "Detta innebär att alla " LC(Guru.Konstanter:gaok) " med samma huvudnummer också byter." SKIP
                     "Vill du ändå fortsätta ?"
                     VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar1 AS LOGICAL.
                     CASE svar1:
                        WHEN TRUE THEN DO:
                           RUN andrabestk_UI IN nyttaoapph (INPUT aonrvar,INPUT bestkundallt.BESTID).                                                      
                           ASSIGN uppaonrtemp.BESTID = bestkundallt.BESTID.
                           FOR EACH valdaao WHERE valdaao.AONR = aonrvar NO-LOCK:
                              ASSIGN valdaao.BESTID = bestkundallt.BESTID.
                           END.
                           FOR EACH utsokaonr WHERE utsokaonr.AONR = aonrvar NO-LOCK:
                              ASSIGN utsokaonr.BESTID = bestkundallt.BESTID.
                           END.
                           FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
                           uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
                        END.
                        WHEN FALSE THEN DO:
                           musz = TRUE.    
                           RETURN NO-APPLY.
                        END.
                     END.
                  END.
               END.
            END.
         END.
      END.
      IF FILL-IN_FASTAAONR NE uppaonrtemp.FASTAAONR THEN DO:
         DEFINE VARIABLE val3 AS LOGICAL NO-UNDO.
         MESSAGE "Du byter " LC(Guru.Konstanter:gaok) ":ets tillhörighet mellan " gtillk " och " gfastk "." SKIP 
         "Detta innebär att alla " LC(Guru.Konstanter:gaok) " med samma huvudnummer också byter." SKIP 
         "Vill du ändå fortsätta ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3.
         CASE val3:
            WHEN TRUE THEN DO:
               RUN andrafast IN nyttaoapph (INPUT aonrvar,INPUT FILL-IN_FASTAAONR).
               OPEN QUERY vbq FOR EACH uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar NO-LOCK.
               GET FIRST vbq NO-LOCK.
               DO WHILE AVAILABLE(uppaonrtemp):
                  DO TRANSACTION:
                     GET CURRENT vbq NO-LOCK.
                     uppaonrtemp.FASTAAONR = FILL-IN_FASTAAONR.
                  END.
                  GET NEXT vbq NO-LOCK.
               END.
               FOR EACH valdaao WHERE valdaao.AONR = aonrvar NO-LOCK:
                  ASSIGN valdaao.FASTAAONR = FILL-IN_FASTAAONR.
               END.
               FOR EACH utsokaonr WHERE utsokaonr.AONR = aonrvar NO-LOCK:
                  ASSIGN utsokaonr.FASTAAONR = FILL-IN_FASTAAONR.
               END.
               
               FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
               uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
            END.
            WHEN FALSE THEN DO:
               musz = TRUE.
               RETURN NO-APPLY.          
            END.
         END CASE.
      END.
      RUN dagbokspar IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT TABLE dagboktemp).
   END.
   ELSE DO:
      MESSAGE "Det gick inte att spara!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   regdatum = TODAY.
   RUN REGVEC.P.
   RUN REGDAG.P.
   
   IF FILL-IN_ANLNR = "" THEN DO:
      FIND FIRST anlaggtemp USE-INDEX ANLAGGNING NO-LOCK NO-ERROR.
      IF AVAILABLE anlaggtemp THEN ASSIGN FILL-IN_ANLNR = anlaggtemp.ANLNR.
   END.      
   IF FILL-IN_ARBARTKOD = ? OR FILL-IN_ARBARTKOD = 0 THEN DO:
      FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
      IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
   END.   
   IF FILL-IN_PKOD = ? OR FILL-IN_PKOD = 0 THEN DO:
      FIND FIRST priotemp USE-INDEX PRI NO-LOCK NO-ERROR.
      IF AVAILABLE priotemp THEN ASSIGN FILL-IN_PKOD = priotemp.PKOD.
   END.       
   IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
      RUN aonrkonspar_UI IN nyttaoapph (INPUT aonrrec,INPUT TABLE aonrkonttemp).
   END.
   
   IF AVAILABLE omrtemp THEN DO:
      RUN strangkoll IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT omrtemp.OMRADE,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
       MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         musz = TRUE.
         DELETE felmeddtemp.
         RETURN.
         
      END.
   END.
   RUN kontrolln IN nyttaoapph (INPUT aonrrec,INPUT tidagamla,INPUT tidbgamla,
                INPUT-OUTPUT FILL-IN_BEREDARE,INPUT-OUTPUT FILL-IN_STARTDAG,
                INPUT-OUTPUT FILL-IN_ARBANSVARIG, 
                INPUT-OUTPUT TABLE beratemp, INPUT-OUTPUT TABLE ansvaraotemp,
                OUTPUT TABLE felmeddtemp).
   OPEN QUERY BRW_ARBAN FOR EACH ansvaraotemp NO-LOCK.
   OPEN QUERY BRW_BERED FOR EACH beratemp NO-LOCK.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF felmeddtemp.VAL = 1 THEN DO:
         DELETE felmeddtemp.
         RUN AOKOPIA.W (OUTPUT TOG_BERKOP,
                     OUTPUT TOG_KALKOP,
                     OUTPUT TOG_KOPPKOP,OUTPUT TOG_TIDLKOP).   
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
         
         RUN aokopia IN nyttaoapph (INPUT aonrrec,INPUT aonrrec2,
                                    INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,
                                    INPUT Guru.Konstanter:globforetag,INPUT vadgora,INPUT Guru.Konstanter:globanv,
                                    INPUT TOG_BERKOP,INPUT TOG_KALKOP,INPUT TOG_KOPPKOP,
                                    OUTPUT TABLE beredningtemp,OUTPUT TABLE aovardtemp,
                                    OUTPUT TABLE varderingtemp,OUTPUT TABLE felmeddtemp).
         
         RUN konthmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT-OUTPUT TABLE aonrkonttemp).
         IF TOG_TIDLKOP = TRUE THEN DO:
            RUN tidlkop_UI IN nyttaoapph (INPUT aonrrec2,INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR).
         END.
         FOR EACH felmeddtemp NO-LOCK :
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            musz = TRUE.
            DELETE felmeddtemp.
         END.         
      END.
      ELSE DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         musz = TRUE.
         DELETE felmeddtemp.
         IF Guru.Konstanter:globforetag = "ORBI" THEN DO:
            ASSIGN
            FILL-IN_STARTDAG:HIDDEN IN FRAME FRAME-BARABREDD = FALSE
            FILL-IN_ARBANSVARIG:HIDDEN IN FRAME FRAME-BARABREDD = FALSE.      
         END.  
         RETURN.
      END.
   END.
   FIND FIRST projtemp WHERE projtemp.PERSONALKOD = FILL-IN_STARTDAG NO-ERROR.
   IF NOT AVAILABLE projtemp THEN DO:
      RUN projextra IN nyttaoapph (INPUT FILL-IN_STARTDAG,OUTPUT TABLE projtemp APPEND).
   END.
   RUN projextra2 IN nyttaoapph (INPUT tidpgamla).
   OPEN QUERY BRW_PROJ FOR EACH projtemp NO-LOCK.      
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF AVAILABLE uppaonrtemp THEN DO:
      
      IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
         IF FILL-IN_ORT:MODIFIED = TRUE OR FILL-IN_AONRAVDATUM:MODIFIED = TRUE THEN DO:
            uppaonrtemp.AUTOREG = TRUE.
         END.
      END.
      ELSE DO:
         uppaonrtemp.AUTOREG = TRUE.
      END.
      ASSIGN
      uppaonrtemp.FASTAAONR = FILL-IN_FASTAAONR 
      uppaonrtemp.ANM[1] = EDITOR_ANM                
      uppaonrtemp.ANLNR = FILL-IN_ANLNR              
      uppaonrtemp.AONR = FILL-IN_AONR                
      uppaonrtemp.ARBANSVARIG = FILL-IN_ARBANSVARIG  
      uppaonrtemp.ARBARTKOD = FILL-IN_ARBARTKOD      
      uppaonrtemp.ARBUPPG[1] = EDITOR_AUPP           
      uppaonrtemp.BEREDARE = FILL-IN_BEREDARE        
      uppaonrtemp.STARTDAG = FILL-IN_STARTDAG        
      uppaonrtemp.DELNR = FILL-IN_DELNR              
      uppaonrtemp.ORT = FILL-IN_ORT                  
      uppaonrtemp.PKOD = FILL-IN_PKOD 
      uppaonrtemp.FAKTTYP = CMB_FAK  
      uppaonrtemp.PRISTYP = CMB_DEBPR                
      uppaonrtemp.TRAKTAMENTE = FILL-IN_TRAKTAMENTE  
      uppaonrtemp.UTRYCKNING = FILL-IN_UTRYCKNING    
      uppaonrtemp.AONRAVDATUM = FILL-IN_AONRAVDATUM  
      uppaonrtemp.UTFARDAT = FILL-IN_UTFARDAT
      uppaonrtemp.ANVANDARE = FILL-IN_ANVANDARE.
      IF AVAILABLE bestkundallt THEN uppaonrtemp.BESTID = bestkundallt.BESTID.          
      IF FILL-IN_UTFARDAT NE "" THEN uppaonrtemp.ARBBESKED = TRUE.
      ELSE uppaonrtemp.ARBBESKED = FALSE.     
      RUN projextra4 IN nyttaoapph (INPUT uppaonrtemp.AONR, INPUT uppaonrtemp.DELNR, INPUT FILL-IN-REF,INPUT FILL-IN-KONAKT).      
      IF AVAILABLE omrtemp THEN ASSIGN uppaonrtemp.OMRADE = omrtemp.OMRADE.
      ELSE IF uppaonrtemp.FASTAAONR = TRUE AND LENGTH(uppaonrtemp.AONR) = 3 THEN uppaonrtemp.OMRADE = "".
      IF uppaonrtemp.AONRAVDATUM = ? THEN DO:
         FILL-IN_PAGAENDE:HIDDEN = FALSE.
      END.     
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      
      IF sparavdat = FILL-IN_AONRAVDATUM THEN FILL-IN_AONRAVDATUM:MODIFIED = FALSE.
      ELSE FILL-IN_AONRAVDATUM:MODIFIED = TRUE.
      CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKINT[1] = vadgora  
      valsoktemp.SOKINT[2] = aonrrec  
      valsoktemp.SOKINT[3] = aonrrec2 
      valsoktemp.SOKINT[4] = artalvar
     /* valsoktemp.SOKINT[5] = RAD_VAL*/
      valsoktemp.SOKCHAR[1] = Guru.Konstanter:globanv  
      valsoktemp.SOKCHAR[2] = FILL-IN_AONR 
      valsoktemp.SOKCHAR[3] = plannrvar
      valsoktemp.SOKCHAR[4] = omradespar
      valsoktemp.SOKCHAR[5] = tidagamla
      valsoktemp.SOKCHAR[7] = bestspar   
      valsoktemp.SOKLOG[1] = nyttaovar
      valsoktemp.SOKLOG[2] = FILL-IN_ORT:MODIFIED
      valsoktemp.SOKLOG[3] = FILL-IN_AONRAVDATUM:MODIFIED
      valsoktemp.SOKLOG[4] = TOG_BERKOP  
      valsoktemp.SOKLOG[5] = TOG_KALKOP  
      valsoktemp.SOKLOG[6] = TOG_KOPPKOP
      valsoktemp.SOKLOG[7] = TOG_TIDLKOP.
      {muswait.i}
      IF AVAILABLE bestkundallt THEN valsoktemp.SOKCHAR[6] = bestkundallt.BESTID.
      ELSE DO:
         valsoktemp.SOKCHAR[6] = bestspar.
      END.
      RUN btnokn IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,
                                INPUT TABLE valsoktemp,
                               INPUT-OUTPUT TABLE uppaonrtemp,
                               OUTPUT TABLE felmeddtemp).
      RUN relaonr IN nyttaoapph.
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
                 RETURN.
      END.
      ELSE DO:
         IF vartpro = "NAOPLAN" THEN musz = musz.
         ELSE DO:   
            FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
            valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE valdaao THEN DO:
               CREATE valdaao.
            END.
            BUFFER-COPY uppaonrtemp TO valdaao.
            FIND FIRST utsokaonr WHERE utsokaonr.AONR = aonrvar AND 
            utsokaonr.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE utsokaonr THEN DO:
               CREATE utsokaonr.
            END.
            BUFFER-COPY uppaonrtemp TO utsokaonr.
         END.
      END.
      nyttsparatao = TRUE.
      
   END.
   IF BTN_PAG:HIDDEN = FALSE THEN RUN vaonrfix IN nyttaoapph (INPUT-OUTPUT TABLE valdaao).
   ASSIGN
   aonrvar2 =  aonrvar 
   delnrvar2 = delnrvar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnstart_UI C-Win 
PROCEDURE btnstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   
   IF NOT VALID-HANDLE(Guru.SharedVariable:btnaonrh) THEN DO:
      CREATE whandltemp.
      ordningnr = 0.   
      ordningnr = ordningnr + 1.      
      RUN whandle_UI (INPUT ordningnr,C-WIN:HANDLE).    
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-AONR:HANDLE).    
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BARABREDD:HANDLE).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_AONR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_DELNR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,BTN_PAG:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_AONRAVDATUM:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,MBTN_ANM:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_FASTAAONR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ORT:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,EDITOR_AUPP:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,CMB_OMR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_BEST:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,MBTN_AVT:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,CMB_DEBPR:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-REF:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_STARTDAG:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_BEREDARE:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ARBANSVARIG:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_UTFARDAT:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,BTN_FASTTRAC:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,BTN_ACCESS:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,MBTN_DOK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_FAK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_KONT:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_KOPPL:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_OVR:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_TIDL:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,MBTN_TIDPERS:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,BTN_OK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,BTN_AVB:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN_PAGAENDE:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NAMN-1:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NAMN-2:HANDLE IN FRAME FRAME-BARABREDD). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-NAMN-3:HANDLE IN FRAME FRAME-BARABREDD).   
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BERPROJ:HANDLE). 
      RUN whandle_UI (INPUT ordningnr,BRW_PROJ:HANDLE IN FRAME FRAME-BERPROJ). 
      RUN whandle_UI (INPUT ordningnr,BRW_BERED:HANDLE IN FRAME FRAME-BERPROJ). 
      RUN whandle_UI (INPUT ordningnr,BRW_ARBAN:HANDLE IN FRAME FRAME-BERPROJ).    
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BEST:HANDLE). 
      RUN whandle_UI (INPUT ordningnr,CMB_BESTAVD:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,BRW_BESTAVD:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,BRW_BEST:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,FILL-IN-VALJBEST:HANDLE IN FRAME FRAME-BEST). 
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Info:HANDLE IN MENU POPUP-MENU-BRW_BEST).                                                      
      RUN whandle_UI (INPUT ordningnr, MENU-ITEM m_Info-2:HANDLE IN MENU POPUP-MENU-BRW_BEST-2).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KOPP:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_KOPP:HANDLE IN FRAME FRAME-KOPP). 
      RUN whandle_UI (INPUT ordningnr,BTN_KALKAR:HANDLE IN FRAME FRAME-KOPP).  
      RUN whandle_UI (INPUT ordningnr,BTN_AKTIV:HANDLE IN FRAME FRAME-KOPP).  
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPLAAONR:HANDLE IN FRAME FRAME-KOPP). 
      RUN whandle_UI (INPUT ordningnr,BTN_GATILL:HANDLE IN FRAME FRAME-KOPP). 
      RUN whandle_UI (INPUT ordningnr,gatill.STATUSNIV:HANDLE IN BROWSE BRW_KOPP). 
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-FAKT:HANDLE).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_FAKTNR:HANDLE IN FRAME FRAME-FAKT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_FAKTTYP:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ANVANDARE:HANDLE IN FRAME FRAME-FAKT).  
      RUN whandle_UI (INPUT ordningnr,CMB_FAK:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_INTERNT:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,FILL-IN_EXTERNT:HANDLE IN FRAME FRAME-FAKT).    
      RUN whandle_UI (INPUT ordningnr,BRW_ANV:HANDLE IN FRAME FRAME-FAKT).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-TILLAVTAL:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AVTAL:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_MAPPTV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_VISATV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_NYATV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDTV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_BDOCTIDTV:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_NYHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_BORTHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BRW_HAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_UPDAT1HAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-STARTDATHAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,TOG_KLAR:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,BTN_MINDAT1HAND:HANDLE IN FRAME FRAME-TILLAVTAL).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-TIDPERS:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_TIDPAONR:HANDLE IN FRAME FRAME-TIDPERS).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLTP:HANDLE IN FRAME FRAME-TIDPERS).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-TIDL:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AOTID:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_ANMTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_BESTTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_VISATIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLTIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,BTN_BORTATIDL:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-DOK:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,FILL-IN-ANM:HANDLE IN FRAME FRAME-TIDL).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-OVRIGT:HANDLE).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_TRAKTAMENTE:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,CMB_TRAK:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_UTRYCKNING:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_PKODOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ARBARTKODOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ANLNR:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,BRW_PRIOOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,BRW_ARBARTOV:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,BRW_ANL:HANDLE IN FRAME FRAME-OVRIGT).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-KONTO:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AONRKTO:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,FILL-IN_ARBARTKOD:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,BRW_ARBART:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLKONTO:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,BRW_PRIO:HANDLE IN FRAME FRAME-KONTO).  
      RUN whandle_UI (INPUT ordningnr,FILL-IN_PKOD:HANDLE IN FRAME FRAME-KONTO).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-DOK:HANDLE).
      RUN whandle_UI (INPUT ordningnr,BRW_AVTALDOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_MAPPDOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_VISADOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_NYADOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDADOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,BTN_BDOCTIDDOK:HANDLE IN FRAME FRAME-DOK).
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-ANM:HANDLE).
      RUN whandle_UI (INPUT ordningnr,EDITOR_ANM:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BRW_DAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_GATILLANM:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_NYDAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_ANDDAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BTN_BORTDAGBOK:HANDLE IN FRAME FRAME-ANM).
      RUN whandle_UI (INPUT ordningnr,BRW_PERS:HANDLE IN FRAME FRAME-BARABREDD).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Vlj_Personal:HANDLE IN MENU POPUP-MENU-FILL-IN_STARTDAG).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Vlj_Personal2:HANDLE IN MENU POPUP-MENU-FILL-IN_BEREDARE).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Vlj_Personal3:HANDLE IN MENU POPUP-MENU-FILL-IN_ARBANSVARIG).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Kund_information:HANDLE IN MENU POPUP-MENU-FILL-IN_BEST).   
      RUN whandle_UI (INPUT ordningnr,FILL-IN-KONAKT:HANDLE IN FRAME FRAME-BARABREDD).   
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Kund_information2:HANDLE IN MENU POPUP-MENU-FILL-IN-KONAKT).   
      RUN whandle_UI (INPUT ordningnr,BTN_VISADAGBOK:HANDLE IN FRAME FRAME-ANM). 
      RUN whandle_UI (INPUT ordningnr,BTN_UPPKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KALKKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KONVKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_EXPKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_IMPKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_UPPBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_BERBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_INKBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_LISTBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_ATGBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_LASBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_EXPBER:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_FAKFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_FLISTAFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KredFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_PRELBFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_UPPFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VFAKFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISAOFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_KOPPFAK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_UPPMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_FASTIGHETMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VARDMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISKALMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_VISMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_OMRAKNA:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,BTN_EXPMARK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr,MENU-ITEM m_Referens_personer:HANDLE IN MENU POPUP-MENU-FILL-IN_UTFARDAT).   
      RUN whandle_UI (INPUT ordningnr,FRAME FRAME-BEH:HANDLE). 
      RUN whandle_UI (INPUT ordningnr, MBTN_AOPERSSEK:HANDLE IN FRAME FRAME-AONR). 
      RUN whandle_UI (INPUT ordningnr, BRW_ANVOMR:HANDLE IN FRAME FRAME-BEH). 
      RUN whandle_UI (INPUT ordningnr, BTN_BEHORAND:HANDLE IN FRAME FRAME-BEH). 
      RUN whandle_UI (INPUT ordningnr, BTN_SCHAKTP:HANDLE IN FRAME FRAME-KOPP).      
      RUN whandle_UI (INPUT ordningnr, BTN_BERKALK:HANDLE IN FRAME FRAME-KOPP).
      RUN whandle_UI (INPUT ordningnr, CMB_DOKTYP:HANDLE IN FRAME FRAME-KOPP). /*DDD*/
      RUN whandle_UI (INPUT ordningnr, CMB_DOKSTATUS:HANDLE IN FRAME FRAME-KOPP).        
      RUN AONRM.P PERSISTENT SET Guru.SharedVariable:btnaonrh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp). 
   END.
   IF FILL-IN_AONRAVDATUM = 01/01/1991 THEN FILL-IN_AONRAVDATUM:HIDDEN = TRUE.
   RUN main_UI IN Guru.SharedVariable:btnaonrh. 
   RUN frame_UI (INPUT "AONR").
   RUN frame_UI (INPUT "KOPP").
   RUN vcbrwkopp_UI IN Guru.SharedVariable:btnaonrh.
   IF vartpro = "NAOPLAN" AND nyttaovar = TRUE THEN DO:
      FRAME FRAME-KOPP:HIDDEN = TRUE.
      DISABLE MBTN_DOK MBTN_FAK MBTN_KOPPL MBTN_TIDL MBTN_TIDPERS WITH FRAME FRAME-AONR.
      ASSIGN
      BRW_DAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE                  
      BTN_GATILLANM:HIDDEN IN FRAME FRAME-ANM = TRUE                   
      BTN_NYDAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE                   
      BTN_ANDDAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE                   
      BTN_BORTDAGBOK:HIDDEN IN FRAME FRAME-ANM = TRUE
      BTN_GATILLKONTO:HIDDEN IN FRAME FRAME-KONTO = TRUE.
   END.
      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvisa_UI C-Win 
PROCEDURE btnvisa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "fix3" VIEW-AS ALERT-BOX.
   /*
   IF RAD_VAL = 6 THEN DO:
      status-ok = BRW_AVTALDOK:SELECT-FOCUSED-ROW() IN FRAME FRAME-DOK NO-ERROR.
      IF AVAILABLE avtalaonrtemp THEN DO:         
         IF avtalaonrtemp.FIL = "" THEN DO:
            MESSAGE "Det finns ingen fil kopplad till dokumentet"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:            
            FILE-INFO:FILE-NAME = avtalaonrtemp.FIL.
            IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
               MESSAGE avtalaonrtemp.FIL " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool. Filtyp " 
               VIEW-AS ALERT-BOX.
               RETURN.
            END.
            RUN OPENDOC.P (avtalaonrtemp.FIL,"","",NO).
         END.      
      END.
      ELSE DO:
         MESSAGE "Det finns inget dokument att visa."
         VIEW-AS ALERT-BOX TITLE "Meddelande".        
         RETURN.
      END.
   END.
   ELSE DO:  
      status-ok = BRW_AVTAL:SELECT-FOCUSED-ROW() IN FRAME FRAME-TILLAVTAL NO-ERROR.
      IF AVAILABLE avtalaonrtemp THEN DO:                  
         IF avtalaonrtemp.FIL = "" THEN DO:
            MESSAGE "Det finns ingen fil kopplad till tillståndet/avtalet"
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:
            FILE-INFO:FILE-NAME = avtalaonrtemp.FIL.
            IF SEARCH(FILE-INFO:FULL-PATHNAME) = ? THEN DO:
               MESSAGE avtalaonrtemp.FIL " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool. Filtyp " 
               VIEW-AS ALERT-BOX.
               RETURN.
            END.
            RUN OPENDOC.P (avtalaonrtemp.FIL,"","",NO).
         END.      
      END.
      ELSE DO:
         MESSAGE "Det finns inget tillstånd/avtal att visa."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.   
   END.
   */
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
  ENABLE RECT-MENY MBTN_AOPERSSEK MBTN_KOPPL BTN_OK BTN_AVB MBTN_ANM MBTN_AVT 
         MBTN_DOK MBTN_FAK MBTN_KONT MBTN_OVR MBTN_TIDL MBTN_TIDPERS 
      WITH FRAME FRAME-AONR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-AONR}
  DISPLAY FILL-IN-TEXT CMB_AVD FILL-IN-PKOD FILL-IN_FORNAMN-2 CMB_OMR 
          FILL-IN-ENFLE FILL-IN-DAG FILL-IN-STARTAR FILL-IN-DAG-2 FILL-IN-SLUTAR 
          FILL-IN-AONR FILL-IN-DELNR FILL-IN-NAMN-1 FILL-IN-NAMN-2 
          FILL-IN-NAMN-3 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  ENABLE CMB_AVD RAD_FAST CMB_OMR FILL-IN-ENFLE FILL-IN-STARTAR FILL-IN-SLUTAR 
         FILL-IN-AONR BTN_SKAPEN FILL-IN-DELNR 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BARABREDD}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BERPROJ}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BEST}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-DOK}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FAKT}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KONTO}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KOPP}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-OVRIGT}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDL}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDPERS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryarban_UI C-Win 
PROCEDURE entryarban_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN frame_UI (INPUT "BERP").
   ASSIGN
   FILL-IN_ARBANSVARIG = INPUT FRAME FRAME-BARABREDD FILL-IN_ARBANSVARIG.
   FILL-IN-NAMN-3 = INPUT FILL-IN-NAMN-3.
   
   IF Guru.Konstanter:globforetag = "ORBI" THEN DO:
      ASSIGN
      BRW_PROJ:HIDDEN IN FRAME FRAME-BERPROJ = TRUE 
      BRW_ARBAN:HIDDEN IN FRAME FRAME-BERPROJ = TRUE.
   /*
      FILL-IN_STARTDAG:HIDDEN IN FRAME FRAME-BARABREDD = TRUE
      FILL-IN_ARBANSVARIG:HIDDEN IN FRAME FRAME-BARABREDD = TRUE.
      */      
   END.   
   ELSE DISPLAY FILL-IN_ARBANSVARIG FILL-IN-NAMN-3 WITH FRAME FRAME-BARABREDD.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frameh_UI C-Win 
PROCEDURE frameh_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS LOGICAL NO-UNDO. 
   ASSIGN
   FRAME FRAME-TILLAVTAL:HIDDEN = vilkenframe
   FRAME FRAME-TIDPERS:HIDDEN = vilkenframe
   FRAME FRAME-TIDL:HIDDEN = vilkenframe
   FRAME FRAME-OVRIGT:HIDDEN = vilkenframe
   FRAME FRAME-KONTO:HIDDEN = vilkenframe
   FRAME FRAME-KOPP:HIDDEN = vilkenframe
   FRAME FRAME-DOK:HIDDEN = vilkenframe
   FRAME FRAME-BEST:HIDDEN = vilkenframe
   FRAME FRAME-BERPROJ:HIDDEN = vilkenframe
   FRAME FRAME-ANM:HIDDEN = vilkenframe
   FRAME FRAME-FAKT:HIDDEN = vilkenframe
   FRAME FRAME-BEH:HIDDEN = vilkenframe.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frame_UI C-Win 
PROCEDURE frame_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS CHARACTER NO-UNDO. 
   RUN frameh_UI (INPUT TRUE).
   IF vilkenframe = "AONR" THEN DO:
      ASSIGN
      FRAME FRAME-BARABREDD:HIDDEN = FALSE
      FRAME FRAME-AONR:HIDDEN = FALSE.
   END.
   IF vilkenframe = "TAVT" THEN FRAME FRAME-TILLAVTAL:HIDDEN = FALSE.
   IF vilkenframe = "TPERS" THEN FRAME FRAME-TIDPERS:HIDDEN = FALSE.
   IF vilkenframe = "TIDL" THEN FRAME FRAME-TIDL:HIDDEN = FALSE.
   IF vilkenframe = "OVR" THEN FRAME FRAME-OVRIGT:HIDDEN = FALSE.
   IF vilkenframe = "KONT" THEN FRAME FRAME-KONTO:HIDDEN = FALSE.
   IF vilkenframe = "KOPP" THEN FRAME FRAME-KOPP:HIDDEN = FALSE.
   IF vilkenframe = "DOK" THEN FRAME FRAME-DOK:HIDDEN = FALSE.
   IF vilkenframe = "ANM" THEN FRAME FRAME-ANM:HIDDEN = FALSE.
   IF vilkenframe = "FAKT" THEN FRAME FRAME-FAKT:HIDDEN = FALSE.
   IF vilkenframe = "BEST" THEN FRAME FRAME-BEST:HIDDEN = FALSE.
   IF vilkenframe = "BERP" THEN FRAME FRAME-BERPROJ:HIDDEN = FALSE.
   IF vilkenframe = "BEH" THEN FRAME FRAME-BEH:HIDDEN = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI C-Win 
PROCEDURE huvud2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "ORBI" OR Guru.Konstanter:globforetag = "GRAN" {GLOBVES.I} THEN DO:
      DISPLAY FILL-IN_UTFARDAT WITH FRAME FRAME-BARABREDD.
      ENABLE FILL-IN_UTFARDAT WITH FRAME FRAME-BARABREDD.
            
   END.
   ELSE DO:
      ASSIGN FILL-IN_UTFARDAT:HIDDEN = TRUE.
      DISABLE FILL-IN_UTFARDAT WITH FRAME FRAME-BARABREDD.      
   END.
   /*dokument*/        
   IF Guru.Konstanter:globanv = "ELPAO" AND datornamn = "ACER" THEN DO: 
      ENABLE BTN_BDOCTIDDOK BTN_VISADOK BTN_MAPPDOK WITH FRAME FRAME-DOK.
      ENABLE BTN_ANMTIDL BTN_BESTTIDL WITH FRAME FRAME-TIDL.
   END.
   ELSE IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "TRAS" OR 
      Guru.Konstanter:globforetag = "PINN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ENABLE BTN_BDOCTIDDOK BTN_VISADOK BTN_MAPPDOK WITH FRAME FRAME-DOK.
      ENABLE BTN_ANMTIDL BTN_BESTTIDL WITH FRAME FRAME-TIDL.
   END.
   ELSE IF Guru.Konstanter:globforetag = "PELLE" {GLOBVES.I} THEN DO:
      ENABLE BTN_ANMTIDL BTN_BESTTIDL WITH FRAME FRAME-TIDL.
      ASSIGN
      BTN_KOPPTIDL:HIDDEN = TRUE     
      BTN_BORTATIDL:HIDDEN = TRUE 
      BTN_VISATIDL:HIDDEN = TRUE.       
   END.

   /*cccc DISABLE CMB_OMR DÅ OMRÅDET REDAN ÄR VALT*/   
   IF nrvalvar = FALSE THEN DISABLE FILL-IN_AONR CMB_OMR WITH FRAME FRAME-BARABREDD.
   ELSE ENABLE FILL-IN_AONR CMB_OMR WITH FRAME FRAME-BARABREDD.
   DISABLE FILL-IN_AONR WITH FRAME FRAME-BARABREDD.
   BTN_PAG:HIDDEN = TRUE.
   IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
      IF uppaonrtemp.AONRAVDATUM NE 01/01/91 THEN DO:
         IF uppaonrtemp.DELNR NE 0 THEN DO:
            RUN avslutkoll_UI IN nyttaoapph (INPUT uppaonrtemp.AONR,OUTPUT musz).
            IF musz = TRUE THEN DO:
               musz = FALSE.
               BTN_PAG:HIDDEN = FALSE.
               DISPLAY FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
               ASSIGN FILL-IN_PAGAENDE:HIDDEN = TRUE.
               IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
                  ENABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
               END.     
            END.
         END.
         ELSE DO:
            BTN_PAG:HIDDEN = FALSE.
            DISPLAY FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
            ASSIGN FILL-IN_PAGAENDE:HIDDEN = TRUE.
            IF Guru.Konstanter:aonrsekvar[9] = TRUE THEN DO:
               ENABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
            END.     
         END.
      END.
      ELSE BTN_PAG:HIDDEN = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR
   Guru.Konstanter:globforetag = "ETA" THEN DO:
      IF Guru.Konstanter:globniv = 0 OR Guru.Konstanter:globniv = 1 OR Guru.Konstanter:globniv = 2 OR Guru.Konstanter:globniv = 33 THEN musz = musz.
      ELSE DO:
         IF SUBSTRING(globomr,1,1) = "4" THEN musz = musz.
         ELSE DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.         
      END.
   END. 
   IF uppaonrtemp.FASTAAONR = TRUE THEN DO:
      musz = musz.      
   END.        
   ELSE IF uppaonrtemp.FASTAAONR = FALSE THEN DO:
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:
         DISABLE FILL-IN_FASTAAONR FILL-IN_AONR WITH FRAME FRAME-BARABREDD.
      END.       
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      DISABLE FILL-IN_FASTAAONR WITH FRAME FRAME-BARABREDD.         
   END.
   IF  Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      FILL-IN_AVDELNING:HIDDEN IN FRAME FRAME-BARABREDD = FALSE.         
   END.
   ELSE FILL-IN_AVDELNING:HIDDEN IN FRAME FRAME-BARABREDD = TRUE.
   IF uppaonrtemp.DELNR NE 0 THEN DISABLE FILL-IN_FASTAAONR WITH FRAME FRAME-BARABREDD. 
   IF nyttaovar = FALSE OR kopiavar = TRUE THEN DO:
      DISABLE FILL-IN_AONR WITH FRAME FRAME-BARABREDD.
   END.
   ASSIGN
   /*gatill.AKTIV:LABEL IN BROWSE BRW_KOPP = "Huvud"*/
   CMB_DEBPR:LABEL = Guru.Konstanter:gdebk
   CMB_OMR = INPUT CMB_OMR. 
   IF varforetypval[2] = 1 THEN DO:
      ASSIGN      
      CMB_TRAK:HIDDEN IN FRAME FRAME-OVRIGT = TRUE 
      FILL-IN_UTRYCKNING:HIDDEN = TRUE.
   END.
   /* undernummer skall ha samma område och beställare som huvudnumret för tidskrivande
   företag*/      
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GRAN" 
    OR Guru.Konstanter:globforetag = "GKAL"   THEN DO:   
      IF FILL-IN_DELNR NE 0 THEN DO:      
         DISABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
         DISABLE  CMB_BESTAVD BRW_BEST BRW_BESTAVD WITH FRAME FRAME-BEST.                       
      END.   
   END.
   IF nyttaovar = FALSE THEN DO:      
      IF FILL-IN_DELNR = 0 THEN DO:      
         ENABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
      END.
      IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "FORS" THEN DO:
         DISABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
      END.   
      ELSE IF varforetypval[2] = 1 THEN DO:
         /*ej tidskrivande företag skall kunna ändra område och beställare*/
         ENABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
         ENABLE  CMB_BESTAVD BRW_BEST BRW_BESTAVD WITH FRAME FRAME-BEST.                                    
      END.
      ELSE DO:
         RUN btnok IN bortaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_delnr,OUTPUT musz).
         IF Guru.Konstanter:globanv = "ELPAO" THEN musz = FALSE.
         IF musz = TRUE THEN DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
         musz = FALSE.
      END.
      IF Guru.Konstanter:globforetag = "UMEA" THEN DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
      /* Om det finns en beredning får den ligga kvar på orginalområde
      men aonr får inte ha fakturaplan*/
      IF uppaonrtemp.FAKTNR NE 0 THEN DO:
         DISABLE CMB_OMR  WITH FRAME FRAME-BARABREDD.
         DISABLE CMB_BESTAVD BRW_BEST BRW_BESTAVD WITH FRAME FRAME-BEST.                                
      END.                  
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI C-Win 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   C-WIN:TITLE = "Guru-" + Guru.Konstanter:gaol.               
   /*här nytt aonr*/
   
   IF nyttaovar = TRUE THEN DO:      
      {NRAOSKAPA.I}         
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR 
      Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO: 
         nrvalvar = TRUE.
      END.
      ELSE DO:
         nrvalvar = FALSE.
      END.
      delnrvar = 0.      
      RUN hamtaonr IN nyttaoapph (INPUT aonrrec,OUTPUT aonrvar,OUTPUT delnrvar,
                                  OUTPUT omrvar,OUTPUT bestvar,OUTPUT radfast,
                                  OUTPUT autoregvar).
      ASSIGN
      aonrvar2 =  aonrvar 
      delnrvar2 = delnrvar.
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE uppaonrtemp THEN DO:
         EMPTY TEMP-TABLE uppaonrtemp NO-ERROR. 
         CREATE uppaonrtemp.
         uppaonrtemp.PLANNR = ?.
      END.
/*       FIND FIRST uppaonrtemp NO-LOCK NO-ERROR. */
      ASSIGN 
      
      uppaonrtemp.AONRAVDATUM = 01/01/1991
      uppaonrtemp.AONR = aonrvar
      uppaonrtemp.DELNR = delnrvar
      uppaonrtemp.OMRADE = omrvar
      uppaonrtemp.BESTID = bestvar
      uppaonrtemp.FASTAAONR = radfast
      uppaonrtemp.AUTOREG = autoregvar.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  OR Guru.Konstanter:globforetag = "cELPA" THEN DO:  
         IF plannrvar = ? AND nyttaovar = TRUE  THEN DO:
            IF aonrvar BEGINS "38" OR aonrvar BEGINS "36" OR aonrvar BEGINS "46" OR aonrvar BEGINS "63" OR aonrvar BEGINS "79"  THEN DO:
               FIND FIRST omrtemp WHERE omrtemp.OMRADE = "1611" USE-INDEX OMR NO-LOCK NO-ERROR.
               IF AVAILABLE omrtemp THEN DO:
                  uppaonrtemp.OMRADE = omrtemp.OMRADE.               
               END.                                 
            END.
                     
         END.        
      END.
      ASSIGN
      sparavdat = uppaonrtemp.AONRAVDATUM
      FILL-IN_AONRAVDATUM = uppaonrtemp.AONRAVDATUM
      FILL-IN_AONR = uppaonrtemp.AONR 
      FILL-IN_DELNR = uppaonrtemp.DELNR.       
      IF kopiavar = TRUE THEN DO:
         RUN AOKOPIA.W (OUTPUT TOG_BERKOP,
                        OUTPUT TOG_KALKOP,
                        OUTPUT TOG_KOPPKOP,OUTPUT TOG_TIDLKOP).
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
         RUN aokopia IN nyttaoapph (INPUT aonrrec,INPUT aonrrec2,
                                    INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,
                                    INPUT Guru.Konstanter:globforetag,INPUT vadgora,INPUT Guru.Konstanter:globanv,
                                    INPUT TOG_BERKOP,INPUT TOG_KALKOP,INPUT TOG_KOPPKOP,
                                    OUTPUT TABLE beredningtemp,OUTPUT TABLE aovardtemp,
                                    OUTPUT TABLE varderingtemp,OUTPUT TABLE felmeddtemp).
         
         RUN konthmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,INPUT-OUTPUT TABLE aonrkonttemp).
         IF TOG_TIDLKOP = TRUE THEN DO:
            RUN tidlkop_UI IN nyttaoapph (INPUT aonrrec2,INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR).
         END.
            
         RUN kopupp_UI IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE uppaonrtemp).
         nyttaovar = FALSE.         
         FOR EACH felmeddtemp NO-LOCK :
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            DELETE felmeddtemp.
         END.
      END.
   END.
   ELSE DO:
      FIND uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.             
   END.
   RUN hamtaorec IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,OUTPUT aonrrec).
   
   IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MALDATUM").
      END.
      ELSE DO:
         RUN TLAGAUTO.P  
         (INPUT Guru.Konstanter:globanv, INPUT aonrrec, INPUT "MALDATUM").                  
      END.    
      RUN laddaaotid IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,OUTPUT TABLE aotidslagtemp).
   END.
        
   RUN laddao_UI.    
   IF FILL-IN_UTFARDAT = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_UTFARDAT = anvandartemp.AV-NAMN.
         END.      
      END.
   END.      
   RUN projextra3 IN nyttaoapph (INPUT uppaonrtemp.AONR, INPUT uppaonrtemp.DELNR, OUTPUT refvar,OUTPUT FILL-IN-KONAKT).   
   ASSIGN   
   FILL-IN-REF = refvar.  
   DISPLAY FILL-IN-REF FILL-IN-KONAKT WITH FRAME FRAME-BARABREDD.

   IF FILL-IN_ARBANSVARIG = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_ARBANSVARIG = anvandartemp.PERSONALKOD.
            FILL-IN-NAMN-3 = anvandartemp.AV-NAMN.
         END.      
      END.
   END.
   IF FILL-IN_BEREDARE = "" THEN DO:
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_BEREDARE = anvandartemp.PERSONALKOD.
            FILL-IN-NAMN-2 = anvandartemp.AV-NAMN.
         END.      
      END.       
   END.  
   IF FILL-IN_STARTDAG = "" THEN DO:      
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      IF AVAILABLE anvandartemp THEN DO:
         IF anvandartemp.PERSONALKOD NE "" THEN DO: 
            FILL-IN_STARTDAG = anvandartemp.PERSONALKOD.
            FILL-IN-NAMN-1 = anvandartemp.AV-NAMN.
         END.      
         ELSE FILL-IN_STARTDAG = FILL-IN_BEREDARE.
      END. 
      ELSE FILL-IN_STARTDAG = FILL-IN_BEREDARE.
   END.
   ELSE DO:
      IF FILL-IN_STARTDAG = "mån" OR 
      FILL-IN_STARTDAG = "tis" OR 
      FILL-IN_STARTDAG = "ons" OR 
      FILL-IN_STARTDAG = "tor" OR 
      FILL-IN_STARTDAG = "fre" OR 
      FILL-IN_STARTDAG = "lör" OR 
      FILL-IN_STARTDAG = "sön" THEN DO:
         FILL-IN_STARTDAG = FILL-IN_BEREDARE.
      END.
   END.
   IF kalkrec = ? THEN DO:
      musz = musz.
   END.   
   ELSE DO:
      FIND FIRST fastspectemp WHERE fastspectemp.FASTSPECREC = kalkrec NO-LOCK NO-ERROR.
   END.
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 2.
      {BESTHMT.I}
      /*HÄR EJ OM BESTÄLLAR EJ ÄR EGNA ORG = FAKT*/
      IF varforetypval[3] >= 1 OR vartpro = "NAOPLAN" THEN DO:

      END.
      ELSE DO:
         OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
         GET FIRST oq NO-LOCK.
         DO WHILE AVAILABLE(omrtemp):
            CREATE bestkundallt.
            ASSIGN 
            bestkundallt.VIBESTID = omrtemp.OMRADE 
            bestkundallt.BESTID = omrtemp.OMRADE 
            bestkundallt.BESTNAMN = omrtemp.NAMN.                      
            CREATE bestkundextra.
            ASSIGN 
            bestkundextra.VIBESTID = omrtemp.OMRADE 
            bestkundextra.BESTID = omrtemp.OMRADE 
            bestkundextra.BESTNAMN = omrtemp.NAMN
            bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
            GET NEXT oq NO-LOCK.
         END.
      END.
   END.
   IF vartpro = "NAOPLAN" THEN musz = musz.
   ELSE DO:
      /*visa bara eget bolags områden*/
      IF nyttaovar = TRUE THEN DO:         
         RUN anvavdhmt IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT "",INPUT "",OUTPUT jid, OUTPUT TABLE eavdtemp ).                 
      END.
      ELSE DO:
         RUN anvavdhmt IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT "",INPUT uppaonrtemp.OMRADE,OUTPUT jid, OUTPUT TABLE eavdtemp ).                 
      END.
      FOR EACH eavdtemp,         
      EACH omrtemp WHERE omrtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
      END.
      
   END.
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   ASSIGN
   aonrkonttemp.K1:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2))
   aonrkonttemp.K2:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2))
   aonrkonttemp.K3:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K3,1,1)) + LC(SUBSTRING(kbenamntemp.K3,2))
   aonrkonttemp.K4:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K4,1,1)) + LC(SUBSTRING(kbenamntemp.K4,2))
   aonrkonttemp.K5:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbenamntemp.K5,1,1)) + LC(SUBSTRING(kbenamntemp.K5,2)).
   
   
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ASSIGN
      CMB_OMR:LABEL IN FRAME FRAME-BARABREDD =  gomrk
      FILL-IN_AVDELNING:LABEL IN FRAME FRAME-BARABREDD =  gavdk.
   END.  
   ELSE CMB_OMR:LABEL IN FRAME FRAME-BARABREDD = gutfk + " " + LC(Guru.Konstanter:gomrk).
   ASSIGN
    
   FILL-IN_BEST:LABEL = Guru.Konstanter:gbestk
   FILL-IN_AONR:LABEL = Guru.Konstanter:gaok            
   FILL-IN_STARTDAG:LABEL = gprojl
   CMB_BESTAVD:LABEL IN FRAME FRAME-BEST= Guru.Konstanter:gavdk
   bestkundallt.VIBESTID:LABEL IN BROWSE BRW_BEST = Guru.Konstanter:gbestk
   bestkundextra.VIBESTID:LABEL IN BROWSE BRW_BESTAVD = Guru.Konstanter:gbestk
   BRW_BEST:TITLE = "Välj " + LC(Guru.Konstanter:gbestk)
   BRW_BESTAVD:TITLE = "Välj " + LC(Guru.Konstanter:gbestk)
   BRW_ARBAN:TITLE IN FRAME FRAME-BERPROJ = "Välj " + LC(Guru.Konstanter:garbal)
   BRW_BERED:TITLE = "Välj " + LC(Guru.Konstanter:gberek)
   BRW_AONRKTO:TITLE IN FRAME FRAME-KONTO = "Kontosträngar lagrade på " + LC(Guru.Konstanter:gaok) + " :et"
   FILL-IN_ARBANSVARIG:LABEL = Guru.Konstanter:garbal 
   FILL-IN_BEREDARE:LABEL = Guru.Konstanter:gberel.  
   IF vartpro = "NAOPLAN" THEN DO:     
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.BESTID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN DO:
         CREATE bestkundallt.
         ASSIGN 
         bestkundallt.VIBESTID = omrtemp.OMRADE 
         bestkundallt.BESTID = omrtemp.OMRADE 
         bestkundallt.BESTNAMN = omrtemp.NAMN.                      
         CREATE bestkundextra.
         ASSIGN 
         bestkundextra.VIBESTID = omrtemp.OMRADE 
         bestkundextra.BESTID = omrtemp.OMRADE 
         bestkundextra.BESTNAMN = omrtemp.NAMN
         bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
      END.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.         
      END.
   END.
   ELSE DO:     
      IF varforetypval[3] >= 1 THEN DO:
         IF uppaonrtemp.OMRADE = "" THEN .
         ELSE DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               FIND FIRST bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE bestkundallt THEN DO:
                  CREATE bestkundallt.
                  ASSIGN 
                  bestkundallt.VIBESTID = omrtemp.OMRADE 
                  bestkundallt.BESTID = omrtemp.OMRADE 
                  bestkundallt.BESTNAMN = omrtemp.NAMN.                      
                  CREATE bestkundextra.
                  ASSIGN 
                  bestkundextra.VIBESTID = omrtemp.OMRADE 
                  bestkundextra.BESTID = omrtemp.OMRADE 
                  bestkundextra.BESTNAMN = omrtemp.NAMN
                  bestkundextra.AVDELNING = STRING(omrtemp.AVDELNINGNR).
               END.
            END.
         END.
      END.             
      IF uppaonrtemp.FASTAAONR = TRUE THEN status-ok = CMB_OMR:ADD-LAST("").
      ASSIGN
      gambestnamn = ""
      gamomrnamn = "".
      IF uppaonrtemp.FASTAAONR = FALSE THEN DO:
         IF uppaonrtemp.OMRADE = "" THEN DO:
            status-ok = CMB_OMR:ADD-LAST("").
            ASSIGN  CMB_OMR:SCREEN-VALUE = " ".
         END.
         ELSE DO:
            FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE
            USE-INDEX OMR NO-LOCK NO-ERROR.
            IF AVAILABLE omrtemp THEN DO:
               ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
            END.
            ELSE DO:
               RUN gamomr IN nyttaoapph (INPUT uppaonrtemp.OMRADE, OUTPUT gamomrnamn).
               IF gamomrnamn NE "" THEN DO:
                  status-ok = CMB_OMR:ADD-LAST(gamomrnamn) IN FRAME FRAME-BARABREDD.
                  ASSIGN  CMB_OMR:SCREEN-VALUE = gamomrnamn.
               END.
            END.
            FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.BESTID
            NO-LOCK NO-ERROR.
            IF AVAILABLE bestkundallt THEN DO:
               FILL-IN_BEST = bestkundallt.BESTNAMN.                                        
            END.
            ELSE DO:        
               RUN gambest IN nyttaoapph (INPUT uppaonrtemp.BESTID, OUTPUT gambestnamn).
               IF gambestnamn NE "" THEN FILL-IN_BEST = gambestnamn.
               ELSE IF gamomrnamn NE "" THEN DO:
                  FILL-IN_BEST = gamomrnamn.                            
               END.               
            END.
            DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD.  
         END.         
      END.
      ELSE DO:
         IF uppaonrtemp.OMRADE = "" THEN DO:
            ASSIGN  CMB_OMR:SCREEN-VALUE = " ".
         END.
         ELSE DO:
             FIND FIRST omrtemp WHERE omrtemp.OMRADE = uppaonrtemp.OMRADE
             USE-INDEX OMR NO-LOCK NO-ERROR.
             ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
         END.
      END.
      
   END.
   OPEN QUERY aq FOR EACH automregtemp USE-INDEX PRISTYP NO-LOCK.
   GET FIRST aq NO-LOCK.
   DO WHILE AVAILABLE(automregtemp):
      status-ok = CMB_DEBPR:ADD-LAST(automregtemp.PRISTYP).
      GET NEXT aq NO-LOCK.
   END.
   
   FIND FIRST automregtemp WHERE
   automregtemp.PRISTYP = "TOT.PRIS." USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:      
     FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
     IF AVAILABLE judavdtemp THEN DO:
        IF uppaonrtemp.FASTAAONR = TRUE AND judavdtemp.JUDID = "ELNÄT" THEN DO:
           FIND FIRST automregtemp WHERE
           automregtemp.PRISTYP = "EJ.KOSTN." USE-INDEX PRISTYP NO-LOCK NO-ERROR.
        END.
     END.
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:      
     FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
     IF AVAILABLE judavdtemp THEN DO:
        IF uppaonrtemp.FASTAAONR = TRUE AND judavdtemp.JUDID = "EL" THEN DO:
           FIND FIRST automregtemp WHERE
           automregtemp.PRISTYP = "EJ.KOSTN." USE-INDEX PRISTYP NO-LOCK NO-ERROR.
        END.
     END.
   END.
   IF uppaonrtemp.FASTAAONR = TRUE THEN DO:
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = uppaonrtemp.BESTID
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN DO:
         FIND FIRST bestkundallt NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD. 
         
      END.
   END.
   ASSIGN  CMB_DEBPR:SCREEN-VALUE = automregtemp.PRISTYP.
   IF nyttaovar = FALSE THEN DO:
      ASSIGN  CMB_DEBPR:SCREEN-VALUE = uppaonrtemp.PRISTYP.
   END.
   
   IF kalkrec = ? THEN DO:
      musz = musz.
   END.
   ELSE DO:
      /*SKAPA AONR FRÅN KALK*/
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = fastspectemp.OMRADE
      USE-INDEX OMR NO-LOCK NO-ERROR.
      ASSIGN
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = fastspectemp.BESTID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN FIND FIRST bestkundallt NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD. 
         
      END.
      
      FILL-IN_ORT = fastspectemp.BENAMNING.
   END.
   IF Guru.Konstanter:globforetag = "SOLE" THEN DO:
      status-ok = CMB_TRAK:ADD-LAST("2") IN FRAME FRAME-OVRIGT.
   END.
   ASSIGN
   CMB_TRAK:SCREEN-VALUE = STRING(uppaonrtemp.TRAKTAMENTE).
   CMB_TRAK = INPUT CMB_TRAK.
   IF FILL-IN_ANLNR = "" THEN DO:
      FIND FIRST anlaggtemp USE-INDEX ANLAGGNING NO-LOCK NO-ERROR.
      IF AVAILABLE anlaggtemp THEN ASSIGN FILL-IN_ANLNR = anlaggtemp.ANLNR.
   END.      
   IF FILL-IN_ARBARTKOD = ? OR FILL-IN_ARBARTKOD = 0 THEN DO:
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
        IF aonrvar BEGINS "38" OR aonrvar BEGINS "36" OR aonrvar BEGINS "46" OR aonrvar BEGINS "63" OR aonrvar BEGINS "79"  THEN DO:
           IF aonrvar BEGINS "386" THEN akoden = 220.
           ELSE IF aonrvar BEGINS "387" THEN akoden = 221.
           ELSE IF aonrvar BEGINS "388" THEN akoden = 222.
           ELSE IF aonrvar BEGINS "360" THEN akoden = 230.
           ELSE IF aonrvar BEGINS "361" THEN akoden = 231.
           ELSE IF aonrvar BEGINS "362" THEN akoden = 232.
           ELSE IF aonrvar BEGINS "363" THEN akoden = 232.
           ELSE IF aonrvar BEGINS "460" THEN akoden = 233.
           ELSE IF aonrvar BEGINS "461" THEN akoden = 233.
           ELSE IF aonrvar BEGINS "462" THEN akoden = 233.

           ELSE IF aonrvar BEGINS "632" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "633" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "634" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "635" THEN akoden = 234.
           ELSE IF aonrvar BEGINS "790" THEN akoden = 234.

           ELSE akoden = 1.
           FIND FIRST arbarttemp WHERE arbarttemp.ARBARTKOD = akoden  USE-INDEX ARB NO-LOCK NO-ERROR.
           IF NOT AVAILABLE arbarttemp THEN DO:
              FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
           END.
           IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.

        END.
        ELSE DO:
           FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
           IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
        END.
      END.
      ELSE DO:      
         FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
         IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
      END.
   END.
   
   IF FILL-IN_PKOD = ? OR FILL-IN_PKOD = 0 THEN DO:
      FIND FIRST priotemp USE-INDEX PRI NO-LOCK NO-ERROR.
      IF AVAILABLE priotemp THEN ASSIGN FILL-IN_PKOD = priotemp.PKOD.
   END.
   ASSIGN
   EDITOR_AUPP = uppaonrtemp.ARBUPPG[1]
   EDITOR_ANM = uppaonrtemp.ANM[1].
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   FOR EACH faktyptemp WHERE faktyptemp.ORDNING >= 0:
      status-ok = CMB_FAK:ADD-LAST(faktyptemp.VIFAKTTYP) IN FRAME FRAME-FAKT.
   END.
   IF uppaonrtemp.ANVANDARE = "" THEN ASSIGN FILL-IN_ANVANDARE = Guru.Konstanter:globanv.
   ELSE FILL-IN_ANVANDARE = uppaonrtemp.ANVANDARE.
   IF uppaonrtemp.FAKTTYP = "" THEN DO:
      IF Guru.Konstanter:globforetag = "ELPAc" {GLOBVES.I} THEN DO:
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Löpande räkning" NO-ERROR.
         FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
         uppaonrtemp.FAKTTYP  = faktyptemp.FAKTTYP.
      END.
      ELSE DO:
         FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Faktureras ej" NO-ERROR.
         FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
      END.
   END.
   ELSE DO:
      FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = uppaonrtemp.FAKTTYP NO-ERROR.
      IF NOT AVAILABLE faktyptemp THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
            FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = "Löpande räkning" NO-ERROR.
            FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
            uppaonrtemp.FAKTTYP  = faktyptemp.FAKTTYP.
         END.
      END.
      ELSE FILL-IN_FAKTTYP = faktyptemp.VIFAKTTYP.
   END.
   ASSIGN
   FILL-IN_INTERNT = uppaonrtemp.BETNR /*- uppaonrtemp.STARTVNR*/
   FILL-IN_EXTERNT = uppaonrtemp.STARTVNR.
   IF varforetypval[10] = 1 THEN DO:     
      DISPLAY FILL-IN_INTERNT FILL-IN_EXTERNT WITH FRAME FRAME-FAKT.
   END.
   ELSE DO:
      DISPLAY FILL-IN_INTERNT  WITH FRAME FRAME-FAKT.
   END.
   ASSIGN
   FILL-IN_FAKTNR = uppaonrtemp.FAKTNR.
   ASSIGN CMB_FAK:SCREEN-VALUE = FILL-IN_FAKTTYP.
   FILL-IN_FASTAAONR = uppaonrtemp.FASTAAONR.
   gfastkh = REPLACE(Guru.Konstanter:gfastl,"/","-").
   gtillkh = REPLACE(Guru.Konstanter:gtillk,"/","-").
   FILL-IN_FASTAAONR:FORMAT = gfastkh + "/" + gtillkh.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddao_UI C-Win 
PROCEDURE laddao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   IF nyttaovar = TRUE OR plannrvar NE ? THEN DO:
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 51
      soktemp.SOKCHAR[1] = plannrvar
      soktemp.SOKINT[1] = artalvar.
      {SOKANROP.I}
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = soktemp.SOKCHAR[1]
      USE-INDEX OMR NO-LOCK NO-ERROR.
      IF AVAILABLE omrtemp THEN DO:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME FRAME-BARABREDD.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
         uppaonrtemp.OMRADE = omrtemp.OMRADE.      
         
      END.
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = soktemp.SOKCHAR[2]
      NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME FRAME-BARABREDD. 
         uppaonrtemp.BESTID = bestkundallt.BESTID.
      END.
      IF nyttaovar = TRUE THEN DO:
         ASSIGN
         FILL-IN_ARBARTKODOV = soktemp.SOKINT[2]
         FILL-IN_ARBARTKOD = soktemp.SOKINT[2]
         FILL-IN_ORT = soktemp.SOKCHAR[3] 
         FILL-IN_ARBANSVARIG = soktemp.SOKCHAR[4]
         FILL-IN_BEREDARE = soktemp.SOKCHAR[4].        
      END.
      
   END.
   IF nyttaovar = FALSE THEN DO:
      ASSIGN
      sparavdat = uppaonrtemp.AONRAVDATUM
      FILL-IN_AONRAVDATUM = uppaonrtemp.AONRAVDATUM
      FILL-IN_ANLNR = uppaonrtemp.ANLNR 
      FILL-IN_AONR = uppaonrtemp.AONR 
      FILL-IN_ARBANSVARIG = uppaonrtemp.ARBANSVARIG
      FILL-IN_ARBARTKODOV = uppaonrtemp.ARBARTKOD
      FILL-IN_ARBARTKOD = uppaonrtemp.ARBARTKOD
      FILL-IN_BEREDARE = uppaonrtemp.BEREDARE
      tidagamla = uppaonrtemp.ARBANSVARIG
      tidbgamla = uppaonrtemp.BEREDARE 
      FILL-IN_STARTDAG = uppaonrtemp.STARTDAG
      tidpgamla = uppaonrtemp.STARTDAG
      FILL-IN_DELNR = uppaonrtemp.DELNR
      FILL-IN_ORT = uppaonrtemp.ORT
      FILL-IN_PKODOV = uppaonrtemp.PKOD
      FILL-IN_PKOD = uppaonrtemp.PKOD 
      FILL-IN_TRAKTAMENTE = uppaonrtemp.TRAKTAMENTE
      FILL-IN_UTRYCKNING = uppaonrtemp.UTRYCKNING
      FILL-IN_UTFARDAT = uppaonrtemp.UTFARDAT.  
   END.
   mappvar = "".   
   mappvar = uppaonrtemp.AONR + STRING(uppaonrtemp.DELNR).
   mappvarhj = grundmappvar +  mappvar.     
   IF grundmappvar NE "" THEN DO:
      RUN mappvar IN nyttaoejapph (INPUT mappvarhj).      
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE main2_UI C-Win 
PROCEDURE main2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FILL-IN-REF:LABEL IN FRAME FRAME-BARABREDD = Guru.Konstanter:grefbefk.
   ENABLE FILL-IN-REF FILL-IN-KONAKT WITH FRAME FRAME-BARABREDD.
  
   FOR EACH avdtemp:
      status-ok = CMB_BESTAVD:ADD-LAST(avdtemp.AVDELNINGNAMN) IN FRAME FRAME-BEST.
   END.
   
   IF CMB_BESTAVD:NUM-ITEMS > 18 THEN CMB_BESTAVD:INNER-LINES = 18.
   ELSE CMB_BESTAVD:INNER-LINES = CMB_BESTAVD:NUM-ITEMS.
   
   CMB_BESTAVD:SCREEN-VALUE = "Alla".
   CMB_BESTAVD:LABEL= Guru.Konstanter:gavdk.
   RUN huvud2_UI.   
   FILL-IN_ORT:MODIFIED = FALSE.
   FILL-IN_AONRAVDATUM:MODIFIED = FALSE.
   {musarrow.i} 
   IF delnrvar NE 0 THEN DO: 
      reslog = ?.
      RUN delnrcheck IN nyttaoapph (INPUT aonrvar, OUTPUT reslog).
      IF reslog = FALSE THEN DISABLE FILL-IN_AONRAVDATUM WITH FRAME FRAME-BARABREDD.
   END.   
   IF vartpro = "NAOPLAN" THEN DO:
      ASSIGN
      BTN_ACCESS:HIDDEN = TRUE 
      BTN_KALKAR:HIDDEN IN FRAME FRAME-KOPP = TRUE
      BTN_AKTIV:HIDDEN = TRUE 
      BTN_ANDADOK:HIDDEN IN FRAME FRAME-DOK = TRUE 
      BTN_BORTATIDL:HIDDEN IN FRAME FRAME-TIDL = TRUE 
      BTN_FASTTRAC:HIDDEN = TRUE 
      BTN_GATILL:HIDDEN = TRUE          
      BTN_BDOCTIDTV:HIDDEN IN FRAME FRAME-TILLAVTAL = TRUE
      BTN_KOPPTIDL:HIDDEN = TRUE 
      BTN_ANMTIDL:HIDDEN = TRUE 
      BTN_BESTTIDL:HIDDEN = TRUE 
      BTN_KOPPLAAONR:HIDDEN = TRUE
      BTN_BERKALK:HIDDEN = TRUE 
      BTN_NYADOK:HIDDEN = TRUE 
      BTN_VISADOK:HIDDEN = TRUE 
      BTN_VISATIDL:HIDDEN = TRUE
      BTN_MAPPDOK:HIDDEN = TRUE. 
      IF nyttaovar = TRUE THEN DISABLE CMB_OMR FILL-IN_BEST WITH FRAME FRAME-BARABREDD.
      ELSE DISABLE CMB_OMR WITH FRAME FRAME-BARABREDD.
   END.
   IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
      DISABLE BRW_KOPP  WITH FRAME FRAME-KOPP. 
   END.
   FIND FIRST projtemp  WHERE projtemp.PERSONALKOD = FILL-IN_STARTDAG NO-LOCK NO-ERROR.
   IF AVAILABLE projtemp THEN DO:
      FILL-IN-NAMN-1 = projtemp.FORNAMN + " " + projtemp.EFTERNAMN.
   END.
   FIND FIRST beratemp  WHERE beratemp.PERSONALKOD = FILL-IN_BEREDARE NO-LOCK NO-ERROR.
   IF AVAILABLE beratemp THEN DO:
      FILL-IN-NAMN-2 = beratemp.FORNAMN + " " + beratemp.EFTERNAMN.
   END.
   FIND FIRST ansvaraotemp  WHERE ansvaraotemp.PERSONALKOD = FILL-IN_ARBANSVARIG NO-LOCK NO-ERROR.
   IF AVAILABLE ansvaraotemp THEN DO:
      FILL-IN-NAMN-3 = ansvaraotemp.FORNAMN + " " + ansvaraotemp.EFTERNAMN.
   END.
   DISPLAY FILL-IN_STARTDAG FILL-IN-NAMN-1 FILL-IN-NAMN-2 FILL-IN-NAMN-3
      WITH FRAME FRAME-BARABREDD.   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE menyrad_UI C-Win 
PROCEDURE menyrad_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN
   MBTN_KONT:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_FAK:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_DOK:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_AVT:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_OVR:HIDDEN IN FRAME FRAME-AONR = TRUE
   MBTN_TIDPERS:HIDDEN IN FRAME FRAME-AONR = TRUE.     
   MBTN_KOPPL:LOAD-IMAGE("BILDER\xbtn_kopp.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_TIDL:LOAD-IMAGE("BILDER\xbtn_tidlage.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_ANM:LOAD-IMAGE("BILDER\xbtn_anmdag.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_AOPERSSEK:LOAD-IMAGE("BILDER\xbtn_pbehor.gif") IN FRAME FRAME-AONR NO-ERROR.
   /*BTN_LASBER:LOAD-IMAGE("BILDER\xbtn_lastaber.gif") IN FRAME FRAME-AONR NO-ERROR./*ccc*/*/
   
   Guru.GlobalaVariabler:collefth = MBTN_KOPPL:HANDLE. 
   Guru.GlobalaVariabler:colrighth = MBTN_TIDL:HANDLE.           
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   MBTN_ANM:HIDDEN IN FRAME FRAME-AONR = FALSE.
   Guru.GlobalaVariabler:colrighth = MBTN_ANM:HANDLE.           
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).           
   IF varforetypval[2] = 0 THEN DO:
      MBTN_KONT:LOAD-IMAGE("BILDER\xbtn_konto.gif") IN FRAME FRAME-AONR NO-ERROR.
      MBTN_KONT:HIDDEN IN FRAME FRAME-AONR = FALSE.
      Guru.GlobalaVariabler:colrighth = MBTN_KONT:HANDLE.           
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.
   IF (varforetypval[3] >= 1 AND varforetypval[3] < 4) OR Guru.Konstanter:globforetag = "VAST" OR 
       
       Guru.Konstanter:globforetag = "PINN" THEN DO: 
      MBTN_FAK:LOAD-IMAGE("BILDER\xbtn_faktura.gif") IN FRAME FRAME-AONR NO-ERROR.
      MBTN_FAK:HIDDEN IN FRAME FRAME-AONR = FALSE.
      Guru.GlobalaVariabler:colrighth = MBTN_FAK:HANDLE. 
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
   END.
   /*DOKUMENT*/
   musz = FALSE.
   IF Guru.Konstanter:globanv = "ELPAO" AND datornamn = "ACER" THEN DO: 
      musz = TRUE.
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ALTE" OR  
      Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "VAST" OR  
       Guru.Konstanter:globforetag = "VNAT" OR Guru.Konstanter:globforetag = "BORL" OR 
      Guru.Konstanter:globforetag = "STRA" OR Guru.Konstanter:globforetag = "TECT" THEN DO:
      grundmappvar = "".
   END.
   IF grundmappvar NE "" THEN DO:
      MBTN_DOK:LOAD-IMAGE("BILDER\xbtn_dok.gif") IN FRAME FRAME-AONR NO-ERROR.
      MBTN_AVT:LOAD-IMAGE("BILDER\xbtn_avtal.gif") IN FRAME FRAME-AONR NO-ERROR.
      MBTN_DOK:HIDDEN IN FRAME FRAME-AONR = FALSE.
      Guru.GlobalaVariabler:colrighth = MBTN_DOK:HANDLE. 
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      MBTN_AVT:HIDDEN IN FRAME FRAME-AONR = FALSE.
      Guru.GlobalaVariabler:colrighth = MBTN_AVT:HANDLE. 
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.  
   
   IF varforetypval[2] = 0 THEN DO:  
      MBTN_TIDPERS:LOAD-IMAGE("BILDER\xbtn_tidpers.gif") IN FRAME FRAME-AONR NO-ERROR.
      MBTN_TIDPERS:HIDDEN IN FRAME FRAME-AONR = FALSE.
      Guru.GlobalaVariabler:colrighth = MBTN_TIDPERS:HANDLE. 
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   MBTN_OVR:LOAD-IMAGE("BILDER\xbtn_ovr.gif") IN FRAME FRAME-AONR NO-ERROR.
   MBTN_OVR:HIDDEN IN FRAME FRAME-AONR = FALSE.
   Guru.GlobalaVariabler:colrighth = MBTN_OVR:HANDLE. 
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   
   /*gatill.AKTIV:LABEL IN BROWSE BRW_KOPP = "Aktiv".*/
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pstart_UI C-Win 
PROCEDURE pstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 77.
   {SOKANROP.I} 
   
   RUN INLOAPI.P (OUTPUT datornamn).
   datornamn = TRIM(SUBSTRING(datornamn,25,20)).
   {DOKUMENTMAP.I}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE radval_UI C-Win 
PROCEDURE radval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE status-mus2 AS LOGICAL NO-UNDO. 
/*
   BTN_GATILL:COLUMN IN FRAME FRAME-KOPP = BTN_KOPPLAAONR:COLUMN IN FRAME FRAME-KOPP.
   IF RAD_VAL = 1 THEN DO:
      IF openqkoll[1] = FALSE THEN DO:
         openqkoll[1] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").        
         OPEN QUERY BRW_KOPP FOR EACH gatill NO-LOCK BY gatill.F1 BY gatill.F2.
         RUN vcbrwkopp_UI.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      ASSIGN
      BRW_KOPP:HIDDEN IN FRAME FRAME-KOPP = FALSE
      BTN_GATILL:HIDDEN = FALSE.
      APPLY "VALUE-CHANGED" TO BRW_KOPP IN FRAME FRAME-KOPP.            
   END.  
   IF RAD_VAL = 2 THEN DO: 
      
      END.
      FIND FIRST aotidslagtemp WHERE 
      aotidslagtemp.AONR = uppaonrtemp.AONR AND
      aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE aotidslagtemp THEN BTN_GATILL:LABEL = "Ändra".
      ELSE BTN_GATILL:LABEL = "Ny".      
      ASSIGN
      BRW_AOTID:HIDDEN IN FRAME FRAME-TIDL = FALSE
      BTN_GATILL:HIDDEN = FALSE.
      IF AVAILABLE aotidslagtemp THEN DO:
         IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" OR 
         Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "PINN" THEN DO:
            APPLY "HOME" TO BRW_AOTID IN FRAME FRAME-TIDL.
            ASSIGN
            FILL-IN-DOK:HIDDEN = FALSE
            FILL-IN-ANM:HIDDEN = FALSE
            BTN_BDOCTIDDOK:HIDDEN IN FRAME FRAME-DOK = FALSE
            BTN_KOPPTIDL:HIDDEN = FALSE
            BTN_ANMTIDL:HIDDEN = FALSE
            BTN_BESTTIDL:HIDDEN = FALSE
            BTN_VISATIDL:HIDDEN = FALSE.
         END.      
         ELSE IF Guru.Konstanter:globforetag = "PELLE" {GLOBVES.I} THEN DO:
            APPLY "HOME" TO BRW_AOTID IN FRAME FRAME-TIDL.
            ASSIGN            
            FILL-IN-ANM:HIDDEN = FALSE            
            BTN_BESTTIDL:HIDDEN = FALSE.
            BTN_ANMTIDL:HIDDEN = FALSE.
         END.
      END.
      ELSE DO:
         ASSIGN
         FILL-IN-DOK:HIDDEN = TRUE
         FILL-IN-ANM:HIDDEN = TRUE
         BTN_BDOCTIDTV:HIDDEN IN FRAME FRAME-TILLAVTAL = TRUE
         BTN_KOPPTIDL:HIDDEN = TRUE
         BTN_ANMTIDL:HIDDEN = TRUE
         BTN_BESTTIDL:HIDDEN = TRUE
         BTN_VISATIDL:HIDDEN = TRUE.
      END.      
   END.
   IF RAD_VAL = 3 THEN DO:
      ASSIGN
      BTN_GATILL:COLUMN = 25
      BTN_GATILL:LABEL = "Fasta anm."
      EDITOR_ANM:HIDDEN IN FRAME FRAME-ANM = FALSE      
      BTN_GATILL:HIDDEN = FALSE
      BRW_DAGBOK:HIDDEN = FALSE
      BTN_NYDAGBOK:HIDDEN = FALSE             
      BTN_ANDDAGBOK:HIDDEN = FALSE            
      BTN_BORTDAGBOK:HIDDEN = FALSE.           
      FIND FIRST dagboktemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE dagboktemp THEN DO:                 
         RUN dagbokhmt IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,OUTPUT TABLE dagboktemp).         
      END.
      RUN openbdynspec_UI IN brwproc[15]. 
   END.
   IF RAD_VAL = 4 THEN DO:
      /*konton*/
      IF openqkoll[4] = FALSE THEN DO:
         openqkoll[4] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         OPEN QUERY BRW_AONRKTO FOR EACH aonrkonttemp WHERE 
         aonrkonttemp.AONR = uppaonrtemp.AONR AND aonrkonttemp.DELNR = uppaonrtemp.DELNR 
         NO-LOCK.
         GET FIRST BRW_AONRKTO NO-LOCK.
         IF NOT AVAILABLE aonrkonttemp THEN DO:
            RUN konthmt IN nyttaoapph (INPUT aonrvar,INPUT delnrvar,INPUT-OUTPUT TABLE aonrkonttemp).
            OPEN QUERY BRW_AONRKTO FOR EACH aonrkonttemp WHERE 
            aonrkonttemp.AONR = uppaonrtemp.AONR AND aonrkonttemp.DELNR = uppaonrtemp.DELNR 
            NO-LOCK.
         END.         
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      BTN_GATILL:LABEL = "Ändra".
      BRW_AONRKTO:HIDDEN IN FRAME FRAME-KONTO = FALSE.      
      BTN_GATILL:HIDDEN = FALSE.
      IF FILL-IN_DELNR NE 0 THEN DO:
         IF varforetypval[12] = 0 THEN BTN_GATILL:HIDDEN = TRUE.
      END.
      IF Guru.Konstanter:globanv = "elpao" THEN BTN_GATILL:HIDDEN = FALSE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         RUN movefillin_UI (INPUT FILL-IN_ARBARTKOD:HANDLE IN FRAME FRAME-KONTO, INPUT FILL-IN_ARBARTKOD:ROW, INPUT BRW_AONRKTO:COLUMN + BRW_AONRKTO:WIDTH-CHAR + 13).                 
         ASSIGN
         BRW_ARBART:COLUMN        = BRW_AONRKTO:COLUMN + BRW_AONRKTO:WIDTH-CHAR + 1. 
         ASSIGN
         BRW_ARBART:HIDDEN = FALSE 
         FILL-IN_ARBARTKOD:HIDDEN = FALSE.
         OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
         GET FIRST BRW_ARBART NO-LOCK.
         IF NOT AVAILABLE arbarttemp THEN DO:
            ASSIGN
            FILL-IN_ARBARTKOD:HIDDEN IN FRAME FRAME-KONTO = TRUE
            BRW_ARBART:HIDDEN IN FRAME FRAME-KONTO = TRUE.          
         END.
         ELSE DO:
            FIND FIRST arbarttemp WHERE arbarttemp.ARBARTKOD = FILL-IN_ARBARTKOD NO-LOCK NO-ERROR.
            IF AVAILABLE arbarttemp THEN DO:
               RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(arbarttemp)).
               RUN lastselectdyn_UI IN brwproc[5]. 
            END.
         END.
      END.
   END. 
   IF RAD_VAL = 5 THEN DO:
      /*Faktura*/
      ASSIGN
      BTN_GATILL:LABEL = "Ändra"
      CMB_FAK:HIDDEN IN FRAME FRAME-FAKT= FALSE
      FILL-IN_ANVANDARE:HIDDEN = FALSE
      FILL-IN_FAKTTYP:HIDDEN = TRUE
      FILL-IN_FAKTNR:HIDDEN = FALSE
      BTN_GATILL:HIDDEN = FALSE.
      DISPLAY FILL-IN_FAKTNR WITH FRAME FRAME-FAKT. 
      RUN visfakupp_UI.           
   END.
   IF RAD_VAL = 6 THEN DO:     
      /*"Dokument",6,*/      
      ASSIGN
      BRW_AVTALDOK:HIDDEN IN FRAME FRAME-DOK = FALSE
      BTN_NYADOK:HIDDEN = FALSE
      BTN_ANDADOK:HIDDEN = FALSE
      BTN_BORTATIDL:HIDDEN = FALSE      
      BTN_VISADOK:HIDDEN = FALSE
      BTN_MAPPDOK:HIDDEN = FALSE.
      IF openqkoll[6] = FALSE THEN DO:
         openqkoll[6] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         OPEN QUERY BRW_AVTALDOK FOR EACH avtalaonrtemp WHERE 
         avtalaonrtemp.AONR = uppaonrtemp.AONR AND avtalaonrtemp.DELNR = uppaonrtemp.DELNR AND 
         avtalaonrtemp.LNK = TRUE NO-LOCK.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      
   END.
   IF RAD_VAL = 7 THEN DO:
      /*""Tillstånd/Avtal"*/
      ASSIGN
      BRW_AVTAL:HIDDEN IN FRAME FRAME-TILLAVTAL = FALSE
      BTN_NYADOK:HIDDEN = FALSE
      BTN_ANDADOK:HIDDEN = FALSE
      BTN_BORTATIDL:HIDDEN = FALSE
      BTN_HAND:HIDDEN = FALSE
      BTN_VISADOK:HIDDEN = FALSE
      BTN_MAPPDOK:HIDDEN = FALSE.
      IF openqkoll[7] = FALSE THEN DO:
         openqkoll[7] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         OPEN QUERY BRW_AVTAL FOR EACH avtalaonrtemp WHERE avtalaonrtemp.AONR = uppaonrtemp.AONR AND 
         avtalaonrtemp.DELNR = uppaonrtemp.DELNR AND avtalaonrtemp.LNK = FALSE NO-LOCK.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      
   END.
   IF RAD_VAL = 8 THEN DO:
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         RUN movefillin_UI (INPUT FILL-IN_ARBARTKOD:HANDLE IN FRAME FRAME-KONTO, INPUT FILL-IN_ARBARTKOD:ROW, INPUT arbfspar).        
         ASSIGN
         BRW_ARBART:COLUMN        = arbbspar.  
      END.      
      /*"Övrigt"*/      
      ASSIGN 
      CMB_TRAK:HIDDEN IN FRAME FRAME-OVRIGT = FALSE 
      FILL-IN_UTRYCKNING:HIDDEN = FALSE
      BRW_ANL:HIDDEN = FALSE 
      BRW_ARBART:HIDDEN = FALSE 
      BRW_PRIOOV:HIDDEN = FALSE 
      FILL-IN_ANLNR:HIDDEN = FALSE 
      FILL-IN_ARBARTKOD:HIDDEN = FALSE       
      FILL-IN_PKODOV:HIDDEN = FALSE.      
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
         ASSIGN
         BTN_ACCESS:HIDDEN IN FRAME FRAME-BARABREDD = FALSE
         BTN_FASTTRAC:HIDDEN = FALSE.
      END.      
      IF openqkoll[8] = FALSE THEN DO:
         openqkoll[8] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         OPEN QUERY BRW_ANL FOR EACH anlaggtemp NO-LOCK.
         OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
         OPEN QUERY BRW_PRIOOV FOR EACH priotemp NO-LOCK.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      GET FIRST BRW_ANL NO-LOCK.
      IF NOT AVAILABLE anlaggtemp THEN DO: 
         ASSIGN
         FILL-IN_ANLNR:HIDDEN IN FRAME FRAME-OVRIGT = TRUE
         BRW_ANL:HIDDEN IN FRAME FRAME-OVRIGT = TRUE.
      END.   
      ELSE DO:
         FIND FIRST anlaggtemp WHERE anlaggtemp.ANLNR = FILL-IN_ANLNR NO-LOCK NO-ERROR.
         IF AVAILABLE anlaggtemp THEN DO: 
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(anlaggtemp)).
            RUN lastselectdyn_UI IN brwproc[1]. 
         END.
      END.
      GET FIRST BRW_PRIOOV NO-LOCK.
      IF NOT AVAILABLE priotemp THEN DO:
         ASSIGN
         FILL-IN_PKODOV:HIDDEN IN FRAME FRAME-OVRIGT = TRUE
         BRW_PRIOOV:HIDDEN IN FRAME FRAME-OVRIGT = TRUE. 
      END.
      ELSE DO:
         FIND FIRST priotemp WHERE priotemp.PKOD = FILL-IN_PKODOV NO-LOCK NO-ERROR.
         IF AVAILABLE priotemp THEN DO: 
            RUN setlastrowid_UI IN brwproc[10] (INPUT ROWID(priotemp)).
            RUN lastselectdyn_UI IN brwproc[10]. 
         END.
      END.
      GET FIRST BRW_ARBART NO-LOCK.
      IF NOT AVAILABLE arbarttemp THEN DO:
         ASSIGN
         FILL-IN_ARBARTKOD:HIDDEN IN FRAME FRAME-KONTO = TRUE
         BRW_ARBART:HIDDEN IN FRAME FRAME-KONTO = TRUE.          
      END.
      ELSE DO:
         FIND FIRST arbarttemp WHERE arbarttemp.ARBARTKOD = FILL-IN_ARBARTKOD NO-LOCK NO-ERROR.
         IF AVAILABLE arbarttemp THEN DO:
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(arbarttemp)).
            RUN lastselectdyn_UI IN brwproc[5]. 
         END.
      END.
      IF varforetypval[2] = 1 THEN DO:
         ASSIGN      
         CMB_TRAK:HIDDEN = TRUE 
         FILL-IN_UTRYCKNING:HIDDEN = TRUE.
      END.
   END.
   IF RAD_VAL = 9 THEN DO:
      RUN btngatill_UI.
   END.
   IF RAD_VAL = 10 THEN DO:
      ASSIGN
      BTN_GATILL:LABEL = "Ändra"
      BTN_GATILL:HIDDEN = FALSE.
      FIND FIRST aonrtidperstemp NO-LOCK NO-ERROR.
      BRW_TIDPAONR:HIDDEN IN FRAME FRAME-TIDPERS = FALSE.
      IF NOT AVAILABLE aonrtidperstemp THEN DO:
         RUN aonrtidpersh IN nyttaoapph (INPUT FILL-IN_AONR,INPUT FILL-IN_DELNR,OUTPUT TABLE aonrtidperstemp).
         RUN openbdynspec_UI IN brwproc[14]. 
      END.
     
   END.
   IF vartpro = "NAOPLAN" THEN DO:
      IF nyttaovar = TRUE AND plannrvar NE ? THEN DO:
         ASSIGN            
         FILL-IN-DOK:HIDDEN = TRUE
         FILL-IN-ANM:HIDDEN = TRUE
         MBTN_TIDPERS:HIDDEN = TRUE
         BRW_DAGBOK:HIDDEN = TRUE        
         BTN_NYDAGBOK:HIDDEN = TRUE
         BTN_ANDDAGBOK:HIDDEN = TRUE
         BTN_BORTDAGBOK:HIDDEN = TRUE         
         BTN_ACCESS:HIDDEN = TRUE 
         BTN_KALKAR:HIDDEN = TRUE
         BTN_AKTIV:HIDDEN = TRUE 
         BTN_ANDADOK:HIDDEN = TRUE 
         BTN_BORTATIDL:HIDDEN = TRUE 
         BTN_FASTTRAC:HIDDEN = TRUE 
         BTN_GATILL:HIDDEN = TRUE 
         BTN_HAND:HIDDEN = TRUE 
         BTN_BDOCTIDTV:HIDDEN = TRUE
         BTN_KOPPTIDL:HIDDEN = TRUE 
         BTN_ANMTIDL:HIDDEN = TRUE 
         BTN_BESTTIDL:HIDDEN = TRUE 
         BTN_KOPPLAAONR:HIDDEN = TRUE 
         BTN_NYADOK:HIDDEN = TRUE 
         BTN_VISADOK:HIDDEN = TRUE 
         BTN_VISATIDL:HIDDEN = TRUE
         BTN_MAPPDOK:HIDDEN = TRUE. 
         IF RAD_VAL = 2 THEN DO:
            FIND FIRST aotidslagtemp WHERE 
            aotidslagtemp.AONR = uppaonrtemp.AONR AND
            aotidslagtemp.DELNR = uppaonrtemp.DELNR NO-LOCK NO-ERROR.
            IF AVAILABLE aotidslagtemp THEN DO:
               BTN_GATILL:HIDDEN = FALSE. 
               BTN_GATILL:LABEL = "Ändra".
            END.
         END.
      END.     
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rlwkopp_UI C-Win 
PROCEDURE rlwkopp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/      
   IF AVAILABLE gatill THEN DO:
      DISPLAY gatill.STATUSNIV WITH BROWSE BRW_KOPP NO-ERROR.
      gatill.STATUSNIV = INPUT BROWSE BRW_KOPP gatill.STATUSNIV.
      DISPLAY gatill.STATUSNIV WITH BROWSE BRW_KOPP NO-ERROR.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispkopp_UI C-Win 
PROCEDURE rowdispkopp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER fargnr AS INTEGER NO-UNDO.
   IF AVAILABLE gatill THEN DO:
      IF gatill.TYPCHAR = "KAL" THEN fargnr = 1.        
      ELSE IF gatill.TYPCHAR = "FAK" THEN. 
      ELSE IF gatill.TYPCHAR = "BER" THEN fargnr = 5.
      ELSE IF gatill.TYPCHAR = "MAR" THEN.
      ELSE IF gatill.TYPCHAR = "FBAONR" THEN fargnr = 2.
      ELSE IF gatill.TYPCHAR = "INTAONR" THEN fargnr = 2.
   END.
   IF fargnr > 0 THEN DO:
      ASSIGN
      gatill.F1:FGCOLOR IN BROWSE BRW_KOPP = fargnr
      gatill.F2:FGCOLOR = fargnr
      gatill.F3:FGCOLOR = fargnr
      gatill.STATUSNIV:FGCOLOR = fargnr.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokaonr_UI C-Win 
PROCEDURE sokaonr_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN FILL-IN_AONR = INPUT FRAME FRAME-BARABREDD FILL-IN_AONR. 
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF Guru.Konstanter:globniv NE 15 THEN DO:
         IF FILL-IN_FASTAAONR = TRUE THEN DO:
            IF SUBSTRING(FILL-IN_AONR,1,1) NE STRING(0) THEN DO:
               MESSAGE gfastk Guru.Konstanter:gaol + " måste börja med en 0:a" VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.      
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:globforetag = "VORD" {GLOBVES.I} THEN DO:
      IF LENGTH(FILL-IN_AONR) < 6 THEN DO:
         MESSAGE Guru.Konstanter:gaol + " måste innehålla 6 tecken." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.      
      END.
   END.
   RUN aokoll_UI.
   IF musz = FALSE THEN DO:
      FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
      uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
      
      ASSIGN aonrvar = uppaonrtemp.AONR.
      ASSIGN
      aonrvar2 =  aonrvar 
      delnrvar2 = delnrvar.
      RUN andraaonr IN nyttaoapph (INPUT aonrvar,INPUT FILL-IN_AONR).
      mappvar = "".
      mappvar = FILL-IN_AONR + "0". 
      IF grundmappvar NE "" THEN DO:
         mappvarhj = grundmappvar + mappvar.
         RUN mappvar IN nyttaoejapph (INPUT mappvarhj).         
      END.

      DISABLE FILL-IN_AONR WITH FRAME FRAME-BARABREDD. 
   END.  
   ELSE DO:
      musz = FALSE.
      APPLY "ENTRY" TO FILL-IN_AONR IN FRAME FRAME-BARABREDD.
      RETURN NO-APPLY.      
   END.   
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valomr_UI C-Win 
PROCEDURE valomr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF SUBSTRING(globomr,1,1) = "4" THEN DO:
      IF SUBSTRING(omrtemp.OMRADE,1,1) = "4" THEN DO:  
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME FRAME-BARABREDD.
      END.
   END.
   ELSE status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI C-Win 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordningnr = ordningnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE widgeth_UI C-Win 
PROCEDURE widgeth_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      ENABLE CMB_FAK WITH FRAME FRAME-FAKT.   
      RETURN.
   END.
   IF vad = 2 THEN DO:
      DISABLE CMB_FAK WITH FRAME FRAME-FAKT.   
      RETURN.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

