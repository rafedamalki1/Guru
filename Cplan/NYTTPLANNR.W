&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */




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
DEFINE INPUT PARAMETER nyplanvar AS LOGICAL NO-UNDO. 
DEFINE INPUT PARAMETER kopiavar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER plannrvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER artalvar AS INTEGER NO-UNDO.
 
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.



DEFINE VARIABLE gatilltyp AS CHARACTER NO-UNDO.
DEFINE VARIABLE gatill1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gatill2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyph AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
{GATILL.I}
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

{HOPPSEK2W.I}
{PRIARBANLTEMP.I}
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE btnnovit AS HANDLE NO-UNDO. 




&Scoped-define SHARED SHARED
{FAKTTYPDEF.I}
{AUTOMREGTEMP.I}
{AVTPLANTEMP.I}
{OMRTEMPW.I}
{ANSVPLANTEMP.I}
{PLANNRTEMP.I}

{SOKDEF.I}


&Scoped-define NEW NEW
{KALKIN.I}
{AVTALAONR.I}
{AVTALTEMP.I}
DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoejapph AS HANDLE NO-UNDO.                    /*dokhant.p*/
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvarhj AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokmapp AS CHARACTER NO-UNDO.
DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kalktypvar AS INTEGER NO-UNDO.
DEFINE VARIABLE openqkoll AS LOGICAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE omr AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparansv AS CHARACTER NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE nrvalvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE stat AS INTEGER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nummer AS LOGICAL NO-UNDO.
DEFINE VARIABLE fast AS LOGICAL NO-UNDO.
DEFINE VARIABLE startartal AS INTEGER NO-UNDO.
DEFINE VARIABLE slutartal AS INTEGER NO-UNDO.
DEFINE VARIABLE arvar AS INTEGER NO-UNDO.
DEFINE VARIABLE gammar AS INTEGER NO-UNDO.
DEFINE VARIABLE gfastkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE gtillkh AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.
DEFINE VARIABLE ordnvar AS INTEGER NO-UNDO.
DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE arbbspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbfspar AS DECIMAL NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{BESTKUNDALLT.I}

DEFINE BUFFER gabuff FOR gatill.
DEFINE QUERY aq FOR automregtemp.
DEFINE QUERY bq FOR bestkundallt.
RUN pstart_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ANSV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ansvplantemp arbarttemp avtalaonrtemp ~
bestkundallt gatill plankonttemp priotemp

/* Definitions for BROWSE BRW_ANSV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_ANSV ansvplantemp.PERSONALKOD ~
ansvplantemp.FORNAMN ansvplantemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANSV ansvplantemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANSV ansvplantemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANSV ansvplantemp
&Scoped-define QUERY-STRING-BRW_ANSV FOR EACH ansvplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANSV OPEN QUERY BRW_ANSV FOR EACH ansvplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANSV ansvplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANSV ansvplantemp


/* Definitions for BROWSE BRW_ARBART                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBART arbarttemp.ARBARTKOD ~
arbarttemp.ARBBENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBART 
&Scoped-define QUERY-STRING-BRW_ARBART FOR EACH arbarttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBART OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBART arbarttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBART arbarttemp


/* Definitions for BROWSE BRW_AVTAL2                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_AVTAL2 avtalaonrtemp.AVTAL ~
avtalaonrtemp.BEN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AVTAL2 
&Scoped-define QUERY-STRING-BRW_AVTAL2 FOR EACH avtalaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AVTAL2 OPEN QUERY BRW_AVTAL2 FOR EACH avtalaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AVTAL2 avtalaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AVTAL2 avtalaonrtemp


/* Definitions for BROWSE BRW_BEST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BEST bestkundallt.VIBESTID ~
bestkundallt.BESTNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEST bestkundallt.VIBESTID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define QUERY-STRING-BRW_BEST FOR EACH bestkundallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEST OPEN QUERY BRW_BEST FOR EACH bestkundallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEST bestkundallt


/* Definitions for BROWSE BRW_KOPP                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KOPP gatill.F1 gatill.F2 gatill.F3 ~
gatill.STATUSNIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOPP 
&Scoped-define QUERY-STRING-BRW_KOPP FOR EACH gatill NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOPP OPEN QUERY BRW_KOPP FOR EACH gatill NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOPP gatill
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOPP gatill


/* Definitions for BROWSE BRW_PLANKTO                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_PLANKTO plankonttemp.K1 plankonttemp.K2 ~
plankonttemp.K3 plankonttemp.K4 plankonttemp.K5 plankonttemp.SATS% 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PLANKTO 
&Scoped-define QUERY-STRING-BRW_PLANKTO FOR EACH plankonttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PLANKTO OPEN QUERY BRW_PLANKTO FOR EACH plankonttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PLANKTO plankonttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PLANKTO plankonttemp


/* Definitions for BROWSE BRW_PRIO                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PRIO priotemp.PKOD priotemp.P-BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRIO 
&Scoped-define QUERY-STRING-BRW_PRIO FOR EACH priotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRIO OPEN QUERY BRW_PRIO FOR EACH priotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRIO priotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRIO priotemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_OK MBTN_ANM RECT-MENY BTN_AVB ~
FILL-IN_PLANNR CMB_ARTAL FILL-IN_FASTAPLANNR FILL-IN_ORT MBTN_DOK CMB_OMR ~
FILL-IN_BEST CMB_DEBPR BTN_NVE BTN_NVE-2 FILL-IN_ARBANSVARIG FILL-IN_STARTD ~
BTN_FVE FILL-IN_SLUTD BTN_FVE-2 FILL-IN_PKOD FILL-IN_ARBARTKOD BRW_AVTAL2 ~
BRW_KOPP BRW_PLANKTO BRW_ANSV BRW_BEST BRW_PRIO BRW_ARBART BTN_GATILL ~
EDITOR_ANM BTN_KOPPLAAONR BTN_AKTIV BTN_KALKAR BTN_VISA MBTN_KONT ~
BTN_UPPKALK BTN_KALKKALK BTN_VISKALK BTN_KONVKALK BTN_ANDRAK BTN_NYA ~
BTN_ANDA BTN_BORTA RAD_VAL MBTN_KOPPL MBTN_OVR 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PLANNR CMB_ARTAL ~
FILL-IN_FASTAPLANNR FILL-IN_ORT CMB_OMR FILL-IN_BEST CMB_DEBPR ~
FILL-IN_ARBANSVARIG FILL-IN_STARTD FILL-IN_SLUTD FILL-IN_PKOD ~
FILL-IN_ARBARTKOD EDITOR_ANM FILL-IN-KOPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AKTIV 
     LABEL "Status" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ANDA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDRAK 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORTA 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_GATILL 
     LABEL "Nya" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALKAR 
     LABEL "KOPIERA" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALKKALK 
     IMAGE-UP FILE "BILDER\xbtn_kalkyl.gif":U
     LABEL "Kalkylera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KONVKALK 
     LABEL "Konvertera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPPLAAONR 
     LABEL "Koppla" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MAPP 
     LABEL "Öppna dokumentmappen" 
     SIZE 16 BY 1.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NYA 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPPKALK 
     LABEL "Kalkyl huvud":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 16 BY 1.

DEFINE BUTTON BTN_VISKALK 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_ANM  NO-FOCUS
     LABEL "Anmärkningar" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON MBTN_DOK  NO-FOCUS
     LABEL "Dokument" 
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

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_DEBPR AS CHARACTER FORMAT "X(9)":U 
     LABEL "Debitering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utförande omr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE EDITOR_ANM AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 47.38 BY 7.04.

DEFINE VARIABLE FILL-IN-KOPP AS CHARACTER FORMAT "X(256)":U INITIAL "Kopplingar" 
      VIEW-AS TEXT 
     SIZE 22.75 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(5)" 
     LABEL "Ansvarig" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "Arbetsart" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_BEST AS CHARACTER FORMAT "x(256)" 
     LABEL "BESTÄLLARE" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_FASTAPLANNR AS LOGICAL FORMAT "Fast/Tillf" INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "Prioritet" 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1.

DEFINE VARIABLE FILL-IN_PLANNR AS CHARACTER FORMAT "X(6)" 
     LABEL "Plannr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_PLANNRAVDATUM AS DATE FORMAT "99/99/99" INITIAL 01/01/91 
     LABEL "Avslutat" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Ange ? för bortag av avslutning" NO-UNDO.

DEFINE VARIABLE FILL-IN_SLUTD AS DATE FORMAT "99/99/99":U 
     LABEL "Slutdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_STARTD AS DATE FORMAT "99/99/99":U 
     LABEL "Startdatum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "olika val", 0
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 103.63 BY .08.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANSV FOR 
      ansvplantemp SCROLLING.

DEFINE QUERY BRW_ARBART FOR 
      arbarttemp SCROLLING.

DEFINE QUERY BRW_AVTAL2 FOR 
      avtalaonrtemp SCROLLING.

DEFINE QUERY BRW_BEST FOR 
      bestkundallt SCROLLING.

DEFINE QUERY BRW_KOPP FOR 
      gatill SCROLLING.

DEFINE QUERY BRW_PLANKTO FOR 
      plankonttemp SCROLLING.

DEFINE QUERY BRW_PRIO FOR 
      priotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANSV C-Win _STRUCTURED
  QUERY BRW_ANSV NO-LOCK DISPLAY
      ansvplantemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      ansvplantemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
            WIDTH 20
      ansvplantemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
            WIDTH 30
  ENABLE
      ansvplantemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 65.5 BY 11
         TITLE "".

DEFINE BROWSE BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBART C-Win _STRUCTURED
  QUERY BRW_ARBART NO-LOCK DISPLAY
      arbarttemp.ARBARTKOD COLUMN-LABEL "Arbetsart" FORMAT ">>>":U
            WIDTH 9.38
      arbarttemp.ARBBENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 28.5 BY 11
         TITLE "Välj arbetsart".

DEFINE BROWSE BRW_AVTAL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AVTAL2 C-Win _STRUCTURED
  QUERY BRW_AVTAL2 NO-LOCK DISPLAY
      avtalaonrtemp.AVTAL COLUMN-LABEL "Dokument" FORMAT "X(256)":U
            WIDTH 32
      avtalaonrtemp.BEN COLUMN-LABEL "Egen text" FORMAT "X(256)":U
            WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 71 BY 11.

DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST C-Win _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      bestkundallt.VIBESTID COLUMN-LABEL "bestid" FORMAT "X(6)":U
            WIDTH 11.5
      bestkundallt.BESTNAMN COLUMN-LABEL "Namn" FORMAT "x(35)":U
  ENABLE
      bestkundallt.VIBESTID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 56.5 BY 11
         TITLE "Välj BESTÄLLARE".

DEFINE BROWSE BRW_KOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOPP C-Win _STRUCTURED
  QUERY BRW_KOPP NO-LOCK DISPLAY
      gatill.F1 COLUMN-LABEL "Typ av koppling" FORMAT "X(256)":U
            WIDTH 20
      gatill.F2 COLUMN-LABEL "Nummer" FORMAT "X(256)":U WIDTH 10
      gatill.F3 COLUMN-LABEL "Benämning" FORMAT "X(256)":U WIDTH 34.75
      gatill.STATUSNIV COLUMN-LABEL "Status" FORMAT "X(3)":U WIDTH 4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 74 BY 11.

DEFINE BROWSE BRW_PLANKTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PLANKTO C-Win _STRUCTURED
  QUERY BRW_PLANKTO NO-LOCK DISPLAY
      plankonttemp.K1 FORMAT "x(6)":U
      plankonttemp.K2 FORMAT "x(6)":U
      plankonttemp.K3 FORMAT "x(6)":U
      plankonttemp.K4 FORMAT "x(6)":U
      plankonttemp.K5 FORMAT "x(6)":U
      plankonttemp.SATS% COLUMN-LABEL "%" FORMAT "999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 41.5 BY 11
         TITLE "Kontosträngar lagrade på plannr:et".

DEFINE BROWSE BRW_PRIO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRIO C-Win _STRUCTURED
  QUERY BRW_PRIO NO-LOCK DISPLAY
      priotemp.PKOD COLUMN-LABEL "Prioritet" FORMAT ">>":U WIDTH 11
      priotemp.P-BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 28.5 BY 11
         TITLE "Välj prioritet".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_OK AT ROW 1 COL 90.5
     MBTN_ANM AT ROW 1 COL 40
     BTN_AVB AT ROW 2.29 COL 90.5
     FILL-IN_PLANNR AT ROW 3.54 COL 15
     CMB_ARTAL AT ROW 3.54 COL 36.13
     FILL-IN_PLANNRAVDATUM AT ROW 3.54 COL 64 COLON-ALIGNED
     FILL-IN_FASTAPLANNR AT ROW 3.54 COL 79 COLON-ALIGNED NO-LABEL
     FILL-IN_ORT AT ROW 4.58 COL 21 COLON-ALIGNED
     MBTN_DOK AT ROW 1 COL 54
     CMB_OMR AT ROW 5.71 COL 21 COLON-ALIGNED
     FILL-IN_BEST AT ROW 6.79 COL 21 COLON-ALIGNED
     CMB_DEBPR AT ROW 7.92 COL 21 COLON-ALIGNED
     BTN_NVE AT ROW 8.5 COL 67
     BTN_NVE-2 AT ROW 8.63 COL 91.63
     FILL-IN_ARBANSVARIG AT ROW 9 COL 21 COLON-ALIGNED
     FILL-IN_STARTD AT ROW 9.38 COL 55 COLON-ALIGNED
     BTN_FVE AT ROW 9.38 COL 67
     FILL-IN_SLUTD AT ROW 9.46 COL 79.63 COLON-ALIGNED
     BTN_FVE-2 AT ROW 9.58 COL 91.63
     FILL-IN_PKOD AT ROW 10.96 COL 54 COLON-ALIGNED WIDGET-ID 2
     FILL-IN_ARBARTKOD AT ROW 11 COL 82.25 COLON-ALIGNED
     BRW_AVTAL2 AT ROW 12.71 COL 1.5
     BRW_KOPP AT ROW 12.71 COL 1.5
     BRW_PLANKTO AT ROW 12.71 COL 1.75
     BRW_ANSV AT ROW 12.71 COL 2.5
     BRW_BEST AT ROW 12.71 COL 23
     BRW_PRIO AT ROW 12.71 COL 44.13 WIDGET-ID 100
     BRW_ARBART AT ROW 12.71 COL 73.5
     BTN_GATILL AT ROW 12.71 COL 81.63
     EDITOR_ANM AT ROW 13.58 COL 29.13 NO-LABEL
     BTN_KOPPLAAONR AT ROW 13.83 COL 81.63
     BTN_AKTIV AT ROW 14.96 COL 81.63
     BTN_MAPP AT ROW 15 COL 81.63
     BTN_KALKAR AT ROW 16.04 COL 81.63
     BTN_VISA AT ROW 16.08 COL 81.63
     MBTN_KONT AT ROW 1 COL 28
     BTN_UPPKALK AT ROW 17.5 COL 81.63
     BTN_KALKKALK AT ROW 18.5 COL 81.63
     BTN_VISKALK AT ROW 19.46 COL 81.63
     BTN_KONVKALK AT ROW 20.58 COL 81.63
     BTN_ANDRAK AT ROW 24.25 COL 13.5
     BTN_NYA AT ROW 24.25 COL 16.5
     BTN_ANDA AT ROW 24.25 COL 30.75
     BTN_BORTA AT ROW 24.25 COL 45.13
     RAD_VAL AT ROW 24.5 COL 2 NO-LABEL
     MBTN_KOPPL AT ROW 1 COL 1
     MBTN_OVR AT ROW 1 COL 66
     FILL-IN-KOPP AT ROW 11.5 COL 2.75 NO-LABEL
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 103.63 BY 25.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ansvplantemp T "?" NO-UNDO temp-db ansvplantemp
      TABLE: arbarttemp T "?" NO-UNDO temp-db arbarttemp
      TABLE: avtalaonrtemp T "?" NO-UNDO temp-db avtalaonrtemp
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: gatill T "?" NO-UNDO temp-db gatill
      TABLE: plankonttemp T "?" NO-UNDO temp-db plankonttemp
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Plannummerhantering"
         HEIGHT             = 25.71
         WIDTH              = 103.75
         MAX-HEIGHT         = 28.54
         MAX-WIDTH          = 107.5
         VIRTUAL-HEIGHT     = 28.54
         VIRTUAL-WIDTH      = 107.5
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
/* BROWSE-TAB BRW_AVTAL2 FILL-IN_ARBARTKOD DEFAULT-FRAME */
/* BROWSE-TAB BRW_KOPP BRW_AVTAL2 DEFAULT-FRAME */
/* BROWSE-TAB BRW_PLANKTO BRW_KOPP DEFAULT-FRAME */
/* BROWSE-TAB BRW_ANSV BRW_PLANKTO DEFAULT-FRAME */
/* BROWSE-TAB BRW_BEST BRW_ANSV DEFAULT-FRAME */
/* BROWSE-TAB BRW_PRIO BRW_BEST DEFAULT-FRAME */
/* BROWSE-TAB BRW_ARBART BRW_PRIO DEFAULT-FRAME */
ASSIGN 
       BRW_ANSV:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_ANSV:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300.

ASSIGN 
       BRW_ARBART:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_ARBART:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_ARBART:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_AVTAL2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_AVTAL2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_AVTAL2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_BEST:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_BEST:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300.

ASSIGN 
       BRW_PLANKTO:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

ASSIGN 
       BRW_PRIO:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_PRIO:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_PRIO:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BTN_AKTIV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_ANDA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_ANDRAK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_BORTA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_GATILL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_KALKAR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_KALKKALK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_KONVKALK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_KOPPLAAONR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_MAPP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_MAPP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NYA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_UPPKALK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_VISA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_VISKALK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       EDITOR_ANM:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KOPP IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-KOPP:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_BEST:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_PLANNR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_PLANNRAVDATUM IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_PLANNRAVDATUM:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN_SLUTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN_STARTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY                                                           */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANSV
/* Query rebuild information for BROWSE BRW_ANSV
     _TblList          = "Temp-Tables.ansvplantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ansvplantemp.PERSONALKOD
"ansvplantemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ansvplantemp.FORNAMN
"ansvplantemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ansvplantemp.EFTERNAMN
"ansvplantemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANSV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBART
/* Query rebuild information for BROWSE BRW_ARBART
     _TblList          = "Temp-Tables.arbarttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbarttemp.ARBARTKOD
"arbarttemp.ARBARTKOD" "Arbetsart" ? "integer" ? ? ? ? ? ? no ? no no "9.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.arbarttemp.ARBBENAMNING
"arbarttemp.ARBBENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ARBART */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AVTAL2
/* Query rebuild information for BROWSE BRW_AVTAL2
     _TblList          = "Temp-Tables.avtalaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.avtalaonrtemp.AVTAL
"avtalaonrtemp.AVTAL" "Dokument" "X(256)" "character" ? ? ? ? ? ? no ? no no "32" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.avtalaonrtemp.BEN
"avtalaonrtemp.BEN" "Egen text" "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AVTAL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestkundallt.VIBESTID
"bestkundallt.VIBESTID" "bestid" ? "character" ? ? ? ? ? ? yes ? no no "11.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bestkundallt.BESTNAMN
"bestkundallt.BESTNAMN" "Namn" "x(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOPP
/* Query rebuild information for BROWSE BRW_KOPP
     _TblList          = "Temp-Tables.gatill"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.gatill.F1
"gatill.F1" "Typ av koppling" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.gatill.F2
"gatill.F2" "Nummer" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.gatill.F3
"gatill.F3" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "34.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.gatill.STATUSNIV
"gatill.STATUSNIV" "Status" "X(3)" "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PLANKTO
/* Query rebuild information for BROWSE BRW_PLANKTO
     _TblList          = "Temp-Tables.plankonttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.plankonttemp.K1
     _FldNameList[2]   = Temp-Tables.plankonttemp.K2
     _FldNameList[3]   = Temp-Tables.plankonttemp.K3
     _FldNameList[4]   = Temp-Tables.plankonttemp.K4
     _FldNameList[5]   = Temp-Tables.plankonttemp.K5
     _FldNameList[6]   > Temp-Tables.plankonttemp.SATS%
"plankonttemp.SATS%" "%" ? "" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PLANKTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRIO
/* Query rebuild information for BROWSE BRW_PRIO
     _TblList          = "Temp-Tables.priotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.priotemp.PKOD
"priotemp.PKOD" "Prioritet" ? "integer" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.priotemp.P-BENAMNING
"priotemp.P-BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PRIO */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Plannummerhantering */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Plannummerhantering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANSV
&Scoped-define SELF-NAME BRW_ANSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANSV C-Win
ON LEAVE OF BRW_ANSV IN FRAME DEFAULT-FRAME
DO:
   ASSIGN FILL-IN_ARBANSVARIG = INPUT FILL-IN_ARBANSVARIG.
   FIND FIRST ansvplantemp WHERE ansvplantemp.PERSONALKOD = FILL-IN_ARBANSVARIG
   USE-INDEX PKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ansvplantemp THEN DO:
      MESSAGE
      "Ingen ansvarig med enhet " fill-in_arbansvarig " finns i registret!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   BRW_ANSV:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ANSV C-Win
ON VALUE-CHANGED OF BRW_ANSV IN FRAME DEFAULT-FRAME
DO:
   status-ok = BRW_ANSV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   ASSIGN FILL-IN_ARBANSVARIG  = ansvplantemp.PERSONALKOD.
   DISPLAY FILL-IN_ARBANSVARIG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ARBART
&Scoped-define SELF-NAME BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ARBART C-Win
ON VALUE-CHANGED OF BRW_ARBART IN FRAME DEFAULT-FRAME /* Välj arbetsart */
DO:
   status-ok = BRW_ARBART:SELECT-FOCUSED-ROW() NO-ERROR.
   ASSIGN FILL-IN_ARBARTKOD  = arbarttemp.ARBARTKOD.
   DISPLAY FILL-IN_ARBARTKOD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AVTAL2
&Scoped-define SELF-NAME BRW_AVTAL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVTAL2 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_AVTAL2 IN FRAME DEFAULT-FRAME
DO:
   
   APPLY "CHOOSE" TO BTN_VISA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AVTAL2 C-Win
ON VALUE-CHANGED OF BRW_AVTAL2 IN FRAME DEFAULT-FRAME
DO:
    status-ok = BRW_AVTAL2:SELECT-FOCUSED-ROW() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEST
&Scoped-define SELF-NAME BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST C-Win
ON VALUE-CHANGED OF BRW_BEST IN FRAME DEFAULT-FRAME /* Välj BESTÄLLARE */
DO:
   status-ok = BRW_BEST:SELECT-FOCUSED-ROW() NO-ERROR.
   ASSIGN 
   FILL-IN_BEST = bestkundallt.BESTNAMN.
   DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOPP
&Scoped-define SELF-NAME BRW_KOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOPP C-Win
ON VALUE-CHANGED OF BRW_KOPP IN FRAME DEFAULT-FRAME
DO:
   RUN vcbrwkopp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRIO
&Scoped-define SELF-NAME BRW_PRIO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRIO C-Win
ON VALUE-CHANGED OF BRW_PRIO IN FRAME DEFAULT-FRAME /* Välj prioritet */
DO:
   status-ok = BRW_PRIO:SELECT-FOCUSED-ROW() NO-ERROR.
   ASSIGN FILL-IN_PKOD = priotemp.PKOD.
   DISPLAY FILL-IN_PKOD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AKTIV C-Win
ON CHOOSE OF BTN_AKTIV IN FRAME DEFAULT-FRAME /* Status */
DO:
   RUN btnaktiv_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDA C-Win
ON CHOOSE OF BTN_ANDA IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   RUN btnanda_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRAK C-Win
ON CHOOSE OF BTN_ANDRAK IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   RUN btngatill_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:   
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
   IF svar THEN DO:      
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF NOT svar THEN DO:
      {BESTALLTTEMPBORT.I}
      musz = TRUE.
      APPLY "END-ERROR":U TO SELF.         
   END.                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTA C-Win
ON CHOOSE OF BTN_BORTA IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   RUN btnborta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE C-Win
ON CHOOSE OF BTN_FVE IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN_STARTD = FILL-IN_STARTD - 1.      
   DISPLAY FILL-IN_STARTD WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 C-Win
ON CHOOSE OF BTN_FVE-2 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN_SLUTD = INPUT FILL-IN_SLUTD.   
   FILL-IN_SLUTD = FILL-IN_SLUTD - 1.      
   DISPLAY FILL-IN_SLUTD WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GATILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GATILL C-Win
ON CHOOSE OF BTN_GATILL IN FRAME DEFAULT-FRAME /* Nya */
DO:
   RUN btngatill_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KALKAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KALKAR C-Win
ON CHOOSE OF BTN_KALKAR IN FRAME DEFAULT-FRAME /* KOPIERA */
DO:
   /*ANVÄNDS EJ*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KALKKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KALKKALK C-Win
ON CHOOSE OF BTN_KALKKALK IN FRAME DEFAULT-FRAME /* Kalkylera */
DO:
   RUN kalk_UI (INPUT 4).           

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONVKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONVKALK C-Win
ON CHOOSE OF BTN_KONVKALK IN FRAME DEFAULT-FRAME /* Konvertera */
DO:
   RUN kalk_UI (INPUT 6).           

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPPLAAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPPLAAONR C-Win
ON CHOOSE OF BTN_KOPPLAAONR IN FRAME DEFAULT-FRAME /* Koppla */
DO:
   RUN btnok_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.    
   RUN kopplaaonr_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MAPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MAPP C-Win
ON CHOOSE OF BTN_MAPP IN FRAME DEFAULT-FRAME /* Öppna dokumentmappen */
DO:
   ASSIGN
   mappvarhj = "PL" + upplantemp.PLANNR + STRING(upplantemp.ARTAL) + "\".          
   mappvar = grundmappvar +  mappvarhj.  
   RUN OPENDOC.P (mappvar,"","",NO).      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE C-Win
ON CHOOSE OF BTN_NVE IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN_STARTD = INPUT FILL-IN_STARTD.   
   FILL-IN_STARTD = FILL-IN_STARTD + 1.        
   DISPLAY FILL-IN_STARTD WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 C-Win
ON CHOOSE OF BTN_NVE-2 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN_SLUTD = INPUT FILL-IN_SLUTD.   
   FILL-IN_SLUTD = FILL-IN_SLUTD + 1.        
   DISPLAY FILL-IN_SLUTD WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYA C-Win
ON CHOOSE OF BTN_NYA IN FRAME DEFAULT-FRAME /* Ny */
DO:
   RUN btnnya_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:  
   RUN btnok_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.     
   {BESTALLTTEMPBORT.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPKALK C-Win
ON CHOOSE OF BTN_UPPKALK IN FRAME DEFAULT-FRAME /* Kalkyl huvud */
DO:
   RUN kalk_UI (INPUT 3).           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA C-Win
ON CHOOSE OF BTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   RUN btnvisa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISKALK C-Win
ON CHOOSE OF BTN_VISKALK IN FRAME DEFAULT-FRAME /* Visa */
DO:
   RUN kalk_UI (INPUT 5).           

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   arvar = CMB_ARTAL
   regdatum = DATE(01,01,arvar).
   ASSIGN    
   FILL-IN_STARTD = regdatum.
   regdatum = DATE(12,31,arvar).
   FILL-IN_SLUTD = regdatum.         
   DISPLAY FILL-IN_STARTD WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN_SLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_DEBPR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_DEBPR C-Win
ON LEAVE OF CMB_DEBPR IN FRAME DEFAULT-FRAME /* Debitering */
DO:
   ASSIGN CMB_DEBPR = INPUT CMB_DEBPR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR C-Win
ON VALUE-CHANGED OF CMB_OMR IN FRAME DEFAULT-FRAME /* Utförande omr */
DO:
   CMB_OMR = INPUT CMB_OMR.        
   {BESTALLTTEMPTILL.I}
   IF Guru.Konstanter:varforetypval[3] >= 1 AND nyplanvar = FALSE THEN DO:
      FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bestkundallt THEN FILL-IN_BEST = CMB_OMR.      
   END.
   IF nyplanvar = TRUE THEN FILL-IN_BEST = CMB_OMR.   
   DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.   
   RUN openbdynspec_UI IN brwproc[2].
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST NO-LOCK NO-ERROR.   
   IF AVAILABLE bestkundallt THEN RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(bestkundallt)).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR_ANM C-Win
ON LEAVE OF EDITOR_ANM IN FRAME DEFAULT-FRAME
DO:
   ASSIGN EDITOR_ANM = INPUT EDITOR_ANM.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ARBANSVARIG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBANSVARIG C-Win
ON ENTRY OF FILL-IN_ARBANSVARIG IN FRAME DEFAULT-FRAME /* Ansvarig */
DO:
   RUN goma_UI.
   ASSIGN
   BRW_ANSV:HIDDEN = FALSE.  
   BRW_ANSV:MAX-DATA-GUESS IN FRAME {&FRAME-NAME} = 300. 
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   FIND FIRST ansvplantemp WHERE ansvplantemp.PERSONALKOD = FILL-IN_ARBANSVARIG USE-INDEX PKOD NO-LOCK NO-ERROR.
   IF AVAILABLE ansvplantemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(ansvplantemp)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBANSVARIG C-Win
ON LEAVE OF FILL-IN_ARBANSVARIG IN FRAME DEFAULT-FRAME /* Ansvarig */
DO:
   ASSIGN FILL-IN_ARBANSVARIG = INPUT FILL-IN_ARBANSVARIG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBANSVARIG C-Win
ON MOUSE-SELECT-CLICK OF FILL-IN_ARBANSVARIG IN FRAME DEFAULT-FRAME /* Ansvarig */
DO:
   APPLY "ENTRY" TO FILL-IN_ARBANSVARIG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ARBARTKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBARTKOD C-Win
ON ENTRY OF FILL-IN_ARBARTKOD IN FRAME DEFAULT-FRAME /* Arbetsart */
DO:
   FIND FIRST arbarttemp WHERE arbarttemp.ARBARTKOD = FILL-IN_ARBARTKOD
   USE-INDEX ARB NO-LOCK NO-ERROR.
   IF AVAILABLE arbarttemp  THEN DO:
      RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(arbarttemp)).
      RUN lastselectdyn_UI IN brwproc[5]. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ARBARTKOD C-Win
ON LEAVE OF FILL-IN_ARBARTKOD IN FRAME DEFAULT-FRAME /* Arbetsart */
DO:
   ASSIGN FILL-IN_ARBARTKOD = INPUT FILL-IN_ARBARTKOD.
   /*
   FIND FIRST ARBETSART WHERE ARBETSART.ARBARTKOD = FILL-IN_ARBARTKOD 
   USE-INDEX ARB NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ARBETSART THEN DO:
      MESSAGE 
      "Ingen arbetsart med nr " fill-in_arbartkod " finns i registret!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEST C-Win
ON ENTRY OF FILL-IN_BEST IN FRAME DEFAULT-FRAME /* BESTÄLLARE */
DO:
   RUN goma_UI.
   BRW_BEST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   OPEN QUERY BRW_BEST FOR EACH bestkundallt NO-LOCK.
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST 
   NO-LOCK NO-ERROR.   
   IF AVAILABLE bestkundallt THEN DO:                                   
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(bestkundallt)).
      RUN lastselectdyn_UI IN brwproc[2]. 
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEST C-Win
ON LEAVE OF FILL-IN_BEST IN FRAME DEFAULT-FRAME /* BESTÄLLARE */
DO:
   ASSIGN FILL-IN_BEST = INPUT FILL-IN_BEST.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEST C-Win
ON MOUSE-SELECT-CLICK OF FILL-IN_BEST IN FRAME DEFAULT-FRAME /* BESTÄLLARE */
DO:
  APPLY "ENTRY" TO FILL-IN_BEST. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FASTAPLANNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTAPLANNR C-Win
ON LEAVE OF FILL-IN_FASTAPLANNR IN FRAME DEFAULT-FRAME
DO:
   ASSIGN FILL-IN_FASTAPLANNR = INPUT FILL-IN_FASTAPLANNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FASTAPLANNR C-Win
ON MOUSE-SELECT-CLICK OF FILL-IN_FASTAPLANNR IN FRAME DEFAULT-FRAME
DO:
   IF INPUT FILL-IN_FASTAPLANNR = FALSE THEN ASSIGN FILL-IN_FASTAPLANNR = TRUE. 
   IF INPUT FILL-IN_FASTAPLANNR = TRUE THEN ASSIGN FILL-IN_FASTAPLANNR = FALSE. 
   DISPLAY FILL-IN_FASTAPLANNR WITH FRAME {&FRAME-NAME}.                       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT C-Win
ON LEAVE OF FILL-IN_ORT IN FRAME DEFAULT-FRAME /* Ort/Benämning */
DO:
   ASSIGN FILL-IN_ORT = INPUT FILL-IN_ORT. 
   FILL-IN_ORT = TRIM(FILL-IN_ORT).
   DISPLAY FILL-IN_ORT WITH FRAME {&FRAME-NAME}.
   IF FILL-IN_ORT = "" THEN DO:
      MESSAGE "Får ej vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RUN AONRNAMNKOLL.P (INPUT 1, INPUT FILL-IN_ORT,OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.  
   /*
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,"\") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,"/") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,":") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,"*") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,"<") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,">") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF INDEX(FILL-IN_ORT:SCREEN-VALUE,"|") NE 0 THEN DO:
      MESSAGE "Benämningen får ej innehålla \/:* <>|" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
*/
   DISPLAY FILL-IN_ORT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PLANNRAVDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PLANNRAVDATUM C-Win
ON MOUSE-MENU-CLICK OF FILL-IN_PLANNRAVDATUM IN FRAME DEFAULT-FRAME /* Avslutat */
DO:
   ASSIGN
   FILL-IN_PLANNRAVDATUM = INPUT FILL-IN_PLANNRAVDATUM.
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_PLANNRAVDATUM.
   RUN AlmanBtn.w.
   IF FILL-IN_PLANNRAVDATUM = Guru.GlobalaVariabler:regdatum THEN FILL-IN_PLANNRAVDATUM:MODIFIED = FALSE.
   ELSE DO: 
      FILL-IN_PLANNRAVDATUM = Guru.GlobalaVariabler:regdatum.
      DISPLAY FILL-IN_PLANNRAVDATUM WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTD C-Win
ON LEAVE OF FILL-IN_SLUTD IN FRAME DEFAULT-FRAME /* Slutdatum */
DO:
   FILL-IN_SLUTD = INPUT FILL-IN_SLUTD.
   IF FILL-IN_SLUTD < FILL-IN_STARTD THEN DO:
      FILL-IN_STARTD = FILL-IN_SLUTD.
      DISPLAY FILL-IN_STARTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN_SLUTD IN FRAME DEFAULT-FRAME /* Slutdatum */
DO:
   ASSIGN
   FILL-IN_SLUTD = INPUT FILL-IN_SLUTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_SLUTD.
   RUN AlmanBtn.w.
   FILL-IN_SLUTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_SLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_STARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_STARTD C-Win
ON LEAVE OF FILL-IN_STARTD IN FRAME DEFAULT-FRAME /* Startdatum */
DO:
   FILL-IN_STARTD = INPUT FILL-IN_STARTD.
   IF FILL-IN_SLUTD < FILL-IN_STARTD THEN DO:
      FILL-IN_SLUTD = FILL-IN_STARTD.
      DISPLAY FILL-IN_SLUTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_STARTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN_STARTD IN FRAME DEFAULT-FRAME /* Startdatum */
DO:
  ASSIGN
   FILL-IN_STARTD = INPUT FILL-IN_STARTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_STARTD.
   RUN AlmanBtn.w.
   FILL-IN_STARTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_STARTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_ANM C-Win
ON CHOOSE OF MBTN_ANM IN FRAME DEFAULT-FRAME /* Anmärkningar */
DO:
   RUN btnvit_UI (INPUT MBTN_ANM:HANDLE).
   RAD_VAL = 3.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_DOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_DOK C-Win
ON CHOOSE OF MBTN_DOK IN FRAME DEFAULT-FRAME /* Dokument */
DO:
   RUN btnvit_UI (INPUT MBTN_DOK:HANDLE).
   RAD_VAL = 6.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KONT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KONT C-Win
ON CHOOSE OF MBTN_KONT IN FRAME DEFAULT-FRAME /* Konton */
DO:
   RUN btnvit_UI (INPUT MBTN_KONT:HANDLE).
   RAD_VAL = 2.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KOPPL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KOPPL C-Win
ON CHOOSE OF MBTN_KOPPL IN FRAME DEFAULT-FRAME /* Kopplingar */
DO:
   RUN btnvit_UI (INPUT MBTN_KOPPL:HANDLE).
   RAD_VAL = 1.
   APPLY "VALUE-CHANGED" TO RAD_VAL.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_OVR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_OVR C-Win
ON CHOOSE OF MBTN_OVR IN FRAME DEFAULT-FRAME /* Övrigt */
DO:
   RUN btnvit_UI (INPUT MBTN_OVR:HANDLE).
   RAD_VAL = 8.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON MOUSE-SELECT-CLICK OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL C-Win
ON VALUE-CHANGED OF RAD_VAL IN FRAME DEFAULT-FRAME
DO:
   DEFINE VARIABLE status-mus2 AS LOGICAL NO-UNDO.
   RUN chbstate_UI.
/*    RAD_VAL = INPUT RAD_VAL. */
   RUN goma_UI.
   IF RAD_VAL = 1 THEN DO:
      FILL-IN-KOPP = "Kopplingar". 
      IF openqkoll[1] = FALSE THEN DO:
         openqkoll[1] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         RUN openbdynspec_UI IN brwproc[9].
         RUN vcbrwkopp_UI.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      ASSIGN
      BTN_KALKAR:HIDDEN = FALSE
      BRW_KOPP:HIDDEN = FALSE
      BTN_GATILL:HIDDEN = FALSE.      
      APPLY "VALUE-CHANGED" TO BRW_KOPP IN FRAME {&FRAME-NAME}.
   END.
   IF RAD_VAL = 2 THEN DO:
      /*konton*/
      FILL-IN-KOPP = "Konton".
      IF openqkoll[2] = FALSE THEN DO:
         openqkoll[2] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         OPEN QUERY BRW_PLANKTO FOR EACH plankonttemp WHERE 
         plankonttemp.PLANNR = upplantemp.PLANNR AND 
         plankonttemp.ARTAL = upplantemp.ARTAL NO-LOCK.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      /*BTN_GATILL:LABEL = "Ändra".
      BTN_GATILL:HIDDEN = FALSE.
      */
      BRW_PLANKTO:HIDDEN = FALSE.
      BTN_ANDRAK:HIDDEN = FALSE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         RUN movefillin_UI (INPUT FILL-IN_PKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT FILL-IN_PKOD:ROW, INPUT BRW_PLANKTO:COLUMN + BRW_PLANKTO:WIDTH-CHAR + 13).                 
         ASSIGN
         BRW_PRIO:COLUMN        = BRW_PLANKTO:COLUMN + BRW_PLANKTO:WIDTH-CHAR + 1.  
         
         ASSIGN
         BRW_PRIO:HIDDEN = FALSE 
         FILL-IN_PKOD:HIDDEN = FALSE.
         OPEN QUERY BRW_PRIO FOR EACH priotemp NO-LOCK.
         GET FIRST BRW_PRIO NO-LOCK.
         IF NOT AVAILABLE priotemp THEN DO:
            ASSIGN
            FILL-IN_PKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_PRIO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.          
         END.
         ELSE DO:
            FIND FIRST priotemp WHERE priotemp.PKOD = FILL-IN_PKOD NO-LOCK NO-ERROR.
            IF AVAILABLE priotemp THEN DO:
               RUN setlastrowid_UI IN brwproc[10] (INPUT ROWID(priotemp)).
               RUN lastselectdyn_UI IN brwproc[10]. 
            END.
         END.
         IF BRW_PRIO:HIDDEN = FALSE THEN DO:
            RUN movefillin_UI (INPUT FILL-IN_ARBARTKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT FILL-IN_ARBARTKOD:ROW, INPUT BRW_PRIO:COLUMN + BRW_PRIO:WIDTH-CHAR + 13).
            BRW_ARBART:COLUMN        = BRW_PRIO:COLUMN + BRW_PRIO:WIDTH-CHAR + 1.
         END.   
         ELSE DO:
            RUN movefillin_UI (INPUT FILL-IN_ARBARTKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT FILL-IN_ARBARTKOD:ROW, INPUT BRW_PLANKTO:COLUMN + BRW_PLANKTO:WIDTH-CHAR + 13).
            BRW_ARBART:COLUMN        = BRW_PLANKTO:COLUMN + BRW_PLANKTO:WIDTH-CHAR + 1.
         END.     

         ASSIGN
         BRW_ARBART:HIDDEN = FALSE 
         FILL-IN_ARBARTKOD:HIDDEN = FALSE.
         OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
         GET FIRST BRW_ARBART NO-LOCK.
         IF NOT AVAILABLE arbarttemp THEN DO:
            ASSIGN
            FILL-IN_ARBARTKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BRW_ARBART:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.          
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
   IF RAD_VAL = 3 THEN DO:     
      /*"Benämning",3,*/      
      FILL-IN-KOPP = "Anmärkning".
      ASSIGN
      EDITOR_ANM:HIDDEN = FALSE.
      IF openqkoll[2] = FALSE THEN DO:
         openqkoll[2] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         ASSIGN EDITOR_ANM = upplantemp.ANM.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.    
   END.
   IF RAD_VAL = 6 THEN DO:     
      /*"Dokument",6,*/      
      FILL-IN-KOPP = "Dokumenthantering".
      ENABLE BTN_MAPP WITH FRAME {&FRAME-NAME}.
      ASSIGN
      BRW_AVTAL2:HIDDEN = FALSE
      BTN_NYA:HIDDEN = FALSE
      BTN_ANDA:HIDDEN = FALSE
      BTN_BORTA:HIDDEN = FALSE      
      BTN_VISA:HIDDEN = FALSE
      BTN_MAPP:HIDDEN = FALSE.
      IF openqkoll[6] = FALSE THEN DO:
         openqkoll[6] = TRUE.
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         OPEN QUERY BRW_AVTAL2 FOR EACH avtalaonrtemp WHERE 
         avtalaonrtemp.AONR = "PL" + upplantemp.PLANNR AND avtalaonrtemp.DELNR = upplantemp.ARTAL AND 
         avtalaonrtemp.LNK = TRUE NO-LOCK.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.     
   END.
   IF RAD_VAL = 8 THEN DO:
      FILL-IN-KOPP = "Övrigt".
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         RUN movefillin_UI (INPUT FILL-IN_ARBARTKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT FILL-IN_ARBARTKOD:ROW, INPUT arbfspar).        
         ASSIGN
         BRW_ARBART:COLUMN        = arbbspar.  
      END.      
      /*"Övrigt"*/      
      ASSIGN 
      BRW_ARBART:HIDDEN = FALSE 
      FILL-IN_ARBARTKOD:HIDDEN = FALSE.             
      IF openqkoll[8] = FALSE THEN DO:
         openqkoll[8] = TRUE.
         OPEN QUERY BRW_ARBART FOR EACH arbarttemp NO-LOCK.
         status-mus2 = SESSION:SET-WAIT-STATE("").
      END.
      GET FIRST BRW_ARBART NO-LOCK.
      IF NOT AVAILABLE arbarttemp THEN DO:
         ASSIGN
         FILL-IN_ARBARTKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_ARBART:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.          
      END.
      ELSE DO:
         FIND FIRST arbarttemp WHERE arbarttemp.ARBARTKOD = FILL-IN_ARBARTKOD NO-LOCK NO-ERROR.
         IF AVAILABLE arbarttemp THEN DO:
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(arbarttemp)).
            RUN lastselectdyn_UI IN brwproc[5]. 
         END.
      END.      
   END.
   DISPLAY FILL-IN-KOPP WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANSV
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
   IF VALID-HANDLE(nyttaoejapph) THEN DELETE PROCEDURE nyttaoejapph NO-ERROR.
   IF VALID-HANDLE(nyph) THEN DELETE PROCEDURE nyph NO-ERROR.
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   
   RUN disable_UI.
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
   FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND 
   valplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF AVAILABLE valplantemp THEN DO:
      RUN laddaplandokao IN nyttaoapph (INPUT plannrvar,INPUT artalvar,OUTPUT TABLE avtalaonrtemp).
      FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND 
      upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE upplantemp THEN DO:
         CREATE upplantemp.
      END.
      BUFFER-COPY valplantemp TO upplantemp.
      ASSIGN 
      upplantemp.TIDANSV = valplantemp.ARBANSVARIG
      upplantemp.TIDOMR = valplantemp.OMRADE
      upplantemp.TIDBESTID = valplantemp.BESTID.
      /*Konvertering av årvecko till datum*/
      IF LENGTH(STRING(upplantemp.SLUTVNR)) <= 3 THEN DO:
         ASSIGN
         regvnr = upplantemp.SLUTVNR
         regdagnamn = "MÅN".
         RUN VECODAT.P.
         upplantemp.SLUTVNR = INTEGER(STRING(regdatum,"99999999")).  
         ASSIGN
         regvnr = upplantemp.STARTVNR
         regdagnamn = "MÅN".
         RUN VECODAT.P.
         upplantemp.STARTVNR = INTEGER(STRING(regdatum,"99999999")).
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 50
         soktemp.SOKCHAR[1] = plannrvar
         soktemp.SOKINT[1] = artalvar
         soktemp.SOKINT[2] = upplantemp.STARTVNR
         soktemp.SOKINT[3] = upplantemp.SLUTVNR.       
         {SOKANROP.I}    
      END.
   END.
   RUN appat_UI (INPUT 1,INPUT nyplanvar).
   RAD_VAL = 1.
   APPLY "VALUE-CHANGED" TO RAD_VAL IN FRAME {&FRAME-NAME}. 
   RUN arbartout_UI IN nyttaoapph (OUTPUT TABLE arbarttemp).
   EMPTY TEMP-TABLE priotemp NO-ERROR. 
   RUN laddaprio IN nyttaoapph ( OUTPUT TABLE priotemp).
   RUN huvud_UI.
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   ASSIGN
   arbbspar = BRW_ARBART:COLUMN   
   arbfspar = FILL-IN_ARBARTKOD:COLUMN.
    /*LADDAR ÅR I CMB_ARTAL*/  
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 64. 
   {SOKANROP.I}
   ASSIGN
   startartal = soktemp.SOKINT[1] 
   slutartal =   soktemp.SOKINT[2]
   uppar = startartal
   slutar = slutartal + 5   
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).
   DO WHILE uppar < slutar:
       uppar = uppar + 1.
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0").
   IF upplantemp.ARTAL = ? THEN DO:
      ASSIGN
      CMB_ARTAL:SCREEN-VALUE = STRING(YEAR(TODAY),"9999").
   END.
   ELSE DO:
      ASSIGN
      CMB_ARTAL:SCREEN-VALUE = STRING(upplantemp.ARTAL,"9999").
   END.
   CMB_ARTAL = INPUT CMB_ARTAL.
   IF upplantemp.STARTVNR = 0 THEN DO:
      regdatum = TODAY.
      RUN REGVEC.P.
      RUN REGDAG.P.
      RUN VECODAT.P.
      ASSIGN
      FILL-IN_SLUTD = regdatum + 365
      FILL-IN_STARTD = regdatum.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_SLUTD = DATE(STRING(upplantemp.SLUTVNR))
      FILL-IN_STARTD = DATE(STRING(upplantemp.STARTVNR)).
   END.
   ASSIGN
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk
   CMB_DEBPR:LABEL = Guru.Konstanter:gdebk
   FILL-IN_PLANNR = upplantemp.PLANNR
   FILL-IN_ARBANSVARIG = upplantemp.ARBANSVARIG
   sparansv = upplantemp.ARBANSVARIG
   CMB_ARTAL = upplantemp.ARTAL
   artalvar = upplantemp.ARTAL
   gammar = upplantemp.ARTAL
   FILL-IN_ORT = upplantemp.ORT.
   RUN enable_UI.
   {FRMSIZE.I}
   
   RUN knappar_UI (INPUT 1).
   RUN huvud2_UI.
   
   ASSIGN
   BRW_ANSV:HIDDEN = TRUE.
   my1hand = FILL-IN_STARTD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_NVE:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_NVE:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_FVE:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_FVE:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_SLUTD:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_SLUTD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_NVE-2:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_NVE-2:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BTN_FVE-2:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BTN_FVE-2:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_ANSV:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BRW_ANSV:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_ARBANSVARIG:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_ARBANSVARIG:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_ANSV:MOVE-AFTER-TAB-ITEM(my1hand).
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:varforetypval[2] = 1 THEN DO:
      ASSIGN
      CMB_DEBPR:HIDDEN = TRUE.
   END.   
   ASSIGN
   FILL-IN_BEST:LABEL IN FRAME {&FRAME-NAME} = Guru.Konstanter:gbestk   
   BRW_BEST:TITLE = "Välj " + LC(Guru.Konstanter:gbestk)
   BRW_ANSV:TITLE = "Välj " + LC(Guru.Konstanter:garbak)
   FILL-IN_ARBANSVARIG:LABEL = Guru.Konstanter:garbak
   FILL-IN_PLANNR:LABEL = Guru.Konstanter:gplk
   bestkundallt.VIBESTID:LABEL IN BROWSE BRW_BEST = Guru.Konstanter:gbestk
   C-Win:TITLE = "Guru- " + LC(Guru.Konstanter:gpll).
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      /*HÄR SELNÄT*/
      ASSIGN CMB_OMR:LABEL  =  Guru.Konstanter:gomrl.     
   END.
   ELSE CMB_OMR:LABEL = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk).
   RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF  Guru.Konstanter:varforetypval[45] = 1 THEN DO:
      /*HÄR SELNÄT*/      
      FILL-IN_BEST = " " .
      FILL-IN_BEST:HIDDEN = TRUE.      
      IF CMB_OMR:SCREEN-VALUE NE "" THEN DO:                                  
         FIND FIRST omrtemp WHERE omrtemp.NAMN = INPUT CMB_OMR NO-LOCK NO-ERROR.
         IF AVAILABLE omrtemp THEN valford = 'priotemp.OMRADE = "'  + omrtemp.OMRADE + '"'.
         ELSE valford = "".
         RUN setcolsortvar_UI IN brwproc[10] (INPUT valford).
         RUN openbdynspec_UI IN brwproc[10].         
      END.           
   END.   
   {musarrow.i}
   RUN menyrad_UI.
   RUN vcbrwkopp_UI.
   RUN btnvit_UI (INPUT MBTN_KOPPL:HANDLE).
   
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
   IF nyplanvar = TRUE THEN ENABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
   ELSE DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
        
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
   ansvplantemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_ANSV = TRUE.
   bestkundallt.VIBESTID:READ-ONLY IN BROWSE BRW_BEST = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANSV:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_BEST:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_PLANKTO:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_ARBART:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[7]
      (INPUT BRW_AVTAL2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[9]
      (INPUT BRW_KOPP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[10]
      (INPUT BRW_PRIO:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN brwsetupstop_UI IN brwproc[9] (INPUT 1).
   RUN setcolindex_UI IN brwproc[9] (INPUT "gatill.F1 BY gatill.F2").  
   RUN dynprogextra IN brwproc[9]  (INPUT "rowdispkopp_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[9] (INPUT TRUE).
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTPLANAPP.P PERSISTENT SET nyph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN NYTTPLANAPP.P PERSISTENT SET nyph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.   
   IF Guru.Konstanter:varforetypval[21] = 1 THEN DO:
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aonr_UI C-Win 
PROCEDURE aonr_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
   {AVBGOM.I}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 34
   soktemp.SOKCHAR[2] = gatill.AONR
   soktemp.SOKINT[1]  = gatill.DELNR.      
   {SOKANROP.I}
   aonrrec = soktemp.SOKINT[3].
   
   IF gatill.AONR  = ? THEN DO:
      CREATE felmeddtemp.
      ASSIGN felmeddtemp.VAL = 1.
      vartpro = "NAOPLAN".
      RUN NYTTAONRU.W (INPUT TRUE,INPUT FALSE, INPUT 1,
                      INPUT-OUTPUT aonrvar, 
                      INPUT-OUTPUT delnrvar, 
                      INPUT upplantemp.PLANNR,
                      INPUT upplantemp.ARTAL).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         DELETE felmeddtemp.
      END.
      vartpro = "".
      IF musz = TRUE THEN DO:
         musz = FALSE.
         IF aonrvar = ? OR aonrvar = "" THEN DO:
            {AVBFRAM.I}
            RETURN.
         END.
         ELSE DO:
            
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 80
            soktemp.SOKCHAR[1] = aonrvar
            soktemp.SOKINT[1] = delnrvar.
            {SOKANROP.I}
         END.
         {AVBFRAM.I}
         RETURN.
      END.
      RUN sattao_UI IN Guru.SharedVariable:btnaonrh (INPUT aonrvar, INPUT delnrvar).
      RUN extrakopp_UI IN Guru.SharedVariable:btnaonrh (INPUT "PLAN").
      
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 34
      soktemp.SOKCHAR[2] = aonrvar
      soktemp.SOKINT[1] = delnrvar.
      {SOKANROP.I}
      aonrrec = soktemp.SOKINT[3].
      {FORETIDL.I}
      RUN kalkkopp_UI (INPUT aonrvar, INPUT delnrvar).       
   END.
   ELSE DO: 
      ASSIGN
      vartpro = "NAOPLAN"
      aonrvar = gatill.AONR 
      delnrvar = gatill.DELNR.
      
      RUN NYTTAONRU.W (INPUT FALSE,INPUT FALSE, INPUT 1,
                      INPUT-OUTPUT aonrvar, 
                      INPUT-OUTPUT delnrvar,
                      INPUT upplantemp.PLANNR,INPUT upplantemp.ARTAL).
     
       IF musz = TRUE THEN DO:
          musz = FALSE.      
          {AVBFRAM.I}
          RETURN.
       END.
      {FORETIDL.I}
      vartpro = "".
   END.
   FOR EACH gatill WHERE gatill.TYPCHAR = "AOR":
      DELETE gatill.
   END.
   RUN gatillhamt_UI (INPUT "AOR").
   
   RUN openbdynspec_UI IN brwproc[9].
   RUN vcbrwkopp_UI.
   {AVBFRAM.I}   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appat_UI C-Win 
PROCEDURE appat_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyplanvar AS LOGICAL NO-UNDO.
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTPLUAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vart,INPUT nyplanvar,INPUT plannrvar,INPUT artalvar,
      INPUT TABLE bestkundallt,OUTPUT TABLE ansvplantemp,
      OUTPUT TABLE plankonttemp,OUTPUT TABLE automregtemp,
      OUTPUT TABLE kbenamntemp,
      INPUT-OUTPUT TABLE upplantemp).    
   END.
   ELSE DO:
      RUN NYTTPLUAPP.P
      (INPUT vart,INPUT nyplanvar,INPUT plannrvar,INPUT artalvar,
      INPUT TABLE bestkundallt,OUTPUT TABLE ansvplantemp,
      OUTPUT TABLE plankonttemp,OUTPUT TABLE automregtemp,
      OUTPUT TABLE kbenamntemp,
      INPUT-OUTPUT TABLE upplantemp).   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bkopp_UI C-Win 
PROCEDURE bkopp_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
   FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND 
   upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF gatill.F2 = ? THEN DO:
      RUN KOPAO.W (INPUT plannrvar,INPUT artalvar, OUTPUT aonrvar,OUTPUT delnrvar).
      
      FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
      upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR. 
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN kalkkopp_UI (INPUT aonrvar, INPUT delnrvar).             
   END.
   ELSE DO:
      MESSAGE "Det finns ett " + LC(Guru.Konstanter:gaok) + " kopplat till " + LC(Guru.Konstanter:gplk) + ":" + upplantemp.PLANNR +
      " med Årtal:" + STRING(upplantemp.ARTAL) + "."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnaktiv2_UI C-Win 
PROCEDURE btnaktiv2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER statusniv AS INTEGER NO-UNDO.
   DEFINE VARIABLE kalknralt AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusnivallt AS CHARACTER NO-UNDO.
   IF statusniv = 1 THEN DO:         
      FIND FIRST gabuff WHERE gabuff.STATUSNIV = "UF" 
      NO-ERROR.      
      IF AVAILABLE gabuff THEN DO:
         IF gabuff.TYP = gatill.TYP  THEN gabuff.STATUSNIV = "ALT".
         ELSE gabuff.STATUSNIV = "HUV".               
         IF Guru.Konstanter:appcon THEN DO:
            RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
         END.
         ELSE DO:
            RUN KALKRUTIN.P 
            (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
         END.               
      END.
      gatill.STATUSNIV = "UF".
      FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "HUV"
      NO-ERROR.      
      IF AVAILABLE gabuff THEN DO:
         gabuff.STATUSNIV = "ALT".
         IF Guru.Konstanter:appcon THEN DO:
            RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
         END.
         ELSE DO:
            RUN KALKRUTIN.P 
            (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
         END.
      END.
   END.   
   IF statusniv = 2 THEN DO:
      IF gatill.STATUSNIV = "HUV" THEN DO:
         FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "ALT"
         NO-ERROR.      
         IF AVAILABLE gabuff THEN DO:
            ASSIGN
            gabuff.STATUSNIV = "HUV"
            gatill.STATUSNIV = "ALT".
            IF Guru.Konstanter:appcon THEN DO:
               RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
               (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.
            ELSE DO:
               RUN KALKRUTIN.P 
               (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.
         END.
      END.
      ELSE DO:
         FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "HUV"
         NO-ERROR.      
         IF AVAILABLE gabuff THEN DO:
            ASSIGN
            gabuff.STATUSNIV = "ALT"
            gatill.STATUSNIV = "HUV".               
            IF Guru.Konstanter:appcon THEN DO:
               RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
               (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.
            ELSE DO:
               RUN KALKRUTIN.P 
               (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.
         END.
         ELSE DO:
            FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "UF"
            NO-ERROR.      
            IF NOT AVAILABLE gabuff THEN DO:
               gatill.STATUSNIV = "HUV".          
            END.
         END.
      END.
   END.
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 1, INPUT INTEGER(gatill.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gatill.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
   END.
   ELSE DO:
      RUN KALKRUTIN.P 
      (INPUT 1, INPUT INTEGER(gatill.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gatill.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
   END.        
   FIND FIRST gatill WHERE gatill.TYPCHAR = gatilltyp AND
   gatill.F1  = gatill1 AND gatill.F2 = gatill2   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gatill THEN DO:
      FIND FIRST gatill WHERE gatill.TYPCHAR = gatilltyp AND
      gatill.F1  = gatill1 NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE gatill THEN RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(gatill)).
   RUN openbdynspec_UI IN brwproc[9].
   RUN lastselectdyn_UI IN brwproc[9].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnaktiv_UI C-Win 
PROCEDURE btnaktiv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE kalknralt AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusnivallt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE statusniv AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusnivspar AS INTEGER NO-UNDO.
   statusnivallt = "".
   status-ok = BRW_KOPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   /*KALKRUTIN*/
 
   IF AVAILABLE gatill THEN DO:
     ASSIGN
     gatilltyp = gatill.TYPCHAR
     gatill1   = gatill.F1
     gatill2   = gatill.F2.
   END.
   IF gatill.F1 = "Kalkyl Nätreg N1" THEN DO:
      IF gatill.STATUSNIV = "UF" THEN statusniv = 1.
      ELSE IF gatill.STATUSNIV = "UR" THEN statusniv = 2.
      ELSE IF gatill.STATUSNIV = "HUV" THEN statusniv = 3.
      ELSE IF gatill.STATUSNIV = "ALT" THEN statusniv = 4.
      statusnivspar = statusniv.
      RUN NATREGVAL.W (INPUT-OUTPUT statusniv).
      IF statusnivspar = statusniv THEN musz = TRUE.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.   
      IF statusniv = 1 THEN DO:
         RUN btnaktiv2_UI (INPUT 1).
         RETURN.
      END.   
      ELSE IF statusniv = 2 THEN DO:
         FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "UR" 
         AND ROWID(gabuff) NE ROWID(gatill)
         NO-ERROR.      
         IF AVAILABLE gabuff THEN DO:
            IF gabuff.TYP = gatill.TYP  THEN gabuff.STATUSNIV = "ALT".                     
            IF Guru.Konstanter:appcon THEN DO:
               RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
               (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.
            ELSE DO:
               RUN KALKRUTIN.P 
              (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.               
         END.
         gatill.STATUSNIV = "UR".
         RUN btnaktiv2_UI (INPUT 0).
         FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "HUV" 
         AND ROWID(gabuff) NE ROWID(gatill)
         NO-ERROR.
         IF NOT AVAILABLE gabuff THEN DO:
            FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "HUV" 
            AND ROWID(gabuff) NE ROWID(gatill)
            NO-ERROR.
         END.         
         IF NOT AVAILABLE gabuff THEN DO:
             MESSAGE "Det finns ingen kalyl under denna typ som är huvudkalkyl!"
                    VIEW-AS ALERT-BOX.
         END.   
         RETURN.
      END.   
      ELSE IF statusniv = 3 THEN DO:
         IF gatill.STATUSNIV = "UF" THEN DO:
            MESSAGE "Detta är en kalkyl för utförande!" SKIP
                    "Välj den kalkyl som skall vara utförandekalkyl"
                    VIEW-AS ALERT-BOX.
            RETURN.
         END.
          
         FIND FIRST gabuff WHERE gabuff.STATUSNIV = "UF" 
         NO-ERROR.      
         IF AVAILABLE gabuff THEN DO:
                      
         END.   
         ELSE DO:
            MESSAGE "Detta är en kalkyl för utförande!" SKIP
            "Välj den kalkyl som skall vara utförandekalkyl"
            VIEW-AS ALERT-BOX.
            RETURN.
         END. 
         FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "UF" 
         AND ROWID(gabuff) NE ROWID(gatill)
         NO-ERROR.
         IF NOT AVAILABLE gabuff THEN DO:
            FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "HUV" 
            AND ROWID(gabuff) NE ROWID(gatill)
            NO-ERROR.
         END.    
         IF AVAILABLE gabuff THEN DO: 
            gabuff.STATUSNIV = "ALT".                     
            IF Guru.Konstanter:appcon THEN DO:
               RUN KALKRUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
               (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.
            ELSE DO:
               RUN KALKRUTIN.P 
              (INPUT 1, INPUT INTEGER(gabuff.F2),INPUT kalknralt,INPUT ?,INPUT ?,INPUT gabuff.STATUSNIV,INPUT statusnivallt,OUTPUT TABLE gatill APPEND).
            END.               
         END.
         gatill.STATUSNIV = "HUV". 
         RUN btnaktiv2_UI (INPUT 0).
         RETURN.
      END.
      ELSE IF statusniv = 4 THEN DO:
         FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "UF" 
         AND ROWID(gabuff) NE ROWID(gatill)
         NO-ERROR. 
         IF NOT AVAILABLE gabuff THEN DO:
            FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "HUV"
            AND ROWID(gabuff) NE ROWID(gatill) 
            NO-ERROR. 
         END.
         IF NOT AVAILABLE gabuff THEN DO:
            FIND FIRST gabuff WHERE gabuff.TYP = gatill.TYP AND gabuff.STATUSNIV = "UR"
            AND ROWID(gabuff) NE ROWID(gatill) 
            NO-ERROR. 
         END.    
         IF AVAILABLE gabuff THEN DO:
            gatill.STATUSNIV = "ALT".
            RUN btnaktiv2_UI (INPUT 0).
            RETURN.
         END.   
         ELSE DO:
            MESSAGE "Det går inte att byta status på denna kalkyl!"
            VIEW-AS ALERT-BOX.
            RETURN.
         END.   
         RETURN.
      END.    
   END.   
   ELSE IF gatill.STATUSNIV = "UF" THEN DO:
      MESSAGE "Detta är en kalkyl för utförande!" SKIP
              "Välj den kalkyl som skall vara utförandekalkyl"
              VIEW-AS ALERT-BOX.
      RETURN.
   END.
   MESSAGE "Vill du att kalkylen skall vara en kalkyl för utförande? " 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE kval AS LOGICAL.
   CASE kval:
      WHEN TRUE THEN DO:
         RUN btnaktiv2_UI (INPUT 1).
         
      END.
      WHEN FALSE THEN DO:
         RUN btnaktiv2_UI (INPUT 2).         
      END.
      OTHERWISE RETURN.
   END CASE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnanda_UI C-Win 
PROCEDURE btnanda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   IF RAD_VAL = 6 THEN DO:
      FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = "PL" + upplantemp.PLANNR
      AND avtalaonrtemp.DELNR = upplantemp.ARTAL AND avtalaonrtemp.LNK = TRUE
      NO-LOCK NO-ERROR.

      IF AVAILABLE avtalaonrtemp THEN DO:                     
         status-ok = BRW_AVTAL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         IF SUBSTRING(avtalaonrtemp.AVTAL,31,7) = "" THEN DO: 
            ordnvar = avtalaonrtemp.ORDN.
            idvar = avtalaonrtemp.AVTID.
            musz = FALSE.
            ASSIGN
            mappvarhj = avtalaonrtemp.AONR + STRING(avtalaonrtemp.DELNR) + "\".          
            mappvar = grundmappvar +  mappvarhj.                                         
            IF grundmappvar NE "" THEN DO:
               FILE-INFO:FILE-NAME = mappvar.
               IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
                  MESSAGE "Mappen " mappvar " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool." VIEW-AS ALERT-BOX.
                  RETURN.
               END.
            END.
            
            RUN NYAVTALAO.W (INPUT avtalaonrtemp.AONR,INPUT avtalaonrtemp.DELNR,INPUT FALSE,
                             INPUT-OUTPUT ordnvar,INPUT-OUTPUT idvar,INPUT TRUE,
                             OUTPUT mappvar).
            
            IF musz = FALSE THEN DO: 
                        
               FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = "PL" + upplantemp.PLANNR AND 
               avtalaonrtemp.DELNR = upplantemp.ARTAL AND avtalaonrtemp.AVTID = idvar AND 
               avtalaonrtemp.ORDN = ordnvar AND avtalaonrtemp.LNK = TRUE NO-LOCK NO-ERROR. 
               
               RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(avtalaonrtemp)).         
               RUN openbdynspec_UI IN brwproc[7]. 
               RUN lastselectdyn_UI IN brwproc[7].                
            END.
            ELSE musz = FALSE.       
         END.
         ELSE DO:
            MESSAGE "Dokument är inaktivt och går ej att ändra."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN.
         END.
      END.
      ELSE DO:
         MESSAGE "Det finns inget dokument att ändra."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
   END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnborta_UI C-Win 
PROCEDURE btnborta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE filnamnvar AS CHARACTER NO-UNDO.
   IF RAD_VAL = 6 THEN DO:
      status-ok = BRW_AVTAL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE avtalaonrtemp THEN DO:         
         MESSAGE "Vill du ta bort dokument: " + avtalaonrtemp.BEN + "?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
         IF svar THEN DO:                          
            MESSAGE "Vill du ta bort filen som är kopplad till dokument: " + avtalaonrtemp.BEN + "?" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Ta bort fil?" UPDATE svar.
            IF svar THEN DO:
               RUN bortfilnamn IN nyttaoapph (INPUT avtalaonrtemp.AVTID,
                                                INPUT avtalaonrtemp.AONR,
                                                INPUT avtalaonrtemp.DELNR,
                                                INPUT avtalaonrtemp.ORDN,
                                                OUTPUT filnamnvar).
               RUN bortfil IN nyttaoejapph  (INPUT filnamnvar).                  
            END.
            RUN bortavtal IN nyttaoapph (INPUT 2,INPUT avtalaonrtemp.AVTID,
                                         INPUT avtalaonrtemp.AONR,
                                         INPUT avtalaonrtemp.DELNR,
                                         INPUT avtalaonrtemp.ORDN).
            DELETE avtalaonrtemp.
            RUN selnextprevrow_UI IN brwproc[7].
            
            
            /*
            OPEN QUERY BRW_AVTAL2 FOR EACH avtalaonrtemp WHERE avtalaonrtemp.AONR = uppaonrtemp.AONR
            AND avtalaonrtemp.DELNR = uppaonrtemp.DELNR AND avtalaonrtemp.LNK = TRUE NO-LOCK.
            */
            RUN openbdynspec_UI IN brwproc[7]. 
            RUN lastselectdyn_UI IN brwproc[7].             
         END.      
      END.
      ELSE DO:
         MESSAGE "Det finns inget dokument att ta bort."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
   END.
   
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

   ASSIGN
   gatilltyp = gatill.TYPCHAR
   gatill1   = gatill.F1
   gatill2   = gatill.F2.

   IF RAD_VAL = 2 THEN DO:
      CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         MESSAGE "Område kan inte vara blankt!" VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
      ELSE DO TRANSACTION:
         FIND CURRENT upplantemp NO-LOCK NO-ERROR.
         FILL-IN_ORT = INPUT FILL-IN_ORT.
         ASSIGN
         upplantemp.ORT = FILL-IN_ORT
         upplantemp.OMRADE = omrtemp.OMRADE.
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "16" THEN DO:
            IF FILL-IN_ARBARTKOD = 1 THEN DO:
               MESSAGE "Arbetsarten får inte vara 1"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
         END.       
      END.
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:               
         IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "10" THEN DO:
            IF FILL-IN_ARBARTKOD = 1 THEN DO:
               MESSAGE "Arbetsarten får inte vara 1"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
         END.         
      END.      
      FIND CURRENT upplantemp NO-LOCK NO-ERROR.
    /*  RUN btnok_UI.*/
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.
      RUN AONRKONTU.W (INPUT 2,INPUT ?,INPUT ?,INPUT plannrvar,
                     INPUT artalvar,INPUT upplantemp.FASTAPLANNR,
                     INPUT upplantemp.OMRADE).
/*       RUN AONRKONT.W (INPUT 2,INPUT ?,INPUT ?,INPUT plannrvar,    */
/*                      INPUT artalvar,INPUT upplantemp.FASTAPLANNR, */
/*                      INPUT upplantemp.OMRADE).                    */
      OPEN QUERY BRW_PLANKTO FOR EACH plankonttemp WHERE 
      plankonttemp.PLANNR = upplantemp.PLANNR AND 
      plankonttemp.ARTAL = upplantemp.ARTAL NO-LOCK.
      musz = FALSE.
      RETURN.
   END.
   ELSE IF gatill.TYPCHAR = "KAL" THEN DO:
      IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:
         IF gatill.F2 = ? THEN DO:               
            IF gatill.F1 = "Kalkyl typ fri" THEN DO:
               MESSAGE "För att skapa en fri kalkyl måste du visa en vanlig kalkyl.".
               {AVBFRAM.I}   
               RETURN.
            END.
            IF Guru.Konstanter:kalk2sekvar[1] = TRUE THEN DO:                  
               RUN btnok_UI.
               IF musz = TRUE THEN DO:
                  musz = FALSE.
                  RETURN.
               END.
               RUN kalk_UI (INPUT 1).
            END.
            ELSE DO:
               MESSAGE "Ni är inte behörig att skapa en kalkyl.".
            END.
         END.
         ELSE DO:
            RUN btnok_UI.
            IF musz = TRUE THEN DO:
               musz = FALSE.
               RETURN.
            END.
            RUN kalk_UI (INPUT 2).
         END.
      END.
      ELSE DO:
         MESSAGE "Ni är inte behörig att gå till kalkylering.".
      END.
   END.
   ELSE IF gatill.TYPCHAR = "AOR" THEN DO:
      IF Guru.Konstanter:hoppsekvar[1] = TRUE THEN DO:
         RUN btnok_UI.
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
         RUN aonr_UI.
      END.
      ELSE DO:
         IF gatill.F2 = ? THEN DO:               
            MESSAGE "Ni är inte behörig att skapa ett " LC(Guru.Konstanter:gaok) ".".            
         END.
         ELSE DO:
            MESSAGE "Ni är inte behörig att gå till " LC(Guru.Konstanter:gaok) "-hantering".
         END.
      END.
   END.

   RUN repa_UI.   
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
   IF RAD_VAL = 6 THEN DO:
      musz = FALSE.
      ordnvar = ?.
      idvar = ?.
      ASSIGN
      mappvarhj = "PL" + upplantemp.PLANNR + STRING(upplantemp.ARTAL) + "\".          
      mappvar = grundmappvar +  mappvarhj.                                         
      IF grundmappvar NE "" THEN DO:
         FILE-INFO:FILE-NAME = mappvar.     
         IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
            MESSAGE "Mappen " mappvar " finns inte. Det är troligen ett sekretessproblem. Kontakta Elpool." VIEW-AS ALERT-BOX.
            RETURN.
         END.
      END.      
      
      RUN NYAVTALAO.W (INPUT "PL" + upplantemp.PLANNR,INPUT upplantemp.ARTAL,INPUT TRUE,
                             INPUT-OUTPUT ordnvar,INPUT-OUTPUT idvar,INPUT TRUE,
                             OUTPUT mappvar).
      
      IF musz = FALSE THEN DO:
         FIND FIRST avtalaonrtemp WHERE avtalaonrtemp.AONR = "PL" + upplantemp.PLANNR AND 
         avtalaonrtemp.DELNR = upplantemp.ARTAL AND avtalaonrtemp.ORDN = ordnvar AND 
         avtalaonrtemp.AVTID = idvar AND avtalaonrtemp.LNK = TRUE NO-LOCK NO-ERROR.         
         RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(avtalaonrtemp)).         
         RUN openbdynspec_UI IN brwproc[7]. 
         RUN lastselectdyn_UI IN brwproc[7]. 
         IF avtalaonrtemp.FIL NE "" THEN DO:
            RUN OPENDOC.P (avtalaonrtemp.FIL,"","",NO).
         END.         
         ELSE DO:
            MESSAGE "Det finns ingen fil kopplad till dokumentet."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
      END.
      ELSE musz = FALSE.
   END.
   
     
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
   FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
   upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   ASSIGN
   musz = FALSE
   FILL-IN_BEST = INPUT FRAME {&FRAME-NAME} FILL-IN_BEST
   CMB_ARTAL = INPUT FRAME {&FRAME-NAME} CMB_ARTAL
   CMB_DEBPR = INPUT CMB_DEBPR
   CMB_OMR = INPUT CMB_OMR
   FILL-IN_PLANNRAVDATUM = INPUT FILL-IN_PLANNRAVDATUM
   FILL-IN_ARBANSVARIG = INPUT FILL-IN_ARBANSVARIG
   FILL-IN_PKOD = INPUT FILL-IN_PKOD
   FILL-IN_ARBARTKOD = INPUT FILL-IN_ARBARTKOD
   FILL-IN_ORT = INPUT FILL-IN_ORT
   FILL-IN_SLUTD = INPUT FILL-IN_SLUTD
   FILL-IN_STARTD = INPUT FILL-IN_STARTD
   EDITOR_ANM = INPUT EDITOR_ANM.
   IF FILL-IN_PLANNRAVDATUM = ? THEN  FILL-IN_PLANNRAVDATUM = 01/01/1991.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      ASSIGN upplantemp.OMRADE = omrtemp.OMRADE.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
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
            MESSAGE "Arbetsarten får inte vara 1"
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.            
         END.

         FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.   
         IF FILL-IN_ARBARTKOD = 11 THEN DO:
            FIND FIRST plankonttemp WHERE plankonttemp.K1 NE "1622" NO-LOCK NO-ERROR.
            IF AVAILABLE plankonttemp THEN DO:
               MESSAGE "Arbetsarten 11 måste ha " + kbenamntemp.K1 + " 1622"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
         END.
         IF FILL-IN_ARBARTKOD = 12 THEN DO:
            FIND FIRST plankonttemp WHERE plankonttemp.K1 NE "1623" NO-LOCK NO-ERROR.
            IF AVAILABLE plankonttemp THEN DO:
               MESSAGE "Arbetsarten 12 måste ha " + kbenamntemp.K1 + " 1623"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
         END.
      END.         
   END.
   IF Guru.Konstanter:varforetypval[45] = 1  THEN DO:
      /*HÄR SELNÄT*/
      FIND FIRST priotemp WHERE priotemp.OMRADE = omrtemp.OMRADE NO-ERROR.
      IF AVAILABLE priotemp THEN DO:
         IF FILL-IN_PKOD = ? OR FILL-IN_PKOD = 0 THEN DO:                                    
            MESSAGE Guru.Konstanter:gpriok " är obligatorisk."
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.            
         END.
      END.   
   END.    
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:         
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      NO-LOCK NO-ERROR.       
      IF AVAILABLE omrtemp AND omrtemp.OMRADE BEGINS "10" THEN DO:
         IF FILL-IN_ARBARTKOD = 1 THEN DO:
            MESSAGE "Arbetsarten får inte vara 1"
            VIEW-AS ALERT-BOX.            
            musz = TRUE.
            RETURN NO-APPLY.            
         END.
         FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.   
         IF FILL-IN_ARBARTKOD = 32 THEN DO:
            FIND FIRST plankonttemp WHERE plankonttemp.K1 NE "016" NO-LOCK NO-ERROR.
            IF AVAILABLE plankonttemp THEN DO:
               MESSAGE "Arbetsarten 32 måste ha " + kbenamntemp.K1 + " 016"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
         END.
         IF FILL-IN_ARBARTKOD = 36 THEN DO:
            FIND FIRST plankonttemp WHERE plankonttemp.K1 NE "017" NO-LOCK NO-ERROR.
            IF AVAILABLE plankonttemp THEN DO:
               MESSAGE "Arbetsarten 36 måste ha " + kbenamntemp.K1 + " 017"
               VIEW-AS ALERT-BOX.            
               musz = TRUE.
               RETURN NO-APPLY.            
            END.
         END.
      END.         
   END.
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BESt
   USE-INDEX BESTID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:
      FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_OMR
      NO-LOCK NO-ERROR.
   END.
   IF CMB_DEBPR NE "TOT.PRIS." THEN DO:
      DEFINE VARIABLE val2 AS LOGICAL NO-UNDO.
      MESSAGE "Vill du att " LC(Guru.Konstanter:gdebk) " skall vara ? " cmb_debpr skip
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
   IF CMB_ARTAL NE artalvar THEN DO:
      IF upplantemp.UPP = TRUE AND upplantemp.UPPNR = TRUE THEN DO:
         MESSAGE "Det går inte att byta årtal då " + LC(Guru.Konstanter:gplk) + ":et är årsuppdelat. Ta bort årsuppdelningen om du vill byta årtal."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         CMB_ARTAL:SCREEN-VALUE = STRING(artalvar,"9999").
         APPLY "ENTRY" TO CMB_ARTAL IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.   
      IF upplantemp.UPP = TRUE AND upplantemp.UPPNR = FALSE THEN DO:
         MESSAGE "Det går inte att byta årtal då det är ett årsuppdelat " + LC(Guru.Konstanter:gplk) + "."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         CMB_ARTAL:SCREEN-VALUE = STRING(artalvar,"9999").
         APPLY "ENTRY" TO CMB_ARTAL IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      RUN nyttplanapp_UI (INPUT 2).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         musz = TRUE.
         APPLY "ENTRY" TO CMB_ARTAL IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
      FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
      upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF AVAILABLE upplantemp THEN DO:
         ASSIGN 
         upplantemp.ARTAL = CMB_ARTAL
         artalvar = CMB_ARTAL. 
      END.   
   END.
   FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
   upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF FILL-IN_ARBARTKOD = ? OR FILL-IN_ARBARTKOD = 0 THEN DO:
      FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
      IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
   END.
   IF FILL-IN_FASTAPLANNR NE upplantemp.FASTAPLANNR THEN DO:
      DEFINE VARIABLE val3 AS LOGICAL NO-UNDO.
      MESSAGE "Du byter " LC(Guru.Konstanter:gplk) ":ets tillhörighet mellan " Guru.Konstanter:gtillk " och " Guru.Konstanter:gfastk "." SKIP 
      "Vill du ändå fortsätta ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3.
      CASE val3:
         WHEN TRUE THEN DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 65
            soktemp.SOKCHAR[1] = upplantemp.PLANNR
            soktemp.SOKLOG[1]  = upplantemp.FASTAPLANNR.
            {SOKANROP.I}         
         END.
         WHEN FALSE THEN DO:
            musz = TRUE.
            RETURN NO-APPLY.     
         END.
      END CASE.
   END.
   ASSIGN
   upplantemp.PLANNR          =   FILL-IN_PLANNR
   upplantemp.ARTAL           =   CMB_ARTAL
   upplantemp.PLANNRAVDATUM   =   FILL-IN_PLANNRAVDATUM
   upplantemp.ARBANSVARIG     =   FILL-IN_ARBANSVARIG
   upplantemp.PRISTYP         =   CMB_DEBPR
   upplantemp.ORT             =   FILL-IN_ORT
   upplantemp.ANM             =   EDITOR_ANM
   upplantemp.SLUTVNR         =   INTEGER(STRING(FILL-IN_SLUTD,"99999999"))
   upplantemp.STARTVNR        =   INTEGER(STRING(FILL-IN_STARTD,"99999999"))
   upplantemp.PKOD = FILL-IN_PKOD
   upplantemp.ARBARTKOD = FILL-IN_ARBARTKOD.      
   IF FILL-IN_BEST = "" THEN ASSIGN upplantemp.BESTID = "".
   ELSE DO:
      IF AVAILABLE bestkundallt THEN ASSIGN upplantemp.BESTID = bestkundallt.BESTID.
      ELSE ASSIGN upplantemp.BESTID = upplantemp.OMRADE.
   END.
   IF CMB_OMR = ? OR CMB_OMR = "" THEN DO:
      MESSAGE "Välj ett " Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk). 
      RETURN.
   END.
   IF upplantemp.PLANNRAVDATUM = ? THEN DO:
      upplantemp.PLANNRAVDATUM = 01/01/91.
   END.
   RUN nyttplanapp_UI (INPUT 1).
   FOR EACH felmeddtemp:
      musz = TRUE.
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
   END.
   FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
   upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
   IF AVAILABLE upplantemp THEN DO:
      FIND FIRST valplantemp WHERE valplantemp.PLANNR = plannrvar AND
      valplantemp.ARTAL = gammar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valplantemp THEN DO:
         CREATE valplantemp.
      END.
      ASSIGN
      valplantemp.PLANNRAVDATUM = upplantemp.PLANNRAVDATUM
      valplantemp.FASTAPLANNR = upplantemp.FASTAPLANNR
      valplantemp.ANLNR = upplantemp.ANLNR
      valplantemp.PLANNR = upplantemp.PLANNR
      valplantemp.AONR = upplantemp.AONR
      valplantemp.DELNR = upplantemp.DELNR
      valplantemp.ARBANSVARIG = upplantemp.ARBANSVARIG
      valplantemp.PKOD = upplantemp.PKOD
      valplantemp.ARBARTKOD = upplantemp.ARBARTKOD
      valplantemp.ARTAL = upplantemp.ARTAL
      valplantemp.OMRADE = upplantemp.OMRADE
      valplantemp.ORT = upplantemp.ORT
      valplantemp.PKOD = upplantemp.PKOD
      valplantemp.KOPPAO = upplantemp.KOPPAO
      valplantemp.PRISTYP = upplantemp.PRISTYP
      valplantemp.SLUTVNR = upplantemp.SLUTVNR
      valplantemp.STARTVNR = upplantemp.STARTVNR
      valplantemp.UPP = upplantemp.UPP
      valplantemp.UPPNR = upplantemp.UPPNR
      valplantemp.BESTID = upplantemp.BESTID
      valplantemp.ANM = upplantemp.ANM.
   END.
   
   FIND FIRST plannrtemp WHERE plannrtemp.PLANNR = plannrvar AND
   plannrtemp.ARTAL = gammar NO-LOCK NO-ERROR.
   IF AVAILABLE plannrtemp THEN DO:
      BUFFER-COPY upplantemp TO plannrtemp.
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
   IF RAD_VAL = 6 THEN DO:
      status-ok = BRW_AVTAL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
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
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnvit_UI C-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buttplace_UI C-Win 
PROCEDURE buttplace_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   Guru.GlobalaVariabler:collefth = MBTN_KOPPL:HANDLE IN FRAME {&FRAME-NAME}. 
   IF MBTN_KONT:HIDDEN = FALSE  THEN DO:
      Guru.GlobalaVariabler:colrighth = MBTN_KONT:HANDLE.           
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
   END.
   Guru.GlobalaVariabler:colrighth = MBTN_ANM:HANDLE.           
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
   ASSIGN
   MBTN_DOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   /*DOKUMENT*/
   IF Guru.Konstanter:globforetag = "TREC" OR Guru.Konstanter:globforetag = "ALTE" OR Guru.Konstanter:globforetag = "VELK" OR 
   Guru.Konstanter:globforetag = "ELCO" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "VAST" OR  
   Guru.Konstanter:globforetag = "VNAT" OR Guru.Konstanter:globforetag = "BORL" OR 
   Guru.Konstanter:globforetag = "STRA" OR Guru.Konstanter:globforetag = "LECA"  OR Guru.Konstanter:globforetag = "TECT" OR
   Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "SKOG" OR Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "SKOK" OR 
   Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "GETB" THEN DO:
       musz = musz.      
   END.
   ELSE DO:
      MBTN_DOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      Guru.GlobalaVariabler:colrighth = MBTN_DOK:HANDLE. 
      RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.         
   MBTN_OVR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   Guru.GlobalaVariabler:colrighth = MBTN_OVR:HANDLE. 
   RUN buttcolm_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chbstate_UI C-Win 
PROCEDURE chbstate_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*
   IF RAD_VAL = 1 THEN DO:
      MBTN_KOPPL:LOAD-IMAGE ("BILDER\btn_kopp_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      MBTN_KONT:LOAD-IMAGE ("BILDER\btn_kont_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_VAL = 3 THEN DO:
      MBTN_ANM:LOAD-IMAGE ("BILDER\btn_anm_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_VAL = 6 THEN DO:
      MBTN_DOK:LOAD-IMAGE ("BILDER\btn_dok_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_VAL = 8 THEN DO:
      MBTN_OVR:LOAD-IMAGE ("BILDER\btn_ovr_over.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   IF radspar NE RAD_VAL THEN DO:
      IF radspar = 1 THEN MBTN_KOPPL:LOAD-IMAGE ("BILDER\btn_kopp.gif") IN FRAME {&FRAME-NAME}.
      IF radspar = 2 THEN MBTN_KONT:LOAD-IMAGE ("BILDER\btn_kont.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      IF radspar = 3 THEN MBTN_ANM:LOAD-IMAGE ("BILDER\btn_anm.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      IF radspar = 6 THEN MBTN_DOK:LOAD-IMAGE ("BILDER\btn_dok.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
      IF radspar = 8 THEN MBTN_OVR:LOAD-IMAGE ("BILDER\btn_ovr.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   */
   radspar = RAD_VAL.
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
  DISPLAY FILL-IN_PLANNR CMB_ARTAL FILL-IN_FASTAPLANNR FILL-IN_ORT CMB_OMR 
          FILL-IN_BEST CMB_DEBPR FILL-IN_ARBANSVARIG FILL-IN_STARTD 
          FILL-IN_SLUTD FILL-IN_PKOD FILL-IN_ARBARTKOD EDITOR_ANM FILL-IN-KOPP 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BTN_OK MBTN_ANM RECT-MENY BTN_AVB FILL-IN_PLANNR CMB_ARTAL 
         FILL-IN_FASTAPLANNR FILL-IN_ORT MBTN_DOK CMB_OMR FILL-IN_BEST 
         CMB_DEBPR BTN_NVE BTN_NVE-2 FILL-IN_ARBANSVARIG FILL-IN_STARTD BTN_FVE 
         FILL-IN_SLUTD BTN_FVE-2 FILL-IN_PKOD FILL-IN_ARBARTKOD BRW_AVTAL2 
         BRW_KOPP BRW_PLANKTO BRW_ANSV BRW_BEST BRW_PRIO BRW_ARBART BTN_GATILL 
         EDITOR_ANM BTN_KOPPLAAONR BTN_AKTIV BTN_KALKAR BTN_VISA MBTN_KONT 
         BTN_UPPKALK BTN_KALKKALK BTN_VISKALK BTN_KONVKALK BTN_ANDRAK BTN_NYA 
         BTN_ANDA BTN_BORTA RAD_VAL MBTN_KOPPL MBTN_OVR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE finnskopplingar_UI C-Win 
PROCEDURE finnskopplingar_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
---/*mumma*/---------------------------------------------------------------------------*/
   /*KALKRUTIN*/
   RUN gatillhamt_UI (INPUT "KAL").
   RUN gatillhamt_UI (INPUT "AOR").
   
   RUN openbdynspec_UI IN brwproc[9].
   RUN vcbrwkopp_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gatillhamt_UI C-Win 
PROCEDURE gatillhamt_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vartvar AS CHARACTER NO-UNDO.
   IF vartvar = "KAL" THEN DO:
      IF Guru.Konstanter:hoppsekvar[3] = FALSE THEN RETURN.
      RUN aonrhmtkalk_UI IN KalkClasserStart (INPUT upplantemp.PLANNR, INPUT upplantemp.ARTAL,INPUT FALSE,OUTPUT TABLE gatill APPEND).
      IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 3 THEN DO:
         IF FILL-IN_PLANNRAVDATUM = 01/01/1991 THEN DO:
            CREATE gatill.
            ASSIGN
            gatill.TYPCHAR = "KAL"
            gatill.TYP = 1
            gatill.F1 = "Kalkyl typ 1"
            gatill.F2 = ?
            gatill.F3 = ""
            gatill.STATUSNIV = "".
            CREATE gatill.
            ASSIGN
            gatill.TYPCHAR = "KAL"
            gatill.TYP = 2
            gatill.F1 = "Kalkyl typ 2"
            gatill.F2 = ?
            gatill.F3 = ""
            gatill.STATUSNIV = "".
            CREATE gatill.
            ASSIGN
            gatill.TYPCHAR = "KAL"
            gatill.TYP = 3
            gatill.F1 = "Kalkyl typ 3"
            gatill.F2 = ?
            gatill.F3 = ""
            gatill.STATUSNIV = "".
            
         END.
         
      END.
      /*kalkfor*/
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" {GLOBVES.I} THEN DO:
         IF FILL-IN_PLANNRAVDATUM = 01/01/1991 THEN DO:
            CREATE gatill.
            ASSIGN
            gatill.TYPCHAR = "KAL"
            gatill.TYP = 5
            gatill.F1 = "Kalkyl Sam B/F"
            gatill.F2 = ?
            gatill.F3 = ""
            gatill.STATUSNIV = "".
         END.
      END.
      IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 2 THEN DO:
         
      END.
      IF Guru.Konstanter:varforetypval[41] = 1 THEN DO:
         IF FILL-IN_PLANNRAVDATUM = 01/01/1991 THEN DO:
            CREATE gatill.
            ASSIGN
            gatill.TYPCHAR = "KAL"
            gatill.TYP = 7
            gatill.F1 = "Kalkyl Nätreg N1"
            gatill.F2 = ?
            gatill.F3 = ""
            gatill.STATUSNIV = "".
         END.
      END.        
   END.
   IF vartvar = "AOR" THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN PLANAORUTIN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 1,INPUT upplantemp.PLANNR,INPUT upplantemp.ARTAL,OUTPUT TABLE gatill APPEND).
      END.
      ELSE DO:
         RUN PLANAORUTIN.P
         (INPUT 1,INPUT upplantemp.PLANNR,INPUT upplantemp.ARTAL,OUTPUT TABLE gatill APPEND).
      END.
      FIND FIRST gatill WHERE gatill.TYPCHAR = "AOR" NO-ERROR.
      IF AVAILABLE gatill THEN DO:
         ASSIGN
         upplantemp.AONR = gatill.AONR 
         upplantemp.DELNR = gatill.DELNR
         upplantemp.KOPPAO = TRUE.
      END.
      IF FILL-IN_PLANNRAVDATUM = 01/01/1991 THEN DO:
         CREATE gatill.
         ASSIGN
         gatill.AONR = ?
         gatill.TYPCHAR = "AOR"
         gatill.F1 = Guru.Konstanter:gaok
         gatill.F2 = ?
         gatill.F3 = ""
         gatill.STATUSNIV = "".
      END.      
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   ASSIGN
   BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   BTN_FVE-2:HIDDEN = FALSE
   BTN_NVE:HIDDEN = FALSE
   BTN_NVE-2:HIDDEN = FALSE
   FILL-IN_SLUTD:HIDDEN = FALSE
   FILL-IN_STARTD:HIDDEN = FALSE
   FILL-IN_BEST:HIDDEN = FALSE
   CMB_DEBPR:HIDDEN = FALSE
   CMB_OMR:HIDDEN = FALSE
   FILL-IN_ORT:HIDDEN = FALSE
   FILL-IN_PLANNR:HIDDEN = FALSE
   CMB_ARTAL:HIDDEN = FALSE
   EDITOR_ANM:HIDDEN = TRUE
   BRW_ARBART:HIDDEN = TRUE 
   FILL-IN_ARBARTKOD:HIDDEN = TRUE
   BRW_PRIO:HIDDEN = TRUE 
   FILL-IN_PKOD:HIDDEN = TRUE
   BRW_ANSV:HIDDEN = TRUE
   BRW_KOPP:HIDDEN = TRUE
   BRW_BEST:HIDDEN = TRUE
   BTN_KALKAR:HIDDEN = TRUE
   BTN_GATILL:HIDDEN = TRUE
   BTN_KOPPLAAONR:HIDDEN = TRUE
   BTN_ANDRAK:HIDDEN = TRUE
   BRW_PLANKTO:HIDDEN = TRUE
   BRW_KOPP:HIDDEN = TRUE
   BTN_GATILL:HIDDEN = TRUE   
   BTN_AKTIV:HIDDEN = TRUE
   BRW_AVTAL2:HIDDEN = TRUE
   BTN_NYA:HIDDEN = TRUE
   BTN_ANDA:HIDDEN = TRUE
   BTN_BORTA:HIDDEN = TRUE
   BTN_MAPP:HIDDEN = TRUE
   BTN_VISA:HIDDEN = TRUE
   BTN_GATILL:HIDDEN = TRUE
   BTN_KOPPLAAONR:HIDDEN = TRUE           
   BTN_AKTIV:HIDDEN = TRUE           
   BTN_UPPKALK:HIDDEN = TRUE           
   BTN_KALKKALK:HIDDEN = TRUE           
   BTN_VISKALK:HIDDEN = TRUE           
   BTN_KALKAR:HIDDEN = TRUE           
   BTN_KONVKALK:HIDDEN = TRUE.           

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
   IF nrvalvar = FALSE THEN DO:
      DISABLE FILL-IN_PLANNR /*CMB_OMR*/ FILL-IN_FASTAPLANNR WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ENABLE FILL-IN_FASTAPLANNR WITH FRAME {&FRAME-NAME}.
   END.
   RUN goma_UI.
   IF upplantemp.PLANNRAVDATUM = 01/01/1991 OR upplantemp.PLANNRAVDATUM = ? THEN DO:
      ASSIGN 
      FILL-IN_PLANNRAVDATUM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.     
   END.
   ELSE DO:
      IF Guru.Konstanter:plansekvar[9] = TRUE THEN DO:  
         DISPLAY FILL-IN_PLANNRAVDATUM WITH FRAME {&FRAME-NAME}.
         FILL-IN_PLANNRAVDATUM:HIDDEN = FALSE.
      END.
   END.
   ASSIGN
   FILL-IN_PLANNRAVDATUM:LABEL IN FRAME {&FRAME-NAME} = "Avslutat"
   CMB_DEBPR:LABEL = Guru.Konstanter:gdebk
   CMB_OMR = INPUT CMB_OMR.
   APPLY "VALUE-CHANGED" TO RAD_VAL IN FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) 
      OR Guru.Konstanter:globanv = "EVAH" THEN musz = musz.
      ELSE DISABLE BRW_BEST WITH FRAME {&FRAME-NAME}.
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
   DEFINE VARIABLE omrnamn AS CHARACTER NO-UNDO.
   status-ok = RAD_VAL:DELETE("olika val") IN FRAME {&FRAME-NAME}.
   ASSIGN
   MBTN_KOPPL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   MBTN_KONT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   MBTN_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   
   RAD_VAL:ADD-LAST("Kopplingar", 1).
   MBTN_KOPPL:LOAD-IMAGE("BILDER\xbtn_kopp.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   MBTN_KONT:LOAD-IMAGE("BILDER\xbtn_konto.gif") NO-ERROR.
   MBTN_KOPPL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF Guru.Konstanter:varforetypval[2] = 0 OR Guru.Konstanter:globforetag = "LULE" THEN DO:
      RAD_VAL:ADD-LAST("Konton", 2).
      MBTN_KONT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   
   RAD_VAL:ADD-LAST("Anmärkning", 3).
   MBTN_ANM:LOAD-IMAGE("BILDER\xbtn_anm.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   MBTN_ANM:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   IF nyplanvar = TRUE THEN DO:
      RUN NRPLAN.W (OUTPUT plannrvar,OUTPUT artalvar,OUTPUT omr,OUTPUT fast).
      IF musz = TRUE THEN DO:
         RETURN.
      END.
      CREATE upplantemp.
      ASSIGN
      upplantemp.PLANNR = plannrvar
      upplantemp.ARTAL = artalvar
      upplantemp.OMRADE = omr
      upplantemp.FASTAPLANNR = fast
      upplantemp.PLANNRAVDATUM = 01/01/1991
      FILL-IN_PLANNR = plannrvar
      CMB_ARTAL = artalvar.
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = omr
      USE-INDEX OMR NO-LOCK NO-ERROR.
      
      CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
      DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
      ENABLE FILL-IN_FASTAPLANNR WITH FRAME {&FRAME-NAME}.
   END.
   RUN laddaplan_UI.
   RUN finnskopplingar_UI.
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 2.
      {BESTHMT.I}      
   END.
   OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(omrtemp):
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      GET NEXT oq NO-LOCK.
   END.
   IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
   END.
   ELSE DO:
      OPEN QUERY oq FOR EACH omrtemp USE-INDEX OMR NO-LOCK.
      GET FIRST oq NO-LOCK.
      DO WHILE AVAILABLE(omrtemp):
         FIND FIRST bestkundallt WHERE bestkundallt.BESTID = omrtemp.OMRADE NO-ERROR.
         IF NOT AVAILABLE bestkundallt THEN DO:
            CREATE bestkundallt.
            ASSIGN 
            bestkundallt.VIBESTID = omrtemp.OMRADE 
            bestkundallt.BESTID = omrtemp.OMRADE 
            bestkundallt.BESTNAMN = omrtemp.NAMN.                      
         END.              
         GET NEXT oq NO-LOCK.
      END.
   END.
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   IF AVAILABLE kbenamntemp THEN DO:
      ASSIGN
      plankonttemp.K1:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2))
      plankonttemp.K2:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2))
      plankonttemp.K3:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbenamntemp.K3,1,1)) + LC(SUBSTRING(kbenamntemp.K3,2))
      plankonttemp.K4:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbenamntemp.K4,1,1)) + LC(SUBSTRING(kbenamntemp.K4,2))
      plankonttemp.K5:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbenamntemp.K5,1,1)) + LC(SUBSTRING(kbenamntemp.K5,2)).
   END.
   IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      /*HÄR SELNÄT*/
      ASSIGN CMB_OMR:LABEL  =  Guru.Konstanter:gomrl.     
   END.  
   ELSE CMB_OMR:LABEL = Guru.Konstanter:gutfk + " " + LC(Guru.Konstanter:gomrk).
   ASSIGN  
   BRW_ANSV:TITLE = "Välj " + LC(Guru.Konstanter:garbak)
   BRW_PLANKTO:TITLE = "Kontosträngar lagrade på " + LC(Guru.Konstanter:gplk) + " :et"
   FILL-IN_ARBANSVARIG:LABEL = Guru.Konstanter:garbal
   FILL-IN_FASTAPLANNR = upplantemp.FASTAPLANNR.
   IF INDEX(Guru.Konstanter:gfastl,"/") = 0  AND INDEX(Guru.Konstanter:gtillk,"/") =  0 THEN DO:
      FILL-IN_FASTAPLANNR:FORMAT = Guru.Konstanter:gfastk + "/" + Guru.Konstanter:gtillk.  
   END.
   ELSE DO:     
      gfastkh = REPLACE(Guru.Konstanter:gfastl,"/","-").
      gtillkh = REPLACE(Guru.Konstanter:gtillk,"/","-").
      FILL-IN_FASTAPLANNR:FORMAT = gfastkh + "/" + gtillkh.  
   END.
   
   EDITOR_ANM = upplantemp.ANM.
   IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = upplantemp.OMRADE
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
         END.
      END.   
   END.             
   IF FILL-IN_ARBARTKOD = ? OR FILL-IN_ARBARTKOD = 0 THEN DO:
      FIND FIRST arbarttemp USE-INDEX ARB NO-LOCK NO-ERROR.
      IF AVAILABLE arbarttemp THEN ASSIGN FILL-IN_ARBARTKOD = arbarttemp.ARBARTKOD.
   END.
   FOR EACH automregtemp USE-INDEX PRISTYP NO-LOCK:
      status-ok = CMB_DEBPR:ADD-LAST(automregtemp.PRISTYP).
   END.

   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = "TOT.PRIS."
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   ASSIGN  CMB_DEBPR:SCREEN-VALUE = automregtemp.PRISTYP.
   IF nyplanvar = FALSE THEN DO:
      ASSIGN  CMB_DEBPR:SCREEN-VALUE = upplantemp.PRISTYP.
   END.
  
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = upplantemp.OMRADE
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF AVAILABLE omrtemp THEN DO:
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      ASSIGN  CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
      DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = upplantemp.BESTID
      USE-INDEX BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = omrtemp.NAMN
         NO-LOCK NO-ERROR.
         IF AVAILABLE bestkundallt THEN DO:
            FILL-IN_BEST = bestkundallt.BESTNAMN.
            DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.            
         END.         
      END.
   END.
   ELSE DO:
      
     IF Guru.Konstanter:appcon THEN DO:
         RUN NAMNBORTAGETOMR.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT upplantemp.OMRADE, OUTPUT omrnamn). 
      END.
      ELSE DO:
         RUN NAMNBORTAGETOMR.P (INPUT upplantemp.OMRADE, OUTPUT omrnamn).
      END.
      status-ok = CMB_OMR:ADD-LAST(omrnamn).
      CMB_OMR:SCREEN-VALUE = omrnamn. 
      DISABLE CMB_OMR WITH FRAME {&FRAME-NAME}.
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = upplantemp.BESTID
      USE-INDEX BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN DO:
         FILL-IN_BEST = bestkundallt.BESTNAMN.
         DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         FILL-IN_BEST = omrnamn.
         DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.            
      END.         
      
   END. 
   RUN chbstate_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalkkopp_UI C-Win 
PROCEDURE kalkkopp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER aonrvar  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
   
   FOR EACH gatill WHERE gatill.TYPCHAR = "KAL":
      IF gatill.F1 = "Kalkyl typ fri" THEN musz = musz.
      ELSE DO:
         IF gatill.F2 NE ? THEN DO:     
            MESSAGE "Vill du kopiera kalkyler till " LC(Guru.Konstanter:gaok) aonrvar STRING(delnrvar,">99") "?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE kval1 AS LOGICAL.
            CASE kval1:
               WHEN TRUE THEN DO:
                  RUN KOPKALK.W 
                  (INPUT 99, INPUT aonrvar,INPUT delnrvar,INPUT "",
                  INPUT omrtemp.OMRADE,INPUT valplantemp.BESTID,INPUT plannrvar,INPUT artalvar).
               END.
               WHEN FALSE THEN DO:
                  
               END.
               
            END CASE.
            RETURN.
         END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kalk_UI C-Win 
PROCEDURE kalk_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER kalvart AS INTEGER NO-UNDO.
   DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kalknrvar AS INTEGER NO-UNDO.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 37
   soktemp.SOKCHAR[1] = valplantemp.PLANNR
   soktemp.SOKINT[1]  = valplantemp.ARTAL.
   {SOKANROP.I}
   aonrrec = soktemp.SOKINT[2].
   kalknrvar = INTEGER(gatill.F2).
   kalktypvar = gatill.TYP.
   ASSIGN
   gatilltyp = gatill.TYPCHAR
   gatill1   = gatill.F1
   gatill2   = gatill.F2.
   EMPTY TEMP-TABLE kalkintemp NO-ERROR.
   EMPTY TEMP-TABLE kalkinmtemp NO-ERROR. 
   CREATE kalkinmtemp.
   ASSIGN
   kalkinmtemp.PLANNR = valplantemp.PLANNR
   kalkinmtemp.ARTAL = valplantemp.ARTAL
   kalkinmtemp.REPO = TRUE.
   {AVBGOM.I}
   
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).

   RUN menykalkval_UI IN Guru.SharedVariable:btnkalkh (INPUT kalvart, INPUT 3, INPUT kalknrvar, INPUT kalktypvar, INPUT TABLE kalkinmtemp).
   /*
   RUN menykalk_UI IN Guru.SharedVariable:btnkalkh (INPUT 3, INPUT kalknrvar, INPUT kalktypvar, INPUT TABLE kalkinmtemp).
   */
   RUN wingom_UI  IN Guru.SharedVariable:btnkalkh.
   EMPTY TEMP-TABLE kalkintemp NO-ERROR.
   EMPTY TEMP-TABLE kalkinmtemp NO-ERROR. 
   FOR EACH gatill WHERE gatill.TYPCHAR = "KAL":
      DELETE gatill.
   END.
   RUN gatillhamt_UI (INPUT "KAL").
   RUN openbdynspec_UI IN brwproc[9].
   FIND FIRST gatill WHERE gatill.TYPCHAR = gatilltyp AND
   gatill.F1  = gatill1 AND gatill.F2 = gatill2   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gatill THEN DO:
      FIND FIRST gatill WHERE gatill.TYPCHAR = gatilltyp NO-ERROR.      
   END.
   IF AVAILABLE gatill THEN DO:
      RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(gatill)).
      RUN lastselectdyn_UI IN brwproc[9].
   END.
   RUN vcbrwkopp_UI.
   kalknrvar  = ?.
   kalktypvar = ?.
   {AVBFRAM.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE knappar_UI C-Win 
PROCEDURE knappar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER varknapp AS INTEGER NO-UNDO.
   IF varknapp = 1 THEN DO:
      
      Guru.GlobalaVariabler:collefth = ?.
      RUN buttrow2_UI IN framesizeh (INPUT 12.71). 
      Guru.GlobalaVariabler:colrighth = BTN_GATILL:HANDLE IN FRAME {&FRAME-NAME}.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
      /*IF typvar = "KAL" THEN DO:*/
      Guru.GlobalaVariabler:colrighth = BTN_KOPPLAAONR:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
      Guru.GlobalaVariabler:colrighth = BTN_AKTIV:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).  
      IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO: 
         BTN_UPPKALK:HIDDEN = TRUE.
         IF Guru.Konstanter:kalk2sekvar[4] = TRUE OR Guru.Konstanter:globniv = 0 THEN DO:
            Guru.GlobalaVariabler:colrighth = BTN_KALKKALK:HANDLE.           
            RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
         END.
         BTN_VISKALK:HIDDEN = TRUE.
         BTN_KALKAR:HIDDEN = TRUE.
         BTN_KONVKALK:HIDDEN = TRUE.
         
      END.
   END.
   ASSIGN
   BTN_GATILL:HIDDEN = TRUE
   BTN_KOPPLAAONR:HIDDEN = TRUE           
   BTN_AKTIV:HIDDEN = TRUE           
   BTN_UPPKALK:HIDDEN = TRUE           
   BTN_KALKKALK:HIDDEN = TRUE           
   BTN_VISKALK:HIDDEN = TRUE           
   BTN_KALKAR:HIDDEN = TRUE           
   BTN_KONVKALK:HIDDEN = TRUE.           
   IF varknapp = 2 THEN DO:
      IF gatill.TYPCHAR = "KAL" THEN DO:
         IF gatill.F2 = ? THEN DO:
            ASSIGN
            BTN_GATILL:HIDDEN = FALSE
            BTN_KOPPLAAONR:HIDDEN = FALSE.
         END.
         ELSE DO:
            IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:    
               BTN_UPPKALK:HIDDEN = TRUE.
               IF Guru.Konstanter:kalk2sekvar[4] = TRUE OR Guru.Konstanter:globniv = 0 THEN BTN_KALKKALK:HIDDEN = FALSE.
               BTN_VISKALK:HIDDEN = TRUE.
               BTN_KALKAR:HIDDEN = TRUE.
               BTN_KONVKALK:HIDDEN = TRUE.
            END.
         END.
         IF gatill.STATUSNIV = "" THEN DO:
            BTN_AKTIV:HIDDEN = TRUE.
            BTN_KALKAR:HIDDEN = TRUE.
         END.
         ELSE DO:
            BTN_AKTIV:HIDDEN = FALSE.
            BTN_KALKAR:HIDDEN = TRUE.
         END.
         IF gatill.F1 = "Kalkyl typ fri" THEN  BTN_KALKAR:HIDDEN = TRUE.
         IF gatill.F2 = ? THEN DO:
            BTN_GATILL:LABEL = "Ny".  
         END.
         ELSE DO:
            BTN_GATILL:LABEL = "Gå till".                  
         END.
      END.
      ELSE DO:
         IF gatill.F2 = ? THEN DO:
            BTN_GATILL:LABEL = "Ny".  
            BTN_GATILL:HIDDEN = FALSE.
         END.
         ELSE DO:
            BTN_GATILL:LABEL = "Gå till".         
            BTN_GATILL:HIDDEN = FALSE.
         END.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopplaaonr_UI C-Win 
PROCEDURE kopplaaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   
   DEFINE VARIABLE inkopp AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kopierkoppla AS INTEGER NO-UNDO.
   inkopp = gatill.TYPCHAR.
   IF BTN_KOPPLAAONR:LABEL IN FRAME {&FRAME-NAME} = "Koppla" OR 
      BTN_KOPPLAAONR:LABEL IN FRAME {&FRAME-NAME} = "Koppla/Kopiera" THEN DO:
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR
      NO-LOCK NO-ERROR.
      IF inkopp = "KAL" THEN DO:
         RUN KOPKOP.W (OUTPUT kopierkoppla).
         IF kopierkoppla = ? THEN kopierkoppla = kopierkoppla. 
         ELSE DO:
            {AVBGOM.I}
            RUN KOPKALK.W 
            (INPUT kopierkoppla + 1, INPUT plannrvar,INPUT artalvar,INPUT gatill.TYP,
            INPUT omrtemp.OMRADE,INPUT valplantemp.BESTID,INPUT ?,INPUT ?).                        
            {AVBFRAM.I}
         END.
      END.
      IF gatill.TYPCHAR = "AOR" THEN DO:
         RUN bkopp_UI.
      END.
      FOR EACH gatill WHERE gatill.TYPCHAR = inkopp:
         DELETE gatill.
      END.
      RUN gatillhamt_UI (INPUT inkopp).
      RUN openbdynspec_UI IN brwproc[9].
      RUN vcbrwkopp_UI.
      
   END.
   ELSE IF BTN_KOPPLAAONR:LABEL = "Koppla från" THEN DO:
      MESSAGE "Vill du ta bort kopplingen?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Koppling" UPDATE svarkopp AS LOGICAL.
      CASE svarkopp:
         WHEN TRUE THEN DO:
            {SOKSTART.I}
            IF gatill.TYPCHAR = "KAL" THEN DO:
               IF gatill.STATUSNIV = "UF" THEN DO:
                  FIND FIRST gatillbuff WHERE gatill.TYPCHAR = "KAL" AND
                  gatillbuff.STATUSNIV = "HUV" NO-ERROR.
                  IF NOT AVAILABLE gatillbuff THEN DO:
                     FIND FIRST gatillbuff WHERE gatill.TYPCHAR = "KAL" AND
                     gatillbuff.STATUSNIV = "ALT" NO-ERROR.
                  END.
                  IF AVAILABLE gatillbuff THEN DO:
                     MESSAGE "Du kan inte ta bort en utförandekalkyl. Ändra status på kalkylen först."
                     VIEW-AS ALERT-BOX.
                     RETURN.
                  END.
               END.
               IF gatill.STATUSNIV = "HUV" THEN DO:
                  FIND FIRST gatillbuff WHERE gatill.TYPCHAR = "KAL" AND
                  gatillbuff.STATUSNIV = "ALT" AND gatillbuff.TYP = gatill.TYP NO-ERROR.
                  IF AVAILABLE gatillbuff THEN DO:
                     MESSAGE "Du kan inte ta bort en huvudkalkyl. Ändra status på kalkylen först."
                     VIEW-AS ALERT-BOX.
                     RETURN.
                  END.
               END.
               ASSIGN
               soktemp.SOKVAL = 46
               soktemp.SOKINT[1] = INTEGER(gatill.F2)
               soktemp.SOKINT[2] = 0.      
               {SOKANROP.I}
               DELETE gatill.
            END.
            ELSE IF gatill.TYPCHAR = "AOR" THEN DO:
               ASSIGN
               soktemp.SOKVAL = 49
               soktemp.SOKCHAR[1] = gatill.AONR
               soktemp.SOKINT[1] = gatill.DELNR.
               {SOKANROP.I}
               DELETE gatill.
               FIND FIRST gatill WHERE gatill.TYPCHAR = "AOR" AND gatill.AONR NE ? NO-ERROR.
               IF AVAILABLE gatill THEN DO:
                  ASSIGN
                  upplantemp.AONR = gatill.AONR 
                  upplantemp.DELNR = gatill.DELNR
                  upplantemp.KOPPAO = TRUE.
               END.
               ELSE DO:
                  ASSIGN
                  upplantemp.AONR = ? 
                  upplantemp.DELNR = ?
                  upplantemp.KOPPAO = FALSE.
               END.
               FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
               upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
               ASSIGN
               upplantemp.AONR = ?
               upplantemp.DELNR = ?.
               
            END.
            RUN refreshbrw_UI IN brwproc[9].
            RUN vcbrwkopp_UI.
         END.
         WHEN FALSE THEN DO:
            RETURN.
         END.
      END CASE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddaplan_UI C-Win 
PROCEDURE laddaplan_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   IF nyplanvar = FALSE THEN DO:
      FIND FIRST upplantemp WHERE upplantemp.PLANNR = plannrvar AND
      upplantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      IF AVAILABLE upplantemp THEN DO:
         ASSIGN
         FILL-IN_PKOD = upplantemp.PKOD
         FILL-IN_ARBARTKOD = upplantemp.ARBARTKOD
         FILL-IN_PLANNRAVDATUM = upplantemp.PLANNRAVDATUM
         FILL-IN_ARBANSVARIG = upplantemp.ARBANSVARIG
         FILL-IN_ORT = upplantemp.ORT
         sparansv = upplantemp.ARBANSVARIG.
      END.
   END.
   IF AVAILABLE upplantemp THEN DO:
      mappvar = "".   
      mappvar = "PL" + upplantemp.PLANNR + STRING(upplantemp.ARTAL).
      mappvarhj = grundmappvar +  mappvar.     
      IF grundmappvar NE "" THEN DO:
         RUN mappvar IN nyttaoejapph (INPUT mappvarhj).         
      END.
      
   END.
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
   /*DOKUMENT*/
   IF Guru.Konstanter:globforetag = "TREC" OR Guru.Konstanter:globforetag = "ALTE" OR Guru.Konstanter:globforetag = "VELK" OR 
   Guru.Konstanter:globforetag = "ELCO" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "VAST" OR  
   Guru.Konstanter:globforetag = "VNAT" OR Guru.Konstanter:globforetag = "BORL" OR 
   Guru.Konstanter:globforetag = "STRA" OR Guru.Konstanter:globforetag = "LECA"  OR Guru.Konstanter:globforetag = "TECT" OR
   Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "SKOG" OR Guru.Konstanter:globforetag = "BHEL" OR Guru.Konstanter:globforetag = "SKOK" OR 
   Guru.Konstanter:globforetag = "ESKO" OR Guru.Konstanter:globforetag = "GETB" THEN DO:
       musz = musz.      
   END.
   ELSE DO:
      RAD_VAL:ADD-LAST("Dokument", 6)  IN FRAME {&FRAME-NAME}.
      MBTN_DOK:LOAD-IMAGE("BILDER\xbtn_dok.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
    END.         
   MBTN_OVR:LOAD-IMAGE("BILDER\xbtn_ovr.gif") IN FRAME {&FRAME-NAME} NO-ERROR.
   RUN buttplace_UI.
  
   ON 'VALUE-CHANGED' OF FRAME DEFAULT-FRAME PERSISTENT RUN buttplace_UI IN THIS-PROCEDURE.
   RUN setapplyvc IN framesizeh (INPUT FRAME DEFAULT-FRAME:HANDLE ).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movefillin_UI C-Win 
PROCEDURE movefillin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER fillinh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER rowvalue AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER colvalue AS DECIMAL NO-UNDO.      
   fillinh:ROW = rowvalue .
   fillinh:COLUMN = colvalue.      
   Guru.Konstanter:LabelFlytt(fillinh). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyttplanapp_UI C-Win 
PROCEDURE nyttplanapp_UI :
/* -----------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   IF vart = 1 THEN DO:
      RUN spara_UI IN nyph
      (INPUT plannrvar,INPUT artalvar,INPUT CMB_ARTAL,
      OUTPUT TABLE felmeddtemp,INPUT TABLE plankonttemp,
      INPUT-OUTPUT TABLE upplantemp).
   END.   
   IF vart = 2 THEN DO:
      RUN flytta_UI IN nyph
      (INPUT plannrvar,INPUT artalvar,INPUT CMB_ARTAL,
      OUTPUT TABLE felmeddtemp, OUTPUT TABLE plankonttemp).      
   END.
   
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
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
   datornamn = TRIM(outdatornamn).
   
   {DOKUMENTMAP.I}
     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repa_UI C-Win 
PROCEDURE repa_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/

 FIND FIRST gatill WHERE gatill.TYPCHAR = gatilltyp AND
   gatill.F1  = gatill1 AND gatill.F2 = gatill2   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE gatill THEN DO:
      FIND FIRST gatill WHERE gatill.TYPCHAR = gatilltyp NO-ERROR.      
   END.
   IF AVAILABLE gatill THEN DO:
      RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(gatill)).
      RUN lastselectdyn_UI IN brwproc[9].
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
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   DEFINE VARIABLE fargnr AS INTEGER NO-UNDO.
   fargnr = 0.
   IF AVAILABLE gatill THEN DO:
      IF gatill.TYPCHAR = "KAL" THEN fargnr = 1.        
      ELSE IF gatill.TYPCHAR = "FAK" THEN. 
      ELSE IF gatill.TYPCHAR = "BER" THEN fargnr = 5.
      ELSE IF gatill.TYPCHAR = "MAR" THEN.
      ELSE IF gatill.TYPCHAR = "FBAONR" THEN fargnr = 2.
      ELSE IF gatill.TYPCHAR = "INTAONR" THEN fargnr = 2.
      ELSE IF gatill.TYPCHAR = "AOR" THEN fargnr = 2.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valomr_UI C-Win 
PROCEDURE valomr_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   IF SUBSTRING(Guru.Konstanter:globomr,1,1) = "4" THEN DO:
      IF SUBSTRING(omrtemp.OMRADE,1,1) = "4" THEN DO:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN) IN FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwkopp_UI C-Win 
PROCEDURE vcbrwkopp_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   status-ok = BRW_KOPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE gatill THEN DO:
      /*
      rowgatill = ROWID(gatill). 
      */
      RUN knappar_UI (INPUT 2).
      IF gatill.TYPCHAR = "KAL" THEN DO:
         ASSIGN
         kalknrvar = INTEGER(gatill.F2)
         kalktypvar = gatill.TYP.
      END.
      ELSE DO:
         ASSIGN
         kalknrvar = ?
         kalktypvar = ?.
      END.
      IF gatill.F2 = ?  THEN DO:
         IF gatill.TYPCHAR = "KAL" THEN DO:
            BTN_KOPPLAAONR:LABEL = "Koppla/Kopiera".
         END.
         ELSE BTN_KOPPLAAONR:LABEL = "Koppla".
      END.
      ELSE BTN_KOPPLAAONR:LABEL = "Koppla från".
      IF gatill.TYPCHAR = "KAL" THEN DO:
         BTN_KOPPLAAONR:HIDDEN = FALSE.
      END.
      ELSE IF gatill.TYPCHAR = "AOR" THEN DO:
         BTN_KOPPLAAONR:HIDDEN = FALSE.
      END.
      ELSE DO:
         BTN_KOPPLAAONR:HIDDEN = TRUE.
      END.
      IF gatill.F2 = ? THEN DO:
         BTN_GATILL:LABEL = "Ny".  
      END.
      ELSE DO:
         BTN_GATILL:LABEL = "Gå till".                  
      END.
      /*
      IF gatill.F2 = ? THEN DO:
         BTN_GATILL:LABEL = "Skapa".
         DISPLAY BTN_GATILL WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         BTN_GATILL:LABEL = "Gå till".
         DISPLAY BTN_GATILL WITH FRAME {&FRAME-NAME}.
      END.
      */
   END.
   IF FILL-IN_PLANNRAVDATUM NE 01/01/1991 THEN DO:
      ASSIGN
      BTN_AKTIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_KALKAR:HIDDEN = TRUE
      BTN_KOPPLAAONR:HIDDEN = TRUE.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

