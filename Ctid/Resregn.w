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
DEFINE INPUT PARAMETER stansdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER stansaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER stansdelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER stansvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{AVDTEMP.I}
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
&Scoped-define SHARED SHARED
{DIRDEF.I}
{FLEXTAB.I}
{PHMT.I}
{SOKDEF.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE egnaao NO-UNDO  LIKE utsokaonr.
&Scoped-define NEW NEW 
{RESDEF.I}
DEFINE NEW SHARED VARIABLE resrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE reddatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum5 AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE nattrakt AS LOGICAL FORMAT "JA/NEJ" NO-UNDO.
DEFINE NEW SHARED VARIABLE tjan AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE enflerdygns AS LOGICAL FORMAT "ENDAGS/FLERDYGNS" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE VARIABLE resrec2 AS RECID NO-UNDO.  
DEFINE VARIABLE enf AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE pristyp1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE pristyp2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE pris1 AS DECIMAL NO-UNDO. 
DEFINE VARIABLE pris2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE enfle AS CHARACTER NO-UNDO.            
DEFINE VARIABLE seku AS INTEGER FORMAT "9999999" NO-UNDO.  
DEFINE VARIABLE antal AS INTEGER FORMAT "9999999" NO-UNDO.
DEFINE VARIABLE halvt AS INTEGER FORMAT "9999999" NO-UNDO.
DEFINE VARIABLE sext AS INTEGER FORMAT "9999999" NO-UNDO.
DEFINE VARIABLE bort AS INTEGER FORMAT "9999999" NO-UNDO. 
DEFINE VARIABLE avslu AS INTEGER FORMAT "9999999" NO-UNDO.
DEFINE VARIABLE avsta AS INTEGER FORMAT "9999999" NO-UNDO.
DEFINE VARIABLE hjdag AS CHARACTER NO-UNDO.
DEFINE VARIABLE restidsek AS INTEGER NO-UNDO.  
DEFINE VARIABLE htim AS INTEGER NO-UNDO. 
DEFINE VARIABLE halvkvart AS INTEGER NO-UNDO.          
DEFINE VARIABLE energiavt AS LOGICAL NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valgubbe AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE maxres AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE spvart AS CHARACTER NO-UNDO.
DEFINE VARIABLE ftro AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.

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


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-ENFLE FILL-IN-STARTAR FILL-IN-SLUTAR ~
FILL-IN-AONR FILL-IN-DELNR FILL-IN-RESTID FILL-IN-BIL CMB_OVERUT ~
FILL-IN-MAT FILL-IN-NATT FILL-IN-FRIMAT FILL-IN-REDTRAKT FILL-IN-OKOST ~
FILL-IN-3MAN FILL-IN-RESMAL BTN_REG BTN_NAAVB BTN_AVB CMB_OMR ~
FILL-IN-FSTART FILL-IN-FSLUT CMB_AVD FILL-IN-TRORD BTN_SKAPEN RAD_FAST 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-OBS FILL-IN-PKOD FILL-IN-ENFLE ~
FILL-IN-STARTAR FILL-IN-SLUTAR FILL-IN-AONR FILL-IN-DELNR FILL-IN-RESTID ~
FILL-IN-BIL CMB_OVERUT FILL-IN-MAT FILL-IN-NATT FILL-IN-FRIMAT ~
FILL-IN-REDTRAKT FILL-IN-OKOST FILL-IN-3MAN FILL-IN-RESMAL FILL-IN-TEXT ~
CMB_OMR FILL-IN-FSTART FILL-IN-FSLUT FILL-IN-DAG FILL-IN-DAG-2 CMB_AVD ~
FILL-IN-TRORD FILL-IN_FORNAMN-2 

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
     LABEL "AvBRYT":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NAAVB 
     LABEL "Nästa person":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_REG 
     LABEL "Fortsätt":L 
     SIZE 14 BY 1.

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

DEFINE VARIABLE CMB_OVERUT AS CHARACTER FORMAT "X(4)":U 
     LABEL "Övertiduttag" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Komp,","Över" 
     DROP-DOWN-LIST
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_REDDAG AS CHARACTER FORMAT "X(3)":U INITIAL ? 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "mån","tis","ons","tor","fre","lör","sön" 
     DROP-DOWN-LIST
     SIZE 6 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-3MAN AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Mer än 3 månader" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-50KM AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BIL AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Bilförare" 
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
     SIZE 9 BY 1 TOOLTIP "Välj mellan Endags- eller Flerdygnsförrättning" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-FRIMAT AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL " Internat" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FSLUT AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Resan avslutas ej" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FSTART AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Resan redan påbörjad" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MAT AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Måltidsavdrag" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Fyll i om frukost,lunch eller middag erhållits någon av dagarna" NO-UNDO.

DEFINE VARIABLE FILL-IN-NATT AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Nattraktamente" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Ja - om övernattning har ordnats privat" NO-UNDO.

DEFINE VARIABLE FILL-IN-OBS AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 76 BY .63
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-OKOST AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Övrig ersättning" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Tex kilometerersättning ,parkeringsavgift eller utlägg" NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-ONATT AS LOGICAL FORMAT "JA/NEJ":U INITIAL NO 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-REDDATUM AS DATE FORMAT "99/99/99":U 
     LABEL "From datum" 
     VIEW-AS FILL-IN 
     SIZE 11.38 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-REDTRAKT AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Reducerat trakt." 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-REDVECKO AS INTEGER FORMAT "999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-RESMAL AS CHARACTER FORMAT "X(40)" 
     LABEL "Resmål" 
     VIEW-AS FILL-IN 
     SIZE 31.5 BY 1 TOOLTIP "Ange destination för resan" NO-UNDO.

DEFINE VARIABLE FILL-IN-RESTID AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Tid morgon/kväll" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTAR AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Sluttid" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Klockslag när tjänsteresan avslutas" NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 TOOLTIP "Datum när tjänsteresan avslutas" NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 5.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTAR AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Starttid" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Klockslag när tjänsteresan påbörjas" NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 TOOLTIP "Datum när tjänsteresan påbörjas" NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr/personal för:" 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-TRORD AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Tr.zon hela dagen" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

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
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", 1,
"Fasta aonr", 2,
"Favorit aonr", 3
     SIZE 39.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_MKTID AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Morgon och kväll", 1,
"Endast morgon", 2,
"Endast kväll", 3
     SIZE 9.13 BY 1.25 NO-UNDO.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50.63 BY 1.21
     BGCOLOR 8 .

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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57.38 BY 12.08
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57.38 BY 12.08
         TITLE "Favorit arbetsordernummer".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS DIALOG-1 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      tidpers.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "X(5)":U
      tidpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(15)":U
      tidpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(25)":U
  ENABLE
      tidpers.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 57.38 BY 12.08.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_EAONR AT ROW 4.54 COL 45.75
     FILL-IN-OBS AT ROW 1.17 COL 2.5 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     FILL-IN-PKOD AT ROW 2 COL 19 COLON-ALIGNED
     FILL-IN-ENFLE AT ROW 3.21 COL 19 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 4.42 COL 19 COLON-ALIGNED
     FILL-IN-STARTAR AT ROW 5.54 COL 19 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 6.75 COL 19 COLON-ALIGNED
     FILL-IN-SLUTAR AT ROW 7.88 COL 19 COLON-ALIGNED
     FILL-IN-AONR AT ROW 9.08 COL 19 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 10.29 COL 19 COLON-ALIGNED
     BRW_AONR AT ROW 4.54 COL 45.75
     FILL-IN-RESTID AT ROW 19 COL 74.5 COLON-ALIGNED
     FILL-IN-BIL AT ROW 20.42 COL 19 COLON-ALIGNED
     CMB_OVERUT AT ROW 20.5 COL 19 COLON-ALIGNED
     FILL-IN-MAT AT ROW 11.5 COL 19 COLON-ALIGNED
     FILL-IN-NATT AT ROW 12.71 COL 19 COLON-ALIGNED
     FILL-IN-FRIMAT AT ROW 18.75 COL 32.5
     FILL-IN-REDTRAKT AT ROW 18.13 COL 19 COLON-ALIGNED
     FILL-IN-REDDATUM AT ROW 19.92 COL 41.5 COLON-ALIGNED
     CMB_REDDAG AT ROW 19.92 COL 81.75 COLON-ALIGNED NO-LABEL
     FILL-IN-REDVECKO AT ROW 19.92 COL 66.75 COLON-ALIGNED NO-LABEL
     FILL-IN-OKOST AT ROW 13.92 COL 19 COLON-ALIGNED
     FILL-IN-3MAN AT ROW 19.25 COL 19 COLON-ALIGNED
     FILL-IN-RESMAL AT ROW 16.88 COL 19 COLON-ALIGNED
     BTN_REG AT ROW 20.54 COL 74.75
     BTN_NAAVB AT ROW 20.54 COL 60.25
     BTN_AVB AT ROW 20.54 COL 90.13
     FILL-IN-ONATT AT ROW 18.75 COL 83 NO-LABEL
     FILL-IN-50KM AT ROW 18.75 COL 86 COLON-ALIGNED NO-LABEL AUTO-RETURN 
     RAD_MKTID AT ROW 18.25 COL 93.5 NO-LABEL
     FILL-IN-TEXT AT ROW 1.25 COL 78.63 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 3.38 COL 78.63 COLON-ALIGNED NO-LABEL
     BRW_PERS AT ROW 4.54 COL 45.75
     BTN_NVE-3 AT ROW 4 COL 31.38
     BTN_FVE-3 AT ROW 4.88 COL 31.38
     BTN_NVE-4 AT ROW 6.5 COL 31.5
     BTN_FVE-4 AT ROW 7.38 COL 31.5
     FILL-IN-SOKPA AT ROW 17.08 COL 51 COLON-ALIGNED NO-LABEL
     FILL-IN_AONRS AT ROW 17.21 COL 62.75 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 17.21 COL 82.75 COLON-ALIGNED
     FILL-IN-FSTART AT ROW 19.08 COL 51.13 COLON-ALIGNED
     FILL-IN-FSLUT AT ROW 19.08 COL 77.13 COLON-ALIGNED
     FILL-IN-DAG AT ROW 4.42 COL 33 COLON-ALIGNED NO-LABEL
     FILL-IN-DAG-2 AT ROW 6.75 COL 33 COLON-ALIGNED NO-LABEL
     CMB_AVD AT ROW 2.21 COL 78.63 COLON-ALIGNED NO-LABEL
     FILL-IN-TRORD AT ROW 15.13 COL 19 COLON-ALIGNED
     FILL-IN_FORNAMN-2 AT ROW 2 COL 30.5 NO-LABEL
     BTN_SKAPEN AT ROW 9.08 COL 31
     RAD_FAST AT ROW 3.33 COL 40 NO-LABEL
     RECT-SOK AT ROW 16.92 COL 52.5
     SPACE(1.36) SKIP(3.82)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tjänsteresor":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: egnaao T "?" NO-UNDO temp-db egnaao
      TABLE: tidpers T "?" NO-UNDO temp-db tidpers
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_EAONR 1 DIALOG-1 */
/* BROWSE-TAB BRW_AONR FILL-IN-DELNR DIALOG-1 */
/* BROWSE-TAB BRW_PERS CMB_OMR DIALOG-1 */
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

/* SETTINGS FOR BROWSE BRW_PERS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_PERS:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_PERS:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

/* SETTINGS FOR BUTTON BTN_FVE-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVE-4 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-3 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE-4 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_REDDAG IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       CMB_REDDAG:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-3MAN IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-50KM IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-50KM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DAG IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DAG-2 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-FRIMAT IN FRAME DIALOG-1
   SHARED ALIGN-L                                                       */
/* SETTINGS FOR FILL-IN FILL-IN-OBS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ONATT IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE ALIGN-L                                  */
ASSIGN 
       FILL-IN-ONATT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-REDDATUM IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-REDDATUM:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-REDTRAKT IN FRAME DIALOG-1
   SHARED                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-REDVECKO IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-REDVECKO:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SLUTDAT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SLUTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKPA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SOKPA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_AONRS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AONRS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN-2 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_MKTID IN FRAME DIALOG-1
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_MKTID:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-SOK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       RECT-SOK:HIDDEN IN FRAME DIALOG-1           = TRUE.

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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Tjänsteresor */
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
   FOR EACH tidpers:
      tidpers.REGKOLL = FALSE.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Tjänsteresor */
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
   sok4 = "".
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


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON LEAVE OF BRW_PERS IN FRAME DIALOG-1
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON VALUE-CHANGED OF BRW_PERS IN FRAME DIALOG-1
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


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* AvBRYT */
DO:
   FOR EACH tidpers:
      tidpers.REGKOLL = FALSE.
   END.
   APPLY "GO" TO BTN_AVB.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON GO OF BTN_AVB IN FRAME DIALOG-1 /* AvBRYT */
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


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 DIALOG-1
ON CHOOSE OF BTN_FVE-3 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   RUN btnfve3_UI.
   
   DISPLAY FILL-IN-DAG WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 DIALOG-1
ON CHOOSE OF BTN_FVE-4 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   RUN btnfve4_UI.
   
   DISPLAY FILL-IN-DAG-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB DIALOG-1
ON CHOOSE OF BTN_NAAVB IN FRAME DIALOG-1 /* Nästa person */
DO:
   ASSIGN
   FILL-IN-SOKPA:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE
   RECT-SOK:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   BRW_AONR:HIDDEN = TRUE    
   RAD_FAST:HIDDEN = TRUE
   FILL-IN_AONRS:HIDDEN = TRUE 
   FILL-IN_ORTS:HIDDEN = TRUE.
   EMPTY TEMP-TABLE maltidfil NO-ERROR.
   EMPTY TEMP-TABLE kosters NO-ERROR.   
   RUN nasta_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 DIALOG-1
ON CHOOSE OF BTN_NVE-3 IN FRAME DIALOG-1 /* + */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 DIALOG-1
ON CHOOSE OF BTN_NVE-4 IN FRAME DIALOG-1 /* + */
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


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* Fortsätt */
DO:
   {muswait.i}      
   RUN btnreg_UI.
   {musarrow.i}   
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
         FILL-IN-SOKPA:HIDDEN = TRUE.   
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
   IF VALID-HANDLE(otbeordapph) THEN DO:
       RUN borthandle_UI IN otbeordapph.
       DELETE PROCEDURE otbeordapph NO-ERROR.
       otbeordapph = ?.
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


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR DIALOG-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME DIALOG-1
DO:
   CMB_OMR = INPUT CMB_OMR.            
   omravdand = 2.    
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


&Scoped-define SELF-NAME CMB_REDDAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_REDDAG DIALOG-1
ON LEAVE OF CMB_REDDAG IN FRAME DIALOG-1
DO:
  CMB_REDDAG = INPUT CMB_REDDAG.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-3MAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-3MAN DIALOG-1
ON LEAVE OF FILL-IN-3MAN IN FRAME DIALOG-1 /* Mer än 3 månader */
DO:
   FILL-IN-3MAN = INPUT FILL-IN-3MAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-3MAN DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-3MAN IN FRAME DIALOG-1 /* Mer än 3 månader */
DO:
  IF INPUT FILL-IN-3MAN = TRUE THEN FILL-IN-3MAN = FALSE.
   IF INPUT FILL-IN-3MAN = FALSE THEN FILL-IN-3MAN = TRUE.
   DISPLAY FILL-IN-3MAN WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-50KM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-50KM DIALOG-1
ON LEAVE OF FILL-IN-50KM IN FRAME DIALOG-1
DO:
   FILL-IN-50KM = INPUT FILL-IN-50KM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:   
   RUN fiaonr_UI.   
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:  
   RUN fiaonr_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BIL DIALOG-1
ON LEAVE OF FILL-IN-BIL IN FRAME DIALOG-1 /* Bilförare */
DO:
   FILL-IN-BIL = INPUT FILL-IN-BIL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BIL DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-BIL IN FRAME DIALOG-1 /* Bilförare */
DO:
   IF INPUT FILL-IN-BIL = TRUE THEN FILL-IN-BIL = FALSE.
   IF INPUT FILL-IN-BIL = FALSE THEN FILL-IN-BIL = TRUE.
   IF FILL-IN-BIL = TRUE AND utryckningtemp.LUFT = TRUE THEN DO:
      CMB_OVERUT:HIDDEN = FALSE.
   END.
   ELSE CMB_OVERUT:HIDDEN = TRUE.     
   DISPLAY FILL-IN-BIL WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENFLE DIALOG-1
ON LEAVE OF FILL-IN-ENFLE IN FRAME DIALOG-1 /* Endag/Flerdygn */
DO:
   FILL-IN-ENFLE = INPUT FILL-IN-ENFLE.   
   RUN fienfle_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENFLE DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-ENFLE IN FRAME DIALOG-1 /* Endag/Flerdygn */
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


&Scoped-define SELF-NAME FILL-IN-FRIMAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRIMAT DIALOG-1
ON LEAVE OF FILL-IN-FRIMAT IN FRAME DIALOG-1 /*  Internat */
DO:
   FILL-IN-FRIMAT = INPUT FILL-IN-FRIMAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRIMAT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FRIMAT IN FRAME DIALOG-1 /*  Internat */
DO:
   IF INPUT FILL-IN-FRIMAT = TRUE THEN FILL-IN-FRIMAT = FALSE.
   IF INPUT FILL-IN-FRIMAT = FALSE THEN FILL-IN-FRIMAT = TRUE.
   DISPLAY FILL-IN-FRIMAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FSLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FSLUT DIALOG-1
ON LEAVE OF FILL-IN-FSLUT IN FRAME DIALOG-1 /* Resan avslutas ej */
DO:
   FILL-IN-FSLUT = INPUT FILL-IN-FSLUT.
   IF FILL-IN-FSLUT = TRUE THEN DO:
      FILL-IN-SLUTAR:HIDDEN = TRUE.
   END.
   ELSE FILL-IN-SLUTAR:HIDDEN = FALSE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FSLUT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FSLUT IN FRAME DIALOG-1 /* Resan avslutas ej */
DO:
   IF INPUT FILL-IN-FSLUT = TRUE THEN FILL-IN-FSLUT = FALSE.
   IF INPUT FILL-IN-FSLUT = FALSE THEN FILL-IN-FSLUT = TRUE.
   DISPLAY FILL-IN-FSLUT WITH FRAME {&FRAME-NAME}.   
   IF FILL-IN-FSLUT = TRUE THEN DO:
      FILL-IN-SLUTAR:HIDDEN = TRUE.
   END.
   ELSE FILL-IN-SLUTAR:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FSTART DIALOG-1
ON LEAVE OF FILL-IN-FSTART IN FRAME DIALOG-1 /* Resan redan påbörjad */
DO:
   FILL-IN-FSTART = INPUT FILL-IN-FSTART.
   IF FILL-IN-FSTART = TRUE THEN DO:
      FILL-IN-STARTAR:HIDDEN = TRUE.
   END.
   ELSE FILL-IN-STARTAR:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FSTART DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-FSTART IN FRAME DIALOG-1 /* Resan redan påbörjad */
DO:
   IF INPUT FILL-IN-FSTART = TRUE THEN FILL-IN-FSTART = FALSE.
   IF INPUT FILL-IN-FSTART = FALSE THEN FILL-IN-FSTART = TRUE.   
   DISPLAY FILL-IN-FSTART WITH FRAME {&FRAME-NAME}.   
   IF FILL-IN-FSTART = TRUE THEN DO:
      FILL-IN-STARTAR:HIDDEN = TRUE.
   END.
   ELSE FILL-IN-STARTAR:HIDDEN = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MAT DIALOG-1
ON LEAVE OF FILL-IN-MAT IN FRAME DIALOG-1 /* Måltidsavdrag */
DO:
   FILL-IN-MAT = INPUT FILL-IN-MAT.  
   DISPLAY FILL-IN-MAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MAT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-MAT IN FRAME DIALOG-1 /* Måltidsavdrag */
DO:
   IF INPUT FILL-IN-MAT = TRUE THEN FILL-IN-MAT = FALSE.
   IF INPUT FILL-IN-MAT = FALSE THEN FILL-IN-MAT = TRUE.
   DISPLAY FILL-IN-MAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NATT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NATT DIALOG-1
ON LEAVE OF FILL-IN-NATT IN FRAME DIALOG-1 /* Nattraktamente */
DO:
   FILL-IN-NATT = INPUT FILL-IN-NATT.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NATT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-NATT IN FRAME DIALOG-1 /* Nattraktamente */
DO:
   IF INPUT FILL-IN-NATT = TRUE THEN FILL-IN-NATT = FALSE.
   IF INPUT FILL-IN-NATT = FALSE THEN FILL-IN-NATT = TRUE.
   DISPLAY FILL-IN-NATT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-OKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OKOST DIALOG-1
ON LEAVE OF FILL-IN-OKOST IN FRAME DIALOG-1 /* Övrig ersättning */
DO:
   FILL-IN-OKOST = INPUT FILL-IN-OKOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-OKOST DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-OKOST IN FRAME DIALOG-1 /* Övrig ersättning */
DO:
   IF INPUT FILL-IN-OKOST = TRUE THEN FILL-IN-OKOST = FALSE.
   IF INPUT FILL-IN-OKOST = FALSE THEN FILL-IN-OKOST = TRUE.
   DISPLAY FILL-IN-OKOST WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ONATT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ONATT DIALOG-1
ON LEAVE OF FILL-IN-ONATT IN FRAME DIALOG-1
DO:
   FILL-IN-ONATT = INPUT FILL-IN-ONATT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-REDDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-REDDATUM DIALOG-1
ON LEAVE OF FILL-IN-REDDATUM IN FRAME DIALOG-1 /* From datum */
DO:
  FILL-IN-REDDATUM = INPUT FILL-IN-REDDATUM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-REDTRAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-REDTRAKT DIALOG-1
ON LEAVE OF FILL-IN-REDTRAKT IN FRAME DIALOG-1 /* Reducerat trakt. */
DO:
   FILL-IN-REDTRAKT = INPUT FILL-IN-REDTRAKT.
   IF FILL-IN-REDTRAKT = TRUE THEN DO:
      ASSIGN
      FILL-IN-REDDATUM = FILL-IN-STARTDAT.   
      ENABLE FILL-IN-REDDATUM WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-REDDATUM WITH FRAME {&FRAME-NAME}.
      ASSIGN       
      FILL-IN-REDDATUM:HIDDEN = FALSE. 
   END.
   ELSE DO:
      ASSIGN 
      FILL-IN-REDDATUM:HIDDEN = TRUE. 
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-REDTRAKT DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-REDTRAKT IN FRAME DIALOG-1 /* Reducerat trakt. */
DO:
   IF INPUT FILL-IN-REDTRAKT = TRUE THEN FILL-IN-REDTRAKT = FALSE.
   IF INPUT FILL-IN-REDTRAKT = FALSE THEN FILL-IN-REDTRAKT = TRUE.
   DISPLAY FILL-IN-REDTRAKT WITH FRAME {&FRAME-NAME}.
   IF FILL-IN-REDTRAKT = TRUE THEN DO:
      ASSIGN
      FILL-IN-REDDATUM = INPUT FILL-IN-STARTDAT.
      ENABLE FILL-IN-REDDATUM WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-REDDATUM WITH FRAME {&FRAME-NAME}.
      ASSIGN 
      FILL-IN-REDDATUM:HIDDEN = FALSE. 
   END.
   ELSE DO:
      ASSIGN 
      FILL-IN-REDDATUM:HIDDEN = TRUE. 
   END.   
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


&Scoped-define SELF-NAME FILL-IN-SLUTAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTAR DIALOG-1
ON LEAVE OF FILL-IN-SLUTAR IN FRAME DIALOG-1 /* Sluttid */
DO:
  FILL-IN-SLUTAR = INPUT FILL-IN-SLUTAR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTAR DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTAR IN FRAME DIALOG-1 /* Sluttid */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Slut datum */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Slut datum */
DO:
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTDAT.
   RUN AlmanBtn.w.
   FILL-IN-SLUTDAT = Guru.GlobalaVariabler:regdatum.
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTAR DIALOG-1
ON LEAVE OF FILL-IN-STARTAR IN FRAME DIALOG-1 /* Starttid */
DO:
  FILL-IN-STARTAR = INPUT FILL-IN-STARTAR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTAR DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTAR IN FRAME DIALOG-1 /* Starttid */
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Start datum */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Start datum */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
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


&Scoped-define SELF-NAME FILL-IN-TRORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TRORD DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN-TRORD IN FRAME DIALOG-1 /* Tr.zon hela dagen */
DO:
   IF INPUT FILL-IN-TRORD = TRUE THEN FILL-IN-TRORD = FALSE.
   IF INPUT FILL-IN-TRORD = FALSE THEN FILL-IN-TRORD = TRUE.
   DISPLAY FILL-IN-TRORD WITH FRAME {&FRAME-NAME}.   
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
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + "/personal för:".
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
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   
   FILL-IN-OBS = "Registrera endast tjänsteresa om avståndet på resan är minst 50 km från tjänstestället eller bostaden.".      
   RUN huvud_UI.         
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
   IF varifran = 1 THEN DO:
      OPEN QUERY BRW_PERS FOR EACH tidpers WHERE USE-INDEX PERSONALKOD NO-LOCK.  
      ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.
      RELEASE tidpers NO-ERROR.
      RAD_FAST:HIDDEN = TRUE.
   END.
   RUN nasta_UI.
   RUN goma_UI.
   ASSIGN
   FILL-IN-RESTID:HIDDEN = TRUE
   FILL-IN-TRORD:HIDDEN = TRUE
   FILL-IN-FSTART:HIDDEN = TRUE 
   FILL-IN-FSLUT:HIDDEN = TRUE
   FILL-IN-REDTRAKT = FALSE   
   FILL-IN-REDTRAKT:HIDDEN = TRUE
   FILL-IN-REDDATUM:HIDDEN = TRUE.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ASSIGN FILL-IN-NATT:LABEL = "Privat logi".                         
   END.
   
   IF Guru.Konstanter:globforetag = "lule" THEN.
   ELSE FILL-IN-OBS:HIDDEN = TRUE.   
   
   IF Guru.Konstanter:globforetag = "XSUND" THEN DO:       
      DISABLE FILL-IN-ENFLE WITH FRAME {&FRAME-NAME}.
   END.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.   
   ASSIGN
   FILL-IN-PKOD = personaltemp.PERSONALKOD
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.
   DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ELPA" THEN DO: 
      FILL-IN-ENFLE = FALSE.                 
      RUN fienfle_UI.   
   END.
   DISABLE FILL-IN-MAT WITH FRAME {&FRAME-NAME}.                 
   APPLY "ENTRY" TO FILL-IN-ENFLE IN FRAME {&FRAME-NAME}.
   IF varifran = 2 THEN DO:
      RUN fiaonr_UI.
   END.
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
   tidpers.PERSONALKOD:READ-ONLY IN BROWSE BRW_PERS = TRUE.
   egnaao.OMRADE:READ-ONLY IN BROWSE BRW_EAONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}). 
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
   FILL-IN-SOKPA = "Sök på:"
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   CMB_AVD:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.   
   IF ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE"  OR Guru.Konstanter:globforetag = "cELPA" )  AND Guru.Konstanter:globallao = TRUE THEN DO:
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
   DISPLAY RAD_FAST FILL-IN-SOKPA WITH FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfve3_UI DIALOG-1 
PROCEDURE btnfve3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-STARTDAT THEN FILL-IN-STARTDAT = tillochmeddatum + 1.      
   END.   
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.    
   regdatum = FILL-IN-STARTDAT. 
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   IF FILL-IN-FSTART = FALSE /*AND FILL-IN-ENFLE = FALSE*/ THEN DO:   
      RUN REGVEC.P.
      {SLUTARBW.I}
      ASSIGN
      FILL-IN-STARTAR = regstart.
      DISPLAY FILL-IN-STARTAR WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnfve4_UI DIALOG-1 
PROCEDURE btnfve4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT - 1.      
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-SLUTDAT THEN FILL-IN-SLUTDAT = tillochmeddatum + 1.      
   END.   
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnve3_UI DIALOG-1 
PROCEDURE btnnve3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = FILL-IN-STARTDAT. 
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   IF FILL-IN-FSTART = FALSE /*AND FILL-IN-ENFLE = FALSE*/ THEN DO:   
      RUN REGVEC.P.
      {SLUTARBW.I}
      ASSIGN
      FILL-IN-STARTAR = regstart.   
      DISPLAY FILL-IN-STARTAR WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnve4_UI DIALOG-1 
PROCEDURE btnnve4_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
   
   EMPTY TEMP-TABLE respers NO-ERROR.      
   FILL-IN-PKOD = INPUT FRAME {&FRAME-NAME} FILL-IN-PKOD.  
   IF FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN.      
   END.   
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD AND 
   personaltemp.AKTIV = TRUE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.      
      RETURN.      
   END.
   ASSIGN 
   FILL-IN-ENFLE = INPUT FILL-IN-ENFLE.
   ASSIGN
   enflerdygns = FILL-IN-ENFLE
   FILL-IN-RESTID = INPUT FILL-IN-RESTID.
   FILL-IN-TRORD = INPUT FILL-IN-TRORD.
   IF FILL-IN-ENFLE = FALSE THEN DO:
      FILL-IN-RESTID = 00.00.
   END.       
   nytid = FILL-IN-RESTID.
   RUN TIMSEK.P.              
   ASSIGN
   restidsek = sekunder.
   {AVBGOMD.I}
   IF restidsek > 0 THEN RUN MORKVTID.W.  
   {AVBFRAMD.I}
   ASSIGN
   FILL-IN-RESMAL = INPUT FILL-IN-RESMAL
   FILL-IN-PKOD = INPUT FILL-IN-PKOD  
   FILL-IN-AONR = INPUT FILL-IN-AONR      
   FILL-IN-DELNR = INPUT FILL-IN-DELNR
   FILL-IN-BIL = INPUT FILL-IN-BIL
   FILL-IN-MAT = INPUT FILL-IN-MAT
   FILL-IN-NATT = INPUT FILL-IN-NATT
   FILL-IN-OKOST = INPUT FILL-IN-OKOST
   FILL-IN-REDDATUM = INPUT FILL-IN-REDDATUM
   FILL-IN-FSTART = INPUT FILL-IN-FSTART
   FILL-IN-FSLUT = INPUT FILL-IN-FSLUT.
   RUN globm_UI.
   IF musz =  TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.       
   IF FILL-IN-ENFLE = FALSE THEN DO:     
      IF FILL-IN-STARTDAT = FILL-IN-SLUTDAT THEN DO:
         IF FILL-IN-FSTART = TRUE OR FILL-IN-FSLUT = TRUE THEN musz = musz.
         ELSE DO:         
            MESSAGE "Startdag och slutdag kan inte vara lika vid flerdygns förättning." VIEW-AS ALERT-BOX.
            RETURN.             
         END.
      END.  
      IF FILL-IN-SLUTAR = 0 THEN DO:
         MESSAGE "Sluttiden måste vara större än 0." VIEW-AS ALERT-BOX.
         RETURN.                      
      END.  
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:
      IF FILL-IN-ENFLE = TRUE THEN DO:     
         IF FILL-IN-STARTDAT NE FILL-IN-SLUTDAT THEN DO:                
            MESSAGE "Startdag och slutdag måste vara lika vid endagsförrättning." VIEW-AS ALERT-BOX.
            RETURN.             
         END.
      END.        
   END.
   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "Celpa" THEN DO:      
      IF FILL-IN-NATT = TRUE THEN DO:     
         MESSAGE "Skall du verkligen ha nattraktamente?"  
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val5 AS LOGICAL.
         IF val5 = TRUE THEN DO:            
         END.
         ELSE IF val5 = FALSE THEN DO: 
            FILL-IN-NATT = FALSE.            
         END.                     
         ELSE DO:
            RETURN.
         END.
      END.        
   END.
   IF FILL-IN-STARTAR = 0 THEN DO:
      MESSAGE "Börjar verkligen förrättningen 00.00?"  
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val6 AS LOGICAL.
      IF val6 = TRUE THEN DO:            
      END.
      ELSE IF val6 = FALSE THEN DO: 
         RETURN.            
      END.                              
   END.
   IF FILL-IN-SLUTAR = 0 THEN DO:
      MESSAGE "Slutar verkligen förrättningen 00.00?"  
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE val7 AS LOGICAL.
      IF val7 = TRUE THEN DO:            
      END.
      ELSE IF val7 = FALSE THEN DO: 
         RETURN.            
      END.                              
   END.   
   
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
      regdatum = FILL-IN-SLUTDAT.
      {AOKOLLERS.I}
      IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
      utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
      ELSE DO:
         MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  THEN DO:       
      IF utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:
         MESSAGE "Lönetillägg kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
         
         RETURN NO-APPLY.
      END.
   END.       
   /*avtal säger att de enbart får restid vid utb om det är längre bort än 5 mil Ingrid 2005-07-01*/
   IF Guru.Konstanter:globforetag = "CSUND" THEN DO:
      IF FILL-IN-ENFLE = TRUE THEN DO:      
         IF utsokaonr.AONR = "01003" OR utsokaonr.AONR = "03003" OR utsokaonr.AONR = "04003"
         OR utsokaonr.AONR = "05003" OR utsokaonr.AONR = "06003" OR utsokaonr.AONR = "08003" OR utsokaonr.AONR = "04404"
         OR utsokaonr.AONR = "01001" OR utsokaonr.AONR = "03001" OR utsokaonr.AONR = "04001"
         OR utsokaonr.AONR = "05001" OR utsokaonr.AONR = "06001" OR utsokaonr.AONR = "08001"  THEN DO:
         
            MESSAGE "Restid kan inte registreras under utbildningar och möten" VIEW-AS ALERT-BOX.             
            RETURN.
         END.
      END.
   END.       
   IF FILL-IN-RESTID > 6 THEN DO:
      MESSAGE "Restid mer än 6 timmar per tillfälle är ej rimligt" VIEW-AS ALERT-BOX.             
      RETURN.
   END.
   IF FILL-IN-RESMAL = "" THEN DO:
      MESSAGE "Obligatoriskt remål" VIEW-AS ALERT-BOX.
      RETURN.      
   END.   
   RUN reskoll_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.     
      RETURN.
   END.
   ELSE DO:       
      RUN regovr_UI.                  
      musz = FALSE.        
      RETURN.
   END. 
   tidpers.REGKOLL = TRUE.  
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
  DISPLAY FILL-IN-OBS FILL-IN-PKOD FILL-IN-ENFLE FILL-IN-STARTAR FILL-IN-SLUTAR 
          FILL-IN-AONR FILL-IN-DELNR FILL-IN-RESTID FILL-IN-BIL CMB_OVERUT 
          FILL-IN-MAT FILL-IN-NATT FILL-IN-FRIMAT FILL-IN-REDTRAKT FILL-IN-OKOST 
          FILL-IN-3MAN FILL-IN-RESMAL FILL-IN-TEXT CMB_OMR FILL-IN-FSTART 
          FILL-IN-FSLUT FILL-IN-DAG FILL-IN-DAG-2 CMB_AVD FILL-IN-TRORD 
          FILL-IN_FORNAMN-2 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-ENFLE FILL-IN-STARTAR FILL-IN-SLUTAR FILL-IN-AONR 
         FILL-IN-DELNR FILL-IN-RESTID FILL-IN-BIL CMB_OVERUT FILL-IN-MAT 
         FILL-IN-NATT FILL-IN-FRIMAT FILL-IN-REDTRAKT FILL-IN-OKOST 
         FILL-IN-3MAN FILL-IN-RESMAL BTN_REG BTN_NAAVB BTN_AVB CMB_OMR 
         FILL-IN-FSTART FILL-IN-FSLUT CMB_AVD FILL-IN-TRORD BTN_SKAPEN RAD_FAST 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE endag_UI DIALOG-1 
PROCEDURE endag_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = 
   personaltemp.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.       
   FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
   USE-INDEX UT NO-LOCK NO-ERROR.   
   FILL-IN-MAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF FILL-IN-ENFLE = TRUE AND utryckningtemp.ENDMALTID = TRUE THEN DO:
      ENABLE FILL-IN-MAT WITH FRAME {&FRAME-NAME}.       
      ASSIGN
      FILL-IN-MAT = TRUE.     
      DISPLAY FILL-IN-MAT WITH FRAME {&FRAME-NAME}.
      FILL-IN-3MAN = FALSE.
      FILL-IN-3MAN:HIDDEN = TRUE.
      /*FILL-IN-STARTAR:HIDDEN = TRUE.
      FILL-IN-SLUTAR:HIDDEN = TRUE.*/
   END.
   ELSE IF FILL-IN-ENFLE = TRUE THEN DO:
      ASSIGN
      FILL-IN-MAT = FALSE.
      DISPLAY FILL-IN-MAT WITH FRAME {&FRAME-NAME}. 
      FILL-IN-MAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      FILL-IN-3MAN = FALSE.
      FILL-IN-3MAN:HIDDEN = TRUE.
      /*FILL-IN-STARTAR:HIDDEN = TRUE.
      FILL-IN-SLUTAR:HIDDEN = TRUE.*/
   END.   
   ELSE IF FILL-IN-ENFLE = FALSE THEN DO:
      ENABLE FILL-IN-MAT FILL-IN-STARTAR FILL-IN-SLUTAR WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-MAT = TRUE.      
      DISPLAY FILL-IN-MAT FILL-IN-STARTAR FILL-IN-SLUTAR WITH FRAME {&FRAME-NAME}.
      ASSIGN
      musz = FALSE
      sok1 = personaltemp.TRAAVTAL.
      RUN nyupp_UI (INPUT 24).     
      IF sok2 = 1 THEN musz = TRUE.
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE      
         FILL-IN-REDTRAKT = FALSE.
         DISPLAY FILL-IN-REDTRAKT WITH FRAME {&FRAME-NAME}.
         FILL-IN-REDTRAKT:HIDDEN = FALSE.
         FILL-IN-REDDATUM:HIDDEN = TRUE.
      END.
      ELSE DO:
         ASSIGN
         FILL-IN-REDTRAKT = FALSE
         FILL-IN-REDTRAKT:HIDDEN = TRUE
         FILL-IN-REDDATUM:HIDDEN = TRUE.
      END.
      energiavt = FALSE.      
      IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.             
      IF utryckningtemp.INTERNAT = FALSE THEN DO:
         FILL-IN-FRIMAT = FALSE.
         FILL-IN-FRIMAT:HIDDEN = TRUE.
      END.
      ELSE DO:
         ASSIGN
         FILL-IN-FRIMAT = FALSE.
         DISPLAY FILL-IN-FRIMAT WITH FRAME {&FRAME-NAME}.     
         FILL-IN-FRIMAT:HIDDEN = FALSE.      
      END.
      sok1 = personaltemp.TRAAVTAL.
      RUN nyupp_UI (INPUT 25).
      IF sok2 = 1 THEN DO:
         ASSIGN
         FILL-IN-3MAN = FALSE.
         DISPLAY FILL-IN-3MAN WITH FRAME {&FRAME-NAME}.     
         FILL-IN-3MAN:HIDDEN = FALSE.      
      END.   
      ELSE DO.
         FILL-IN-3MAN = FALSE.
         FILL-IN-3MAN:HIDDEN = TRUE.
      END.
   END.    
   RUN nyupp_UI (INPUT 26).
   IF sok2 = 1 THEN DO:        
      ENABLE FILL-IN-BIL WITH FRAME {&FRAME-NAME}.
      FILL-IN-BIL:HIDDEN = FALSE.        
   END.
   ELSE DO:               
      ASSIGN
      FILL-IN-BIL = FALSE           
      FILL-IN-BIL:HIDDEN = TRUE.
   END.
   IF FILL-IN-ENFLE = TRUE THEN DO:
      ASSIGN
      sok1 = ansttemp.KOD
      sok3 = "ENBIL".
      RUN nyupp_UI (INPUT 27).
      IF sok2 = 1 THEN DO:    
         ASSIGN
         FILL-IN-BIL = FALSE              
         FILL-IN-BIL:HIDDEN = TRUE.
      END.
   END.   
   ELSE IF FILL-IN-ENFLE = FALSE THEN DO:
      ASSIGN
      sok1 = ansttemp.KOD
      sok3 = "FLBIL".
      RUN nyupp_UI (INPUT 27).
      IF sok2 = 1 THEN DO:    
         ASSIGN
         FILL-IN-BIL = FALSE              
         FILL-IN-BIL:HIDDEN = TRUE.
      END.
   END.      
   /*IF FILL-IN-ENFLE = FALSE THEN*/ DISABLE FILL-IN-MAT WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fiaonr_UI DIALOG-1 
PROCEDURE fiaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   RAD_FAST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
   BRW_PERS:HIDDEN = TRUE 
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE
   CMB_OMR:HIDDEN = FALSE
   CMB_AVD:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE. 
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.       
   ASSIGN 
   FILL-IN-SOKPA:HIDDEN = TRUE
   RECT-SOK:HIDDEN = FALSE.
   RUN aoladda_UI.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fienfle_UI DIALOG-1 
PROCEDURE fienfle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN 
   FILL-IN-FSTART  = FALSE 
   FILL-IN-FSLUT = FALSE
   FILL-IN-FSTART:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-FSLUT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   IF FILL-IN-ENFLE = TRUE THEN DO:
      ASSIGN
      FILL-IN-NATT = FALSE
      FILL-IN-FRIMAT = FALSE  
      FILL-IN-3MAN = FALSE
      FILL-IN-REDTRAKT = FALSE    
      FILL-IN-NATT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-RESTID:HIDDEN = TRUE
      FILL-IN-TRORD:HIDDEN = TRUE
      FILL-IN-FRIMAT:HIDDEN = TRUE
      FILL-IN-3MAN:HIDDEN = TRUE
      FILL-IN-REDTRAKT:HIDDEN = TRUE      
      FILL-IN-REDDATUM:HIDDEN = TRUE.                         
   END.   
   ELSE DO:
      ENABLE FILL-IN-NATT FILL-IN-FRIMAT FILL-IN-3MAN FILL-IN-REDTRAKT WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FILL-IN-NATT:HIDDEN = FALSE
      FILL-IN-RESTID:HIDDEN = TRUE
      FILL-IN-TRORD:HIDDEN = TRUE
      FILL-IN-FRIMAT:HIDDEN = FALSE
      FILL-IN-3MAN:HIDDEN = FALSE
      FILL-IN-REDTRAKT:HIDDEN = FALSE
      FILL-IN-REDDATUM:HIDDEN = TRUE.      
      IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fislutdat_UI DIALOG-1 
PROCEDURE fislutdat_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fistartdat_UI DIALOG-1 
PROCEDURE fistartdat_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   regdatum = FILL-IN-STARTDAT. 
   RUN REGDAG.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   IF FILL-IN-FSTART = FALSE /*AND FILL-IN-ENFLE = FALSE*/ THEN DO:   
      RUN REGVEC.P.
      {SLUTARBW.I}
      ASSIGN
      FILL-IN-STARTAR = regstart.   
      DISPLAY FILL-IN-STARTAR WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE globm_UI DIALOG-1 
PROCEDURE globm_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FRAME {&FRAME-NAME} FILL-IN-STARTDAT
   FILL-IN-SLUTDAT = INPUT FRAME {&FRAME-NAME} FILL-IN-SLUTDAT.                  
   IF FILL-IN-STARTDAT > FILL-IN-SLUTDAT THEN DO:
      MESSAGE "Felaktigt angivet datum. Startdatum kan ej vara större än slutdatum."
      VIEW-AS ALERT-BOX.
      status-mus = SESSION:SET-WAIT-STATE("").
      musz = TRUE.
      APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.         
      RETURN.
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   /*  OBS för att få lägga upp en tjänsteresa på en godkänd och ekonomikörd, ej lönekörd tidsedel  LENA*/
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-STARTDAT THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         {musarrow.i}
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.            
         APPLY "ENTRY" TO FILL-IN-STARTDAT IN FRAME {&FRAME-NAME}.
         RETURN.            
      END.            
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-SLUTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-SLUTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-SLUTDAT THEN DO:
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
         RETURN.            
      END.            
   END.
   IF YEAR(FILL-IN-STARTDAT) = YEAR(FILL-IN-SLUTDAT) AND MONTH(FILL-IN-STARTDAT) = MONTH(FILL-IN-SLUTDAT) THEN musz = musz.
   ELSE DO:         
      IF MONTH(FILL-IN-STARTDAT) = 12 THEN DO:
         regdatum = DATE(01,01,YEAR(FILL-IN-STARTDAT) + 1). 
      END.
      ELSE DO:
         regdatum = DATE(MONTH(FILL-IN-STARTDAT) + 1,01,YEAR(FILL-IN-STARTDAT)).
      END.
      REPEAT:   
         IF regdatum > FILL-IN-SLUTDAT THEN LEAVE.
         IF MONTH(regdatum) = MONTH(FILL-IN-SLUTDAT) AND YEAR(regdatum) = YEAR(FILL-IN-SLUTDAT) THEN LEAVE.
         ELSE DO:
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
            END.
            ELSE DO:
               RUN GODKOLLA.P  
               (INPUT personaltemp.PERSONALKOD,INPUT regdatum,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
            END.
            IF tillochmeddatum NE ? THEN DO:
               MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
               tillochmeddatum VIEW-AS ALERT-BOX.
               status-mus = SESSION:SET-WAIT-STATE("").
               RETURN NO-APPLY.

            END.
         END.            
         IF MONTH(regdatum) = 12 THEN DO:
            regdatum = DATE(01,01,YEAR(regdatum) + 1). 
         END.
         ELSE DO:
            regdatum = DATE(MONTH(regdatum) + 1,01,YEAR(regdatum)).
         END.   
      END. 
   END.
   IF musz = TRUE THEN DO:         
      status-mus = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.    
   IF FILL-IN-STARTDAT > FILL-IN-SLUTDAT THEN DO:
      MESSAGE "Felaktigt angivet datum. Startdatum kan ej vara större än slutdatum."
      VIEW-AS ALERT-BOX.
      status-mus = SESSION:SET-WAIT-STATE("").
      musz = TRUE.
      APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.         
      RETURN.
   END.                          
   regdatum = FILL-IN-STARTDAT.
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN      
   regdatum = FILL-IN-SLUTDAT.
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN      
   regdatum = FILL-IN-STARTDAT.                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI DIALOG-1 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF varifran = 1 THEN DO:
      musz = musz.
   END.
   ELSE IF varifran = 2 THEN DO:
      BTN_NAAVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ASSIGN    
   CMB_OMR:HIDDEN = TRUE
   CMB_AVD:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE
   BRW_AONR:HIDDEN = TRUE
   FILL-IN-FSTART:HIDDEN = TRUE 
   FILL-IN-FSLUT:HIDDEN = TRUE. 
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
      FILL-IN-REDDATUM:HIDDEN = TRUE
      FILL-IN-FSTART = FALSE
      FILL-IN-FSLUT = FALSE.      
   END.   
   ELSE DO:
      ENABLE FILL-IN-NATT FILL-IN-FRIMAT FILL-IN-3MAN FILL-IN-REDTRAKT WITH FRAME {&FRAME-NAME}.      
      ASSIGN
      FILL-IN-NATT:HIDDEN = FALSE
      FILL-IN-FRIMAT:HIDDEN = FALSE
      FILL-IN-3MAN:HIDDEN = FALSE
      FILL-IN-REDTRAKT:HIDDEN = FALSE
      FILL-IN-REDDATUM:HIDDEN = TRUE.      
      IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
         ASSIGN
         FILL-IN-FSTART:HIDDEN = FALSE 
         FILL-IN-FSLUT:HIDDEN = FALSE.         
      END.
   END.    
   sok1 = personaltemp.TRAAVTAL.
   RUN nyupp_UI (INPUT 25).
   IF sok2 = 0 THEN FILL-IN-3MAN:HIDDEN = TRUE.
   IF utryckningtemp.INTERNAT = FALSE THEN FILL-IN-FRIMAT:HIDDEN = TRUE.
   energiavt = FALSE.   
   IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.
     
   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI DIALOG-1 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   EMPTY TEMP-TABLE respers NO-ERROR.
   EMPTY TEMP-TABLE maltidfil NO-ERROR.
   EMPTY TEMP-TABLE kosters NO-ERROR.      
   ASSIGN
   tidtabrec = 0
   vart = "RES"   
   tjan = "RES"
   FILL-IN-ENFLE = TRUE
   FILL-IN-BIL = FALSE
   FILL-IN-MAT = FALSE
   FILL-IN-NATT = FALSE
   FILL-IN-OKOST = FALSE
   FILL-IN-RESMAL = "".       
   IF stansvar = FALSE THEN regdatum = TODAY.   
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN
   musz = FALSE
   FILL-IN-STARTDAT = regdatum
   FILL-IN-SLUTDAT = regdatum .
   RUN REGDAG.P.
   RUN REGVEC.P.
   IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
   FILL-IN-DAG = regdagnamn + "dag".
   FILL-IN-DAG-2 = regdagnamn + "dag".
   {SLUTARBW.I}
   ASSIGN
   FILL-IN-STARTAR = regstart
   FILL-IN-SLUTAR = regslut.
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT FILL-IN-DAG FILL-IN-DAG-2 FILL-IN-STARTAR FILL-IN-SLUTAR WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.                                   
   ASSIGN
   FILL-IN-AONR = ""
   FILL-IN-DELNR = 00.
   CMB_AVD:DELIMITER = "$". 
   status-ok = CMB_AVD:ADD-LAST(Guru.Konstanter:gavdk + " : alla").   
   status-ok = CMB_OMR:ADD-LAST(Guru.Konstanter:gomrk + " : alla").   
   {ANVAVDSO.I}
  
   
   FOR EACH eavdtemp,         
   EACH avdelningtemp WHERE avdelningtemp.AVDELNINGNR = eavdtemp.AVDELNINGNR.
      status-ok = CMB_AVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.   
   CMB_AVD:SCREEN-VALUE= Guru.Konstanter:gavdk + " : alla".
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.
   IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
      CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
   END.
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallpers = TRUE THEN DO:
      ASSIGN CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
      CMB_OMR = INPUT CMB_OMR.
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   END.
   IF stansvar = TRUE THEN DO:
      IF stansdatum NE ? THEN DO:
         ASSIGN
         FILL-IN-STARTDAT = stansdatum
         FILL-IN-SLUTDAT = stansdatum.
         regdatum = FILL-IN-STARTDAT. 
         RUN REGDAG.P.
         IF regdagnamn = "tor" THEN regdagnamn = regdagnamn + "s".
         FILL-IN-DAG = regdagnamn + "dag".
         FILL-IN-DAG-2 = FILL-IN-DAG.
         RUN REGVEC.P.
         {SLUTARBW.I}
         ASSIGN
         FILL-IN-STARTAR = regstart.
         FILL-IN-SLUTAR = regslut.
         DISPLAY FILL-IN-DAG FILL-IN-STARTAR WITH FRAME {&FRAME-NAME}.
      END.
      IF stansaonr NE "" THEN DO:   
         ASSIGN
         FILL-IN-AONR = stansaonr
         FILL-IN-DELNR = stansdelnr.
      END.   
      DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT FILL-IN-AONR FILL-IN-DELNR
      WITH FRAME {&FRAME-NAME}.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nastapers_UI DIALOG-1 
PROCEDURE nastapers_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODKOLLA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN GODKOLLA.P  
      (INPUT personaltemp.PERSONALKOD,INPUT FILL-IN-STARTDAT,OUTPUT tillochmeddatum,OUTPUT TABLE felmeddtemp).
   END.
   IF tillochmeddatum NE ? THEN DO:
      IF tillochmeddatum >= FILL-IN-STARTDAT THEN DO:
         FILL-IN-STARTDAT = tillochmeddatum + 1.
      END.
   END.  
   RUN REGVEC.P.
   RUN REGDAG.P.
   FILL-IN-SLUTDAT = regdatum.
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nasta_UI DIALOG-1 
PROCEDURE nasta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF valgubbe = FALSE THEN DO:  
      IF NOT AVAILABLE tidpers THEN DO:
         FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
         persrec = tidpers.TIDPERSREC.
      END.
      ELSE DO:
         FIND NEXT tidpers WHERE tidpers.REGKOLL = FALSE USE-INDEX PERSONALKOD NO-ERROR.   
      END.
      IF NOT AVAILABLE tidpers THEN DO:
         FIND FIRST tidpers WHERE tidpers.REGKOLL = FALSE USE-INDEX PERSONALKOD NO-ERROR.   
         IF NOT AVAILABLE tidpers THEN DO:
            musz = TRUE.
            RETURN.
         END.
      END.   
   END.
   ASSIGN
   pkod = tidpers.PERSONALKOD
   FILL-IN-PKOD = tidpers.PERSONALKOD.
   persrec2 = RECID(tidpers).
   RUN anst_UI.
   RUN nastapers_UI.
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      DISPLAY BRW_PERS WITH FRAME {&FRAME-NAME}.
      RUN repo_UI (INPUT 2, INPUT persrec2).                    
      status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   END.
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.          
   IF personaltemp.OVERTIDUTTAG = "K" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Komp" .
   IF personaltemp.OVERTIDUTTAG = "Ö" THEN ASSIGN CMB_OVERUT:SCREEN-VALUE = "Över".   
   ASSIGN CMB_OVERUT = INPUT CMB_OVERUT
   CMB_OVERUT:HIDDEN = TRUE.  
   FILL-IN-MAT:HIDDEN = TRUE.
   FILL-IN_FORNAMN-2 = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.   
   DISPLAY FILL-IN-PKOD FILL-IN_FORNAMN-2 WITH FRAME {&FRAME-NAME}.   
   RUN endag_UI.   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE regovr_UI DIALOG-1 
PROCEDURE regovr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   {muswait.i}      
   IF FILL-IN-ENFLE = FALSE THEN DO:               
      IF FILL-IN-REDTRAKT = TRUE THEN DO:
         regdatum = FILL-IN-REDDATUM.
         RUN REGVEC.P.
         FILL-IN-REDVECKO = regvnr.
         RUN REGDAG.P.
         CMB_REDDAG = regdagnamn.
         reddatum = FILL-IN-REDDATUM.
      END.   
      IF musz = FALSE THEN DO:   
         IF FILL-IN-MAT = TRUE THEN DO:               
            {AVBGOMD.I}
            RUN MATFRAGA.W (INPUT pkod).
            {AVBFRAMD.I}
            IF musz = FALSE THEN DO:
               IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GKAL" 
               OR Guru.Konstanter:globforetag = "LULE"  THEN DO:                  
                  {AVBGOMD.I}
                  RUN KOSTFORM.W (INPUT pkod).
                  {AVBFRAMD.I}
               END.
               ELSE IF  Guru.Konstanter:globforetag = "MISV"  THEN DO:
                  /*registrera kostförmån utan att visa*/
                  RUN KOSTFDOLD.P (INPUT pkod).
               END.     
            END.   
         END.  
      END.            
   END.      
   IF musz = FALSE THEN DO:
      IF FILL-IN-ENFLE = TRUE AND FILL-IN-MAT = TRUE THEN DO:
         {AVBGOMD.I}
         RUN MATFRAGA.W (INPUT pkod).
         {AVBFRAMD.I}
         IF musz = FALSE THEN DO:
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"
            OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
               {AVBGOMD.I}
               RUN KOSTFORM.W (INPUT pkod).
               {AVBFRAMD.I}
            END.   
         END.
         ELSE IF  Guru.Konstanter:globforetag = "MISV"  THEN DO:
            /*registrera kostförmån utan att visa*/
            RUN KOSTFDOLD.P (INPUT pkod).
         END.   
      END.
      IF musz = FALSE THEN DO: 
         IF FILL-IN-OKOST = TRUE THEN DO:
            ASSIGN
            aonrrec = RECID(utsokaonr)
            enflerdygns = FILL-IN-ENFLE
            bdatum = FILL-IN-STARTDAT
            avdatum = FILL-IN-SLUTDAT
            regdatum = bdatum.
            RUN REGVEC.P.
            {AVBGOMD.I}                 
            EMPTY TEMP-TABLE okost NO-ERROR.
            RUN OVRKOSTN.W (INPUT pkod,INPUT utsokaonr.AONR,INPUT utsokaonr.DELNR).
            {AVBFRAMD.I}
         END.    
      END.                 
      IF musz = FALSE THEN DO:                       
         RUN resbild_UI.
      END.
      IF musz = FALSE THEN DO:
         IF FILL-IN-NATT = TRUE THEN DO:
            {AVBGOMD.I}
            RUN NATTFRAG.W (INPUT pkod).
            {AVBFRAMD.I}
         END. 
      END.   
      IF musz = FALSE THEN DO:
         FIND LAST respers USE-INDEX RESPERS NO-LOCK NO-ERROR.         
         spvart = vart.
         SUBSTRING(vart,1,5) = STRING(FILL-IN-STARTAR,"99.99").
         SUBSTRING(vart,6,5) = STRING(FILL-IN-SLUTAR,"99.99").         
         status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").
         {AVBGOMD.I}
         RUN RESINMD.W (INPUT varifran,INPUT pkod).
         {AVBFRAMD.I}
         vart = spvart.
         IF musz = TRUE THEN DO:
            musz = FALSE.  
            EMPTY TEMP-TABLE respers NO-ERROR.
            EMPTY TEMP-TABLE maltidfil NO-ERROR.
            EMPTY TEMP-TABLE kostfil NO-ERROR.
            EMPTY TEMP-TABLE kosters NO-ERROR.            
         END. 
         ELSE DO:          
            FIND FIRST ansttemp WHERE ansttemp.ANSTALLNING = 
            personaltemp.ANSTALLNING USE-INDEX ANSTF NO-LOCK NO-ERROR.       
            FIND FIRST utryckningtemp WHERE  utryckningtemp.KOD = ansttemp.KOD
            USE-INDEX UT NO-LOCK NO-ERROR.   
            energiavt = FALSE.               
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN ASSIGN energiavt = TRUE.
            IF Guru.Konstanter:globforetag = "GKAL" THEN ASSIGN energiavt = TRUE.
            IF Guru.Konstanter:globforetag = "LULE" THEN ASSIGN energiavt = TRUE.            
            RUN resdel_UI. 
            IF utryckningtemp.RHALV = TRUE THEN DO:  
               IF energiavt = TRUE THEN DO: 
                  RUN rhalvkom_UI.
                  IF enflerdygns = TRUE THEN DO:                  
                     /*max 2 timmars restid per dag vid endagsförrättning*/
                     IF Guru.Konstanter:globforetag = "cSUND" OR Guru.Konstanter:globforetag = "elpa" THEN RUN resmax_UI.
                  END.
               END.               
               ELSE RUN reshalv_UI.
            END.               
            
            EMPTY TEMP-TABLE resapptemp NO-ERROR.
            EMPTY TEMP-TABLE respersextra NO-ERROR.            
            status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").                               
            
            RUN resapp_UI.
            {RESRENIN.I}
            {FELTEXINUT.I}
            IF musz = TRUE THEN DO:
               musz = FALSE.                  
               {musarrow.i}
               RETURN.
            END.          
            IF FILL-IN-OKOST = TRUE THEN DO:
               /*övriga kostnader faktura*/
            END.              
            status-mus = SESSION:SET-WAIT-STATE("").
            IF varifran = 1 THEN DO:
               MESSAGE "Vill du registrera flera tjänsteresor ?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Tjänsteresor"
               UPDATE answer AS LOGICAL.
               IF answer THEN APPLY "CHOOSE" TO BTN_NAAVB IN FRAME {&FRAME-NAME}. 
               ELSE APPLY "GO" TO BTN_AVB IN FRAME {&FRAME-NAME}.
            END.  
            ELSE APPLY "GO" TO BTN_AVB IN FRAME {&FRAME-NAME}.
         END.  
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_AONR
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_PERS
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resapp_UI DIALOG-1 
PROCEDURE resapp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   CREATE resapptemp.
   ASSIGN
   resapptemp.FORETAG = Guru.Konstanter:globforetag
   resapptemp.ANVANDARE = Guru.Konstanter:globanv
   resapptemp.ENFLE = FILL-IN-ENFLE 
   resapptemp.TREMAN = FILL-IN-3MAN
   resapptemp.FRIMAT = FILL-IN-FRIMAT
   resapptemp.MAT = FILL-IN-MAT
   resapptemp.RESMAL = FILL-IN-RESMAL
   resapptemp.REDTRAKT = FILL-IN-REDTRAKT               
   resapptemp.PKOD = FILL-IN-PKOD
   resapptemp.RECPERS = persrec
   resapptemp.INDATUM = bdatum
   resapptemp.UTDATUM = avdatum
   resapptemp.DATUMRED = reddatum
   resapptemp.MTRL = FILL-IN-TRORD
   resapptemp.FSTART = FILL-IN-FSTART
   resapptemp.FSLUT = FILL-IN-FSLUT
   resapptemp.RSTART = FILL-IN-STARTAR
   resapptemp.RSLUT = FILL-IN-SLUTAR.
   FOR EACH respers:
      CREATE respersextra.
      ASSIGN
      respersextra.AONR = respers.AONR 
      respersextra.DELNR = respers.DELNR
      respersextra.VECKONUMMER = respers.VECKONUMMER 
      respersextra.DATUM = respers.DATUM 
      respersextra.DAG = respers.DAG 
      respersextra.START = respers.START 
      respersextra.SLUT = respers.SLUT  
      respersextra.PRIS = respers.PRIS 
      respersextra.PRISTYP = respers.PRISTYP 
      respersextra.NATTRAKT = respers.NATTRAKT 
      respersextra.OVERTIDUTTAG = respers.OVERTIDUTTAG  
      respersextra.BILFORARE = respers.BILFORARE    
      respersextra.ENFLERDAGS = respers.ENFLERDAGS 
      respersextra.TIDREC = respers.TIDREC.
   END.                                                                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resbild_UI DIALOG-1 
PROCEDURE resbild_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
 /*RESBILD3.P  LAGGER UT DAGAR FRAN STARTDAG TILL SLUTDAG MED TIDER ENLIGT VECKOSCHEMAT*/
   EMPTY TEMP-TABLE respers NO-ERROR.   
   regdatum = bdatum.
   REPEAT:
      IF regdatum > avdatum THEN LEAVE.
      IF FILL-IN-ENFLE = TRUE THEN enf = "Endag".
      IF FILL-IN-ENFLE = FALSE THEN enf = "Flerdag".
      RUN REGVEC.P.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 2 THEN DO:
         regdagnamn = "mån".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 3 THEN DO:
         regdagnamn = "tis".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 4 THEN DO:
         regdagnamn = "ons".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 5 THEN DO:
         regdagnamn = "tor".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 6 THEN DO:
         regdagnamn = "fre".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE. 
      END.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 7 THEN DO:
         regdagnamn = "lör".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.
      {SLUTARBW.I}
      IF WEEKDAY(regdatum) = 1 THEN DO:
         regdagnamn = "sön".
         RUN restid_UI.
         regdatum = regdatum + 1.
         IF regdatum > avdatum THEN LEAVE.
      END.     
   END. /*REPEAT*/   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resdel_UI DIALOG-1 
PROCEDURE resdel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   ASSIGN
   sok3 = "RESTID..."
   sok4 = personaltemp.PERSONALKOD.
   RUN nyupp_UI (INPUT 3).     
   ASSIGN  
   pristyp1 = sok3
   pris1 = sok5. 
   ASSIGN
   sok3 = "RESTIDARB"
   sok4 = personaltemp.PERSONALKOD.
   RUN nyupp_UI (INPUT 3).     
   ASSIGN  
   pristyp2 = sok3
   pris2 = sok5. 
   FOR EACH respers:             
      IF FILL-IN-ENFLE = TRUE THEN DO:      
         IF utryckningtemp.ENDMALTID = TRUE AND FILL-IN-MAT = TRUE THEN regdatum = regdatum.
         ELSE DO:
            regvnr = respers.VECKONUMMER.
            regdatum = respers.DATUM.
            {SLUTARBW.I}
            IF respers.START = regstart AND respers.SLUT = regstart THEN DO:
               DELETE respers.
               NEXT.
            END.  
            IF respers.START = regslut AND respers.SLUT = regslut THEN DO:
               DELETE respers.
               NEXT.
            END. 
         END.           
      END.         
      ASSIGN 
      respers.PRISTYP = pristyp1 
      respers.PRIS = pris1. 
   END. /*FOR EACH respers*/     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reshalv_UI DIALOG-1 
PROCEDURE reshalv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   halvt = 0. /*JÄMNA HALVTIMMAR */
   sext = 0.  /*MAX 6 TIMMAR RESTID EJ BILFÖRARE*/
   FIND FIRST respers USE-INDEX RESPERS NO-LOCK NO-ERROR.
   IF NOT AVAILABLE respers THEN RETURN.
   regvnr = respers.VECKONUMMER.
   regdatum = respers.DATUM.
   {SLUTARBW.I}   
   ASSIGN 
   hjdag = respers.DAG.
   IF respers.START NE respers.SLUT AND respers.START GE regstart AND respers.START LE regslut  THEN DO:
      IF respers.SLUT GE regslut THEN nytid = regslut.
      ELSE nytid = respers.SLUT.
   END.
   ELSE nytid = respers.START.
   RUN TIMSEK.P.  
   ASSIGN
   seku = sekunder.
   IF respers.START NE respers.SLUT AND respers.SLUT GE regstart AND respers.SLUT LE regslut  THEN DO:
      IF respers.START LE regstart THEN nytid = regstart.
      ELSE nytid = respers.START.
   END.
   ELSE nytid = respers.SLUT.
   RUN TIMSEK.P.
   ASSIGN
   seku = sekunder - seku.
         
   ASSIGN halvt = halvt + seku.
   IF respers.BILFORARE = FALSE THEN  ASSIGN sext = sext + seku.
   resrec = RECID(respers).                   
   halv:
   REPEAT:
      FIND NEXT respers USE-INDEX RESPERS NO-LOCK NO-ERROR.
      IF NOT AVAILABLE respers THEN LEAVE halv.
      regvnr = respers.VECKONUMMER.
      regdatum = respers.DATUM.
      {SLUTARBW.I}
      IF respers.START = respers.SLUT THEN NEXT halv. 
      IF respers.START > regstart AND respers.SLUT LE regslut THEN NEXT halv.
      IF respers.START >= regstart AND respers.SLUT < regslut THEN NEXT halv.
      resrec2 = RECID(respers).
      IF hjdag = respers.DAG THEN DO:    
         IF respers.START > regstart AND respers.START LE regslut THEN DO:
           nytid = regslut.
         END.  
         ELSE IF respers.START GE regslut OR respers.START LE regstart THEN DO:
           nytid = respers.START.
         END. 
         RUN TIMSEK.P.
         seku = sekunder. 
         IF respers.SLUT GE regstart AND respers.SLUT < regslut THEN DO:
           nytid = regstart.
         END.  
         ELSE IF respers.SLUT GE regslut OR respers.SLUT LE regstart THEN DO:
           nytid = respers.SLUT.
         END. 
         RUN TIMSEK.P. 
         ASSIGN
         seku = sekunder - seku.              
         ASSIGN halvt = halvt + seku.
         IF respers.BILFORARE = FALSE THEN ASSIGN sext = sext + seku.
         ASSIGN
         hjdag = respers.DAG
         resrec = RECID(respers).
      END.
      ELSE DO:
         FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.         
         halvkvart = 1800.
         IF respers.BILFORARE = FALSE AND sext > 21600 THEN bort = sext - 21600.
         ELSE DO:   
            antal = TRUNCATE((halvt / halvkvart),0).    
            bort = halvt - (antal * halvkvart).           
         END.   
         FIND respers WHERE RECID(respers) = resrec EXCLUSIVE-LOCK NO-ERROR.
         regvnr = respers.VECKONUMMER.
         regdatum = respers.DATUM.
         {SLUTARBW.I}
         IF bort < seku THEN DO:
            IF regstart = regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT GE regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT LE regstart THEN DO:
               nytid = respers.START.   
               RUN TIMSEK.P.
               sekunder = sekunder + bort.
               RUN SEKTIM.P.
               ASSIGN respers.START = nytid.
            END.   
         END. 
         ELSE DO:     
            ASSIGN respers.SLUT = respers.START.
            bort = bort - seku.
            FIND FIRST respers WHERE respers.START NE respers.SLUT AND
            respers.DAG = hjdag USE-INDEX RESPERS EXCLUSIVE-LOCK NO-ERROR. 
            IF AVAILABLE  respers THEN DO: 
               IF respers.SLUT GE regslut THEN DO:
                  nytid = respers.SLUT.
                  RUN TIMSEK.P.
                  sekunder = sekunder - bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.SLUT = nytid.
               END. 
               ELSE IF respers.SLUT LE regstart THEN DO:
                  nytid = respers.START.   
                  RUN TIMSEK.P.
                  sekunder = sekunder + bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.START = nytid.
               END.     
            END.   
         END. 
         FIND respers WHERE RECID(respers) = resrec2 NO-LOCK NO-ERROR.
         regvnr = respers.VECKONUMMER.
         regdatum = respers.DATUM.
         {SLUTARBW.I}
         halvt = 0.
         sext = 0.
         IF respers.START > regstart AND respers.START LE regslut THEN DO:
           nytid = regslut.
         END.  
         ELSE IF respers.START GE regslut OR respers.START LE regstart THEN DO:
           nytid = respers.START.
         END. 
         RUN TIMSEK.P.
         seku = sekunder.
         IF respers.SLUT GE regstart AND respers.SLUT < regslut THEN DO:
           nytid = regstart.
         END.  
         ELSE IF respers.SLUT GE regslut OR respers.SLUT LE regstart THEN DO:
           nytid = respers.SLUT.
         END. 
         RUN TIMSEK.P.      
         ASSIGN
         seku = sekunder - seku.              
         ASSIGN halvt = halvt + seku.
         IF respers.BILFORARE = FALSE THEN ASSIGN sext = sext + seku.
         ASSIGN hjdag = respers.DAG
         resrec = RECID(respers).
      END.
   END.            
   IF halvt > 0 OR sext > 0  THEN DO:
      FIND respers WHERE RECID(respers) = resrec EXCLUSIVE-LOCK NO-ERROR.      
      halvkvart = 1800.
      IF respers.BILFORARE = FALSE AND sext > 21600 THEN bort = sext - 21600.
      ELSE DO:          
         antal = TRUNCATE((halvt / halvkvart),0).    
         bort = halvt - (antal * halvkvart).        
      END.   
      FIND respers WHERE RECID(respers) = resrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE respers THEN DO:
         regvnr = respers.VECKONUMMER.
         regdatum = respers.DATUM.
         {SLUTARBW.I}
         IF bort < seku THEN DO:
            IF regstart = regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT GE regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT LE regstart THEN DO:
               nytid = respers.START.   
               RUN TIMSEK.P.
               sekunder = sekunder + bort.
               RUN SEKTIM.P.
               ASSIGN respers.START = nytid.
            END.   
         END. 
         ELSE DO:     
            ASSIGN respers.SLUT = respers.START.
            bort = bort - seku.
            FIND FIRST respers WHERE respers.START NE respers.SLUT AND
            respers.DAG = hjdag USE-INDEX RESPERS EXCLUSIVE-LOCK NO-ERROR. 
            IF AVAILABLE  respers THEN DO: 
               IF respers.SLUT GE regslut THEN DO:
                  nytid = respers.SLUT.
                  RUN TIMSEK.P.
                  sekunder = sekunder - bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.SLUT = nytid.
               END. 
               ELSE IF respers.SLUT LE regstart THEN DO:
                  nytid = respers.START.   
                  RUN TIMSEK.P.
                  sekunder = sekunder + bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.START = nytid.
               END.     
            END.   
         END. 
      END. 
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reskoll_UI DIALOG-1 
PROCEDURE reskoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF FILL-IN-ENFLE = TRUE THEN enfle = "Endag".
   IF FILL-IN-ENFLE = FALSE THEN enfle = "Flerdag".
   ASSIGN
   bdatum = FILL-IN-STARTDAT 
   avdatum = FILL-IN-SLUTDAT.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN RESKOLLAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 1,INPUT pkod,INPUT bdatum,INPUT avdatum,INPUT enfle,INPUT Guru.Konstanter:globanv,INPUT FILL-IN-STARTAR,INPUT FILL-IN-SLUTAR,OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN RESKOLLAN.P 
      (INPUT 1,INPUT pkod,INPUT bdatum,INPUT avdatum,INPUT enfle,INPUT Guru.Konstanter:globanv,INPUT FILL-IN-STARTAR,INPUT FILL-IN-SLUTAR,OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      FOR EACH felmeddtemp:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      END.
      musz = TRUE.
      RETURN.
   END.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resmax_UI DIALOG-1 
PROCEDURE resmax_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   sext = 0.  /*MAX 6 TIMMAR RESTID EJ BILFÖRARE*/
   maxres = 7200.  /*MAX 2 timmar restid per dag endagförrättning Sundsvall*/
   FIND FIRST respers USE-INDEX RESPERS NO-LOCK NO-ERROR.
   IF NOT AVAILABLE respers THEN RETURN.
   regvnr = respers.VECKONUMMER.
   regdatum = respers.DATUM.
   {SLUTARBW.I}   
   ASSIGN 
   hjdag = respers.DAG.
   IF respers.START NE respers.SLUT AND respers.START GE regstart AND respers.START LE regslut  THEN DO:
      IF respers.SLUT GE regslut THEN nytid = regslut.
      ELSE nytid = respers.SLUT.
   END.
   ELSE nytid = respers.START.
   RUN TIMSEK.P.  
   ASSIGN
   seku = sekunder.
   IF respers.START NE respers.SLUT AND respers.SLUT GE regstart AND respers.SLUT LE regslut  THEN DO:
      IF respers.START LE regstart THEN nytid = regstart.
      ELSE nytid = respers.START.
   END.
   ELSE nytid = respers.SLUT.
   RUN TIMSEK.P.
   ASSIGN
   seku = sekunder - seku.   
   IF enflerdygns = TRUE THEN  ASSIGN sext = sext + seku.
   resrec = RECID(respers).                   
   halv:
   REPEAT:
      FIND NEXT respers USE-INDEX RESPERS NO-LOCK NO-ERROR.
      IF NOT AVAILABLE respers THEN LEAVE halv.
      regvnr = respers.VECKONUMMER.
      regdatum = respers.DATUM.
      {SLUTARBW.I}
      IF respers.START = respers.SLUT THEN NEXT halv. 
      IF respers.START > regstart AND respers.SLUT LE regslut THEN NEXT halv.
      IF respers.START >= regstart AND respers.SLUT < regslut THEN NEXT halv.
      resrec2 = RECID(respers).
      IF hjdag = respers.DAG THEN DO:    
         IF respers.START > regstart AND respers.START LE regslut THEN DO:
           nytid = regslut.
         END.  
         ELSE IF respers.START GE regslut OR respers.START LE regstart THEN DO:
           nytid = respers.START.
         END. 
         RUN TIMSEK.P.
         seku = sekunder. 
         IF respers.SLUT GE regstart AND respers.SLUT < regslut THEN DO:
           nytid = regstart.
         END.  
         ELSE IF respers.SLUT GE regslut OR respers.SLUT LE regstart THEN DO:
           nytid = respers.SLUT.
         END. 
         RUN TIMSEK.P. 
         ASSIGN
         seku = sekunder - seku.
         IF enflerdygns = TRUE THEN ASSIGN sext = sext + seku.
         ASSIGN
         hjdag = respers.DAG
         resrec = RECID(respers).
      END.
      ELSE DO:
         FIND respers WHERE RECID(respers) = resrec NO-LOCK NO-ERROR.         
         IF enflerdygns = TRUE AND sext > maxres THEN bort = sext - maxres.         
         FIND respers WHERE RECID(respers) = resrec EXCLUSIVE-LOCK NO-ERROR.
         regvnr = respers.VECKONUMMER.
         regdatum = respers.DATUM.
         {SLUTARBW.I}
         IF bort < seku THEN DO:
            IF regstart = regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT GE regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT LE regstart THEN DO:
               nytid = respers.START.   
               RUN TIMSEK.P.
               sekunder = sekunder + bort.
               RUN SEKTIM.P.
               ASSIGN respers.START = nytid.
            END.   
         END. 
         ELSE DO:     
            ASSIGN respers.SLUT = respers.START.
            bort = bort - seku.
            FIND FIRST respers WHERE respers.START NE respers.SLUT AND
            respers.DAG = hjdag USE-INDEX RESPERS EXCLUSIVE-LOCK NO-ERROR. 
            IF AVAILABLE  respers THEN DO: 
               IF respers.SLUT GE regslut THEN DO:
                  nytid = respers.SLUT.
                  RUN TIMSEK.P.
                  sekunder = sekunder - bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.SLUT = nytid.
               END. 
               ELSE IF respers.SLUT LE regstart THEN DO:
                  nytid = respers.START.   
                  RUN TIMSEK.P.
                  sekunder = sekunder + bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.START = nytid.
               END.     
            END.   
         END. 
         FIND respers WHERE RECID(respers) = resrec2 NO-LOCK NO-ERROR.
         regvnr = respers.VECKONUMMER.
         regdatum = respers.DATUM.
         {SLUTARBW.I}
         halvt = 0.
         sext = 0.
         IF respers.START > regstart AND respers.START LE regslut THEN DO:
           nytid = regslut.
         END.  
         ELSE IF respers.START GE regslut OR respers.START LE regstart THEN DO:
           nytid = respers.START.
         END. 
         RUN TIMSEK.P.
         seku = sekunder.
         IF respers.SLUT GE regstart AND respers.SLUT < regslut THEN DO:
           nytid = regstart.
         END.  
         ELSE IF respers.SLUT GE regslut OR respers.SLUT LE regstart THEN DO:
           nytid = respers.SLUT.
         END. 
         RUN TIMSEK.P.      
         ASSIGN
         seku = sekunder - seku.         
         IF enflerdygns = TRUE THEN ASSIGN sext = sext + seku.
         ASSIGN hjdag = respers.DAG
         resrec = RECID(respers).
      END.
   END.            
   IF sext > 0  THEN DO:
      FIND respers WHERE RECID(respers) = resrec EXCLUSIVE-LOCK NO-ERROR.      
      IF enflerdygns = TRUE AND sext > maxres THEN bort = sext - maxres.      
      FIND respers WHERE RECID(respers) = resrec EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE respers THEN DO:
         regvnr = respers.VECKONUMMER.
         regdatum = respers.DATUM.
         {SLUTARBW.I}
         IF bort < seku THEN DO:
            IF regstart = regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT GE regslut THEN DO:
               nytid = respers.SLUT.   
               RUN TIMSEK.P.
               sekunder = sekunder - bort.
               RUN SEKTIM.P.
               ASSIGN respers.SLUT = nytid.
            END.   
            ELSE IF respers.SLUT LE regstart THEN DO:
               nytid = respers.START.   
               RUN TIMSEK.P.
               sekunder = sekunder + bort.
               RUN SEKTIM.P.
               ASSIGN respers.START = nytid.
            END.   
         END. 
         ELSE DO:     
            ASSIGN respers.SLUT = respers.START.
            bort = bort - seku.
            FIND FIRST respers WHERE respers.START NE respers.SLUT AND
            respers.DAG = hjdag USE-INDEX RESPERS EXCLUSIVE-LOCK NO-ERROR. 
            IF AVAILABLE  respers THEN DO: 
               IF respers.SLUT GE regslut THEN DO:
                  nytid = respers.SLUT.
                  RUN TIMSEK.P.
                  sekunder = sekunder - bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.SLUT = nytid.
               END. 
               ELSE IF respers.SLUT LE regstart THEN DO:
                  nytid = respers.START.   
                  RUN TIMSEK.P.
                  sekunder = sekunder + bort.
                  RUN SEKTIM.P.
                  ASSIGN respers.START = nytid.
               END.     
            END.   
         END. 
      END. 
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restid_UI DIALOG-1 
PROCEDURE restid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*MORGON*/
   CREATE respers.
   ASSIGN 
   respers.AONR = FILL-IN-AONR
   respers.DELNR = FILL-IN-DELNR
   respers.VECKONUMMER = regvnr 
   respers.DATUM = regdatum 
   respers.DAG = regdagnamn
   respers.START = regstart
   respers.SLUT = regstart  
   respers.NATTRAKT = FILL-IN-NATT 
   respers.OVERTIDUTTAG = personaltemp.OVERTIDUTTAG
   respers.BILFORARE = FILL-IN-BIL                                  
   respers.ENFLERDAGS = enf
   respers.TIDREC = ?.   
   IF personaltemp.OVERTIDUTTAG = "F" THEN respers.OVERTIDUTTAG = "Ö".
   IF FILL-IN-ENFLE = FALSE AND regdatum = bdatum THEN DO:
       IF regstart = regslut THEN DO:
          ASSIGN respers.START = FILL-IN-STARTAR.
          ASSIGN respers.SLUT = FILL-IN-STARTAR.
       END.
       ELSE IF FILL-IN-STARTAR < regstart THEN DO:
           ASSIGN respers.START = FILL-IN-STARTAR.
       END.
       ELSE IF FILL-IN-STARTAR > regslut THEN DO:
          ASSIGN respers.START = FILL-IN-STARTAR.
          ASSIGN respers.SLUT = FILL-IN-STARTAR.
       END.       
       IF ftro = TRUE THEN DO:
          /*ingen restid*/
          ASSIGN respers.SLUT = respers.START.
       END.            
   END.   
   IF CMB_OVERUT = "Komp" THEN ASSIGN respers.OVERTIDUTTAG = "K". 
   IF CMB_OVERUT = "Över" THEN ASSIGN respers.OVERTIDUTTAG = "Ö".
   nytid = regstart.
   RUN TIMSEK.P. 
   /*MORGON*/    
   ASSIGN
   sok1 = pkod
   sok4 = STRING(respers.DATUM)
   sok5 = regstart.
   RUN nyupp_UI (INPUT 28).     
   IF sok2 = 1 THEN musz = TRUE.                   
   IF musz = FALSE THEN DO:   
      IF sekunder < restidsek THEN ASSIGN respers.START = 00.00.
      ELSE DO:             
         IF FILL-IN-ENFLE = TRUE THEN DO:                           
            IF RAD_MKTID = 1 OR RAD_MKTID = 2 THEN sekunder = sekunder - restidsek. 
            RUN SEKTIM.P.
            ASSIGN respers.START = nytid.
         END.
      END.      
   END.       
   IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      IF FILL-IN-ENFLE = TRUE AND regdatum = bdatum AND bdatum = avdatum THEN DO:
         IF respers.START = respers.START AND respers.START = regstart AND FILL-IN-STARTAR < regstart THEN DO:
            ASSIGN respers.START = FILL-IN-STARTAR.
         END.
      END.
   END.
   musz = FALSE.
   /*KVÄLL*/
   CREATE respers.
   ASSIGN 
   respers.AONR = FILL-IN-AONR
   respers.DELNR = FILL-IN-DELNR
   respers.VECKONUMMER = regvnr 
   respers.DATUM = regdatum 
   respers.DAG = regdagnamn
   respers.NATTRAKT = FILL-IN-NATT 
   respers.OVERTIDUTTAG = personaltemp.OVERTIDUTTAG 
   respers.BILFORARE = FILL-IN-BIL
   respers.ENFLERDAGS = enf
   respers.START = regslut
   respers.SLUT = regslut
   respers.TIDREC = ?.
   IF personaltemp.OVERTIDUTTAG = "F" THEN respers.OVERTIDUTTAG = "Ö".
   IF FILL-IN-ENFLE = FALSE AND regdatum = avdatum - 1 THEN DO:
      IF FILL-IN-SLUTAR < regstart THEN DO:      
         IF regstart = regslut THEN.
         ELSE ASSIGN respers.SLUT = 24.         
      END.
   END.
   IF FILL-IN-ENFLE = FALSE AND regdatum = avdatum THEN DO:
      IF regstart = regslut THEN DO:
         ASSIGN respers.START = FILL-IN-SLUTAR.
         ASSIGN respers.SLUT = FILL-IN-SLUTAR.
      END.
      ELSE IF FILL-IN-SLUTAR > regslut THEN DO:
         ASSIGN respers.SLUT = FILL-IN-SLUTAR.
      END.
      ELSE IF FILL-IN-SLUTAR < regstart  THEN DO:
         ASSIGN respers.START = 0.
         ASSIGN respers.SLUT = FILL-IN-SLUTAR.
      END. 
      IF ftro = TRUE THEN DO:
          /*ingen restid*/
          ASSIGN respers.START = respers.SLUT.
       END.  
   END.
   IF CMB_OVERUT = "Komp" THEN ASSIGN respers.OVERTIDUTTAG = "K". 
   IF CMB_OVERUT = "Över" THEN ASSIGN respers.OVERTIDUTTAG = "Ö".
   nytid = regslut.
   RUN TIMSEK.P.
   ASSIGN
   sok1 = pkod
   sok4 = STRING(respers.DATUM)
   sok5 = regslut.
   RUN nyupp_UI (INPUT 29).     
   IF sok2 = 1 THEN musz = TRUE.
   IF musz = FALSE THEN DO:      
      IF (sekunder + restidsek) > 86400 THEN ASSIGN respers.SLUT = 24.00.
      ELSE DO:       
         IF FILL-IN-ENFLE = TRUE THEN DO:
            IF RAD_MKTID = 1 OR RAD_MKTID = 3 THEN sekunder = sekunder + restidsek. 
            RUN SEKTIM.P.
            ASSIGN respers.SLUT = nytid.
         END.
      END.        
   END.            
   IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:   
      IF FILL-IN-ENFLE = TRUE AND regdatum = bdatum AND bdatum = avdatum THEN DO:         
         IF respers.SLUT = respers.SLUT AND respers.SLUT = regslut AND FILL-IN-SLUTAR > regslut THEN DO:
            ASSIGN respers.SLUT = FILL-IN-SLUTAR.
         END.
      END.
   END.   
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rhalvkom_UI DIALOG-1 
PROCEDURE rhalvkom_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   halvt = 0.
   sext = 0.  /*MAX 2 TIMMAR RESTID endags Sund*/
   FIND FIRST respers USE-INDEX RESPERS NO-LOCK NO-ERROR.
   IF NOT AVAILABLE respers THEN RETURN.
   ASSIGN  
   nytid = respers.START.
   RUN TIMSEK.P.  
   ASSIGN
   avsta = sekunder
   seku = sekunder
   nytid = respers.SLUT.
   RUN TIMSEK.P.
   ASSIGN
   avslu = sekunder.
   sekunder = sekunder - seku.       
   IF sekunder > 0 THEN DO:           
      htim = TRUNCATE( sekunder / 1800 ,0).     
      IF htim = 0 THEN DO: 
         IF FILL-IN-SLUTAR = respers.SLUT THEN FILL-IN-SLUTAR = respers.START.         
         ASSIGN respers.SLUT = respers.START.
         
      END.
      ELSE DO:       
         sekunder = 1800 - sekunder + ( htim * 1800 ).
         IF sekunder = 1800 THEN sekunder = 0.
         IF sekunder > 0 THEN DO:
            IF respers.START < regstart THEN DO:
               IF respers.START = 0 THEN.
               ELSE DO:               
                  avsta = avsta - sekunder.
                  sekunder = avsta.    
                  RUN SEKTIM.P.   
                  ASSIGN respers.START = nytid.
               END.
            END.
            ELSE IF respers.START GE regslut THEN DO:
               IF respers.SLUT = 24 THEN.
               ELSE DO:               
                  avslu = avslu + sekunder.
                  sekunder = avslu.    
                  RUN SEKTIM.P.   
                  ASSIGN respers.SLUT = nytid.        
                  IF respers.SLUT > 24 THEN respers.SLUT = 24.
               END.
            END.   
         END.   
      END.   
      
      IF enflerdygns = TRUE THEN  ASSIGN sext = sext + seku.
   END.   
   halv:
   REPEAT:
      FIND NEXT respers USE-INDEX RESPERS NO-LOCK NO-ERROR.
      IF NOT AVAILABLE respers THEN LEAVE halv.     
      {SLUTARBW.I}
      IF respers.START = respers.SLUT THEN NEXT halv. 
      IF respers.START > regstart AND respers.SLUT LE regslut THEN NEXT halv.
      IF respers.START >= regstart AND respers.SLUT < regslut THEN NEXT halv.    
      ASSIGN  
      nytid = respers.START.
      RUN TIMSEK.P.  
      ASSIGN
      avsta = sekunder
      seku = sekunder
      nytid = respers.SLUT.
      RUN TIMSEK.P.
      ASSIGN
      avslu = sekunder.
      sekunder = sekunder - seku.       
      IF sekunder > 0 THEN DO:           
         htim = TRUNCATE( sekunder / 1800 ,0). 
         IF htim = 0 THEN DO: 
            IF FILL-IN-SLUTAR = respers.SLUT THEN FILL-IN-SLUTAR = respers.START.         
            ASSIGN respers.SLUT = respers.START.            
         END.
         ELSE DO:           
            sekunder = 1800 - sekunder + ( htim * 1800 ).
            IF sekunder = 1800 THEN sekunder = 0.
            IF sekunder > 0 THEN DO:
               IF respers.START < regstart THEN DO:
                  IF respers.START = 0 THEN.
                  ELSE DO:               
                     avsta = avsta - sekunder.
                     sekunder = avsta.    
                     RUN SEKTIM.P.   
                     ASSIGN respers.START = nytid.
                  END.
               END.
               ELSE IF respers.START GE regslut THEN DO:
                  IF respers.SLUT = 24 THEN.
                  ELSE DO:               
                     avslu = avslu + sekunder.
                     sekunder = avslu.    
                     RUN SEKTIM.P.   
                     ASSIGN respers.SLUT = nytid.        
                     IF respers.SLUT > 24 THEN respers.SLUT = 24.
                  END.
               END.   
            END.   
         END.   
      END.   
   END.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

