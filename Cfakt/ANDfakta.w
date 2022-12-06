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

  Created: 04/15/97 -  9:24 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{ANVPERS.I}
&Scoped-define NEW 
{FAKTPLANTEMP.I}                 
{FAKTTEMP.I}
{FAKTTYPDEF.I}
{OMRTEMPW.I}
{EXTRATAB.I}  
{FAKTSTART.I}
{SOKDEF.I}
DEFINE INPUT PARAMETER ingang AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER faonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER fdelnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER outfakt AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
&Scoped-define NEW 
DEFINE NEW SHARED VARIABLE bestapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE  VARIABLE faktupphmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.

DEFINE VARIABLE nyfakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE openqkoll AS LOGICAL EXTENT 50 NO-UNDO.
DEFINE VARIABLE tot% AS INTEGER NO-UNDO.
DEFINE VARIABLE belopp% AS DECIMAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE val1 AS LOGICAL NO-UNDO.
DEFINE VARIABLE morgon AS DECIMAL NO-UNDO.
DEFINE VARIABLE kvall AS DECIMAL NO-UNDO.
DEFINE VARIABLE varbestid AS INTEGER NO-UNDO.
DEFINE VARIABLE faktprocentvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE uppprocentvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE nyupparb AS INTEGER NO-UNDO.
DEFINE VARIABLE nyprisvar AS DECIMAL NO-UNDO.   
DEFINE VARIABLE svarvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE sortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vilkavar AS CHARACTER NO-UNDO.
DEFINE BUFFER faktuppbuff FOR faktuppplantemp.
DEFINE TEMP-TABLE artemp NO-UNDO
   FIELD ARTAL AS INTEGER
   FIELD PROARTAL AS INTEGER
   FIELD BELOPP AS DECIMAL 
   INDEX ARTAL ARTAL.
DEFINE TEMP-TABLE aarttemp
   FIELD ARBARTKOD AS INTEGER
   FIELD ARBBENAMNING AS CHARACTER
   INDEX ARBARTKOD ARBARTKOD ASCENDING.

{AONRTEMP.I} 
&Scoped-define SHARED SHARED 
{BESTKUNDALLT.I}

/*{AONRDEF.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aonrtemp bestkundallt faktstarttemp ~
faktuppplantemp faktaonrtemp

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR aonrtemp.AONR aonrtemp.DELNR ~
aonrtemp.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR aonrtemp.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR aonrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR aonrtemp
&Scoped-define QUERY-STRING-BRW_AONR FOR EACH aonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH aonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONR aonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR aonrtemp


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


/* Definitions for BROWSE BRW_PLAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PLAN faktstarttemp.PLAN% ~
faktstarttemp.PLANDATUM faktstarttemp.BELOPP faktstarttemp.FAKTURADATUM ~
faktstarttemp.START faktstarttemp.FRITEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PLAN 
&Scoped-define QUERY-STRING-BRW_PLAN FOR EACH faktstarttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PLAN OPEN QUERY BRW_PLAN FOR EACH faktstarttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PLAN faktstarttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PLAN faktstarttemp


/* Definitions for BROWSE BRW_UPPARB                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_UPPARB faktuppplantemp.UPLAN% ~
faktuppplantemp.FAKT% faktuppplantemp.KRITERIUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPPARB faktuppplantemp.UPLAN% ~
faktuppplantemp.FAKT% 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_UPPARB faktuppplantemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_UPPARB faktuppplantemp
&Scoped-define QUERY-STRING-BRW_UPPARB FOR EACH faktuppplantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPPARB OPEN QUERY BRW_UPPARB FOR EACH faktuppplantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPPARB faktuppplantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPPARB faktuppplantemp


/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR faktaonrtemp.AONR ~
faktaonrtemp.DELNR faktaonrtemp.OPRIS faktaonrtemp.ORT faktaonrtemp.STOPP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR faktaonrtemp.AONR ~
faktaonrtemp.OPRIS faktaonrtemp.STOPP 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VAONR faktaonrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VAONR faktaonrtemp
&Scoped-define QUERY-STRING-BRW_VAONR FOR EACH faktaonrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH faktaonrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR faktaonrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR faktaonrtemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BEST FILL-IN-rRUBRIK FILL-IN-FAKTSTART ~
CMB_HANSV CMB_ANSV FILL-IN_NAMN CMB_FAK FILL-IN_PROJEKTKOD RAD_TAK TOG_AONR ~
BTN_OVER BTN_BACK BRW_VAONR FILL-IN_EAONR FILL-IN_DELNR BTN_OK BTN_AVSL ~
BTN_REGL BRW_PLAN BTN_NY BTN_UPP BTN_BORT BTN_DIRFKAT FILL-IN-rRUBRIK-2 ~
RECT-46 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-rRUBRIK FILL-IN-FAKTSTART ~
CMB_HANSV CMB_ANSV FILL-IN_BEST FILL-IN_NAMN FILL-IN_FAKTNR CMB_FAK ~
FILL-IN_TOTPRIS FILL-IN_PROJEKTKOD FILL-IN_SENASTFAK RAD_TAK TOG_AONR ~
FILL-IN_EAONR FILL-IN_DELNR FILL-IN-VAL FILL-IN-rRUBRIK-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ARSP 
     LABEL "Ny plan":L 
     SIZE 12 BY 1 TOOLTIP "Skapa en ny plan för innevarande år".

DEFINE BUTTON BTN_ARSPATER 
     LABEL "Återst. plan":L 
     SIZE 12 BY 1 TOOLTIP "Tar bort senaste året och lägger tillbaka offertpris från föregående år.".

DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta Bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_DIRFKAT AUTO-GO 
     IMAGE-UP FILE "bilder/xbtn_faktu.gif":U
     LABEL "Fakturera":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXTRA 
     LABEL "Extra aonr" 
     SIZE 4.5 BY 1 TOOLTIP "Tryck här för att lägga in aonr till fakturan.".

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_REGL 
     LABEL "Fakt.regler och priser" 
     SIZE 14 BY 1 TOOLTIP "Ändring av faktureringsregler".

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_ANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ARBART AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsart" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(16)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 8
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_HANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Huvudansavarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAKTSTART AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Ta med poster från och med. Välj för varje post!" NO-UNDO.

DEFINE VARIABLE FILL-IN-rRUBRIK AS CHARACTER FORMAT "X(256)":U INITIAL "från och med" 
      VIEW-AS TEXT 
     SIZE 15.5 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-rRUBRIK-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Ta med poster" 
      VIEW-AS TEXT 
     SIZE 15.5 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VAL AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta:" 
      VIEW-AS TEXT 
     SIZE 7.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_BEST AS CHARACTER FORMAT "x(256)" 
     LABEL "BESTÄLLARE" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_FAKTNR AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Fakturaplannr." 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(40)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1.

DEFINE VARIABLE FILL-IN_PROJEKTKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Projektkod" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1.

DEFINE VARIABLE FILL-IN_SENASTFAK AS DATE FORMAT "99/99/99" 
     LABEL "Senast fakturerad" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FILL-IN_TOTPRIS AS DECIMAL FORMAT "->>>>>>>>9.99" INITIAL 0 
     LABEL "Offererat fast pris" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1
     FGCOLOR 9 .

DEFINE VARIABLE RAD_TAK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Vanlig takpris", 1,
"Mot upparb. kostnad", 2,
"Alltid takpris", 3,
"Upparb. löpande", 4
     SIZE 58.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-46
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 58.75 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_AONR AS LOGICAL INITIAL no 
     LABEL "Hämta aonr för fakturering" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.75 BY .92 NO-UNDO.

DEFINE VARIABLE TOG_EGNA AS LOGICAL INITIAL no 
     LABEL "Kundens egna övertidsregler" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.13 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      aonrtemp SCROLLING.

DEFINE QUERY BRW_BEST FOR 
      bestkundallt SCROLLING.

DEFINE QUERY BRW_PLAN FOR 
      faktstarttemp SCROLLING.

DEFINE QUERY BRW_UPPARB FOR 
      faktuppplantemp SCROLLING.

DEFINE QUERY BRW_VAONR FOR 
      faktaonrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR DIALOG-1 _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      aonrtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      aonrtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      aonrtemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(25)":U
  ENABLE
      aonrtemp.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 42.5 BY 10
         TITLE "Aonr utan fakturanummer".

DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST DIALOG-1 _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      bestkundallt.VIBESTID COLUMN-LABEL "bestid" FORMAT "X(256)":U
            WIDTH 15
      bestkundallt.BESTNAMN COLUMN-LABEL "Namn" FORMAT "x(35)":U
  ENABLE
      bestkundallt.VIBESTID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44.5 BY 9.04
         TITLE "Välj BESTÄLLARE".

DEFINE BROWSE BRW_PLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PLAN DIALOG-1 _STRUCTURED
  QUERY BRW_PLAN NO-LOCK DISPLAY
      faktstarttemp.PLAN% COLUMN-LABEL "Andel!i %" FORMAT ">>9":U
      faktstarttemp.PLANDATUM COLUMN-LABEL "Planerat!datum" FORMAT "99/99/99":U
      faktstarttemp.BELOPP COLUMN-LABEL "Belopp" FORMAT ">>>>>>9.99":U
            COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
      faktstarttemp.FAKTURADATUM COLUMN-LABEL "Fakturerat!datum" FORMAT "99/99/99":U
      faktstarttemp.START FORMAT "X(5)":U
      faktstarttemp.FRITEXT COLUMN-LABEL "Fritext" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 58.75 BY 9.04
         TITLE "Faktureringstidpunkter".

DEFINE BROWSE BRW_UPPARB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPPARB DIALOG-1 _STRUCTURED
  QUERY BRW_UPPARB NO-LOCK DISPLAY
      faktuppplantemp.UPLAN% COLUMN-LABEL "Upparbetad %" FORMAT ">>9":U
      faktuppplantemp.FAKT% COLUMN-LABEL "Fakturera %" FORMAT ">>9":U
      faktuppplantemp.KRITERIUM FORMAT "X(14)":U
  ENABLE
      faktuppplantemp.UPLAN%
      faktuppplantemp.FAKT%
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 43.25 BY 9.04
         TITLE "Fakturera efter upparbetadkostnad".

DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR DIALOG-1 _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      faktaonrtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      faktaonrtemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      faktaonrtemp.OPRIS COLUMN-LABEL "Offertpris" FORMAT "->>>>>>>>9":U
            COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
      faktaonrtemp.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(256)":U
            WIDTH 20
      faktaonrtemp.STOPP COLUMN-LABEL "Slutfakturerat" FORMAT "Ja/Nej":U
  ENABLE
      faktaonrtemp.AONR
      faktaonrtemp.OPRIS
      faktaonrtemp.STOPP
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 58.75 BY 10
         TITLE "Valda aonr".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_BEST AT ROW 2.67 COL 54.63
     FILL-IN-rRUBRIK AT ROW 18.67 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     FILL-IN-FAKTSTART AT ROW 19.75 COL 109.88 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     CMB_HANSV AT ROW 6.29 COL 20.38 COLON-ALIGNED
     CMB_ANSV AT ROW 7.46 COL 20.38 COLON-ALIGNED
     FILL-IN_BEST AT ROW 1.5 COL 20.38 COLON-ALIGNED
     FILL-IN_NAMN AT ROW 2.71 COL 20.38 COLON-ALIGNED
     FILL-IN_FAKTNR AT ROW 3.92 COL 20.38 COLON-ALIGNED
     CMB_FAK AT ROW 5.13 COL 20.38 COLON-ALIGNED
     FILL-IN_TOTPRIS AT ROW 9.83 COL 20.38 COLON-ALIGNED
     FILL-IN_PROJEKTKOD AT ROW 11 COL 20.38 COLON-ALIGNED
     FILL-IN_SENASTFAK AT ROW 12.04 COL 20.38 COLON-ALIGNED
     RAD_TAK AT ROW 8.63 COL 3.5 NO-LABEL
     TOG_AONR AT ROW 15 COL 9.88
     BRW_AONR AT ROW 16 COL 1.5
     BTN_OVER AT ROW 17.83 COL 44.38
     BTN_BACK AT ROW 21.33 COL 44.38
     BRW_VAONR AT ROW 16 COL 50.25
     FILL-IN_EAONR AT ROW 26.38 COL 65.13 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR AT ROW 26.38 COL 74 COLON-ALIGNED NO-LABEL
     BTN_OK AT ROW 25.29 COL 111.5
     BTN_AVSL AT ROW 26.42 COL 111.5
     BTN_REGL AT ROW 8 COL 111.5
     BRW_PLAN AT ROW 2.71 COL 50.25
     BRW_UPPARB AT ROW 2.71 COL 61
     TOG_EGNA AT ROW 12.13 COL 2.75
     BTN_NY AT ROW 12.42 COL 62.5
     BTN_UPP AT ROW 12.42 COL 75.13
     BTN_BORT AT ROW 12.42 COL 87.75
     BTN_ARSP AT ROW 14 COL 62.5
     BTN_ARSPATER AT ROW 14 COL 75.13
     BTN_EXTRA AT ROW 24.5 COL 99.5
     FILL-IN-VAL AT ROW 26.38 COL 49.5 COLON-ALIGNED NO-LABEL
     CMB_ARBART AT ROW 13.75 COL 11 COLON-ALIGNED
     BTN_DIRFKAT AT ROW 24.21 COL 111.5 WIDGET-ID 2
     FILL-IN-rRUBRIK-2 AT ROW 17.5 COL 108 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     RECT-46 AT ROW 26.21 COL 50.25
     SPACE(16.88) SKIP(0.11)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fakturasammanställning".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: faktstarttemp T "?" NO-UNDO temp-db faktstarttemp
      TABLE: faktuppplantemp T "?" NO-UNDO temp-db faktuppplantemp
      TABLE: ? T "?" NO-UNDO temp-db aonrtemp
      TABLE: ? T "?" NO-UNDO temp-db faktaonrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_BEST 1 DIALOG-1 */
/* BROWSE-TAB BRW_AONR TOG_AONR DIALOG-1 */
/* BROWSE-TAB BRW_VAONR BTN_BACK DIALOG-1 */
/* BROWSE-TAB BRW_PLAN BTN_REGL DIALOG-1 */
/* BROWSE-TAB BRW_UPPARB BRW_PLAN DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 10000.

ASSIGN 
       BRW_BEST:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_BEST:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

ASSIGN 
       BRW_PLAN:HIDDEN  IN FRAME DIALOG-1                = TRUE.

/* SETTINGS FOR BROWSE BRW_UPPARB IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UPPARB:HIDDEN  IN FRAME DIALOG-1                = TRUE.

ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 10000.

/* SETTINGS FOR BUTTON BTN_ARSP IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ARSP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_ARSPATER IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ARSPATER:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       BTN_BORT:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR BUTTON BTN_EXTRA IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXTRA:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       BTN_NY:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       BTN_REGL:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       BTN_UPP:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARBART IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_ARBART:HIDDEN IN FRAME DIALOG-1           = TRUE.

ASSIGN 
       FILL-IN-rRUBRIK:READ-ONLY IN FRAME DIALOG-1        = TRUE.

ASSIGN 
       FILL-IN-rRUBRIK-2:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VAL IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BEST IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_BEST:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FAKTNR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_SENASTFAK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TOTPRIS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_TOTPRIS:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_EGNA IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_EGNA:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.aonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aonrtemp.AONR
"aonrtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aonrtemp.DELNR
"aonrtemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aonrtemp.ORT
"aonrtemp.ORT" "Ort/Benämning" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestkundallt.VIBESTID
"bestkundallt.VIBESTID" "bestid" "X(256)" "character" ? ? ? ? ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bestkundallt.BESTNAMN
"bestkundallt.BESTNAMN" "Namn" "x(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PLAN
/* Query rebuild information for BROWSE BRW_PLAN
     _TblList          = "Temp-Tables.faktstarttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktstarttemp.PLAN%
"faktstarttemp.PLAN%" "Andel!i %" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.faktstarttemp.PLANDATUM
"faktstarttemp.PLANDATUM" "Planerat!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.faktstarttemp.BELOPP
"faktstarttemp.BELOPP" "Belopp" ">>>>>>9.99" "decimal" ? 9 ? ? 9 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.faktstarttemp.FAKTURADATUM
"faktstarttemp.FAKTURADATUM" "Fakturerat!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.faktstarttemp.START
     _FldNameList[6]   > Temp-Tables.faktstarttemp.FRITEXT
"faktstarttemp.FRITEXT" "Fritext" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PLAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPPARB
/* Query rebuild information for BROWSE BRW_UPPARB
     _TblList          = "Temp-Tables.faktuppplantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktuppplantemp.UPLAN%
"faktuppplantemp.UPLAN%" "Upparbetad %" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.faktuppplantemp.FAKT%
"faktuppplantemp.FAKT%" "Fakturera %" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.faktuppplantemp.KRITERIUM
"faktuppplantemp.KRITERIUM" ? "X(14)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UPPARB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.faktaonrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktaonrtemp.AONR
"AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.faktaonrtemp.DELNR
"DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.faktaonrtemp.OPRIS
"OPRIS" "Offertpris" "->>>>>>>>9" "decimal" ? 9 ? ? 9 ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.faktaonrtemp.ORT
"ORT" "Ort/Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.faktaonrtemp.STOPP
"STOPP" "Slutfakturerat" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAONR */
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
ON END-ERROR OF FRAME DIALOG-1 /* Fakturasammanställning */
DO:  

   {muswait.i}                 
   RUN avb_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      {musarrow.i}
      RETURN NO-APPLY.
   END.      
   musz = TRUE.
   IF VALID-HANDLE(bestapph) THEN DELETE PROCEDURE bestapph.
   {BORTBRWPROC.I}
   RETURN.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON ANY-KEY OF BRW_AONR IN FRAME DIALOG-1 /* Aonr utan fakturanummer */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_AONR IN FRAME DIALOG-1 /* Aonr utan fakturanummer */
DO:
   APPLY "CHOOSE" TO BTN_OVER. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEST
&Scoped-define SELF-NAME BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST DIALOG-1
ON ENTRY OF BRW_BEST IN FRAME DIALOG-1 /* Välj BESTÄLLARE */
DO:
   FILL-IN_BEST = INPUT FILL-IN_BEST.
   
   IF openqkoll[3] = FALSE THEN DO:
      openqkoll[3] = TRUE.
      RUN openbdyn_UI IN brwproc[3] (INPUT "").
   END.
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST 
   NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE bestkundallt THEN DO:
      FIND FIRST bestkundallt USE-INDEX VIBESTID NO-ERROR. 
      FILL-IN_BEST = bestkundallt.BESTNAMN.
      DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.        
   END.
   IF AVAILABLE bestkundallt THEN DO:                                   
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(bestkundallt)).
      RUN lastselectdyn_UI IN brwproc[3]. 
   END.
   ASSIGN
   /*FILL-IN-BTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE */
   BTN_ARSPATER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_ARSP:HIDDEN = TRUE
   BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_REGL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   /*SNATFAKT*/
   IF Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN DO:
      
      ASSIGN 
      BTN_REGL:HIDDEN = FALSE
      BTN_NY:HIDDEN = FALSE 
      BTN_UPP:HIDDEN = FALSE.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST DIALOG-1
ON LEAVE OF BRW_BEST IN FRAME DIALOG-1 /* Välj BESTÄLLARE */
DO:
   /* APPLY "LEAVE" TO FILL-IN_BEST.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_BEST IN FRAME DIALOG-1 /* Välj BESTÄLLARE */
DO:
      status-ok = BRW_BEST:SELECT-FOCUSED-ROW() NO-ERROR.
   
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 81
   soktemp.SOKCHAR[1] = bestkundallt.BESTID.
   {SOKANROP.I}      
   IF soktemp.SOKLOG[1] = TRUE THEN DO:
      MESSAGE "Denna kund är inte riktigt upplaggd och går ej att fakturera!" 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY. 
   END.
   /*SNATFAKT*/
   IF Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN.
   ELSE DO:   
      FIND FIRST faktaonrtemp WHERE faktaonrtemp.FAKTNR = FILL-IN_FAKTNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE faktaonrtemp THEN DO:
         MESSAGE 
         "Det finns " + LC(Guru.Konstanter:gaok) + " på denna faktura dessa kommer att tas bort om du byter "
         LC(Guru.Konstanter:gbestk) ". Vill du fortsätta ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1.   
         CASE val1:
            WHEN TRUE THEN DO:
               musz = musz.                                  
            END.
            WHEN FALSE THEN DO:
               RETURN NO-APPLY.          
            END.
         END CASE.   
      END.       
      FOR EACH faktaonrtemp WHERE faktaonrtemp.FAKTNR = FILL-IN_FAKTNR 
      USE-INDEX AONR:               
         DELETE faktaonrtemp.                       
      END.
   END.   
   {muswait.i}
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAPRISL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
   END.
   ELSE DO:
      RUN FAPRISL.P 
      (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
   END.
   RUN faktrgl_UI.      
   
   EMPTY TEMP-TABLE aonrtemp NO-ERROR.    
   RUN valao_UI.   
   IF TOG_AONR = TRUE THEN RUN aoopen_UI.
   ASSIGN 
   FILL-IN_BEST = bestkundallt.BESTNAMN.
   DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST DIALOG-1
ON VALUE-CHANGED OF BRW_BEST IN FRAME DIALOG-1 /* Välj BESTÄLLARE */
DO:
   /*
   status-ok = BRW_BEST:SELECT-FOCUSED-ROW() NO-ERROR.
   
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 81
   soktemp.SOKCHAR[1] = bestkundallt.BESTID.
   {SOKANROP.I}      
   IF soktemp.SOKLOG[1] = TRUE THEN DO:
      MESSAGE "Denna kund är inte riktigt upplaggd och går ej att fakturera!" 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY. 
   END.
   /*SNATFAKT*/
   IF Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN.
   ELSE DO:   
      FIND FIRST faktaonrtemp WHERE faktaonrtemp.FAKTNR = FILL-IN_FAKTNR USE-INDEX AONR NO-LOCK NO-ERROR.
      IF AVAILABLE faktaonrtemp THEN DO:
         MESSAGE 
         "Det finns " + LC(Guru.Konstanter:gaok) + " på denna faktura dessa kommer att tas bort om du byter "
         LC(Guru.Konstanter:gbestk) ". Vill du fortsätta ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1.   
         CASE val1:
            WHEN TRUE THEN DO:
               musz = musz.                                  
            END.
            WHEN FALSE THEN DO:
               RETURN NO-APPLY.          
            END.
         END CASE.   
      END.       
      FOR EACH faktaonrtemp WHERE faktaonrtemp.FAKTNR = FILL-IN_FAKTNR 
      USE-INDEX AONR:               
         DELETE faktaonrtemp.                       
      END.
   END.   
   {muswait.i}
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAPRISL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
   END.
   ELSE DO:
      RUN FAPRISL.P 
      (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
   END.
   RUN faktrgl_UI.      
   
   EMPTY TEMP-TABLE aonrtemp NO-ERROR.    
   RUN valao_UI.   
   IF TOG_AONR = TRUE THEN RUN aoopen_UI.
   ASSIGN 
   FILL-IN_BEST = bestkundallt.BESTNAMN.
   DISPLAY FILL-IN_BEST WITH FRAME {&FRAME-NAME}.
   */      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PLAN
&Scoped-define SELF-NAME BRW_PLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PLAN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_PLAN IN FRAME DIALOG-1 /* Faktureringstidpunkter */
DO:               
   status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF faktstarttemp.FAKTURERAD = FALSE THEN DO:
      ASSIGN
      BTN_BORT:HIDDEN = FALSE
      BTN_UPP:HIDDEN = FALSE.
      RUN andra_UI.
   END.
   ELSE DO:
      ASSIGN
      BTN_BORT:HIDDEN = TRUE
      BTN_UPP:HIDDEN = TRUE.
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PLAN DIALOG-1
ON VALUE-CHANGED OF BRW_PLAN IN FRAME DIALOG-1 /* Faktureringstidpunkter */
DO:
   status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE faktstarttemp THEN DO:
      IF faktstarttemp.FAKTURERAD = FALSE THEN DO:
         ASSIGN
         BTN_BORT:HIDDEN = FALSE
         BTN_UPP:HIDDEN = FALSE.
      END.
      ELSE DO:
         ASSIGN
         BTN_BORT:HIDDEN = TRUE
         BTN_UPP:HIDDEN = TRUE.
      END.   
   END.
   ELSE DO:
      ASSIGN
      BTN_BORT:HIDDEN = TRUE
      BTN_UPP:HIDDEN = TRUE.
   END.
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   IF faktyptemp.TYP  = 2 THEN DO:
      ASSIGN
      BTN_ARSPATER:HIDDEN = TRUE
      BTN_ARSP:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_UPP:HIDDEN = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UPPARB
&Scoped-define SELF-NAME BRW_UPPARB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPPARB DIALOG-1
ON LEAVE OF BRW_UPPARB IN FRAME DIALOG-1 /* Fakturera efter upparbetadkostnad */
DO:
   /*
   ccc
   IF BRW_UPPARB:VISIBLE = TRUE THEN DO:
      IF AVAILABLE faktuppplantemp THEN DO:
         ASSIGN
         faktuppplantemp.FAKT% = INPUT BROWSE BRW_UPPARB faktuppplantemp.FAKT%           
         faktuppplantemp.UPLAN% = INPUT BROWSE BRW_UPPARB faktuppplantemp.UPLAN%.           
      END.
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPPARB DIALOG-1
ON ROW-LEAVE OF BRW_UPPARB IN FRAME DIALOG-1 /* Fakturera efter upparbetadkostnad */
DO:
   IF AVAILABLE faktuppplantemp THEN DO:
      DISPLAY faktuppplantemp.UPLAN%  faktuppplantemp.FAKT% WITH BROWSE BRW_UPPARB. 
      ASSIGN
      faktuppplantemp.UPLAN% = INPUT BROWSE BRW_UPPARB faktuppplantemp.UPLAN%
      faktuppplantemp.FAKT%  = INPUT BROWSE BRW_UPPARB faktuppplantemp.FAKT%. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPPARB DIALOG-1
ON VALUE-CHANGED OF BRW_UPPARB IN FRAME DIALOG-1 /* Fakturera efter upparbetadkostnad */
DO:
   status-ok = BRW_UPPARB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktuppplantemp.UPLAN%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktuppplantemp.UPLAN% BRW_UPPARB _BROWSE-COLUMN DIALOG-1
ON ENTRY OF faktuppplantemp.UPLAN% IN BROWSE BRW_UPPARB /* Upparbetad % */
DO:
   DISPLAY faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktuppplantemp.UPLAN% BRW_UPPARB _BROWSE-COLUMN DIALOG-1
ON LEAVE OF faktuppplantemp.UPLAN% IN BROWSE BRW_UPPARB /* Upparbetad % */
DO:
   nyupparb = INPUT BROWSE BRW_UPPARB faktuppplantemp.UPLAN%.  
   IF nyupparb > 100 THEN DO:
      MESSAGE "Procentsatsen kan inte vara större än 100."   
      VIEW-AS ALERT-BOX.
      BRW_UPPARB:SELECT-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
      RUN refreshbrw_UI IN brwproc[5].
      DISPLAY faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB.
      RETURN NO-APPLY.
   END.                 
   IF RAD_TAK = 4 THEN DO:
      ASSIGN 
      faktuppplantemp.UPLAN% = INPUT faktuppplantemp.UPLAN%           
      faktuppplantemp.FAKT% = 100 - INPUT faktuppplantemp.UPLAN%.
      DISPLAY faktuppplantemp.UPLAN% faktuppplantemp.FAKT% WITH BROWSE BRW_UPPARB.          
   END.
   ELSE DO:
      FIND FIRST faktuppbuff WHERE faktuppbuff.FAKTNR = faktuppplantemp.FAKTNR AND
      faktuppbuff.UPLAN% = nyupparb AND ROWID(faktuppbuff) NE ROWID(faktuppplantemp) NO-LOCK NO-ERROR.
      IF AVAILABLE faktuppbuff THEN DO:
         MESSAGE "Det finns redan en post med denna procentsats."   
         VIEW-AS ALERT-BOX.
         BRW_UPPARB:SELECT-ROW(1) NO-ERROR.
         RUN refreshbrw_UI IN brwproc[5].
         DISPLAY faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB.
         RETURN NO-APPLY.
      END.               
      IF faktuppplantemp.UPLAN% NE INPUT BROWSE BRW_UPPARB faktuppplantemp.UPLAN% THEN DO:           
         IF faktuppplantemp.KRITERIUM = "SLUT" THEN DO:
            faktuppplantemp.UPLAN% = 100.
            DISPLAY faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB.
            MESSAGE "Det går inte att ändra på denna post då den alltid skall vara 100%."   
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END. 
         ELSE DO: 
            ASSIGN 
            faktuppplantemp.UPLAN% = INPUT faktuppplantemp.UPLAN%.
            IF faktuppplantemp.KRITERIUM = "START" OR faktuppplantemp.KRITERIUM = "SLUT" THEN musz = musz.
            ELSE faktuppplantemp.KRITERIUM = "UPPARBETA " + STRING(INPUT faktuppplantemp.UPLAN%,">>9") + "%". 
            DISPLAY faktuppplantemp.UPLAN% faktuppplantemp.KRITERIUM WITH BROWSE BRW_UPPARB.    
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktuppplantemp.FAKT%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktuppplantemp.FAKT% BRW_UPPARB _BROWSE-COLUMN DIALOG-1
ON ENTRY OF faktuppplantemp.FAKT% IN BROWSE BRW_UPPARB /* Fakturera % */
DO:
   DISPLAY faktuppplantemp.FAKT% WITH BROWSE BRW_UPPARB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktuppplantemp.FAKT% BRW_UPPARB _BROWSE-COLUMN DIALOG-1
ON LEAVE OF faktuppplantemp.FAKT% IN BROWSE BRW_UPPARB /* Fakturera % */
DO:
   nyupparb = INPUT BROWSE BRW_UPPARB faktuppplantemp.FAKT%.
   IF nyupparb > 100 THEN DO:
      MESSAGE "Procentsatsen kan inte vara större än 100."   
      VIEW-AS ALERT-BOX.
      BRW_UPPARB:SELECT-ROW(1) IN FRAME {&FRAME-NAME} NO-ERROR.
      RUN refreshbrw_UI IN brwproc[5].
      DISPLAY faktuppplantemp.FAKT% WITH BROWSE BRW_UPPARB.
      RETURN NO-APPLY.
   END.

   IF faktuppplantemp.FAKT% NE INPUT BROWSE BRW_UPPARB faktuppplantemp.FAKT% THEN DO:
      ASSIGN                                
      faktuppplantemp.FAKT% = INPUT faktuppplantemp.FAKT%.
      DISPLAY faktuppplantemp.UPLAN% faktuppplantemp.FAKT% WITH BROWSE BRW_UPPARB.
   END.
   IF RAD_TAK = 4 THEN DO:
      ASSIGN                                
      faktuppplantemp.FAKT% = INPUT faktuppplantemp.FAKT%.      
      faktuppplantemp.UPLAN% = 100 - INPUT faktuppplantemp.FAKT%.      
      DISPLAY faktuppplantemp.UPLAN% faktuppplantemp.FAKT% WITH BROWSE BRW_UPPARB.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAONR
&Scoped-define SELF-NAME BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR DIALOG-1
ON ANY-KEY OF BRW_VAONR IN FRAME DIALOG-1 /* Valda aonr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_BACK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR DIALOG-1
ON VALUE-CHANGED OF BRW_VAONR IN FRAME DIALOG-1 /* Valda aonr */
DO:
   status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() NO-ERROR.
   FIND FIRST faktkolltemp WHERE faktkolltemp.AONR = faktaonrtemp.AONR AND faktkolltemp.DELNR = faktaonrtemp.DELNR NO-LOCK NO-ERROR.
   IF AVAILABLE faktkolltemp THEN DO:
      FILL-IN-FAKTSTART:SCREEN-VALUE = STRING(faktkolltemp.SENASTTID).
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktaonrtemp.OPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.OPRIS BRW_VAONR _BROWSE-COLUMN DIALOG-1
ON ENTRY OF faktaonrtemp.OPRIS IN BROWSE BRW_VAONR /* Offertpris */
DO:
   DISPLAY faktaonrtemp.OPRIS WITH BROWSE BRW_VAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.OPRIS BRW_VAONR _BROWSE-COLUMN DIALOG-1
ON LEAVE OF faktaonrtemp.OPRIS IN BROWSE BRW_VAONR /* Offertpris */
DO:
   RUN koll_UI (OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   IF faktyptemp.TYP = 1 OR faktyptemp.TYP = 3 OR faktyptemp.TYP = 2 THEN DO:
      IF faktaonrtemp.OPRIS NE INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS THEN DO:
         RUN prisoff_UI (INPUT INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS, INPUT FALSE).         
      END.   
      APPLY "LEAVE" TO FILL-IN_TOTPRIS IN FRAME {&FRAME-NAME}.
   END.  
   ELSE IF faktyptemp.TYP = 5 THEN DO:
      IF faktaonrtemp.OPRIS NE INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS THEN DO:         
         IF vfaktplantemp.SENASTFAK NE ? THEN DO:       
            IF BRW_UPPARB:VISIBLE = TRUE THEN DO:  
               RUN fupparbkoll_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT faktaonrtemp.AONR,INPUT faktaonrtemp.DELNR,OUTPUT musz).
               IF musz = TRUE THEN DO:                  
                  musz = FALSE.
                  IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN musz = musz.
                  ELSE DO:
                     DISPLAY faktaonrtemp.OPRIS WITH BROWSE BRW_VAONR.
                     MESSAGE "Denna post går ej att ändra." VIEW-AS ALERT-BOX. 
                     RETURN NO-APPLY.                 
                  END.                  
               END.               
            END.                     
         END.   
         FILL-IN_TOTPRIS = FILL-IN_TOTPRIS - faktaonrtemp.OPRIS.
         faktaonrtemp.OPRIS = INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS.     
         FILL-IN_TOTPRIS = FILL-IN_TOTPRIS + faktaonrtemp.OPRIS.             
         IF FILL-IN_TOTPRIS < 0 THEN FILL-IN_TOTPRIS = 0.        
         DISPLAY FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.
      END.
   END.
   ELSE faktaonrtemp.OPRIS = INPUT BROWSE BRW_VAONR faktaonrtemp.OPRIS.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktaonrtemp.STOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.STOPP BRW_VAONR _BROWSE-COLUMN DIALOG-1
ON ENTRY OF faktaonrtemp.STOPP IN BROWSE BRW_VAONR /* Slutfakturerat */
DO:
    DISPLAY faktaonrtemp.STOPP WITH BROWSE BRW_VAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.STOPP BRW_VAONR _BROWSE-COLUMN DIALOG-1
ON LEAVE OF faktaonrtemp.STOPP IN BROWSE BRW_VAONR /* Slutfakturerat */
DO:
   RUN koll_UI (OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   faktaonrtemp.STOPP = INPUT BROWSE BRW_VAONR faktaonrtemp.STOPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktaonrtemp.STOPP BRW_VAONR _BROWSE-COLUMN DIALOG-1
ON MOUSE-SELECT-CLICK OF faktaonrtemp.STOPP IN BROWSE BRW_VAONR /* Slutfakturerat */
DO:
   IF faktaonrtemp.STOPP = TRUE THEN faktaonrtemp.STOPP = FALSE.
   ELSE IF faktaonrtemp.STOPP = FALSE THEN faktaonrtemp.STOPP = TRUE. 
   DISPLAY faktaonrtemp.STOPP WITH BROWSE BRW_VAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ARSP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ARSP DIALOG-1
ON CHOOSE OF BTN_ARSP IN FRAME DIALOG-1 /* Ny plan */
DO:
   FIND FIRST faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND YEAR(faktstarttemp.PLANDATUM) = YEAR(TODAY) 
   NO-LOCK NO-ERROR.
   IF AVAILABLE faktstarttemp THEN musz = TRUE.
   IF musz = FALSE THEN DO:
      FIND FIRST faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND faktstarttemp.FAKTURERAD = FALSE AND 
      faktstarttemp.START = ""
      NO-LOCK NO-ERROR.
      IF AVAILABLE faktstarttemp THEN musz = TRUE.   
   END.
   IF musz = FALSE THEN DO:
      FIND FIRST faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND 
      faktstarttemp.START = "START"
      NO-LOCK NO-ERROR.
      IF AVAILABLE faktstarttemp THEN DO:
         IF faktstarttemp.BELOPP NE 0 THEN musz = TRUE.   
      END.
   END.
   IF musz = FALSE THEN DO:
      FIND FIRST faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND 
      faktstarttemp.START = "SLUT"
      NO-LOCK NO-ERROR.
      IF AVAILABLE faktstarttemp THEN DO:
         IF faktstarttemp.BELOPP NE 0 THEN musz = TRUE.   
      END.
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      MESSAGE "Det finns redan poster detta år eller är föregående år ej slut fakturerat." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   RUN spartfaktstart_UI IN fakthmth (INPUT TABLE faktstarttemp).
   
   RUN FASTARPLAN.W (INPUT-OUTPUT TABLE faktaonrtemp,OUTPUT nyprisvar).
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      
   END.
   ELSE DO:  
      RUN fovrighmt_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT bestkundallt.BESTID,INPUT FALSE,
                                    OUTPUT TABLE faktstarttemp,OUTPUT TABLE faktuppplantemp). 
      RUN openbdyn_UI IN brwproc[4] (INPUT "").
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      DISPLAY  FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.      
      FILL-IN_TOTPRIS = nyprisvar.

      DISPLAY FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ARSPATER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ARSPATER DIALOG-1
ON CHOOSE OF BTN_ARSPATER IN FRAME DIALOG-1 /* Återst. plan */
DO:

   FIND FIRST faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND YEAR(faktstarttemp.PLANDATUM) = YEAR(TODAY) AND
   faktstarttemp.FAKTURERAD = TRUE
   NO-LOCK NO-ERROR.
   IF AVAILABLE faktstarttemp THEN musz = TRUE.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      MESSAGE "Det finns poster som är fakturerade detta år och då går det ej att återställa planen." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {muswait.i}
   IF Guru.Konstanter:appcon THEN DO:
      RUN PLANARFAST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 2,INPUT 0,INPUT-OUTPUT FILL-IN_TOTPRIS,INPUT-OUTPUT TABLE faktaonrtemp).
   END.
   ELSE DO:
      RUN PLANARFAST.P
      (INPUT 2,INPUT 0,INPUT-OUTPUT FILL-IN_TOTPRIS,INPUT-OUTPUT TABLE faktaonrtemp).
   END.
   FOR EACH faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND YEAR(faktstarttemp.PLANDATUM) = YEAR(TODAY):
      IF faktstarttemp.START = "START" OR faktstarttemp.START = "SLUT" THEN musz = musz.
      ELSE DELETE faktstarttemp.     
   END.
   RUN openbdyn_UI IN brwproc[4] (INPUT "").
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   DISPLAY  FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:               
   {muswait.i}                 
   RUN avb_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      {musarrow.i}
      RETURN NO-APPLY.
   END.   
   
   musz = TRUE.
   IF VALID-HANDLE(bestapph) THEN DELETE PROCEDURE bestapph.
   {BORTBRWPROC.I}
   APPLY "GO" TO BTN_AVSL IN FRAME {&FRAME-NAME}.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK DIALOG-1
ON CHOOSE OF BTN_BACK IN FRAME DIALOG-1
DO:  
   RUN koll_UI (OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.

           /*
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:
      MESSAGE "Fakturaplanen är fakturerad bortag ej möjlig!"
      VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.
                    */
   /*OM MAN TAR BORT AONR FRÅN FAKTPLAN TABORT FAKTUPPARB SOM EJ ÄR FAKTURERADE*/
   antal_valda = BRW_VAONR:NUM-SELECTED-ROWS.
   antal_raknare = 1.   
   musz = FALSE.
   DO WHILE antal_raknare LE antal_valda:                                      
      status-ok = BRW_VAONR:FETCH-SELECTED-ROW(antal_raknare).                                  
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(faktaonrtemp)).              
      /*
      IF faktyptemp.TYP = 2 THEN DO:
         FIND FIRST FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.AONR = faktaonrtemp.AONR AND FAKTAVTALAONR.DELNR = faktaonrtemp.DELNR AND
         FAKTAVTALAONR.BELOPP NE 0 NO-LOCK NO-ERROR.
         IF AVAILABLE FAKTAVTALAONR THEN DO:
            MESSAGE "Du kan inte ta bort detta " + LC(Guru.Konstanter:gaok) + " då den finns med i planer och beloppet är större än 0"
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
         END.
      END.
      */
      RUN bortaonr_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT faktyptemp.TYP,INPUT faktaonrtemp.AONR,INPUT faktaonrtemp.DELNR,OUTPUT musz).
      IF musz = TRUE THEN DO:
         MESSAGE faktaonrtemp.AONR faktaonrtemp.DELNR "Går ej att ta bort. " Guru.Konstanter:gaok " är redan fakturerad!" 
         VIEW-AS ALERT-BOX.

      END.
      ELSE DO:                 
         IF faktyptemp.TYP = 5 THEN DO:         
            FILL-IN_TOTPRIS = FILL-IN_TOTPRIS - faktaonrtemp.OPRIS.
            IF FILL-IN_TOTPRIS < 0 THEN FILL-IN_TOTPRIS = 0.
            DISPLAY  FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.
         END.
         IF faktyptemp.TYP = 1 OR faktyptemp.TYP = 3 OR faktyptemp.TYP = 2 THEN DO:
            FILL-IN_TOTPRIS = FILL-IN_TOTPRIS - faktaonrtemp.OPRIS.
            IF FILL-IN_TOTPRIS < 0 THEN FILL-IN_TOTPRIS = 0.
            DISPLAY FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.
            FILL-IN_TOTPRIS = ?.
            APPLY "LEAVE" TO FILL-IN_TOTPRIS.
         END.
         IF TOG_AONR = TRUE THEN DO:
            FIND FIRST aonrtemp WHERE aonrtemp.AONR = faktaonrtemp.AONR AND aonrtemp.DELNR = faktaonrtemp.DELNR
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE aonrtemp THEN DO:
               CREATE aonrtemp.
               ASSIGN     
               aonrtemp.AONR        = faktaonrtemp.AONR        
               aonrtemp.DELNR       = faktaonrtemp.DELNR       
               aonrtemp.OMRADE      = faktaonrtemp.OMRADE      
               aonrtemp.ORT         = faktaonrtemp.ORT         
               aonrtemp.AONRAVDATUM = faktaonrtemp.AONRAVDATUM 
               aonrtemp.STARTDATUM  = faktaonrtemp.STARTDATUM  
               aonrtemp.ELVOMRKOD   = faktaonrtemp.GDATUM      
               aonrtemp.PLANOFFERT  = faktaonrtemp.OPRIS.       
            END.
         END.
         DELETE faktaonrtemp.               
      END.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&RIGHT-BROWSE}].
      antal_raknare = antal_raknare + 1.   
      /*
      IF faktyptemp.TYP = 2 THEN DO:
         FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.AONR = faktaonrtemp.AONR AND FAKTAVTALAONR.DELNR = faktaonrtemp.DELNR:
            DELETE FAKTAVTALAONR.
         END.
      END.      
      */
      
   END.
   
   RUN valao_UI.
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   
   IF TOG_AONR = TRUE THEN RUN aoopen_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-1
ON CHOOSE OF BTN_BORT IN FRAME DIALOG-1 /* Ta Bort */
DO:
   IF BRW_UPPARB:VISIBLE = TRUE THEN RUN bortupparb_UI.
   ELSE RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DIRFKAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DIRFKAT DIALOG-1
ON CHOOSE OF BTN_DIRFKAT IN FRAME DIALOG-1 /* Fakturera */
DO: 
   outfakt = TRUE.
   APPLY "CHOOSE" TO BTN_OK.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXTRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXTRA DIALOG-1
ON CHOOSE OF BTN_EXTRA IN FRAME DIALOG-1 /* Extra aonr */
DO:
   {muswait.i}
   ASSIGN
   FILL-IN_EAONR = INPUT FILL-IN_EAONR
   FILL-IN_DELNR = INPUT FILL-IN_DELNR.
   IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
      IF FILL-IN_DELNR NE 0 THEN DO:
         MESSAGE "Företaget använder sig ej av delnr i faktura rutinen."
         VIEW-AS ALERT-BOX.         
         RETURN NO-APPLY.
      END.
   END.
   FIND FIRST faktaonrtemp WHERE faktaonrtemp.FAKTNR = FILL-IN_FAKTNR AND
   faktaonrtemp.AONR = FILL-IN_EAONR AND faktaonrtemp.DELNR = FILL-IN_DELNR 
   NO-LOCK NO-ERROR.
   IF AVAILABLE faktaonrtemp THEN DO:
      MESSAGE Guru.Konstanter:gaok + " är redan upplagt på denna faktura!" 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:
      IF BRW_UPPARB:VISIBLE = TRUE THEN DO:         
         MESSAGE "Obs! Du lägger in " + LC(Guru.Konstanter:gaok) + " på en pågående faktura!" 
         skip
         "Det går inte på denna typ av faktura."         
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      MESSAGE "Obs! Du lägger in " + LC(Guru.Konstanter:gaok) + " på en pågående faktura!" 
      skip
      skip
      "lägger du upp ett " + LC(Guru.Konstanter:gaok) + " för mycket måste du trycka på avbryt"
      skip
      "och göra om inläggning på samtliga nyinlagda " + LC(Guru.Konstanter:gaok) + "."  
      VIEW-AS ALERT-BOX.         
   END.
   
   RUN tilleaonr_UI IN fakthmth (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globniv,INPUT vfaktplantemp.FAKTNR,
                                 INPUT faktyptemp.TYP,
                                 INPUT bestkundallt.BESTID,INPUT FILL-IN_EAONR,INPUT FILL-IN_DELNR,
                                 INPUT TABLE omrtemp,
                                 OUTPUT musz, OUTPUT svarvar).
   IF musz = TRUE THEN DO:
      MESSAGE svarvar VIEW-AS ALERT-BOX.        
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   IF musz = ? THEN DO:
      MESSAGE svarvar
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av koppling"
      UPDATE answer AS LOGICAL. 
      musz = FALSE.
      IF answer THEN DO:        
         RUN bortkop_UI IN fakthmth (INPUT FILL-IN_EAONR,INPUT FILL-IN_DELNR).         
      END.
      ELSE RETURN NO-APPLY.
   END.
   FIND FIRST aonrtemp WHERE aonrtemp.AONR = FILL-IN_EAONR AND
   aonrtemp.DELNR = FILL-IN_DELNR USE-INDEX AONR NO-ERROR.
   IF AVAILABLE aonrtemp THEN DELETE aonrtemp.
   IF TOG_AONR = TRUE THEN DO:
       BRW_AONR:SELECT-ROW(1) NO-ERROR.
       RUN refreshbrw_UI IN brwproc[1].
   END.
   RUN addaonr_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT FILL-IN_EAONR,INPUT FILL-IN_DELNR, 
                               OUTPUT TABLE faktaonrtemp APPEND).
   
   FIND FIRST faktaonrtemp WHERE faktaonrtemp.AONR = FILL-IN_EAONR AND
   faktaonrtemp.DELNR = FILL-IN_DELNR NO-ERROR.
   IF FILL-IN_NAMN = "" THEN FILL-IN_NAMN = faktaonrtemp.ORT.                          
   RUN prisoff_UI (INPUT faktaonrtemp.OPRIS,INPUT TRUE).
   RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(faktaonrtemp)).              
   RUN valao_UI.
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   /*SNATFAKT*/
   IF BRW_BEST:VISIBLE = TRUE AND Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN DO:
      RUN nybest_UI (INPUT 1).
   END.
   ELSE IF BRW_UPPARB:VISIBLE = TRUE THEN DO:
      RUN nyupparb_UI.
      APPLY "ENTRY" TO faktuppplantemp.UPLAN% IN BROWSE BRW_UPPARB.
   END.
   ELSE RUN ny_UI.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO: 
   RUN klar_UI.
   {musarrow.i}
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.   
   ELSE DO:
      IF VALID-HANDLE(bestapph) THEN DELETE PROCEDURE bestapph.
      {BORTBRWPROC.I}
      RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER DIALOG-1
ON CHOOSE OF BTN_OVER IN FRAME DIALOG-1
DO:
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:
      MESSAGE "Obs! Du lägger in " + LC(Guru.Konstanter:gaok) + " på en pågående faktura!" 
      skip
      skip
      "Lägger du upp ett " + LC(Guru.Konstanter:gaok) + " för mycket måste du trycka på avbryt"
      skip
      "och göra om inläggning på samtliga nyinlagda " + LC(Guru.Konstanter:gaok) + "." 
      VIEW-AS ALERT-BOX.                  
   END.
   
   antal_valda = BRW_AONR:NUM-SELECTED-ROWS.
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_AONR:FETCH-SELECTED-ROW(antal_raknare).  
      RUN nyaonr_UI.                      
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(faktaonrtemp)).              
      antal_raknare = antal_raknare + 1.   
   END.       
   BRW_AONR:SELECT-ROW(1) NO-ERROR.
   RUN refreshbrw_UI IN brwproc[1].
   RUN dispao_UI.  
   RUN valao_UI.
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   IF TOG_AONR = TRUE THEN DO:
      BRW_AONR:SELECT-ROW(1) NO-ERROR.
      RUN refreshbrw_UI IN brwproc[1].
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REGL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REGL DIALOG-1
ON CHOOSE OF BTN_REGL IN FRAME DIALOG-1 /* Fakt.regler och priser */
DO:  
    
   IF vfaktplantemp.FDELNR NE 0 THEN DO:
      MESSAGE "Preliminärfaktura är skapad. Inga ändrigar kan göras!" SKIP
         "Ta bort preliminärfakturan först!" VIEW-AS ALERT-BOX.      
   END.
   ELSE DO: 
      RUN FREGPRIS.W (INPUT vfaktplantemp.FAKTNR,INPUT faktyptemp.TYP).       
      TOG_EGNA = vfaktplantemp.EGNAOVR.     
      RUN goma_UI.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP DIALOG-1
ON CHOOSE OF BTN_UPP IN FRAME DIALOG-1 /* Ändra */
DO:
   /*SNATFAKT*/
   IF BRW_BEST:VISIBLE = TRUE AND Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN DO:
      RUN nybest_UI (INPUT 2).
   END.
   ELSE IF BRW_UPPARB:VISIBLE = TRUE THEN DO:
      RUN setcolindex_UI IN brwproc[5] (INPUT sortvar).
      RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).      
   END.
   ELSE RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ANSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ANSV DIALOG-1
ON VALUE-CHANGED OF CMB_ANSV IN FRAME DIALOG-1 /* Ansvarig */
DO:
   CMB_ANSV = INPUT CMB_ANSV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FAK DIALOG-1
ON VALUE-CHANGED OF CMB_FAK IN FRAME DIALOG-1 /* Fakturakategori */
DO:
   CMB_FAK = INPUT CMB_FAK.
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   vfaktplantemp.FAKTTYP = faktyptemp.FAKTTYP.     
   RUN sortvar_UI.
   RUN goma_UI.
   EMPTY TEMP-TABLE aonrtemp NO-ERROR.    
   IF TOG_AONR = TRUE THEN RUN aoopen_UI.   
   IF faktyptemp.TYP = 3 THEN DO:
      ASSIGN
      BTN_ARSPATER:HIDDEN = TRUE
      BTN_ARSP:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_UPP:HIDDEN = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_HANSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_HANSV DIALOG-1
ON VALUE-CHANGED OF CMB_HANSV IN FRAME DIALOG-1 /* Huvudansavarig */
DO:
  CMB_HANSV = INPUT CMB_HANSV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FAKTSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FAKTSTART DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-FAKTSTART IN FRAME DIALOG-1
DO:
   Guru.GlobalaVariabler:regdatum = DATE(FILL-IN-FAKTSTART:SCREEN-VALUE).
   IF Guru.GlobalaVariabler:regdatum = ? THEN Guru.GlobalaVariabler:regdatum = TODAY + 1.
   RUN AlmanBtn.w.
   FILL-IN-FAKTSTART:SCREEN-VALUE = STRING(Guru.GlobalaVariabler:regdatum).
   APPLY "VALUE-CHANGED" TO FILL-IN-FAKTSTART. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FAKTSTART DIALOG-1
ON VALUE-CHANGED OF FILL-IN-FAKTSTART IN FRAME DIALOG-1
DO:

   FIND FIRST faktkolltemp WHERE faktkolltemp.AONR = faktaonrtemp.AONR AND faktkolltemp.DELNR = faktaonrtemp.DELNR NO-LOCK NO-ERROR.
   IF AVAILABLE faktkolltemp THEN DO:
      faktkolltemp.SENASTTID = DATE(FILL-IN-FAKTSTART:SCREEN-VALUE).
     
   END.  
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEST DIALOG-1
ON ENTRY OF FILL-IN_BEST IN FRAME DIALOG-1 /* BESTÄLLARE */
DO:
   FILL-IN_BEST = INPUT FILL-IN_BEST.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEST DIALOG-1
ON LEAVE OF FILL-IN_BEST IN FRAME DIALOG-1 /* BESTÄLLARE */
DO:
   ASSIGN FILL-IN_BEST = INPUT FILL-IN_BEST.
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST 
   NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE bestkundallt THEN DO:                                   
      MESSAGE FILL-IN_BEST " finns ej!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {musarrow.i}                
     
   MESSAGE 
   "Du har valt en ny " LC(Guru.Konstanter:gbestk) " till fakturan"
   SKIP                                
   bestkundallt.BESTNAMN 
   SKIP
   "Är det rätt?"            
   SKIP                
   "Blir det ändå fel gå på Knappen 'Avbryt' och börja om från början."
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valbest AS LOGICAL.   
   CASE valbest:
      WHEN TRUE THEN DO:          
         IF Guru.Konstanter:appcon THEN DO:
            RUN FAPRISL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
         END.
         ELSE DO:
            RUN FAPRISL.P 
            (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
         END.         
         RUN faktrgl_UI.      
         RUN goma_UI.
         RUN sortvar_UI.
         RUN fovrighmt_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT bestkundallt.BESTID,INPUT TRUE,OUTPUT TABLE faktstarttemp,OUTPUT TABLE faktuppplantemp). 
         RUN setcolindex_UI IN brwproc[5] (INPUT sortvar).                                                                                                       
         RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).     
         RUN openbdyn_UI IN brwproc[4] (INPUT "").   
      END.
      WHEN FALSE THEN DO:               
         APPLY  "ENTRY" TO BRW_BEST.                               
      END.
   END CASE.                      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR DIALOG-1
ON ANY-KEY OF FILL-IN_DELNR IN FRAME DIALOG-1
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR DIALOG-1
ON ENTRY OF FILL-IN_DELNR IN FRAME DIALOG-1
DO:
   /*BTN_EXTRA:DEFAULT = TRUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR DIALOG-1
ON LEAVE OF FILL-IN_DELNR IN FRAME DIALOG-1
DO:
    /*
   BTN_EXTRA:DEFAULT = FALSE. 
   BTN_VISA:DEFAULT = TRUE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DELNR IN FRAME DIALOG-1
DO:
   APPLY "CHOOSE" TO BTN_EXTRA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR DIALOG-1
ON ANY-KEY OF FILL-IN_EAONR IN FRAME DIALOG-1 /* Aonr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EAONR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR DIALOG-1
ON ENTRY OF FILL-IN_EAONR IN FRAME DIALOG-1 /* Aonr */
DO:
    /*
    BTN_EXTRA:DEFAULT = TRUE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR DIALOG-1
ON LEAVE OF FILL-IN_EAONR IN FRAME DIALOG-1 /* Aonr */
DO:
    /*
   BTN_EXTRA:DEFAULT = FALSE.
   BTN_VISA:DEFAULT = TRUE.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EAONR IN FRAME DIALOG-1 /* Aonr */
DO:
   APPLY "CHOOSE" TO BTN_EXTRA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_NAMN DIALOG-1
ON LEAVE OF FILL-IN_NAMN IN FRAME DIALOG-1 /* Namn */
DO:
   FILL-IN_NAMN = INPUT FILL-IN_NAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TOTPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TOTPRIS DIALOG-1
ON LEAVE OF FILL-IN_TOTPRIS IN FRAME DIALOG-1 /* Offererat fast pris */
DO:
   IF FILL-IN_TOTPRIS NE INPUT FILL-IN_TOTPRIS THEN DO:
      FILL-IN_TOTPRIS = INPUT FILL-IN_TOTPRIS.
      DO TRANSACTION:
         GET FIRST BRW_PLAN EXCLUSIVE-LOCK.
         DO WHILE AVAILABLE(faktstarttemp):
            IF faktstarttemp.FAKTURERAD = FALSE THEN DO:
               ASSIGN
               faktstarttemp.BELOPP = FILL-IN_TOTPRIS * faktstarttemp.PLAN% / 100.         
            END.
            GET NEXT BRW_PLAN EXCLUSIVE-LOCK.         
         END.
         BRW_PLAN:SELECT-ROW(1) NO-ERROR.
         RUN refreshbrw_UI IN brwproc[4].
      END.      
      
      /*AVTAL
      IF faktyptemp.TYP = 2 THEN DO:
         
         GET FIRST BRW_PLAN NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTART):
            IF FAKTSTART.FAKTURERAD = TRUE THEN DO:
               IF FAKTSTART.START NE "" THEN DO:
                  OPEN QUERY faktavtq FOR EACH FAKTAVTALAONR WHERE 
                  FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
                  FAKTAVTALAONR.START = FAKTSTART.START 
                  NO-LOCK.
               END.
               ELSE DO:
                  OPEN QUERY faktavtq FOR EACH FAKTAVTALAONR WHERE 
                  FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
                  FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM 
                  NO-LOCK.
               END.
               DO TRANSACTION:
                  GET FIRST faktavtq EXCLUSIVE-LOCK.
                  IF AVAILABLE FAKTAVTALAONR THEN DO: 
                     ASSIGN
                     FAKTAVTALAONR.BELOPP = FILL-IN_TOTPRIS * (FAKTSTART.PLAN% / 100) * (FAKTAVTALAONR.PLAN% / 100).         
                  END.
               END.
               REPEAT:
                  DO TRANSACTION:
                     GET NEXT faktavtq EXCLUSIVE-LOCK.
                     IF AVAILABLE FAKTAVTALAONR THEN DO: 
                        ASSIGN
                        FAKTAVTALAONR.BELOPP = FILL-IN_TOTPRIS * (FAKTSTART.PLAN% / 100) * (FAKTAVTALAONR.PLAN% / 100).         
                     END.
                     ELSE LEAVE.
                  END.
               END.
            END.
            GET NEXT BRW_PLAN NO-LOCK.            
         END.   
         BRW_PLAN:SELECT-ROW(1) NO-ERROR.
         status-ok = BRW_PLAN:REFRESH() IN FRAME {&FRAME-NAME} NO-ERROR.
      END.   
      */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_TAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_TAK DIALOG-1
ON VALUE-CHANGED OF RAD_TAK IN FRAME DIALOG-1
DO:
   RAD_TAK = INPUT RAD_TAK. 
   
   RUN sortvar_UI.
   RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(faktuppplantemp)).                 
   RUN setcolindex_UI IN brwproc[5] (INPUT sortvar).
   RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).     
   RUN goma_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AONR DIALOG-1
ON VALUE-CHANGED OF TOG_AONR IN FRAME DIALOG-1 /* Hämta aonr för fakturering */
DO:
   TOG_AONR = INPUT TOG_AONR.
   CMB_ARBART = INPUT CMB_ARBART.
   IF TOG_AONR = TRUE THEN DO: 
      RUN aoopen_UI.
      BRW_VAONR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      EMPTY TEMP-TABLE aonrtemp NO-ERROR.       
      BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EGNA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EGNA DIALOG-1
ON VALUE-CHANGED OF TOG_EGNA IN FRAME DIALOG-1 /* Kundens egna övertidsregler */
DO:
  TOG_EGNA = INPUT TOG_EGNA.
  RUN goma_UI.
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
   {muswait.i}   
   {ALLSTARTDYN.I}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 5,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 5,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FOR EACH anvandartemp:
      status-ok = CMB_ANSV:ADD-LAST(anvandartemp.ANVANDARE).      
      status-ok = CMB_HANSV:ADD-LAST(anvandartemp.ANVANDARE).      
   END.
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-LOCK NO-ERROR.
  
   ASSIGN
   CMB_ARBART:LABEL = Guru.Konstanter:gartk
   bestkundallt.VIBESTID:LABEL IN BROWSE BRW_BEST = Guru.Konstanter:gbestk
   BRW_BEST:TITLE = "Välj " + LC(Guru.Konstanter:gbestk)
   FILL-IN_BEST:LABEL = Guru.Konstanter:gbestk
   BRW_AONR:TITLE = Guru.Konstanter:gaol + " utan fakturanummer"
   aonrtemp.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok
   BRW_VAONR:TITLE = "Valda " + LC(Guru.Konstanter:gaol) 
   faktaonrtemp.AONR:LABEL IN BROWSE BRW_VAONR = Guru.Konstanter:gaok
   BTN_EXTRA:LABEL = "Extra " + LC(Guru.Konstanter:gaok) 
   FILL-IN_EAONR:LABEL =  Guru.Konstanter:gaok
   TOG_AONR:LABEL = "Hämta " + LC(Guru.Konstanter:gaok) + " för fakturering".
   &Scoped-define FORMATNAMN aonrtemp.AONR
   &Scoped-define BROWSE-NAME BRW_AONR
   {AOFORMAT1.I}
   &Scoped-define FORMATNAMN faktaonrtemp.AONR
   &Scoped-define BROWSE-NAME BRW_VAONR
   {AOFORMAT1.I}
   &Scoped-define FORMATNAMN FILL-IN_EAONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN_DELNR   
   {DELNRFORMAT.I}
   
   IF vfaktplantemp.FAKTTYPUNDER NE 3 THEN DO:
      status-ok = RAD_TAK:DELETE("Alltid takpris").              
   END.
   
   ASSIGN   
   /*TOG_ALLTAK = FAKTPLAN.ALLTTAK*/
   TOG_EGNA = vfaktplantemp.EGNAOVR
   FILL-IN_TOTPRIS = vfaktplantemp.OFFERTPRIS
   CMB_ANSV:SCREEN-VALUE = vfaktplantemp.ANVANDARE
   CMB_HANSV:SCREEN-VALUE = vfaktplantemp.HANVANDARE
   CMB_ANSV = vfaktplantemp.ANVANDARE
   CMB_HANSV = vfaktplantemp.HANVANDARE
      /*
      FILL-IN_ANVANDARE = vfaktplantemp.ANVANDARE
      */
   FILL-IN_FAKTNR = vfaktplantemp.FAKTNR   
   FILL-IN_NAMN = vfaktplantemp.NAMN
   FILL-IN_SENASTFAK = vfaktplantemp.SENASTFAK
   FILL-IN_PROJEKTKOD = vfaktplantemp.PROJEKTKOD.  
   FIND FIRST bestkundallt WHERE bestkundallt.BESTID = vfaktplantemp.BESTID 
   NO-LOCK NO-ERROR.   
   IF vfaktplantemp.FAKTTYP = "Takprisfakt." THEN DO:
      IF vfaktplantemp.FAKTTYPUNDER = 0 THEN DO:
         IF vfaktplantemp.ALLTTAK = TRUE THEN vfaktplantemp.FAKTTYPUNDER = 3.
         ELSE vfaktplantemp.FAKTTYPUNDER = 1.
      END.
      RAD_TAK = vfaktplantemp.FAKTTYPUNDER. 
   END.
   RUN sortvar_UI.      
   IF AVAILABLE bestkundallt THEN DO:
      ASSIGN  
      FILL-IN_BEST = bestkundallt.BESTNAMN. 
      DISABLE FILL-IN_BEST WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:                                                    
      IF vfaktplantemp.BESTID NE "" THEN DO:
         MESSAGE "Denna " Guru.Konstanter:gbestk " är inte riktigt upplaggd och går ej att fakturera!" 
         VIEW-AS ALERT-BOX.                  
         LEAVE MAIN-BLOCK. 
      END.
      FIND FIRST bestkundallt NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE bestkundallt THEN LEAVE MAIN-BLOCK.      
      ENABLE FILL-IN_BEST WITH FRAME {&FRAME-NAME}.        
      nyfakt = TRUE.
   END.          
   IF vfaktplantemp.BESTID NE "" THEN DO:
      IF ingang = 3 THEN DO:
         ENABLE RAD_TAK WITH FRAME {&FRAME-NAME}.
         RUN fovrighmt_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT bestkundallt.BESTID,INPUT TRUE,OUTPUT TABLE faktstarttemp,OUTPUT TABLE faktuppplantemp). 
         /*RUN openbdyn_UI IN brwproc[4] (INPUT ""). */
      END.
      ELSE RUN fovrighmt_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT bestkundallt.BESTID,INPUT FALSE,OUTPUT TABLE faktstarttemp,OUTPUT TABLE faktuppplantemp). 
   END.
   RUN faktrgl_UI.
   DEBUGGER:SET-BREAK().

   RUN faohmt_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,OUTPUT TABLE faktaonrtemp).   
   FOR EACH faktyptemp WHERE faktyptemp.ORDNING > 0:
      status-ok = CMB_FAK:ADD-LAST(faktyptemp.VIFAKTTYP).      
   END.  
   FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = vfaktplantemp.FAKTTYP NO-ERROR.
   CMB_FAK = faktyptemp.VIFAKTTYP.    
   /*beövs detta ????*/
   IF nyfakt = FALSE THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN FAPRISL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT FALSE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
      END.
      ELSE DO:
         RUN FAPRISL.P 
         (INPUT FALSE, INPUT vfaktplantemp.FAKTNR, INPUT bestkundallt.BESTID).
      END. 
   END.
   
   RUN enable_UI.       
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ARBARTS.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE aarttemp).
   END.
   ELSE DO:
      RUN ARBARTS.P 
      (OUTPUT TABLE aarttemp).                  
   END.
   status-ok = CMB_ARBART:ADD-FIRST("Alla").
   FOR EACH aarttemp USE-INDEX ARBARTKOD NO-LOCK:
      status-ok = CMB_ARBART:ADD-LAST(aarttemp.ARBBENAMNING).
   END.
   CMB_ARBART = "ALLA".
   FIND FIRST aarttemp NO-ERROR.
   IF AVAILABLE aarttemp THEN DO:   
      CMB_ARBART:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
      CMB_ARBART:SCREEN-VALUE = "Alla".
      ENABLE CMB_ARBART WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
       CMB_ARBART:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   {FRMSIZED.I}    
   RUN valao_UI. 
   RUN goma_UI.      
   IF vfaktplantemp.SENASTFAK NE ? THEN DO: 
      faktuppplantemp.FAKT%:READ-ONLY IN BROWSE BRW_UPPARB = TRUE. 
      faktuppplantemp.UPLAN%:READ-ONLY IN BROWSE BRW_UPPARB = TRUE.
  
   END.
   IF faktyptemp.TYP = 3 THEN DO:
      ASSIGN
      BTN_ARSPATER:HIDDEN = TRUE
      BTN_ARSP:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE
      BTN_BORT:HIDDEN = TRUE
      BTN_UPP:HIDDEN = TRUE.
   END.
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:       
      IF BRW_UPPARB:VISIBLE = TRUE THEN DO:
         /*faktaonrtemp.OPRIS:READ-ONLY IN BROWSE BRW_VAONR = TRUE.          */
      END.
   END.
   IF vfaktplantemp.SENASTFAK NE ? THEN DISABLE FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.   
   {musarrow.i}
   {DIA_M_SLUT.I}
   IF BRW_VAONR:VISIBLE = TRUE THEN DO:
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
      RUN getfirst_UI IN brwproc[{&RIGHT-BROWSE}].
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}]. 
   END.   
   IF ingang > 1 THEN RUN altstart_UI.     
   IF ingang = 3 THEN DO:
      IF faktyptemp.TYP = 5 THEN ENABLE RAD_TAK WITH FRAME {&FRAME-NAME}.         
      ELSE RAD_TAK:HIDDEN = TRUE.
   END.
   IF nyfakt = TRUE THEN  DO:
      BRW_BEST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      APPLY "ENTRY" TO BRW_BEST.     
   END.
   /*SNATFAKT*/
   
   IF Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN DO:
      ASSIGN
      BRW_BEST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NY:HIDDEN = FALSE 
      BTN_UPP:HIDDEN = FALSE.
      APPLY "ENTRY" TO BRW_BEST. 
   END.    
   FILL-IN_PROJEKTKOD       :HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "ELPA" THEN FILL-IN_PROJEKTKOD       :HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   RUN koll_UI (OUTPUT musz).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      faktaonrtemp.OPRIS:READ-ONLY IN BROWSE BRW_VAONR = TRUE.
      faktaonrtemp.STOPP:READ-ONLY IN BROWSE BRW_VAONR = TRUE.
      BTN_DIRFKAT:HIDDEN = TRUE.
      BTN_OK:HIDDEN = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "SNAT" AND vfaktplantemp.BESTID = "1" THEN DO:
      RUN faktkollhmt_UI IN faktupphmth (INPUT infakplannr,OUTPUT TABLE faktkolltemp).
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-rRUBRIK:HIDDEN = TRUE 
      FILL-IN-FAKTSTART:HIDDEN = TRUE  
      FILL-IN-rRUBRIK-2:HIDDEN = TRUE.  
   END.      
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
   ASSIGN
   bestkundallt.VIBESTID:READ-ONLY IN BROWSE BRW_BEST = TRUE
   aonrtemp.AONR:READ-ONLY IN BROWSE BRW_AONR = TRUE    
   faktaonrtemp.AONR:READ-ONLY IN BROWSE  BRW_VAONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VAONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_BEST:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_PLAN:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_UPPARB:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setcolindex_UI IN brwproc[4] (INPUT "ORDNING").
   IF Guru.Konstanter:appcon THEN DO:
      RUN BESTKUNDAPP.P PERSISTENT SET bestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BESTKUNDAPP.P PERSISTENT SET bestapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKTUPPHMT.P PERSISTENT SET faktupphmth ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FAKTUPPHMT.P PERSISTENT SET faktupphmth.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE altstart_UI DIALOG-1 
PROCEDURE altstart_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
/*vartifran = 1 HUVUDMENY   vartifran = 2 AOMENY  ÄNDRA vartifran = 3 AOMENY  NY 
vartifran = 4 AOMENY KOPPLING BORT 
vartifran = 5 AOMENY KOPPLA*/   
   IF ingang = 3 THEN DO:
      ASSIGN
      FILL-IN_EAONR = faonr 
      FILL-IN_DELNR = fdelnr.
      DISPLA FILL-IN_EAONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}. 
      APPLY "CHOOSE" TO BTN_EXTRA.
      ASSIGN
      FILL-IN_EAONR = "" 
      FILL-IN_DELNR = 0.
      DISPLAY FILL-IN_EAONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}. 
   END.
   IF ingang = 4 THEN DO:
      FIND FIRST faktaonrtemp WHERE faktaonrtemp.AONR = faonr AND faktaonrtemp.DELNR = fdelnr 
      NO-LOCK NO-ERROR.           
      BRW_VAONR:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION BRW_VAONR TO RECID (RECID(faktaonrtemp)) NO-ERROR.
      status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() NO-ERROR.
   END.
   IF ingang = 5 THEN DO:
      ASSIGN
      FILL-IN_EAONR = faonr 
      FILL-IN_DELNR = fdelnr.
      DISPLAY FILL-IN_EAONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}. 
      APPLY "CHOOSE" TO BTN_EXTRA.
      FIND FIRST faktaonrtemp WHERE faktaonrtemp.AONR = faonr AND faktaonrtemp.DELNR = fdelnr 
      NO-LOCK NO-ERROR.           
      BRW_VAONR:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION BRW_VAONR TO RECID (RECID(faktaonrtemp)) NO-ERROR.
      status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() NO-ERROR.
      ASSIGN
      FILL-IN_EAONR = "" 
      FILL-IN_DELNR = 0.
      DISPLAY FILL-IN_EAONR FILL-IN_DELNR WITH FRAME {&FRAME-NAME}. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI DIALOG-1 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF faktstarttemp.VFAKTNR NE 0 THEN DO:
      MESSAGE "Du kan inte ändra denna post! Den är redan fakturerad."   
      VIEW-AS ALERT-BOX TITLE "Ändring av betalningsplan".
   END.
   EMPTY TEMP-TABLE efaktstarttemp  NO-ERROR.    
   CREATE efaktstarttemp.    
   BUFFER-COPY faktstarttemp TO efaktstarttemp.
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   vfaktplantemp.FAKTTYP = faktyptemp.FAKTTYP.     
      
   RUN avtalao_UI.
   RUN NYFPLANPROA.W (INPUT vfaktplantemp.FAKTNR,INPUT FILL-IN_TOTPRIS,INPUT-OUTPUT TABLE efaktstarttemp).
   FIND FIRST efaktstarttemp NO-LOCK NO-ERROR.
   IF musz = FALSE THEN DO:     
      BUFFER-COPY efaktstarttemp TO faktstarttemp.
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(faktstarttemp)).              
      RUN openbdyn_UI IN brwproc[4] (INPUT "").   
      RUN lastselectdyn_UI IN brwproc[4].        
   END.
   IF AVAILABLE efaktstarttemp THEN DELETE efaktstarttemp.
   ASSIGN
   musz = FALSE.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aoopen_UI DIALOG-1 
PROCEDURE aoopen_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   DEFINE VARIABLE arbartvar AS INTEGER NO-UNDO.
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.   
   IF CMB_ARBART = "Alla" THEN arbartvar = 0.
   ELSE DO:
      FIND FIRST aarttemp WHERE aarttemp.ARBBENAMNING = CMB_ARBART USE-INDEX ARBARTKOD NO-LOCK NO-ERROR.
      IF AVAILABLE aarttemp THEN DO:           
         arbartvar = aarttemp.ARBARTKOD.
      END.
   END.
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAAOOPEN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globniv, INPUT Guru.Konstanter:globforetag, INPUT bestkundallt.BESTID, INPUT faktyptemp.FAKTTYP, INPUT CMB_ANSV,
      INPUT arbartvar, INPUT FILL-IN_FAKTNR, INPUT-OUTPUT TABLE aonrtemp,OUTPUT musz).
   END.
   ELSE DO:
      RUN FAAOOPEN.P 
      (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globniv, INPUT Guru.Konstanter:globforetag, INPUT bestkundallt.BESTID, INPUT faktyptemp.FAKTTYP, INPUT CMB_ANSV,
      INPUT arbartvar, INPUT FILL-IN_FAKTNR,INPUT-OUTPUT TABLE aonrtemp,OUTPUT musz).   
   END.  
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END. 
   ELSE DO TRANSACTION:
      FOR EACH aonrtemp:                             
         FIND FIRST faktaonrtemp WHERE faktaonrtemp.FAKTNR = FILL-IN_FAKTNR AND
         faktaonrtemp.AONR = aonrtemp.AONR AND faktaonrtemp.DELNR = aonrtemp.DELNR 
         NO-LOCK NO-ERROR.
         IF AVAILABLE faktaonrtemp THEN DELETE aonrtemp.
      END.           
      FIND FIRST aonrtemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE aonrtemp THEN BRW_AONR:HIDDEN = TRUE.
      ELSE DO:
         RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
         ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
         BRW_AONR:HIDDEN = FALSE.
      END.
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI DIALOG-1 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE startslut% AS DECIMAL NO-UNDO.
   DEFINE VARIABLE startslutbelopp AS DECIMAL NO-UNDO.
   ASSIGN       
   FILL-IN_TOTPRIS = INPUT FRAME {&FRAME-NAME} FILL-IN_TOTPRIS
   RAD_TAK = INPUT FRAME {&FRAME-NAME} RAD_TAK
   FILL-IN_BEST = INPUT FRAME {&FRAME-NAME} FILL-IN_BEST
   FILL-IN_PROJEKTKOD = INPUT FILL-IN_PROJEKTKOD.
   CMB_FAK = INPUT FRAME {&FRAME-NAME} CMB_FAK.
   EMPTY TEMP-TABLE artemp NO-ERROR. 
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   IF faktyptemp.TYP = 3 OR faktyptemp.TYP = 1 OR faktyptemp.TYP = 2 THEN DO:
      ASSIGN
      belopp% = 0
      tot% = 0.
      GET FIRST BRW_PLAN NO-LOCK.
      DO WHILE AVAILABLE(faktstarttemp): 
         IF faktstarttemp.PLANDATUM = ? THEN DO:
            ASSIGN 
            startslut% = startslut% + faktstarttemp.PLAN%.
            startslutbelopp = startslutbelopp + faktstarttemp.BELOPP.
         END.
         ELSE DO:
            FIND FIRST artemp WHERE artemp.ARTAL = YEAR(faktstarttemp.PLANDATUM) NO-ERROR.
            IF NOT AVAILABLE artemp THEN CREATE artemp.
            ASSIGN
            artemp.ARTAL = YEAR(faktstarttemp.PLANDATUM)
            artemp.BELOPP = artemp.BELOPP + faktstarttemp.BELOPP      
            artemp.PROARTAL = artemp.PROARTAL + faktstarttemp.PLAN%.
         END.
         
         ASSIGN 
         tot% = tot% + faktstarttemp.PLAN% 
         belopp% = belopp% + faktstarttemp.BELOPP.
         GET NEXT BRW_PLAN NO-LOCK.
      END.
      IF belopp% NE FILL-IN_TOTPRIS THEN DO:
         musz = TRUE.
         IF faktyptemp.TYP = 1 THEN DO:                                    
            musz = FALSE.
            FIND LAST faktstarttemp WHERE faktstarttemp.FAKTNR = vfaktplantemp.FAKTNR AND faktstarttemp.START = ""
            NO-LOCK NO-ERROR.
            IF AVAILABLE faktstarttemp THEN DO:
               FIND FIRST artemp WHERE artemp.ARTAL = YEAR(faktstarttemp.PLANDATUM) NO-ERROR.
               IF FILL-IN_TOTPRIS NE artemp.BELOPP + startslutbelopp THEN DO:
                  musz = FALSE.
                  IF artemp.PROARTAL + startslut% > 100 THEN DO:
                     MESSAGE "Summan av betalningsplanen blir inte 100% "
                     VIEW-AS ALERT-BOX.      
                     musz = TRUE.
                     RETURN.
                  END.
                 /*FLERA ÅR FINNS KOLLEN GÄLLER EJ*/
               END.
               ELSE DO:
                  /*FLERA ÅR FINNS DET SKALL EJ BLI LIKA*/
               END.
            END.
            ELSE musz = TRUE.
         END.
         IF musz = TRUE THEN DO:
            MESSAGE "Summan av betalningsplanen blir inte 100% " SKIP
            "Summan blir " belopp% " den borde bli " FILL-IN_TOTPRIS
            VIEW-AS ALERT-BOX.      
            musz = TRUE.
            RETURN. 
         END.
      END. 
      /*årsplaner*/
      IF faktyptemp.TYP = 1 THEN DO:                                    
         FIND FIRST faktaonrtemp WHERE faktaonrtemp.FAKTNR = vfaktplantemp.FAKTNR AND      
         faktaonrtemp.EXTRAPRIS NE 0 NO-LOCK NO-ERROR.
         IF AVAILABLE faktaonrtemp THEN DO:
            FOR EACH faktaonrtemp BREAK BY faktaonrtemp.FAKTNR.
               ACCUMULATE faktaonrtemp.OPRIS (TOTAL BY faktaonrtemp.FAKTNR).
            END.
            IF (ACCUM TOTAL faktaonrtemp.OPRIS) NE FILL-IN_TOTPRIS THEN DO:
               MESSAGE "Summan av ingående "  STRING(Guru.Konstanter:gaok) (ACCUM TOTAL faktaonrtemp.OPRIS ) SKIP
               "Den borde bli " FILL-IN_TOTPRIS
               VIEW-AS ALERT-BOX.      
               musz = TRUE.
               RETURN. 
            END.        
         END.
      END.
   END.       
      
   IF faktyptemp.TYP = 5 AND RAD_TAK = 4 THEN DO:
      RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).
      GET FIRST BRW_UPPARB NO-LOCK.
      DO WHILE AVAILABLE(faktuppplantemp):
         IF faktuppplantemp.UPLAN% + faktuppplantemp.FAKT% NE 100 THEN DO:         
            MESSAGE "Procenttalen skall bli 100!" 
            VIEW-AS ALERT-BOX.      
            musz = TRUE.
            RETURN.
         END.
         GET NEXT BRW_UPPARB NO-LOCK.
      END.
   END.  
   IF faktyptemp.TYP = 5 AND RAD_TAK = 2 THEN DO:
      ASSIGN
      faktprocentvar = 0
      uppprocentvar = -1.
      RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).
      GET FIRST BRW_UPPARB NO-LOCK.
      DO WHILE AVAILABLE(faktuppplantemp):
         ASSIGN
         faktprocentvar = faktprocentvar + faktuppplantemp.FAKT%.
         IF uppprocentvar >= faktuppplantemp.UPLAN% THEN DO:
            GET FIRST BRW_UPPARB NO-LOCK.            
            DISPLAY faktuppplantemp.FAKT% faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB. 
            MESSAGE "Procenttalen skall vara ökande!" 
            VIEW-AS ALERT-BOX.      
            musz = TRUE.
            RETURN.
         END.
         uppprocentvar = faktuppplantemp.UPLAN%.
         GET NEXT BRW_UPPARB NO-LOCK.
      END. 
      GET LAST BRW_UPPARB NO-LOCK.
      IF faktuppplantemp.UPLAN% NE 100 THEN DO:
         GET FIRST BRW_UPPARB NO-LOCK.
         DISPLAY faktuppplantemp.FAKT% faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB.
         MESSAGE "Sista posten skall vara 100%!" 
         VIEW-AS ALERT-BOX.      
         musz = TRUE.
         RETURN.
      END.
      IF faktprocentvar NE 100 THEN DO:
         GET FIRST BRW_UPPARB NO-LOCK.
         DISPLAY faktuppplantemp.FAKT% faktuppplantemp.UPLAN% WITH BROWSE BRW_UPPARB.
         MESSAGE "Summan av betalningsplanen blir inte 100%!" 
         VIEW-AS ALERT-BOX.      
         musz = TRUE.
         RETURN.
      END.   
   END.
   RUN avb_UI IN fakthmth (INPUT infakplannr).
   /*AVTAL
   OPEN QUERY faktavtq
   FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR 
   NO-LOCK.
   DO TRANSACTION:     
      GET FIRST faktavtq EXCLUSIVE-LOCK.      
      DO WHILE AVAILABLE(FAKTAVTALAONR):
         FIND FIRST FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAONR.AONR = FAKTAVTALAONR.AONR AND
         FAKTAONR.DELNR = FAKTAVTALAONR.DELNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FAKTAONR THEN DELETE FAKTAVTALAONR.                  
         GET NEXT faktavtq EXCLUSIVE-LOCK.               
      END.
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avtalao_UI DIALOG-1 
PROCEDURE avtalao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RETURN.
   /*
   {muswait.i}
   DEFINE VARIABLE aoraknare AS INTEGER NO-UNDO.
   IF faktyptemp.TYP NE 2 THEN RETURN.
   aoraknare = 0.
   GET FIRST BRW_VAONR NO-LOCK.
   DO WHILE AVAILABLE(faktaonrtemp):
      aoraknare = aoraknare + 1.
      GET NEXT BRW_VAONR NO-LOCK.
   END.   
   GET FIRST BRW_VAONR NO-LOCK.   
   DO WHILE AVAILABLE(faktaonrtemp):
      IF FAKTSTART.START NE "" THEN DO:
         FIND FIRST FAKTAVTALAONR WHERE 
         FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.START = FAKTSTART.START NO-LOCK NO-ERROR.
         IF AVAILABLE FAKTAVTALAONR THEN aoraknare = 0.
         FIND FIRST FAKTAVTALAONR WHERE 
         FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.AONR = faktaonrtemp.AONR AND 
         FAKTAVTALAONR.DELNR = faktaonrtemp.DELNR AND 
         FAKTAVTALAONR.START = FAKTSTART.START NO-LOCK NO-ERROR.                      
      END.
      ELSE DO:            
         FIND FIRST FAKTAVTALAONR WHERE 
         FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM 
         NO-LOCK NO-ERROR.      
         IF AVAILABLE FAKTAVTALAONR THEN aoraknare = 0.
         FIND FIRST FAKTAVTALAONR WHERE 
         FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.AONR = faktaonrtemp.AONR AND 
         FAKTAVTALAONR.DELNR = faktaonrtemp.DELNR AND 
         FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM 
         NO-LOCK NO-ERROR.                      
      END.            
      IF NOT AVAILABLE FAKTAVTALAONR THEN DO TRANSACTION:
         CREATE FAKTAVTALAONR.
         ASSIGN
         FAKTAVTALAONR.AONR = faktaonrtemp.AONR
         FAKTAVTALAONR.BELOPP = 0
         FAKTAVTALAONR.DELNR = faktaonrtemp.DELNR 
         FAKTAVTALAONR.FAKTNR = faktaonrtemp.FAKTNR
         FAKTAVTALAONR.FDELNR = 0
         FAKTAVTALAONR.PLAN% = 0 
         FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM
         FAKTAVTALAONR.START = FAKTSTART.START
         FAKTAVTALAONR.VFAKTNR = 0.           
         IF aoraknare > 0 THEN DO:
            FAKTAVTALAONR.BELOPP = FAKTSTART.BELOPP / aoraknare.
            IF FAKTSTART.BELOPP NE 0 THEN DO:
               FAKTAVTALAONR.PLAN% = 
               100 * ((FAKTSTART.BELOPP / aoraknare) / FAKTSTART.BELOPP). 
            END.
         END.
      END.   
      GET NEXT BRW_VAONR NO-LOCK.
   END.   
   {musarrow.i}
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bortupparb_UI DIALOG-1 
PROCEDURE bortupparb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_UPPARB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:
      MESSAGE "Du kan inte ta bort några poster då fakturering har redan har skett på denna fakturaplan."   
      VIEW-AS ALERT-BOX TITLE "Bortag av betalningsplan".
      RETURN.
   END.
   IF faktuppplantemp.KRITERIUM = "START" OR faktuppplantemp.KRITERIUM = "SLUT" THEN DO:
      MESSAGE "Du kan inte ta bort denna post! Sätt fakturera procent = 0."   
      VIEW-AS ALERT-BOX TITLE "Bortag av betalningsplan".
      RETURN.
   END.     
   MESSAGE "Vill du verkligen ta bort denna post ? "   
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av betalningsplan"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:        
      EMPTY TEMP-TABLE efaktuppplantemp NO-ERROR.       
      CREATE efaktuppplantemp.
      BUFFER-COPY faktuppplantemp TO efaktuppplantemp.
      RUN bortupparb_UI IN fakthmth (INPUT TABLE efaktuppplantemp).      
      DELETE faktuppplantemp.      
      RUN selnextprevrow_UI IN brwproc[5].
      RUN refreshbrw_UI IN brwproc[5].
      RUN lastselectdyn_UI IN brwproc[5].              
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI DIALOG-1 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF faktstarttemp.VFAKTNR NE 0 THEN DO:
      MESSAGE "Du kan inte ta bort denna post! Den är redan fakturerad."   
      VIEW-AS ALERT-BOX TITLE "Bortag av betalningsplan".
      RETURN.
   END.
   IF faktstarttemp.START NE "" THEN DO:
      MESSAGE "Du kan inte ta bort denna post! Sätt % eller belopp lika med noll."   
      VIEW-AS ALERT-BOX TITLE "Bortag av betalningsplan".
      RETURN.
   END.
   MESSAGE "Vill du verkligen ta bort detta betalningsdatum ? "   
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av betalningsplan"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:        
      EMPTY TEMP-TABLE efaktstarttemp NO-ERROR.       
      CREATE efaktstarttemp.    
      BUFFER-COPY faktstarttemp TO efaktstarttemp.
      RUN bortfast_UI IN fakthmth (INPUT TABLE efaktstarttemp).      
      DELETE faktstarttemp.      
      RUN selnextprevrow_UI IN brwproc[4].
      RUN refreshbrw_UI IN brwproc[4].
      RUN lastselectdyn_UI IN brwproc[4].              
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dispao_UI DIALOG-1 
PROCEDURE dispao_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
  
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
  DISPLAY FILL-IN-rRUBRIK FILL-IN-FAKTSTART CMB_HANSV CMB_ANSV FILL-IN_BEST 
          FILL-IN_NAMN FILL-IN_FAKTNR CMB_FAK FILL-IN_TOTPRIS FILL-IN_PROJEKTKOD 
          FILL-IN_SENASTFAK RAD_TAK TOG_AONR FILL-IN_EAONR FILL-IN_DELNR 
          FILL-IN-VAL FILL-IN-rRUBRIK-2 
      WITH FRAME DIALOG-1.
  ENABLE BRW_BEST FILL-IN-rRUBRIK FILL-IN-FAKTSTART CMB_HANSV CMB_ANSV 
         FILL-IN_NAMN CMB_FAK FILL-IN_PROJEKTKOD RAD_TAK TOG_AONR BTN_OVER 
         BTN_BACK BRW_VAONR FILL-IN_EAONR FILL-IN_DELNR BTN_OK BTN_AVSL 
         BTN_REGL BRW_PLAN BTN_NY BTN_UPP BTN_BORT BTN_DIRFKAT 
         FILL-IN-rRUBRIK-2 RECT-46 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktrgl_UI DIALOG-1 
PROCEDURE faktrgl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}   
   RUN faktrgl_UI IN fakthmth (INPUT vfaktplantemp.FAKTNR,INPUT bestkundallt.BESTID,OUTPUT TOG_EGNA).
   RUN setcolindex_UI IN brwproc[5] (INPUT sortvar).
   RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).
   RUN openbdyn_UI IN brwproc[4] (INPUT "").
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI DIALOG-1 
PROCEDURE goma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   BTN_UPP:LABEL IN FRAME {&FRAME-NAME} = "Ändra".   
   IF faktyptemp.TYP = 1 THEN DO:

      ASSIGN
      BTN_ARSPATER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ARSP:HIDDEN = FALSE.
      ENABLE BTN_ARSP BTN_ARSPATER WITH FRAME {&FRAME-NAME}.
   END.
   IF faktyptemp.TYP = 3 THEN DO:
      ASSIGN
      FILL-IN_TOTPRIS = 0
      RAD_TAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_TOTPRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*BTN_EGNA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE         
      BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE */
      BTN_REGL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*FILL-IN-BTEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE*/.       
      APPLY "VALUE-CHANGED" TO BRW_PLAN.
   END.
   ELSE IF faktyptemp.TYP = 5 THEN DO:
      RAD_TAK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ASSIGN
      BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_TOTPRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_TOTPRIS:LABEL = "Takpris"
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_ARSPATER:HIDDEN = TRUE
      BTN_ARSP:HIDDEN = TRUE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      /*
      BTN_EGNA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE         
      BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      */
      BTN_REGL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*FILL-IN-BTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE*/.       
      
      IF RAD_TAK = 2 THEN DO:
         ASSIGN
         BTN_UPP:LABEL IN FRAME {&FRAME-NAME} = "Sortera"
         BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ENABLE BRW_UPPARB WITH FRAME {&FRAME-NAME}.
      END.
      IF RAD_TAK = 4 THEN DO:
         ASSIGN
         BTN_UPP:LABEL IN FRAME {&FRAME-NAME} = "Sortera"
         BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_ARSPATER:HIDDEN = TRUE
         BTN_ARSP:HIDDEN = TRUE
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         ENABLE BRW_UPPARB WITH FRAME {&FRAME-NAME}.
      END.
      IF vfaktplantemp.SENASTFAK NE ? THEN DO:
         DISABLE RAD_TAK FILL-IN_TOTPRIS
         WITH FRAME {&FRAME-NAME}.      
      END.
   END.
   ELSE IF faktyptemp.TYP = 1 OR  faktyptemp.TYP = 2 THEN DO:
      ASSIGN
      RAD_TAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_TOTPRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*
      BTN_EGNA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE         
      BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      */
      BTN_REGL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*FILL-IN-BTEXT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE*/.       
      IF faktyptemp.TYP = 1 THEN DO:
         ASSIGN
         BTN_ARSPATER:HIDDEN = FALSE
         BTN_ARSP:HIDDEN = FALSE.
      END.
      APPLY "VALUE-CHANGED" TO BRW_PLAN.
   END.
   ELSE IF faktyptemp.TYP = 4 OR faktyptemp.TYP = 8 THEN DO:
      ASSIGN
      RAD_TAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_TOTPRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_ARSPATER:HIDDEN = TRUE
      BTN_ARSP:HIDDEN = TRUE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      /*
      BTN_EGNA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE         
      BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      */
      BTN_REGL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*FILL-IN-BTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE*/.       
   END.
   ELSE IF faktyptemp.TYP = 6 OR faktyptemp.TYP = 7 THEN DO:
      ASSIGN
      RAD_TAK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_UPPARB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN_TOTPRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_NY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_ARSPATER:HIDDEN = TRUE
      BTN_ARSP:HIDDEN = TRUE
      BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      /*
      BTN_EGNA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE         
      BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      */
      BTN_REGL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /*FILL-IN-BTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE*/.       
   END.    
     /*SNATFAKT*/
   IF vfaktplantemp.BESTID = "1" THEN DO: 
      ASSIGN 
      BTN_NY:HIDDEN = TRUE
      BTN_UPP:HIDDEN = TRUE.
   END.   
   BRW_BEST:HIDDEN = TRUE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI DIALOG-1 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   {muswait.i} 
   ASSIGN  
   CMB_ANSV =  INPUT FRAME {&FRAME-NAME} CMB_ANSV 
   CMB_HANSV = INPUT FRAME {&FRAME-NAME} CMB_HANSV
   FILL-IN_TOTPRIS = INPUT FRAME {&FRAME-NAME} FILL-IN_TOTPRIS
   RAD_TAK = INPUT FRAME {&FRAME-NAME} RAD_TAK
   FILL-IN_BEST = INPUT FRAME {&FRAME-NAME} FILL-IN_BEST
   FILL-IN_PROJEKTKOD = INPUT FILL-IN_PROJEKTKOD.
   CMB_FAK = INPUT FRAME {&FRAME-NAME} CMB_FAK.
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   RUN avb_UI.
   IF musz = TRUE THEN DO:
      musz = TRUE.
      RETURN.
   END.    
   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = FILL-IN_BEST 
   NO-LOCK NO-ERROR.      
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   ASSIGN   
   vfaktplantemp.ANVANDARE  =  CMB_ANSV 
   vfaktplantemp.HANVANDARE =  CMB_HANSV
   vfaktplantemp.FAKTTYP = faktyptemp.FAKTTYP
   vfaktplantemp.FAKTTYPUNDER = RAD_TAK
   vfaktplantemp.ALLTTAK = FALSE 
   vfaktplantemp.NAMN = FILL-IN_NAMN
   vfaktplantemp.BESTID = bestkundallt.BESTID
   vfaktplantemp.EGNAOVR = TOG_EGNA           
   vfaktplantemp.VISAONR = ""
   vfaktplantemp.OFFERTPRIS = FILL-IN_TOTPRIS
   vfaktplantemp.TAKPRIS = FILL-IN_TOTPRIS
   vfaktplantemp.PROJEKTKOD = FILL-IN_PROJEKTKOD.
   IF RAD_TAK = 3 THEN DO:
      vfaktplantemp.ALLTTAK = TRUE.
   END.
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   GET FIRST BRW_VAONR NO-LOCK.           
   IF AVAILABLE faktaonrtemp THEN DO:       
      ASSIGN
      vfaktplantemp.VISAONR = faktaonrtemp.AONR + STRING(faktaonrtemp.DELNR,Guru.Konstanter:varforetypchar[1])
      vfaktplantemp.OMRADE = faktaonrtemp.OMRADE.
      GET NEXT BRW_VAONR NO-LOCK.
      IF AVAILABLE faktaonrtemp THEN vfaktplantemp.VISAONR = "Sammfakt.".    
   END. 
    
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:
      IF faktyptemp.TYP = 8 THEN DO:
         FOR EACH faktaonrtemp WHERE faktaonrtemp.NY = TRUE:
            MESSAGE 
            "Du lägger upp flera" LC(Guru.Konstanter:gaok) " på faktura som är på börjad!"
            SKIP                                
            "All tidskrivning och alla kostnadsregistreringar kommer att tas med för"
            SKIP
            faktaonrtemp.AONR STRING(faktaonrtemp.DELNR,Guru.Konstanter:varforetypchar[1])             
            VIEW-AS ALERT-BOX.
         END.
      END.
      ELSE DO:   
         FOR EACH faktaonrtemp WHERE faktaonrtemp.NY = TRUE:     
             MESSAGE 
             "Du lägger upp flera" LC(Guru.Konstanter:gaok) " på faktura som är påbörjad!"
             SKIP                                
             "Vill du att all tidskrivning och alla kostnadsregistreringar skall tas med för" 
             SKIP
             faktaonrtemp.AONR STRING(faktaonrtemp.DELNR,Guru.Konstanter:varforetypchar[1]) " ? Svara Ja"            
             SKIP                
             "Vill du att tidskrivning och kostnadsregistreringar skall tas med från senaste faktureringen?"                    
             SKIP
             " Svara Nej"
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vallja AS LOGICAL.   
             CASE vallja:
                WHEN TRUE THEN DO:          
                   faktaonrtemp.ALLT = TRUE.                          
                END.
                WHEN FALSE THEN DO:               
                   faktaonrtemp.ALLT = FALSE.                      
                END.
             END CASE.           
         END.
      END.
   END.
   EMPTY TEMP-TABLE evfaktplantemp NO-ERROR.    
   CREATE evfaktplantemp.
   BUFFER-COPY vfaktplantemp TO evfaktplantemp.
   
   RUN klar_UI IN fakthmth (INPUT TABLE evfaktplantemp,INPUT TABLE faktaonrtemp,
                            INPUT TABLE faktstarttemp,INPUT TABLE faktuppplantemp,INPUT TABLE faktkolltemp,
                            OUTPUT svarvar,OUTPUT musz).
   IF musz = TRUE THEN DO:
      MESSAGE svarvar VIEW-AS ALERT-BOX.
      RETURN.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koll_UI DIALOG-1 
PROCEDURE koll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER kollvar AS LOGICAL NO-UNDO.
   IF vfaktplantemp.FDELNR NE 0 THEN DO:
      MESSAGE "Preliminärfaktura är skapad. Inga ändrigar kan göras!" SKIP
         "Ta bort preliminärfakturan först!" VIEW-AS ALERT-BOX. 
      kollvar = TRUE.
      RETURN.
   END. 
   ELSE kollvar = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyaonr_UI DIALOG-1 
PROCEDURE nyaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF FILL-IN_NAMN = "" THEN FILL-IN_NAMN = aonrtemp.ORT.                          
   CREATE faktaonrtemp.
   ASSIGN     
   faktaonrtemp.NY = TRUE
   faktaonrtemp.FAKTNR = FILL-IN_FAKTNR
   faktaonrtemp.AONR = aonrtemp.AONR
   faktaonrtemp.DELNR = aonrtemp.DELNR
   faktaonrtemp.ORT = aonrtemp.ORT
   faktaonrtemp.OMRADE = aonrtemp.OMRADE
   faktaonrtemp.AONRAVDATUM = aonrtemp.AONRAVDATUM
   faktaonrtemp.STARTDATUM = aonrtemp.STARTDATUM
   faktaonrtemp.GDATUM = aonrtemp.ELVOMRKOD
   faktaonrtemp.OPRIS = aonrtemp.PLANOFFERT.
   RUN prisoff_UI (INPUT faktaonrtemp.OPRIS,INPUT TRUE).
   CREATE faktkolltemp.
   ASSIGN
   faktkolltemp.FAKTNR = faktaonrtemp.FAKTNR 
   faktkolltemp.AONR = faktaonrtemp.AONR 
   faktkolltemp.DELNR = faktaonrtemp.DELNR.
   DELETE aonrtemp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nybest_UI DIALOG-1 
PROCEDURE nybest_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE VARIABLE vemanvandare AS CHARACTER NO-UNDO.
   {muswait.i}   
   IF vad = 1 THEN DO:
      vemanvandare = ?.
      RUN BESTNYU.W (INPUT-OUTPUT vemanvandare).     
      IF musz = FALSE THEN DO: 
         RUN hamtaen_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundallt APPEND).                         
         RUN hamtaenextra_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundextra APPEND).
         FIND FIRST bestkundallt WHERE bestkundallt.BESTID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE bestkundallt THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(bestkundallt)).              
            RUN openbdyn_UI IN brwproc[3] (INPUT "").
            RUN lastselectdyn_UI IN brwproc[3].     
         END.         
      END.
      musz = FALSE.
   END.
   IF vad = 2 THEN DO:
      vemanvandare = bestkundallt.BESTID.   
      RUN BESTNYU.W (INPUT-OUTPUT vemanvandare).     
      IF musz = FALSE THEN DO: 
         DELETE bestkundallt.
         RUN hamtaen_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundallt APPEND).                         
         FIND FIRST bestkundextra WHERE bestkundextra.BESTID = vemanvandare NO-ERROR.
         IF AVAILABLE bestkundextra THEN DO:
            DELETE bestkundextra.
         END.
         RUN hamtaenextra_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundextra APPEND).
         FIND FIRST bestkundallt WHERE bestkundallt.BESTID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE bestkundallt THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(bestkundallt)).              
            RUN openbdyn_UI IN brwproc[3] (INPUT "").
            RUN lastselectdyn_UI IN brwproc[3].              
         END.         
      END.   
   END.   
   musz = FALSE.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyupparb_UI DIALOG-1 
PROCEDURE nyupparb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE sista AS INTEGER NO-UNDO.
   IF vfaktplantemp.SENASTFAK NE ? THEN DO:
      MESSAGE "Du kan inte lägga till några poster då fakturering har redan har skett på denna fakturaplan."   
      VIEW-AS ALERT-BOX TITLE "Bortag av betalningsplan".
      RETURN.
   END.
   FIND LAST faktuppplantemp WHERE faktuppplantemp.FAKTNR = vfaktplantemp.FAKTNR 
   USE-INDEX ORDNING NO-LOCK NO-ERROR.   
   sista = faktuppplantemp.ORDNING + 1.
   CREATE faktuppplantemp. 
   ASSIGN
   faktuppplantemp.FAKT% = 0
   faktuppplantemp.FAKTNR = vfaktplantemp.FAKTNR
   faktuppplantemp.FAKTURERAD = FALSE
   faktuppplantemp.KRITERIUM = ""
   faktuppplantemp.ORDNING = sista
   faktuppplantemp.UPLAN% = 0.      
   
   RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(faktuppplantemp)).              
   RUN setcolindex_UI IN brwproc[5] (INPUT sortvar).
   RUN openbdyn_UI IN brwproc[5] (INPUT vilkavar).
   RUN lastselectdyn_UI IN brwproc[5].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI DIALOG-1 
PROCEDURE ny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE efaktstarttemp NO-ERROR.    
   CREATE efaktstarttemp.    
   ASSIGN
   efaktstarttemp.FAKTNR = vfaktplantemp.FAKTNR
   efaktstarttemp.PLANDATUM = TODAY
   efaktstarttemp.ORDNING = 2.      
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   vfaktplantemp.FAKTTYP = faktyptemp.FAKTTYP.     
      
   RUN avtalao_UI.
   RUN NYFPLANPROA.W (INPUT vfaktplantemp.FAKTNR,INPUT FILL-IN_TOTPRIS,INPUT-OUTPUT TABLE efaktstarttemp).
   FIND FIRST efaktstarttemp NO-LOCK NO-ERROR.
   IF musz = FALSE THEN DO:
      CREATE faktstarttemp.
      BUFFER-COPY efaktstarttemp TO faktstarttemp.
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(faktstarttemp)).                   
      RUN openbdyn_UI IN brwproc[4] (INPUT "").   
      RUN lastselectdyn_UI IN brwproc[4].        
      MESSAGE 
      "Vill du lägga upp flera poster ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1.   
      CASE val1:
         WHEN TRUE THEN DO:          
            RUN ny_UI.                          
         END.
         WHEN FALSE THEN DO:               
         END.
      END CASE.
   END.
   ELSE DO:
      ASSIGN
      musz = FALSE.      
      
      /*AVTAL
      IF FAKTSTART.START NE "" THEN DO:
         OPEN QUERY faktavtq FOR EACH FAKTAVTALAONR WHERE 
         FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.START = FAKTSTART.START 
         NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY faktavtq FOR EACH FAKTAVTALAONR WHERE 
         FAKTAVTALAONR.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM 
         NO-LOCK.
      END.
      DO TRANSACTION:
         GET FIRST faktavtq EXCLUSIVE-LOCK.
         IF AVAILABLE FAKTAVTALAONR THEN DO: 
            DELETE FAKTAVTALAONR.         
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT faktavtq EXCLUSIVE-LOCK.
            IF AVAILABLE FAKTAVTALAONR THEN DO: 
               DELETE FAKTAVTALAONR.
            END.
            ELSE LEAVE.
         END.
      END.
      */
      
   END.
   FIND FIRST efaktstarttemp NO-ERROR.
   IF AVAILABLE efaktstarttemp THEN DELETE efaktstarttemp.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prisoff_UI DIALOG-1 
PROCEDURE prisoff_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER oprisvar AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER nyvar AS LOGICAL NO-UNDO.
   IF nyvar = FALSE THEN FILL-IN_TOTPRIS = FILL-IN_TOTPRIS - faktaonrtemp.OPRIS.
   faktaonrtemp.OPRIS = oprisvar.     
   FILL-IN_TOTPRIS = FILL-IN_TOTPRIS + faktaonrtemp.OPRIS.
   IF FILL-IN_TOTPRIS < 0 THEN FILL-IN_TOTPRIS = 0.
   DISPLAY FILL-IN_TOTPRIS WITH FRAME {&FRAME-NAME}.   
   FILL-IN_TOTPRIS = ?.
   APPLY "LEAVE" TO FILL-IN_TOTPRIS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortvar_UI DIALOG-1 
PROCEDURE sortvar_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   
   IF vfaktplantemp.FAKTTYP = "Takprisfakt." THEN DO:
      ASSIGN
      sortvar =  'faktuppplantemp.UPLAN%'
      vilkavar = ' WHERE faktuppplantemp.FAKTNR = ' + STRING(vfaktplantemp.FAKTNR) + ' AND faktuppplantemp.KRITERIUM = "SLUT" '.   
      IF RAD_TAK = 4 THEN DO:        
      END.
      ELSE DO:
         vilkavar = ' WHERE faktuppplantemp.FAKTNR = ' + STRING(vfaktplantemp.FAKTNR).         
      END.
   END.
   ELSE DO:
      ASSIGN
      sortvar =  ""
      vilkavar = "".   
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valao_UI DIALOG-1 
PROCEDURE valao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   GET FIRST BRW_VAONR NO-LOCK.     
   IF AVAILABLE faktaonrtemp THEN DO:
      DISABLE CMB_FAK RAD_TAK WITH FRAME {&FRAME-NAME}.
      ENABLE BRW_VAONR WITH FRAME {&FRAME-NAME}.
      BRW_VAONR:HIDDEN = FALSE.      
   END.     
   ELSE DO:      
      /*
      BRW_VAONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      */
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

