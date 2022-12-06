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
DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{ANVPERS.I}
{OMRTEMPW.I}
DEFINE TEMP-TABLE omranvandartemp NO-UNDO LIKE anvandartemp.
DEFINE TEMP-TABLE pt2 NO-UNDO LIKE personaltemp.
{SEKXGURUTEMP.I}
/* {GURUSKEKMENY.I} */
DEFINE SHARED VARIABLE anvapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE nivapph AS HANDLE NO-UNDO.               /* NIVPERAPP.P */
DEFINE VARIABLE anvtabrec AS RECID NO-UNDO.
DEFINE VARIABLE hjalpvar AS INTEGER NO-UNDO.
DEFINE VARIABLE ordningvar AS INTEGER NO-UNDO.
DEFINE VARIABLE gamanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE spar_ANVANDARE AS CHARACTER NO-UNDO.
DEFINE VARIABLE losenwebh AS HANDLE NO-UNDO.
DEFINE VARIABLE meddelandevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felNr AS INTEGER NO-UNDO.
DEFINE VARIABLE alltOk AS LOGICAL NO-UNDO.



{TIDUTTTNEW.I}
{PERSTEMP.I}
/*DEFINE TEMP-TABLE perstemp  NO-UNDO
  FIELD EFTERNAMN AS CHARACTER 
  FIELD FORNAMN AS CHARACTER 
  FIELD PERSONALKOD AS CHARACTER
  FIELD OMRADE AS CHARACTER
  FIELD TYP AS CHARACTER
  FIELD ANVANDARE AS CHARACTER
  INDEX PERSONALKOD IS PRIMARY TYP PERSONALKOD
  INDEX OMRADE TYP OMRADE PERSONALKOD
  INDEX FORNAMN TYP FORNAMN
  INDEX EFTERNAMN TYP EFTERNAMN.  */
DEFINE TEMP-TABLE tidstemp NO-UNDO LIKE perstemp.
{BOLAGSEKTEMP.I}


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_BOLAG

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bolagsektemp ebolagsektemp omrtemp pt2 ~
perstemp tidstemp omrtempsek

/* Definitions for BROWSE BRW_BOLAG                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_BOLAG bolagsektemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BOLAG 
&Scoped-define QUERY-STRING-BRW_BOLAG FOR EACH bolagsektemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BOLAG OPEN QUERY BRW_BOLAG FOR EACH bolagsektemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BOLAG bolagsektemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BOLAG bolagsektemp


/* Definitions for BROWSE BRW_BOLAGSEK                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_BOLAGSEK ebolagsektemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BOLAGSEK 
&Scoped-define QUERY-STRING-BRW_BOLAGSEK FOR EACH ebolagsektemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BOLAGSEK OPEN QUERY BRW_BOLAGSEK FOR EACH ebolagsektemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BOLAGSEK ebolagsektemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BOLAGSEK ebolagsektemp


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp.OMRADE omrtemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS pt2.PERSONALKOD pt2.FORNAMN ~
pt2.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS pt2.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS pt2
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS pt2
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH pt2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH pt2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS pt2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS pt2


/* Definitions for BROWSE BRW_PERSEK                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_PERSEK perstemp.OMRADE ~
perstemp.OMRADENUM perstemp.PERSONALKOD perstemp.FORNAMN perstemp.EFTERNAMN ~
perstemp.ANVANDARE perstemp.AKTIV perstemp.KATEGORI 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERSEK perstemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERSEK perstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERSEK perstemp
&Scoped-define QUERY-STRING-BRW_PERSEK FOR EACH perstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERSEK OPEN QUERY BRW_PERSEK FOR EACH perstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERSEK perstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERSEK perstemp


/* Definitions for BROWSE BRW_TIDSEK                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDSEK tidstemp.OMRADE ~
tidstemp.OMRADENUM tidstemp.PERSONALKOD tidstemp.FORNAMN tidstemp.EFTERNAMN ~
tidstemp.ANVANDARE tidstemp.AKTIV tidstemp.KATEGORI 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDSEK tidstemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TIDSEK tidstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TIDSEK tidstemp
&Scoped-define QUERY-STRING-BRW_TIDSEK FOR EACH tidstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TIDSEK OPEN QUERY BRW_TIDSEK FOR EACH tidstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TIDSEK tidstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDSEK tidstemp


/* Definitions for BROWSE BRW_VOMR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VOMR omrtempsek.OMRADE omrtempsek.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOMR 
&Scoped-define QUERY-STRING-BRW_VOMR FOR EACH omrtempsek NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VOMR OPEN QUERY BRW_VOMR FOR EACH omrtempsek NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VOMR omrtempsek
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOMR omrtempsek


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_VOMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_APKOD FILL-IN_ANVANDARE BRW_TIDSEK ~
BRW_BOLAGSEK BRW_PERS BRW_PERSEK BRW_BOLAG BRW_VOMR BRW_OMR FILL-IN-PKOD ~
BTN_ALLOVER FILL-IN-KATE FILL-IN_AV-NAMN BTN_OVER FILL-IN_ALLPERS BTN_BACK ~
FILL-IN_ALLAONR BTN_VISA CMB_NIV BTN_ALLBACK FILL-IN_AV-NYTTLOSE BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_ANVANDARE FILL-IN-PKOD ~
FILL-IN-KATE FILL-IN_AV-NAMN FILL-IN_ALLPERS FILL-IN_ALLAONR ~
FILL-IN_AV-NYTTLOSE FILL-IN-TS FILL-IN-FAKT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_VISA 
     LABEL "Visa":L 
     SIZE 10 BY 1.

DEFINE VARIABLE CMB_NIV AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Användarnivå" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAKT AS CHARACTER FORMAT "X(256)":U INITIAL "Sekretess övrigt" 
      VIEW-AS TEXT 
     SIZE 17 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-KATE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kategori" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LASTNIV AS CHARACTER FORMAT "X(256)":U INITIAL "Låst sekretess nivå" 
      VIEW-AS TEXT 
     SIZE 12.75 BY .63
     FONT 7 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TS AS CHARACTER FORMAT "X(256)":U INITIAL "Sekretess Tid" 
      VIEW-AS TEXT 
     SIZE 16 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_ALLAONR AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Visa alla ao" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ALLPERS AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Visa alla p." 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "X(256)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 28.25 BY 1.

DEFINE VARIABLE FILL-IN_AV-LOSEN AS CHARACTER FORMAT "X(256)" 
     LABEL "Lösenord" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE FILL-IN_AV-NAMN AS CHARACTER FORMAT "X(256)" 
     LABEL "Användarnamn" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN_AV-NYTTLOSE AS CHARACTER FORMAT "X(256)" 
     LABEL "Nytt Lösenord" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE RAD_ANV AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grundregistreringar", 1
     SIZE 120.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_APKOD AS LOGICAL INITIAL yes 
     LABEL "Endast aktiva" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.88 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BOLAG FOR 
      bolagsektemp SCROLLING.

DEFINE QUERY BRW_BOLAGSEK FOR 
      ebolagsektemp SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      omrtemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      pt2 SCROLLING.

DEFINE QUERY BRW_PERSEK FOR 
      perstemp SCROLLING.

DEFINE QUERY BRW_TIDSEK FOR 
      tidstemp SCROLLING.

DEFINE QUERY BRW_VOMR FOR 
      omrtempsek SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BOLAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BOLAG DIALOG-1 _STRUCTURED
  QUERY BRW_BOLAG NO-LOCK DISPLAY
      bolagsektemp.NAMN FORMAT "X(256)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 30.38 BY 11
         TITLE "Får ändra på".

DEFINE BROWSE BRW_BOLAGSEK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BOLAGSEK DIALOG-1 _STRUCTURED
  QUERY BRW_BOLAGSEK NO-LOCK DISPLAY
      ebolagsektemp.NAMN FORMAT "X(256)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 30.38 BY 11
         TITLE "Får ej ändra på".

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR DIALOG-1 _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 50 BY 10.92
         FONT 4
         TITLE "Område".

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS DIALOG-1 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      pt2.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      pt2.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U WIDTH 13
      pt2.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
  ENABLE
      pt2.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 50.63 BY 11.

DEFINE BROWSE BRW_PERSEK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERSEK DIALOG-1 _STRUCTURED
  QUERY BRW_PERSEK NO-LOCK DISPLAY
      perstemp.OMRADE FORMAT "X(256)":U WIDTH 20
      perstemp.OMRADENUM COLUMN-LABEL "Omr" FORMAT "X(256)":U WIDTH 5
      perstemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(256)":U
            WIDTH 7
      perstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      perstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(15)":U
      perstemp.ANVANDARE COLUMN-LABEL "Anvandare" FORMAT "X(256)":U
            WIDTH 12
      perstemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
      perstemp.KATEGORI COLUMN-LABEL "Kategori" FORMAT "x(256)":U
            WIDTH 10
  ENABLE
      perstemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.38 BY 11
         TITLE "Får ändra på".

DEFINE BROWSE BRW_TIDSEK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDSEK DIALOG-1 _STRUCTURED
  QUERY BRW_TIDSEK NO-LOCK DISPLAY
      tidstemp.OMRADE COLUMN-LABEL "Områdenamn" FORMAT "X(256)":U
            WIDTH 20
      tidstemp.OMRADENUM COLUMN-LABEL "Omr" FORMAT "X(256)":U WIDTH 5
      tidstemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(256)":U
            WIDTH 7
      tidstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(15)":U
      tidstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(15)":U
      tidstemp.ANVANDARE COLUMN-LABEL "Anvandare" FORMAT "X(256)":U
            WIDTH 12
      tidstemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
      tidstemp.KATEGORI COLUMN-LABEL "Kategori" FORMAT "x(256)":U
            WIDTH 10
  ENABLE
      tidstemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 50 BY 11
         TITLE "Får ej ändra på".

DEFINE BROWSE BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOMR DIALOG-1 _STRUCTURED
  QUERY BRW_VOMR NO-LOCK DISPLAY
      omrtempsek.OMRADE FORMAT "X(6)":U
      omrtempsek.NAMN FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 50 BY 11
         FONT 4
         TITLE "Valda områden".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG_APKOD AT ROW 2.29 COL 2.13 WIDGET-ID 4
     RAD_ANV AT ROW 4.42 COL 1.5 NO-LABEL
     FILL-IN_ANVANDARE AT ROW 5.92 COL 14 COLON-ALIGNED
     BRW_TIDSEK AT ROW 6 COL 1.75
     BRW_BOLAGSEK AT ROW 6 COL 27.38
     BRW_PERS AT ROW 6 COL 59.5
     BRW_PERSEK AT ROW 6 COL 66.13
     BRW_BOLAG AT ROW 6 COL 66.25
     BRW_VOMR AT ROW 6 COL 66.25 WIDGET-ID 200
     BRW_OMR AT ROW 6.08 COL 7.5 WIDGET-ID 100
     FILL-IN-PKOD AT ROW 7.21 COL 14 COLON-ALIGNED
     BTN_ALLOVER AT ROW 7.92 COL 59.63
     FILL-IN-KATE AT ROW 8.5 COL 14 COLON-ALIGNED WIDGET-ID 2
     FILL-IN_AV-NAMN AT ROW 9.79 COL 14 COLON-ALIGNED
     BTN_OVER AT ROW 10.08 COL 59.63
     FILL-IN_ALLPERS AT ROW 11.08 COL 14 COLON-ALIGNED
     BTN_BACK AT ROW 12.29 COL 59.63
     FILL-IN_ALLAONR AT ROW 12.38 COL 14 COLON-ALIGNED
     BTN_VISA AT ROW 13.63 COL 38
     CMB_NIV AT ROW 13.67 COL 14 COLON-ALIGNED
     BTN_ALLBACK AT ROW 14.5 COL 59.63
     FILL-IN_AV-NYTTLOSE AT ROW 14.83 COL 14 COLON-ALIGNED WIDGET-ID 6
     FILL-IN_AV-LOSEN AT ROW 14.83 COL 14 COLON-ALIGNED
     BTN_OK AT ROW 18.08 COL 93.5
     BTN_AVB AT ROW 18.08 COL 108.5
     FILL-IN-TS AT ROW 3.17 COL 56 COLON-ALIGNED NO-LABEL
     FILL-IN-FAKT AT ROW 3.17 COL 90.5 COLON-ALIGNED NO-LABEL
     FILL-IN-LASTNIV AT ROW 13.88 COL 20.75 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     "Sekretess Personal" VIEW-AS TEXT
          SIZE 22.5 BY 1 AT ROW 3.17 COL 4.25
          FONT 17
     "Ändra eller registrera användare" VIEW-AS TEXT
          SIZE 34.5 BY 1.13 AT ROW 1.17 COL 1.5
          FONT 17
     SPACE(87.49) SKIP(17.44)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändra eller registrera användare":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: bolagsektemp T "?" NO-UNDO temp-db bolagsektemp
      TABLE: ebolagsektemp T "?" NO-UNDO temp-db ebolagsektemp
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: omrtempsek T "?" NO-UNDO temp-db omrtempsek
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: perstemp T "?" NO-UNDO temp-db perstemp
      TABLE: pt2 T "?" NO-UNDO temp-db pt2
      TABLE: tidstemp T "?" NO-UNDO temp-db tidstemp
      TABLE: vomrtemp T "?" NO-UNDO temp-db vomrtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_TIDSEK FILL-IN_ANVANDARE DIALOG-1 */
/* BROWSE-TAB BRW_BOLAGSEK BRW_TIDSEK DIALOG-1 */
/* BROWSE-TAB BRW_PERS BRW_BOLAGSEK DIALOG-1 */
/* BROWSE-TAB BRW_PERSEK BRW_PERS DIALOG-1 */
/* BROWSE-TAB BRW_BOLAG BRW_PERSEK DIALOG-1 */
/* BROWSE-TAB BRW_VOMR BRW_BOLAG DIALOG-1 */
/* BROWSE-TAB BRW_OMR BRW_VOMR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       BRW_BOLAG:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

ASSIGN 
       BRW_BOLAGSEK:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

ASSIGN 
       BRW_OMR:HIDDEN  IN FRAME DIALOG-1                = TRUE.

ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

ASSIGN 
       BRW_PERSEK:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

ASSIGN 
       BRW_TIDSEK:MAX-DATA-GUESS IN FRAME DIALOG-1         = 300.

ASSIGN 
       BRW_VOMR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_VOMR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 10000.

/* SETTINGS FOR COMBO-BOX CMB_NIV IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
/* SETTINGS FOR FILL-IN FILL-IN-FAKT IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-FAKT:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LASTNIV IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-LASTNIV:HIDDEN IN FRAME DIALOG-1           = TRUE
       FILL-IN-LASTNIV:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TS IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-TS:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_AV-LOSEN IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_AV-LOSEN:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ANV IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_ANV:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_APKOD IN FRAME DIALOG-1
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_APKOD:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BOLAG
/* Query rebuild information for BROWSE BRW_BOLAG
     _TblList          = "Temp-Tables.bolagsektemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bolagsektemp.NAMN
"bolagsektemp.NAMN" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BOLAG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BOLAGSEK
/* Query rebuild information for BROWSE BRW_BOLAGSEK
     _TblList          = "Temp-Tables.ebolagsektemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ebolagsektemp.NAMN
"ebolagsektemp.NAMN" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BOLAGSEK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp.OMRADE
"omrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.omrtemp.NAMN
"omrtemp.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.pt2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.pt2.PERSONALKOD
"pt2.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.pt2.FORNAMN
"pt2.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.pt2.EFTERNAMN
"pt2.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERSEK
/* Query rebuild information for BROWSE BRW_PERSEK
     _TblList          = "Temp-Tables.perstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.perstemp.OMRADE
"perstemp.OMRADE" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.perstemp.OMRADENUM
"perstemp.OMRADENUM" "Omr" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.perstemp.PERSONALKOD
"perstemp.PERSONALKOD" "Enhet/!Sign" "x(256)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.perstemp.FORNAMN
"perstemp.FORNAMN" "Förnamn" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.perstemp.EFTERNAMN
"perstemp.EFTERNAMN" "Efternamn" "x(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.perstemp.ANVANDARE
"perstemp.ANVANDARE" "Anvandare" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.perstemp.AKTIV
"perstemp.AKTIV" "Aktiv" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.perstemp.KATEGORI
"perstemp.KATEGORI" "Kategori" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERSEK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDSEK
/* Query rebuild information for BROWSE BRW_TIDSEK
     _TblList          = "Temp-Tables.tidstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidstemp.OMRADE
"tidstemp.OMRADE" "Områdenamn" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidstemp.OMRADENUM
"tidstemp.OMRADENUM" "Omr" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidstemp.PERSONALKOD
"tidstemp.PERSONALKOD" "Enhet/!Sign" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tidstemp.FORNAMN
"tidstemp.FORNAMN" "Förnamn" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tidstemp.EFTERNAMN
"tidstemp.EFTERNAMN" "Efternamn" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tidstemp.ANVANDARE
"tidstemp.ANVANDARE" "Anvandare" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tidstemp.AKTIV
"tidstemp.AKTIV" "Aktiv" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tidstemp.KATEGORI
"tidstemp.KATEGORI" "Kategori" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TIDSEK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOMR
/* Query rebuild information for BROWSE BRW_VOMR
     _TblList          = "Temp-Tables.omrtempsek"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.omrtempsek.OMRADE
     _FldNameList[2]   = Temp-Tables.omrtempsek.NAMN
     _Query            is OPENED
*/  /* BROWSE BRW_VOMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändra eller registrera användare */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nivapph) THEN DELETE PROCEDURE nivapph.
   nivapph = ?.
   IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
   losenwebh = ?.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
   edataapph = ?.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändra eller registrera användare */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nivapph) THEN DELETE PROCEDURE nivapph.
   nivapph = ?.
   IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
   losenwebh = ?.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
   edataapph = ?.
  /* musz = TRUE.
   RETURN.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON WINDOW-CLOSE OF FRAME DIALOG-1 /* Ändra eller registrera användare */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nivapph) THEN DELETE PROCEDURE nivapph.
   nivapph = ?.
   IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
   losenwebh = ?.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
   edataapph = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON LEAVE OF BRW_PERS IN FRAME DIALOG-1
DO:
    FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   IF FILL-IN-PKOD NE "" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD 
      NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE personaltemp THEN DO:
         MESSAGE "Enhet/sign" FILL-IN-PKOD " finns inte." VIEW-AS ALERT-BOX.       
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
   /*      APPLY "ENDKEY" TO BRW_PERS IN FRAME {&FRAME-NAME}.*/
      END. 
      ELSE DO:
         FILL-IN_AV-NAMN = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.     
      END.   
   END.  
   IF FILL-IN-PKOD = "" THEN DO:
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = TRUE
      FILL-IN_ALLAONR:HIDDEN = TRUE
      FILL-IN_ALLPERS = TRUE
      FILL-IN_ALLAONR = TRUE.
   END. 
   ELSE DO:                 
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = FALSE
      FILL-IN_ALLAONR:HIDDEN = FALSE.      
   END. 
   DISPLAY FILL-IN_AV-NAMN WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON VALUE-CHANGED OF BRW_PERS IN FRAME DIALOG-1
DO:
   IF NOT AVAILABLE pt2 THEN DO:
      RETURN NO-APPLY.
   END.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = pt2.PERSONALKOD NO-LOCK NO-ERROR.
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   IF FILL-IN-PKOD = "" THEN DO:    
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = TRUE
      FILL-IN_ALLAONR:HIDDEN = TRUE
      FILL-IN_ALLPERS = TRUE
      FILL-IN_ALLAONR = TRUE.
   END. 
   ELSE DO:                 
      ASSIGN
      FILL-IN_AV-NAMN = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
      FILL-IN_ALLPERS:HIDDEN = FALSE
      FILL-IN_ALLAONR:HIDDEN = FALSE.      
   END. 
   DISPLAY FILL-IN-PKOD  FILL-IN_AV-NAMN 
   WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:    
   ASSIGN
   FILL-IN_ALLPERS = INPUT FILL-IN_ALLPERS
   FILL-IN_ALLAONR = INPUT FILL-IN_ALLAONR
   FILL-IN-KATE = INPUT FILL-IN-KATE
   FILL-IN-PKOD = INPUT FILL-IN-PKOD. 
   IF FILL-IN-PKOD = "" THEN DO:      
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = TRUE
      FILL-IN_ALLAONR:HIDDEN = TRUE
      FILL-IN_ALLPERS = TRUE
      FILL-IN_ALLAONR = TRUE.
   END.                     
   ASSIGN
   CMB_NIV = INPUT CMB_NIV
   /*
   FILL-IN_AV-LOSEN = INPUT FILL-IN_AV-LOSEN
   */
   FILL-IN_AV-NYTTLOSE = INPUT FILL-IN_AV-NYTTLOSE 
   FILL-IN_AV-NAMN = INPUT FILL-IN_AV-NAMN 
   FILL-IN_ANVANDARE = INPUT FILL-IN_ANVANDARE.
   IF FILL-IN_ANVANDARE = "" THEN DO:
      MESSAGE "Användare får inte vara blankt." VIEW-AS ALERT-BOX.      
      APPLY "ENTRY" TO FILL-IN_ANVANDARE IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = FILL-IN_ANVANDARE 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE anvandartemp THEN DO:  
      FIND anvandartemp WHERE RECID(anvandartemp) = anvtabrec NO-LOCK NO-ERROR.
   END.
   IF RECID(anvandartemp) = anvtabrec THEN anvtabrec = anvtabrec.
   ELSE DO:
      MESSAGE "Användare finns redan." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_ANVANDARE IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.     
   IF FILL-IN-PKOD NE "" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD 
      NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE personaltemp THEN DO:
         MESSAGE "Enhet/sign " FILL-IN-PKOD " finns inte." VIEW-AS ALERT-BOX.       
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END. 
   END.
   IF FILL-IN-PKOD NE "" THEN DO:
      RUN kollanvpkod_UI IN anvapph (INPUT FILL-IN-PKOD,INPUT spar_ANVANDARE,OUTPUT musz).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE "Enhet/sign " FILL-IN-PKOD " är redan kopplad till en användare." VIEW-AS ALERT-BOX.       
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.      
      END.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      IF NOT VALID-HANDLE(losenwebh) THEN RUN LOSENKOLLWEB.P PERSISTENT SET losenwebh ON Guru.Konstanter:apphand TRANSACTION DISTINCT . 
   END.
   ELSE DO:
      IF NOT VALID-HANDLE(losenwebh) THEN RUN LOSENKOLLWEB.P PERSISTENT SET losenwebh .    
   END.
   RUN AppSpringSet_UI  IN losenwebh (INPUT  Guru.Konstanter:AppSpringSet).
   IF FILL-IN_AV-NYTTLOSE = "" THEN.
   ELSE FILL-IN_AV-LOSEN = FILL-IN_AV-NYTTLOSE. 
   RUN NylosenReglerKoll_UI IN losenwebh (INPUT FILL-IN_ANVANDARE,INPUT-OUTPUT FILL-IN_AV-LOSEN,INPUT FILL-IN-PKOD, OUTPUT felNr, OUTPUT alltOk,OUTPUT meddelandevar).
   IF alltOk = FALSE THEN DO:
      MESSAGE meddelandevar
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   gamanv = anvandartemp.ANVANDARE.
   ASSIGN
   anvandartemp.PERSONALKOD = FILL-IN-PKOD
   anvandartemp.KATEGORI = FILL-IN-KATE
   anvandartemp.ANVANDARE = FILL-IN_ANVANDARE
   anvandartemp.AV-LEVEL = CMB_NIV
   
   anvandartemp.AV-NAMN = FILL-IN_AV-NAMN
   anvandartemp.ALLPERS = FILL-IN_ALLPERS
   anvandartemp.ALLAONR = FILL-IN_ALLAONR
   SUBSTRING(anvandartemp.SKRIVARID,1,1) = "0".      
   vem = anvandartemp.ANVANDARE.
   
   IF Guru.GlobalaVariabler:Kryptonit = TRUE THEN DO:
      IF FILL-IN_AV-NYTTLOSE = "" THEN anvandartemp.AV-LOSEN = FILL-IN_AV-LOSEN.
      ELSE anvandartemp.AV-LOSEN = Guru.Konstanter:SaltRetur(FILL-IN_AV-LOSEN).
   END.   
   ELSE anvandartemp.AV-LOSEN = FILL-IN_AV-LOSEN. 
   
   CREATE exanvandartemp.
   BUFFER-COPY anvandartemp TO exanvandartemp.
   DEFINE VARIABLE pcode AS CHARACTER NO-UNDO.
   pcode = STRING(FILL-IN-PKOD:SCREEN-VALUE).
   {muswait.i}
   DEBUGGER:SET-BREAK().
   RUN sparpers_UI IN anvapph (INPUT Guru.Konstanter:globanv,INPUT pcode, INPUT gamanv,INPUT TABLE exanvandartemp,INPUT TABLE perstemp).
   RUN sparbolag_UI IN anvapph (INPUT vem,INPUT TABLE bolagsektemp). 
   IF Guru.Konstanter:varforetypchar[4] NE "" THEN DO:
      RUN omrsparhamtaanv_UI IN anvapph (INPUT anvandartemp.ANVANDARE,INPUT TABLE omrtempsek). 
   END.
              
   {musarrow.i}
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {BORTBRWPROC.I}
    IF VALID-HANDLE(nivapph) THEN DELETE PROCEDURE nivapph.
   nivapph = ?.
   IF VALID-HANDLE(losenwebh) THEN DELETE PROCEDURE losenwebh NO-ERROR.
   losenwebh = ?.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
   edataapph = ?.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA DIALOG-1
ON CHOOSE OF BTN_VISA IN FRAME DIALOG-1 /* Visa */
DO:
   CMB_NIV = INPUT CMB_NIV.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   RUN utvisa_UI IN nivapph 
   (INPUT CMB_NIV,OUTPUT TABLE tidut).  
   RUN UTRAPPD.W ("Behörighetsnivåer").
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_NIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_NIV DIALOG-1
ON VALUE-CHANGED OF CMB_NIV IN FRAME DIALOG-1 /* Användarnivå */
DO:
   CMB_NIV = INPUT CMB_NIV.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD DIALOG-1
ON ENTRY OF FILL-IN-PKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:   
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.
   FIND FIRST pt2 WHERE pt2.PERSONALKOD = FILL-IN-PKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE pt2 THEN DO:
      APPLY "HOME" TO BRW_PERS.
      status-ok = BRW_PERS:SELECT-FOCUSED-ROW() NO-ERROR.      
   END.
   ELSE DO:                      
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(pt2)).
      RUN lastselectdyn_UI IN brwproc[4]. 
   END.    
   BRW_PERS:HIDDEN = FALSE.  
   /*
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKODNO-LOCK NO-ERROR.   
   IF NOT AVAILABLE personaltemp THEN DO:
      APPLY "HOME" TO BRW_PERS.
      status-ok = BRW_PERS:SELECT-FOCUSED-ROW() NO-ERROR.      
   END.
   ELSE DO:                      
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(personaltemp)).
      RUN lastselectdyn_UI IN brwproc[4]. 
   END.    
   BRW_PERS:HIDDEN = FALSE.  
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PKOD DIALOG-1
ON LEAVE OF FILL-IN-PKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   IF INPUT FILL-IN-PKOD = "" THEN DO:
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = TRUE
      FILL-IN_ALLAONR:HIDDEN = TRUE
      FILL-IN_ALLPERS = TRUE
      FILL-IN_ALLAONR = TRUE.
   END.
   ELSE DO:                 
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = FALSE
      FILL-IN_ALLAONR:HIDDEN = FALSE.      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ALLAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ALLAONR DIALOG-1
ON LEAVE OF FILL-IN_ALLAONR IN FRAME DIALOG-1 /* Visa alla ao */
DO:
   FILL-IN_ALLAONR = INPUT FILL-IN_ALLAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ALLAONR DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_ALLAONR IN FRAME DIALOG-1 /* Visa alla ao */
DO:
   IF INPUT FILL-IN_ALLAONR = FALSE THEN  FILL-IN_ALLAONR = TRUE.
   IF INPUT FILL-IN_ALLAONR = TRUE THEN  FILL-IN_ALLAONR = FALSE.
   DISPLAY FILL-IN_ALLAONR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ALLPERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ALLPERS DIALOG-1
ON LEAVE OF FILL-IN_ALLPERS IN FRAME DIALOG-1 /* Visa alla p. */
DO:
   FILL-IN_ALLPERS = INPUT FILL-IN_ALLPERS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ALLPERS DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_ALLPERS IN FRAME DIALOG-1 /* Visa alla p. */
DO:
   IF INPUT FILL-IN_ALLPERS = FALSE THEN  FILL-IN_ALLPERS = TRUE.
   IF INPUT FILL-IN_ALLPERS = TRUE THEN  FILL-IN_ALLPERS = FALSE.
   DISPLAY FILL-IN_ALLPERS WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ANVANDARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ANVANDARE DIALOG-1
ON LEAVE OF FILL-IN_ANVANDARE IN FRAME DIALOG-1 /* Användare */
DO:
   FILL-IN_ANVANDARE = INPUT FILL-IN_ANVANDARE.
   IF FILL-IN_ANVANDARE = "" THEN DO:
      MESSAGE "Användare får inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = FILL-IN_ANVANDARE 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE anvandartemp THEN DO:  
      FIND anvandartemp WHERE RECID(anvandartemp) = anvtabrec NO-LOCK NO-ERROR.
   END.
   ELSE DO: 
      IF RECID(anvandartemp) = anvtabrec THEN anvtabrec = anvtabrec.
      ELSE DO:
         MESSAGE "Användare finns redan upplagd." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AV-LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AV-LOSEN DIALOG-1
ON LEAVE OF FILL-IN_AV-LOSEN IN FRAME DIALOG-1 /* Lösenord */
DO:
   FILL-IN_AV-LOSEN = INPUT FILL-IN_AV-LOSEN.
   IF FILL-IN_AV-LOSEN = "" THEN DO:
      MESSAGE "Lösen ordet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   
   IF FILL-IN_AV-LOSEN = INPUT FILL-IN_ANVANDARE OR FILL-IN_AV-LOSEN = INPUT FILL-IN-PKOD THEN DO:
      MESSAGE "Lösen får inte vara samma som användarnamn/Sign. Byt lösen!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AV-NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AV-NAMN DIALOG-1
ON ENTRY OF FILL-IN_AV-NAMN IN FRAME DIALOG-1 /* Användarnamn */
DO:
   FILL-IN-PKOD = INPUT FILL-IN-PKOD.
   IF FILL-IN-PKOD NE "" THEN DO:
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE personaltemp THEN DO:
         MESSAGE "Enhet/sign" FILL-IN-PKOD "finns inte." VIEW-AS ALERT-BOX.       
         APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
        /* APPLY "ENDKEY" TO BRW_PERS IN FRAME {&FRAME-NAME}.*/
      END. 
      ELSE DO:
         FILL-IN_AV-NAMN = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.     
      END.   
   END.  
   IF FILL-IN-PKOD = "" THEN DO:
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = TRUE
      FILL-IN_ALLAONR:HIDDEN = TRUE
      FILL-IN_ALLPERS = TRUE
      FILL-IN_ALLAONR = TRUE.
   END. 
   ELSE DO:                 
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = FALSE
      FILL-IN_ALLAONR:HIDDEN = FALSE.      
   END. 
   DISPLAY FILL-IN_AV-NAMN WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AV-NAMN DIALOG-1
ON LEAVE OF FILL-IN_AV-NAMN IN FRAME DIALOG-1 /* Användarnamn */
DO:
   FILL-IN_AV-NAMN = INPUT FILL-IN_AV-NAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_AV-NYTTLOSE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_AV-NYTTLOSE DIALOG-1
ON LEAVE OF FILL-IN_AV-NYTTLOSE IN FRAME DIALOG-1 /* Nytt Lösenord */
DO:
   FILL-IN_AV-NYTTLOSE = INPUT FILL-IN_AV-NYTTLOSE.
   IF FILL-IN_AV-LOSEN = "" AND FILL-IN_AV-NYTTLOSE = ""THEN DO:
      MESSAGE "Lösen ordet kan inte vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   
   IF FILL-IN_AV-NYTTLOSE = INPUT FILL-IN_ANVANDARE OR FILL-IN_AV-NYTTLOSE = INPUT FILL-IN-PKOD THEN DO:
      MESSAGE "Lösen får inte vara samma som användarnamn/Sign. Byt lösen!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ANV DIALOG-1
ON VALUE-CHANGED OF RAD_ANV IN FRAME DIALOG-1
DO:
   RAD_ANV = INPUT RAD_ANV.
   RUN goma_UI.
  
   IF VALID-HANDLE(brwproc[{&ARROWS}]) THEN DELETE PROCEDURE brwproc[{&ARROWS}] NO-ERROR.
   brwproc[{&ARROWS}] = ?.
   IF RAD_ANV = 7 THEN DO:    
      RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_BOLAGSEK:HANDLE, INPUT BRW_BOLAG:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   END.
   ELSE IF RAD_ANV = 8 THEN DO:    
      RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_OMR:HANDLE, INPUT BRW_VOMR:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   END.
   ELSE DO:
      RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_TIDSEK:HANDLE, INPUT BRW_PERSEK:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   END.
   RUN settitlenum_UI IN brwproc[{&ARROWS}] (INPUT TRUE).
   
   IF RAD_ANV = 8 THEN RUN deleteleftbrw_UI IN brwproc[{&ARROWS}] (INPUT FALSE).    
   ELSE RUN deleteleftbrw_UI IN brwproc[{&ARROWS}] (INPUT TRUE).
   IF RAD_ANV = 1 THEN DO:
      ASSIGN
      BTN_VISA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_NIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_ALLAONR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN_ALLPERS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN_ANVANDARE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_AV-NYTTLOSE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      /* 
      FILL-IN_AV-LOSEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      */
      FILL-IN_AV-NAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-KATE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-PKOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN LastNiv_UI.
   END.


   IF RAD_ANV = 2 THEN DO:
      ASSIGN
      tidstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      tidstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      perstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      perstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      BRW_PERSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_TIDSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      tidstemp.FORNAMN:LABEL IN BROWSE BRW_TIDSEK = "Förnamn" 
      perstemp.FORNAMN:LABEL IN BROWSE BRW_PERSEK = "Förnamn" 
      BRW_PERSEK:TITLE = "Får ändra på"
      BRW_TIDSEK:TITLE = "Får ej ändra på".
      brwsortvar = 'perstemp.TYP = "PERS"'.
      DEBUGGER:SET-BREAK().
      
      RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].      
      IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
         IF TOG_APKOD = TRUE THEN brwsortvar = ' WHERE tidstemp.TYP = "PERS" AND tidstemp.AKTIV = TRUE '.
         ELSE brwsortvar = 'tidstemp.TYP = "PERS"'.
      END.
      ELSE brwsortvar = 'tidstemp.TYP = "PERS"'.
             
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT brwsortvar).      
      FIND FIRST tidstemp WHERE tidstemp.PERSONALKOD = FILL-IN-PKOD AND tidstemp.TYP = "PERS" NO-ERROR.         
      IF AVAILABLE tidstemp THEN DO:                           
         RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(tidstemp)).          
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].                   
      END.
      ELSE DO:         
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      
      END.
   END.
   
   IF RAD_ANV = 3 THEN DO:
      ASSIGN
      tidstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      tidstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      perstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      perstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      BRW_PERSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_TIDSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      tidstemp.FORNAMN:LABEL IN BROWSE BRW_TIDSEK = "Förnamn" 
      perstemp.FORNAMN:LABEL IN BROWSE BRW_PERSEK = "Förnamn" 
      BRW_PERSEK:TITLE = "Får ändra på"
      BRW_TIDSEK:TITLE = "Får ej ändra på".
      brwsortvar = 'perstemp.TYP = "TID"'.
      RUN settitlenum_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT TRUE).
      RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].      
      IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
         IF TOG_APKOD = TRUE THEN brwsortvar = ' WHERE tidstemp.TYP = "TID" AND tidstemp.AKTIV = TRUE '.
         ELSE brwsortvar = 'tidstemp.TYP = "TID"'.
      END.
      RUN settitlenum_UI IN brwproc[{&LEFT-BROWSE}] (INPUT TRUE).
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT brwsortvar).      
      FIND FIRST tidstemp WHERE tidstemp.PERSONALKOD = FILL-IN-PKOD AND tidstemp.TYP = "TID"  NO-ERROR.         
      IF AVAILABLE tidstemp THEN DO:         
         RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(tidstemp)).          
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].          
      END.
      ELSE DO:
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      
      END.
   END.
   IF RAD_ANV = 4 THEN DO:
      ASSIGN                           
      tidstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_TIDSEK = FALSE
      tidstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_TIDSEK = FALSE
      perstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_PERSEK = FALSE
      perstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_PERSEK = FALSE
      BRW_PERSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_TIDSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      tidstemp.FORNAMN:LABEL IN BROWSE BRW_TIDSEK = "Benämning" 
      perstemp.FORNAMN:LABEL IN BROWSE BRW_PERSEK = "Benämning" 
      BRW_PERSEK:TITLE = "Får fakturera"
      BRW_TIDSEK:TITLE = "Får ej fakturera".

      brwsortvar = 'perstemp.TYP = "OMR"'.
      RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
      brwsortvar = 'tidstemp.TYP = "OMR"'.
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      
   END.
   IF RAD_ANV = 5 THEN DO:
      ASSIGN
      tidstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      tidstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      perstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      perstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      BRW_PERSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_TIDSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      tidstemp.FORNAMN:LABEL IN BROWSE BRW_TIDSEK = "Förnamn" 
      perstemp.FORNAMN:LABEL IN BROWSE BRW_PERSEK = "Förnamn" 
      BRW_PERSEK:TITLE = "Följande får ändra på: " + STRING(FILL-IN-PKOD:SCREEN-VALUE).
      /*BRW_PERSEK:TITLE = BRW_PERSEK:TITLE + ": " + STRING(FILL-IN-PKOD:SCREEN-VALUE).*/
      BRW_TIDSEK:TITLE = "Får ej ändra på".
      brwsortvar = 'perstemp.TYP = "MULTIPERS"'.
      RUN settitlenum_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT TRUE).
      RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
      IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
         IF TOG_APKOD = TRUE THEN brwsortvar = ' WHERE tidstemp.TYP = "MULTIPERS" AND tidstemp.AKTIV = TRUE '.
         ELSE brwsortvar = 'tidstemp.TYP = "MULTIPERS"'.
      END.
      ELSE brwsortvar = 'tidstemp.TYP = "MULTIPERS"'.
      RUN settitlenum_UI IN brwproc[{&LEFT-BROWSE}] (INPUT TRUE).
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT brwsortvar).
      
      FIND FIRST tidstemp WHERE tidstemp.PERSONALKOD = FILL-IN-PKOD AND tidstemp.TYP = "MULTIPERS" NO-ERROR.         
      IF AVAILABLE tidstemp THEN DO:         
         RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(tidstemp)).          
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].          
      END.
      ELSE DO:
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      
      END.
   END.
   IF RAD_ANV = 6 THEN DO:
      ASSIGN
      tidstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      tidstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_TIDSEK = TRUE
      perstemp.PERSONALKOD:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      perstemp.EFTERNAMN:VISIBLE IN BROWSE BRW_PERSEK = TRUE
      BRW_PERSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_TIDSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      tidstemp.FORNAMN:LABEL IN BROWSE BRW_TIDSEK = "Förnamn" 
      perstemp.FORNAMN:LABEL IN BROWSE BRW_PERSEK = "Förnamn" 
      BRW_PERSEK:TITLE = "Följande får ändra på: " + STRING(FILL-IN-PKOD:SCREEN-VALUE).
      BRW_TIDSEK:TITLE = "Får ej ändra på".
      brwsortvar = 'perstemp.TYP = "MULTITID"'.
      RUN settitlenum_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT TRUE).
      RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
      /*brwsortvar = 'tidstemp.TYP = "MULTITID"'.*/
      IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:
         IF TOG_APKOD = TRUE THEN brwsortvar = ' WHERE tidstemp.TYP = "MULTITID" AND tidstemp.AKTIV = TRUE '.
         ELSE brwsortvar = 'tidstemp.TYP = "MULTITID"'.
      END.
      ELSE brwsortvar = 'tidstemp.TYP = "MULTITID"'.
      RUN settitlenum_UI IN brwproc[{&LEFT-BROWSE}] (INPUT TRUE).
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT brwsortvar).

      /*RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      */
      FIND FIRST tidstemp WHERE tidstemp.PERSONALKOD = FILL-IN-PKOD AND tidstemp.TYP = "MULTITID"  NO-ERROR.         
      IF AVAILABLE tidstemp THEN DO:         
         RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(tidstemp)).          
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
         RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].          
      END.
      ELSE DO:
         RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].      
      END.
   END.
   
   IF RAD_ANV = 7 THEN DO:
      ASSIGN                           
      BRW_BOLAGSEK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_BOLAG:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN openbdynspec_UI IN brwproc[5].
      RUN openbdynspec_UI IN brwproc[6].      
   END.
   IF RAD_ANV = 8 THEN DO:
      ASSIGN                           
      BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN openbdynspec_UI IN brwproc[7].
      RUN openbdynspec_UI IN brwproc[8].      
   END.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_APKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_APKOD DIALOG-1
ON VALUE-CHANGED OF TOG_APKOD IN FRAME DIALOG-1 /* Endast aktiva */
DO:
  TOG_APKOD = INPUT TOG_APKOD.
  APPLY "VALUE-CHANGED" TO RAD_ANV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BOLAG
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
   
   status-ok = RAD_ANV:ADD-LAST("ANVÄNDAREN får ändra på pers", 2).
   status-ok = RAD_ANV:ADD-LAST("Får ändra på användarens pers", 5).
   IF Guru.Konstanter:varforetypval[2] = 0 THEN RAD_ANV:ADD-LAST("ANVÄNDAREN får ändra på", 3).
   IF Guru.Konstanter:varforetypval[2] = 0 THEN RAD_ANV:ADD-LAST("Får ändra på användaren", 6).
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN RAD_ANV:ADD-LAST("Fakturering", 4).
   IF Guru.Konstanter:varforetypval[18] = 1 OR Guru.Konstanter:globforetag = CHR(69) + CHR(76) + CHR(80) + CHR(65) THEN RAD_ANV:ADD-LAST("Bolag Sekretess", 7).
   IF Guru.Konstanter:varforetypchar[4] = "ja" THEN RAD_ANV:ADD-LAST(Guru.Konstanter:gomrl + " ansvariga", 8).
   
   ASSIGN 
   bolagsektemp.NAMN:LABEL IN BROWSE BRW_BOLAG = Guru.Konstanter:gjul
   ebolagsektemp.NAMN:LABEL IN BROWSE BRW_BOLAGSEK = Guru.Konstanter:gjul
   omrtemp.OMRADE:LABEL IN BROWSE BRW_OMR = Guru.Konstanter:gomrk
   omrtemp.NAMN:LABEL IN BROWSE BRW_OMR = "Benamning"
   omrtempsek.OMRADE:LABEL IN BROWSE BRW_VOMR = Guru.Konstanter:gomrk
   omrtempsek.NAMN:LABEL IN BROWSE BRW_VOMR = "Benamning".
   BRW_VOMR:TITLE = Guru.Konstanter:gomrl + " som användaren har tillgång till " + Guru.Konstanter:gaok + " Beredningar, Kalkyler och Markvärderingar. ". 
   
   EMPTY TEMP-TABLE personaltemp NO-ERROR. 
   IF NOT AVAILABLE personaltemp THEN FIND FIRST personaltemp NO-ERROR.
   IF NOT AVAILABLE personaltemp THEN DO:   
      RUN allapers_UI IN anvapph (OUTPUT TABLE personaltemp).          
   END.

   FOR EACH personaltemp:
      FIND FIRST anvandartemp WHERE anvandartemp.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE anvandartemp THEN DO:
         CREATE pt2.
         BUFFER-COPY personaltemp TO pt2.
      END.
      ELSE DO:
         
      END.
   END.
   IF vad = 2 THEN DO:
      /*från persreg*/
      pkod = vem.
      RUN hamtaenanv_UI IN anvapph (INPUT pkod,INPUT TRUE,OUTPUT vem,OUTPUT TABLE anvandartemp APPEND).
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = vem NO-LOCK NO-ERROR.
      FILL-IN-PKOD = pkod.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
      IF AVAILABLE personaltemp THEN DO:
         FIND FIRST pt2 WHERE pt2.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE pt2 THEN DO:
            CREATE pt2.
         END.
         
         BUFFER-COPY personaltemp TO pt2.
         FILL-IN_AV-NAMN = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN.     
      END.
   END.
  
   FIND FIRST xgurutemp NO-ERROR.
   IF NOT AVAILABLE xgurutemp THEN RUN allxguru_UI IN anvapph (OUTPUT TABLE xgurutemp).          
   FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = vem NO-LOCK NO-ERROR.
   IF NOT AVAILABLE anvandartemp THEN DO:
      FIND FIRST xgurutemp WHERE xgurutemp.MENYVART = "GURU" NO-ERROR.
      hjalpvar = xgurutemp.AV-LEVEL.
      FIND FIRST xgurutemp WHERE xgurutemp.MENYVART = "GURU" AND xgurutemp.AV-LEVEL > hjalpvar NO-ERROR.
      IF AVAILABLE xgurutemp THEN DO:
         hjalpvar = xgurutemp.AV-LEVEL.
      END.
      CREATE anvandartemp.
      ASSIGN
      anvandartemp.AV-NAMN = FILL-IN_AV-NAMN
      anvandartemp.PERSONALKOD = FILL-IN-PKOD
      anvandartemp.AV-LEVEL = hjalpvar.
   END.
   IF Guru.Konstanter:varforetypchar[4] NE "" THEN DO:
      RUN omrhamtaanv_UI IN anvapph (INPUT anvandartemp.ANVANDARE,OUTPUT TABLE omrtempsek).
      FOR EACH omrtempsek WHERE NO-LOCK,
      EACH omrtemp WHERE omrtemp.OMRADE = omrtempsek.OMRADE:
         BUFFER-COPY omrtemp TO omrtempsek.  
      END.
   END.
   anvtabrec = RECID(anvandartemp).
   hjalpvar = 0.
   FIND FIRST xgurutemp WHERE xgurutemp.MENYVART = "GURU" NO-ERROR.
   IF AVAILABLE xgurutemp THEN DO:
      REPEAT:
         hjalpvar = xgurutemp.AV-LEVEL.
         status-ok = CMB_NIV:ADD-LAST(STRING(xgurutemp.AV-LEVEL)). 
         FIND FIRST xgurutemp WHERE xgurutemp.MENYVART = "GURU" AND xgurutemp.AV-LEVEL > hjalpvar NO-ERROR.
         IF NOT AVAILABLE xgurutemp THEN LEAVE.
      END.
   END.
   ASSIGN
   FILL-IN_ALLPERS:LABEL = "Visa alla personer"
   FILL-IN_ALLAONR:LABEL = "Visa alla " + LC(Guru.Konstanter:gaok).
   status-ok = CMB_NIV:DELETE("0").
   ASSIGN  
   CMB_NIV:SCREEN-VALUE = STRING(anvandartemp.AV-LEVEL).   
   CMB_NIV = INPUT CMB_NIV.
   RUN grundtid_UI. 

   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = FILL-IN-PKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF AVAILABLE personaltemp THEN DO:
      FIND FIRST pt2 WHERE pt2.PERSONALKOD = personaltemp.PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE pt2 THEN DO:
         CREATE pt2.
         BUFFER-COPY personaltemp TO pt2.
      END.
   END.
   
   RUN perssek_UI IN anvapph (INPUT anvandartemp.PERSONALKOD,INPUT vem,INPUT Guru.Konstanter:varforetypval[2],INPUT Guru.Konstanter:varforetypval[3],OUTPUT TABLE perstemp,OUTPUT TABLE tidstemp).
   RUN bolagsekhmt_UI IN anvapph (INPUT vem,OUTPUT TABLE bolagsektemp,OUTPUT TABLE ebolagsektemp).
   IF anvandartemp.PERSONALKOD = "" THEN DO:
      ASSIGN
      FILL-IN_ALLPERS:HIDDEN = TRUE
      FILL-IN_ALLAONR:HIDDEN = TRUE
      FILL-IN_ALLPERS = TRUE
      FILL-IN_ALLAONR = TRUE.
   END. 
   ELSE DO:  

      ASSIGN
      FILL-IN_ALLPERS = anvandartemp.ALLPERS
      FILL-IN_ALLAONR = anvandartemp.ALLAONR.
   END. 
   
   RUN enable_UI.       
   IF Guru.Konstanter:varforetypval[2] = 0 THEN DO:      
      tidstemp.AKTIV:VISIBLE IN BROWSE BRW_TIDSEK = TRUE.
      perstemp.AKTIV:VISIBLE IN BROWSE BRW_PERSEK = TRUE.
   END.   
   ELSE DO:
      tidstemp.AKTIV:VISIBLE IN BROWSE BRW_TIDSEK = FALSE.
      perstemp.AKTIV:VISIBLE IN BROWSE BRW_PERSEK = FALSE.
   END.   
   IF Guru.Konstanter:varforetypval[2] NE 0 THEN FILL-IN-TS:HIDDEN = TRUE.
   IF Guru.Konstanter:varforetypval[2] = 0 THEN  DO:
      TOG_APKOD = TRUE.
      TOG_APKOD:HIDDEN = FALSE.
      DISPLAY TOG_APKOD WITH FRAME {&FRAME-NAME}. 
      ENABLE TOG_APKOD WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE TOG_APKOD:HIDDEN = TRUE.
   RAD_ANV = 1.
   DISPLAY RAD_ANV WITH FRAME {&FRAME-NAME}. 
   ENABLE RAD_ANV WITH FRAME {&FRAME-NAME}. 
   APPLY "VALUE-CHANGED" TO RAD_ANV.
   {FRMSIZED.I}  
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVB:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_OK:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
   my1hand = FILL-IN_ANVANDARE:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN-PKOD:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN-PKOD:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = BRW_PERS:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN-KATE:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN-KATE:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_AV-NAMN:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_AV-NAMN:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_ALLPERS:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_ALLPERS:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = FILL-IN_ALLAONR:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_ALLAONR:HANDLE IN FRAME {&FRAME-NAME}.
   status-ok = CMB_NIV:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = CMB_NIV:HANDLE IN FRAME {&FRAME-NAME}.
   
   status-ok = FILL-IN_AV-NYTTLOSE:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_AV-NYTTLOSE:HANDLE IN FRAME {&FRAME-NAME}.   
    /*  
   status-ok = FILL-IN_AV-LOSEN:MOVE-AFTER-TAB-ITEM(my1hand).
   my1hand = FILL-IN_AV-LOSEN:HANDLE IN FRAME {&FRAME-NAME}.
  */
  
  
   status-ok = BTN_OK:MOVE-AFTER-TAB-ITEM(my1hand).

   perstemp.OMRADENUM:LABEL IN BROWSE BRW_PERSEK = Guru.Konstanter:gomrk.  
   tidstemp.OMRADENUM:LABEL IN BROWSE BRW_TIDSEK = Guru.Konstanter:gomrk.  

   RUN openbdynspec_UI IN brwproc[4].
   IF FILL-IN-PKOD = "" THEN.
   ELSE DO:
      FIND FIRST pt2 WHERE pt2.PERSONALKOD = FILL-IN-PKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE pt2 THEN DO:
         FIND FIRST pt2 USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
      END.
      IF AVAILABLE pt2 THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(pt2)).
         RUN lastselectdyn_UI IN brwproc[4]. 
      END.
   END.   
   FILL-IN-FAKT:HIDDEN = TRUE.
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN FILL-IN-FAKT:HIDDEN = FALSE.
   IF Guru.Konstanter:varforetypval[18] = 1 OR Guru.Konstanter:globforetag = CHR(69) + CHR(76) + CHR(80) + CHR(65) THEN FILL-IN-FAKT:HIDDEN = FALSE. 
   /*
   Guru.Konstanter:LabelFlytt(FILL-IN_ANVANDARE:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN-PKOD:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN-KATE:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN_AV-NAMN:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN_ALLPERS:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN_ALLAONR:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN_AV-NYTTLOSE:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN-TS:HANDLE).
   Guru.Konstanter:LabelFlytt(FILL-IN-FAKT:HANDLE).
   Guru.Konstanter:LabelFlytt(CMB_NIV:HANDLE).
   */
   {musarrow.i}  
   {DIA_M_SLUT.I}
   
   
   IF FILL-IN_ANVANDARE NE "" OR vad = 2 THEN DISABLE /*FILL-IN-PKOD*/ BRW_PERS WITH FRAME DIALOG-1.
   
    
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
   pt2.PERSONALKOD:READ-ONLY IN BROWSE BRW_PERS = TRUE
   tidstemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_TIDSEK = TRUE
   perstemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_PERSEK = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[4]   (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] (INPUT BRW_TIDSEK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] (INPUT BRW_PERSEK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN settitlenum_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[{&LEFT-BROWSE}] (INPUT TRUE).  
   RUN setpdatacolsort_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "EFTERNAMN",INPUT "perstemp.EFTERNAMN BY perstemp.PERSONALKOD").
   RUN setpdatacolsort_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "FORNAMN",INPUT "perstemp.FORNAMN BY perstemp.PERSONALKOD").
   RUN setpdatacolsort_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "PERSONALKOD",INPUT "perstemp.PERSONALKOD BY perstemp.OMRADE").
   RUN setpdatacolsort_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "OMRADE",INPUT "perstemp.OMRADE BY perstemp.PERSONALKOD").
   RUN setpdatacolsort_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "EFTERNAMN",INPUT "tidstemp.EFTERNAMN BY tidstemp.PERSONALKOD").
   RUN setpdatacolsort_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "FORNAMN",INPUT "tidstemp.FORNAMN BY tidstemp.PERSONALKOD").
   RUN setpdatacolsort_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "PERSONALKOD",INPUT "tidstemp.PERSONALKOD BY tidstemp.OMRADE").
   RUN setpdatacolsort_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "OMRADE",INPUT "tidstemp.OMRADE BY tidstemp.PERSONALKOD").     
   /*
   RUN startsearchproc IN brwproc[{&LEFT-BROWSE}].
   RUN startsearchproc IN brwproc[{&RIGHT-BROWSE}].
   */
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_BOLAGSEK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[6] (INPUT BRW_BOLAG:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[7] (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[8] (INPUT BRW_VOMR:HANDLE IN FRAME {&FRAME-NAME}).
   
   
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN NIVPERAPP.P PERSISTENT SET nivapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NIVPERAPP.P PERSISTENT SET nivapph.
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
  DISPLAY FILL-IN_ANVANDARE FILL-IN-PKOD FILL-IN-KATE FILL-IN_AV-NAMN 
          FILL-IN_ALLPERS FILL-IN_ALLAONR FILL-IN_AV-NYTTLOSE FILL-IN-TS 
          FILL-IN-FAKT 
      WITH FRAME DIALOG-1.
  ENABLE TOG_APKOD FILL-IN_ANVANDARE BRW_TIDSEK BRW_BOLAGSEK BRW_PERS 
         BRW_PERSEK BRW_BOLAG BRW_VOMR BRW_OMR FILL-IN-PKOD BTN_ALLOVER 
         FILL-IN-KATE FILL-IN_AV-NAMN BTN_OVER FILL-IN_ALLPERS BTN_BACK 
         FILL-IN_ALLAONR BTN_VISA CMB_NIV BTN_ALLBACK FILL-IN_AV-NYTTLOSE 
         BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
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
   ASSIGN
   BTN_VISA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_BOLAGSEK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_BOLAG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_PERSEK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_TIDSEK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_NIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-LASTNIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_ALLAONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_ALLPERS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_ANVANDARE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_AV-NYTTLOSE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_AV-LOSEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_AV-NAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-KATE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-PKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
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
   FILL-IN-KATE = anvandartemp.KATEGORI
   FILL-IN-PKOD = anvandartemp.PERSONALKOD
   spar_ANVANDARE = anvandartemp.ANVANDARE    
   FILL-IN_ANVANDARE = anvandartemp.ANVANDARE    
   FILL-IN_AV-LOSEN = anvandartemp.AV-LOSEN
   FILL-IN_AV-NAMN = anvandartemp.AV-NAMN.
   
   RUN LastNiv_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LastNiv_UI DIALOG-1 
PROCEDURE LastNiv_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF NOT VALID-HANDLE(edataapph) THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      END.
      ELSE DO:
         RUN EXTRADATAHMT.P PERSISTENT SET edataapph.   
      END.
   END.   
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AVLEVANM"                 
   inextradatatemp.HUVUDCH = ?              
   inextradatatemp.HUVUDINT =  CMB_NIV.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
  
   IF AVAILABLE extradatatemp THEN DO:
      IF extradatatemp.SOKLOG[1] = TRUE THEN  DO:
         FILL-IN-LASTNIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
         DISABLE CMB_NIV  WITH FRAME {&FRAME-NAME}.
         DISPLAY FILL-IN-LASTNIV WITH FRAME {&FRAME-NAME}.
      END.   
      ELSE FILL-IN-LASTNIV:HIDDEN = TRUE. 
      IF vem = "" THEN DO:
         ENABLE CMB_NIV  WITH FRAME {&FRAME-NAME}.
         FILL-IN-LASTNIV:HIDDEN = TRUE.
      END.   
      Guru.GlobalaVariabler:StartKolumnForKnappar = CMB_NIV:COLUMN + CMB_NIV:WIDTH-CHARS + 0.1. 
      
      Guru.Konstanter:PlaceraKnapparVagratt(FILL-IN-LASTNIV:HANDLE,TRUE).
      Guru.Konstanter:PlaceraKnapparVagratt(BTN_VISA:HANDLE,TRUE).
     
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

