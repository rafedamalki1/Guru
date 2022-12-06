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
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{AVDTEMP.I}
{ALLDEF.I}
&Scoped-define NEW
{UPPGHMT.I}
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMPW.I}
&Scoped-define SHARED SHARED
{DIRDEF.I}
{FLEXTAB.I}
{PHMT.I}
{SOKDEF.I}
DEFINE NEW SHARED VARIABLE reco AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE bustart3 AS DECIMAL  NO-UNDO.   
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE bereavt AS CHARACTER NO-UNDO.
DEFINE VARIABLE berplus AS INTEGER NO-UNDO.
DEFINE VARIABLE bjour AS LOGICAL NO-UNDO.
DEFINE VARIABLE utdatum AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE valgubbe AS LOGICAL NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.                     /* NYTTAOAPP.P */
DEFINE VARIABLE omravdand AS INTEGER NO-UNDO.
DEFINE VARIABLE bavt AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE bertemp
   FIELD FORETAG AS CHARACTER
   FIELD ANVANDARE AS CHARACTER
   FIELD DAG2 AS CHARACTER    
   FIELD DAG AS CHARACTER
   FIELD PKOD AS CHARACTER 
   FIELD SLUT2 AS DECIMAL  
   FIELD VECKA2 AS INTEGER
   FIELD START AS DECIMAL
   FIELD VECKA AS INTEGER
   FIELD DATUM1 AS DATE
   FIELD DATUM2 AS DATE
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD BERBEORD AS LOGICAL
   FIELD BDATUM AS DATE 
   FIELD AVDATUM AS DATE
   FIELD JOUR AS LOGICAL
   FIELD FAKTOR AS LOGICAL
   FIELD BERALT AS INTEGER.

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
&Scoped-define INTERNAL-TABLES utsokaonr tidpers

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
&Scoped-Define ENABLED-OBJECTS FILL-IN-START FILL-IN-SLUT-2 ~
FILL-IN_BERBEORD FILL-IN_FAKTOR FILL-IN-AONR FILL-IN-DELNR BTN_REG ~
BTN_NAAVB BTN_AVB CMB_OMR CMB_AVD BTN_SNABB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PKOD FILL-IN-START FILL-IN-SLUT-2 ~
FILL-IN_BERBEORD FILL-IN_FAKTOR FILL-IN-AONR FILL-IN-DELNR FILL-IN-TEXT ~
CMB_OMR CMB_AVD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
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
     LABEL "ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SNABB 
     LABEL "Spara":L 
     SIZE 14 BY 1 TOOLTIP "Sparar registrering men står kvar i bilden för ytterligare registrering".

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
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PKOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUT-2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Slut tid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTDAT AS DATE FORMAT "9999/99/99":U 
     LABEL "Slut datum" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKPA AS CHARACTER FORMAT "X(7)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Start tid" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "9999/99/99":U 
     LABEL "Start datum" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Visa aonr för:" 
     VIEW-AS FILL-IN 
     SIZE 22.63 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_BERBEORD AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Beordrad" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_FAKTOR AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Plus 30%" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1.

DEFINE VARIABLE FILL-IN_ORTS AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_AB AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "A-beredskap", 1,
"B-beredskap", 2
     SIZE 61 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 29.75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-SOK
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52.75 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE TOG_JOUR AS LOGICAL INITIAL no 
     LABEL "Jour" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 52.75 BY 11.83
         TITLE "Aktiva arbetsordernummer".

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
    WITH NO-ROW-MARKERS SIZE 47.5 BY 12.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_AONR AT ROW 5.33 COL 37
     FILL-IN-PKOD AT ROW 3.5 COL 13.5 COLON-ALIGNED
     FILL-IN-STARTDAT AT ROW 5.42 COL 13.5 COLON-ALIGNED
     FILL-IN-START AT ROW 7.21 COL 13.5 COLON-ALIGNED
     FILL-IN-SLUTDAT AT ROW 9.13 COL 13.5 COLON-ALIGNED
     FILL-IN-SLUT-2 AT ROW 10.96 COL 13.5 COLON-ALIGNED
     FILL-IN_BERBEORD AT ROW 12.88 COL 13.5 COLON-ALIGNED
     FILL-IN_FAKTOR AT ROW 14.79 COL 13.5 COLON-ALIGNED
     FILL-IN-AONR AT ROW 16.71 COL 13.5 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 18.5 COL 13.5 COLON-ALIGNED
     BTN_REG AT ROW 20.92 COL 75.75
     BTN_NAAVB AT ROW 7.13 COL 91.75
     BTN_AVB AT ROW 20.92 COL 91.75
     FILL-IN_AONRS AT ROW 18.5 COL 62.25 COLON-ALIGNED
     FILL-IN_ORTS AT ROW 19.58 COL 62.25 COLON-ALIGNED
     FILL-IN-SOKPA AT ROW 18.5 COL 36.13 COLON-ALIGNED NO-LABEL
     TOG_JOUR AT ROW 1.88 COL 38.25
     RAD_FAST AT ROW 4.08 COL 37.25 NO-LABEL
     FILL-IN-TEXT AT ROW 1.88 COL 65.13 COLON-ALIGNED NO-LABEL
     CMB_OMR AT ROW 4.08 COL 65.25 COLON-ALIGNED NO-LABEL
     BRW_PERS AT ROW 5.33 COL 42.25
     BTN_NVE-3 AT ROW 5.08 COL 25.75
     BTN_FVE-3 AT ROW 5.96 COL 25.75
     BTN_NVE-4 AT ROW 8.92 COL 25.75
     BTN_FVE-4 AT ROW 9.79 COL 25.75
     RAD_AB AT ROW 1.88 COL 1.5 NO-LABEL
     CMB_AVD AT ROW 2.88 COL 65.25 COLON-ALIGNED NO-LABEL
     BTN_SNABB AT ROW 20.92 COL 60.63 WIDGET-ID 2
     RECT-SOK AT ROW 18.29 COL 37
     SPACE(16.24) SKIP(1.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Beredskapsregistrering":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: tidpers T "?" NO-UNDO temp-db tidpers
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB BRW_AONR 1 DIALOG-1 */
/* BROWSE-TAB BRW_PERS CMB_OMR DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR BROWSE BRW_AONR IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       BRW_AONR:HIDDEN  IN FRAME DIALOG-1                = TRUE
       BRW_AONR:MAX-DATA-GUESS IN FRAME DIALOG-1         = 1000.

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

/* SETTINGS FOR FILL-IN FILL-IN-PKOD IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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

/* SETTINGS FOR FILL-IN FILL-IN_ORTS IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_ORTS:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_AB IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_AB:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-SOK IN FRAME DIALOG-1
   NO-ENABLE                                                            */
ASSIGN 
       RECT-SOK:HIDDEN IN FRAME DIALOG-1           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_JOUR IN FRAME DIALOG-1
   NO-DISPLAY NO-ENABLE                                                 */
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

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Beredskapsregistrering */
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END.
   FOR EACH tidpers:
      tidpers.REGKOLL = FALSE.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Beredskapsregistrering */
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


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS DIALOG-1
ON VALUE-CHANGED OF BRW_PERS IN FRAME DIALOG-1
DO:
   FILL-IN-PKOD = tidpers.PERSONALKOD.      
   persrec = tidpers.TIDPERSREC.
   valgubbe = TRUE.
   RUN nasta_UI.
   valgubbe = FALSE.
   musz = FALSE.      
   {musarrow.i}  
   DISPLAY FILL-IN-PKOD FILL-IN-START FILL-IN-SLUT-2 WITH FRAME {&FRAME-NAME}.
   RUN form_UI.      
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   FOR EACH tidpers:
      tidpers.REGKOLL = FALSE.
   END.
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON GO OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
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


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 DIALOG-1
ON CHOOSE OF BTN_FVE-3 IN FRAME DIALOG-1 /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1. 
   IF tillochmeddatum NE ? THEN DO:
     IF tillochmeddatum >= FILL-IN-STARTDAT THEN DO:
         FILL-IN-STARTDAT = tillochmeddatum + 1.
      END.
   END.
   RUN form_UI.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
   RUN lulekoll_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 DIALOG-1
ON CHOOSE OF BTN_FVE-4 IN FRAME DIALOG-1 /* - */
DO:   
   ASSIGN
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.   
   FILL-IN-SLUTDAT = FILL-IN-SLUTDAT - 1.      
   IF tillochmeddatum NE ? THEN DO:
     IF tillochmeddatum >= FILL-IN-SLUTDAT THEN DO:
         FILL-IN-SLUTDAT = tillochmeddatum + 1.
      END.
   END.
   RUN form_UI.   
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB DIALOG-1
ON CHOOSE OF BTN_NAAVB IN FRAME DIALOG-1 /* Nästa person */
DO:   
   RUN nasta_UI.
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "CHOOSE" TO BTN_AVB.
   END.
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
   RUN form_UI.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
   RUN lulekoll_UI.
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
   RUN form_UI.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_REG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_REG DIALOG-1
ON CHOOSE OF BTN_REG IN FRAME DIALOG-1 /* ok */
DO:
   RUN btnreg_UI.
   APPLY "GO" TO BTN_REG IN FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SNABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SNABB DIALOG-1
ON CHOOSE OF BTN_SNABB IN FRAME DIALOG-1 /* Spara */
DO:  
   RUN btnreg_UI.
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


&Scoped-define SELF-NAME FILL-IN-AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON ENTRY OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:                          
   ASSIGN
   FILL-IN-SOKPA:HIDDEN = FALSE
   RECT-SOK:HIDDEN = FALSE   
   BRW_PERS:HIDDEN = TRUE       
   CMB_AVD:HIDDEN = FALSE
   CMB_OMR:HIDDEN = FALSE
   FILL-IN-TEXT:HIDDEN = FALSE
   FILL-IN-SOKPA:HIDDEN = TRUE.
   ASSIGN 
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:"
   FILL-IN-SOKPA = "Sök på:". 
   DISPLAY FILL-IN-TEXT FILL-IN-SOKPA WITH FRAME {&FRAME-NAME}.   
   ENABLE FILL-IN_AONRS FILL-IN_ORTS RAD_FAST WITH FRAME {&FRAME-NAME}.  
   ASSIGN
   RAD_FAST:HIDDEN = FALSE   
   FILL-IN_AONRS:HIDDEN = FALSE 
   FILL-IN_ORTS:HIDDEN = FALSE.
   FIND FIRST utsokaonr WHERE utsokaonr.AONR = FILL-IN-AONR AND utsokaonr.DELNR = FILL-IN-DELNR AND
   utsokaonr.AONRAVDATUM = 01/01/1991 USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE utsokaonr THEN DO:   
      IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = TRUE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN NO-ERROR.                        
         IF CMB_OMR:SCREEN-VALUE = ? THEN DO:
            CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         END.
      END.
      ELSE IF Guru.Konstanter:globallao = FALSE AND utsokaonr.FASTAAONR = FALSE AND utsokaonr.OMRADE = " " THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.           
         aonrrec = RECID(utsokaonr).
         RAD_FAST = utsokaonr.FASTAAONR.
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
      END.
      ASSIGN
      aonrrec = RECID(utsokaonr)
      RAD_FAST = utsokaonr.FASTAAONR.
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
      RAD_FAST = FALSE.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.
   ENABLE BRW_AONR WITH FRAME {&FRAME-NAME}.
   BRW_AONR:HIDDEN = FALSE. 
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AONR DIALOG-1
ON LEAVE OF FILL-IN-AONR IN FRAME DIALOG-1 /* Aonr */
DO:
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELNR DIALOG-1
ON LEAVE OF FILL-IN-DELNR IN FRAME DIALOG-1 /* Delnr */
DO:  
   ASSIGN
   FILL-IN-AONR = INPUT FILL-IN-AONR
   FILL-IN-DELNR = INPUT FILL-IN-DELNR.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT-2 DIALOG-1
ON LEAVE OF FILL-IN-SLUT-2 IN FRAME DIALOG-1 /* Slut tid */
DO:
  
   FILL-IN-SLUT-2 = INPUT FILL-IN-SLUT-2.
   IF FILL-IN-SLUT-2 > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END. 
    IF SUBSTRING(STRING(FILL-IN-SLUT-2,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   IF FILL-IN-STARTDAT = FILL-IN-SLUTDAT AND FILL-IN-SLUT-2 < FILL-IN-START THEN DO:
      MESSAGE "Start kan inte vara större än slut." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUT-2 DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-SLUT-2 IN FRAME DIALOG-1 /* Slut tid */
DO:
   klocka = INPUT FILL-IN-SLUT-2.
   {AVBGOMD.I}
   RUN KLOCKAN.W.
   {AVBFRAMD.I}
   IF klocka = 00.00 THEN klocka = 24.00.
   FILL-IN-SLUT-2 = klocka.
   DISPLAY FILL-IN-SLUT-2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON ENTRY OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Slut datum */
DO:
   regdatum = FILL-IN-STARTDAT + berplus.
   RUN REGVEC.P.     
   FILL-IN-SLUTDAT = regdatum.                  
   RUN form_UI.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTDAT DIALOG-1
ON LEAVE OF FILL-IN-SLUTDAT IN FRAME DIALOG-1 /* Slut datum */
DO:
  FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.
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
   RUN form_UI.
   DISPLAY FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-1
ON LEAVE OF FILL-IN-START IN FRAME DIALOG-1 /* Start tid */
DO:
   FILL-IN-START = INPUT FILL-IN-START.
   IF FILL-IN-START > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-START DIALOG-1
ON MOUSE-MENU-CLICK OF FILL-IN-START IN FRAME DIALOG-1 /* Start tid */
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


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON ENTRY OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Start datum */
DO:
   RUN form_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT DIALOG-1
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DIALOG-1 /* Start datum */
DO:

   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   RUN lulekoll_UI.
   
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
   RUN form_UI.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
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
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "AONR", INPUT FILL-IN_AONRS).
   RUN fillinupdate_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BERBEORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BERBEORD DIALOG-1
ON LEAVE OF FILL-IN_BERBEORD IN FRAME DIALOG-1 /* Beordrad */
DO:
   FILL-IN_BERBEORD = INPUT FILL-IN_BERBEORD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BERBEORD DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_BERBEORD IN FRAME DIALOG-1 /* Beordrad */
DO:
   IF INPUT FILL-IN_BERBEORD = TRUE THEN FILL-IN_BERBEORD = FALSE.
   IF INPUT FILL-IN_BERBEORD = FALSE THEN FILL-IN_BERBEORD = TRUE.
   DISPLAY FILL-IN_BERBEORD WITH FRAME {&FRAME-NAME}.  
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKTOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKTOR DIALOG-1
ON LEAVE OF FILL-IN_FAKTOR IN FRAME DIALOG-1 /* Plus 30% */
DO:
   FILL-IN_FAKTOR = INPUT FILL-IN_FAKTOR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKTOR DIALOG-1
ON MOUSE-SELECT-CLICK OF FILL-IN_FAKTOR IN FRAME DIALOG-1 /* Plus 30% */
DO:
   IF INPUT FILL-IN_FAKTOR = TRUE THEN FILL-IN_FAKTOR = FALSE.
   IF INPUT FILL-IN_FAKTOR = FALSE THEN FILL-IN_FAKTOR = TRUE.
   DISPLAY FILL-IN_FAKTOR WITH FRAME {&FRAME-NAME}.  
   
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
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "ORT", INPUT FILL-IN_ORTS).
   RUN fillinupdate_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_AB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_AB DIALOG-1
ON VALUE-CHANGED OF RAD_AB IN FRAME DIALOG-1
DO:
   RAD_AB = INPUT RAD_AB.           
   DISPLAY RAD_AB WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST DIALOG-1
ON VALUE-CHANGED OF RAD_FAST IN FRAME DIALOG-1
DO:
   RAD_FAST = INPUT RAD_FAST.   
   IF RAD_FAST = FALSE THEN DO:
      CMB_OMR = sparomrade.
      CMB_OMR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = sparomrade.
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR 
      USE-INDEX OMRNAMN NO-LOCK NO-ERROR.
   END.  
   IF  Guru.Konstanter:globforetag = "ELPA" OR  Guru.Konstanter:globforetag = "GKAL" THEN DO:
      IF RAD_FAST = TRUE THEN DO:
         ASSIGN 
         sparomrade = CMB_OMR. 
         CMB_OMR:SCREEN-VALUE = Guru.Konstanter:gomrk + " : alla".
         CMB_OMR = INPUT CMB_OMR.      
      END.
   END.
   RUN nycolsortprep_UI (INPUT 1).
   RUN openbdynspec_UI IN brwproc[1].
   IF AVAILABLE utsokaonr THEN DO:   
      status-ok = BRW_AONR:DESELECT-FOCUSED-ROW() NO-ERROR.
   END.
   DISPLAY RAD_FAST WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_JOUR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_JOUR DIALOG-1
ON VALUE-CHANGED OF TOG_JOUR IN FRAME DIALOG-1 /* Jour */
DO:
   TOG_JOUR = INPUT TOG_JOUR.
   bjour = TRUE.
   RUN nastapers_UI.
   bjour = FALSE.   
   DISPLAY TOG_JOUR WITH FRAME {&FRAME-NAME}.


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
   {muswait.i}        
   ASSIGN   
   BRW_AONR:TITLE = "Aktiva " + LC(Guru.Konstanter:gaol)
   FILL-IN_AONRS:LABEL = Guru.Konstanter:gaok 
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok 
   FILL-IN-TEXT = "Visa " + LC(Guru.Konstanter:gaok) + " för:". 
   {TILLFAST.I}
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
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod NO-LOCK NO-ERROR.   
   tidtabrec = 0.       
   bjour = FALSE.
   RUN enable_UI.         

   ASSIGN RAD_AB = 2.
   ASSIGN TOG_JOUR = FALSE. 
   RUN nasta_UI.
   IF musz = TRUE THEN DO: 
      status-mus2 = SESSION:SET-WAIT-STATE("").
      musz = FALSE.
      LEAVE MAIN-BLOCK.                                         
   END.
   FIND FIRST tidpers NO-ERROR.
   IF NOT AVAILABLE tidpers THEN DO:
      LEAVE MAIN-BLOCK.                                           
   END.
   IF varifran = 1 THEN DO:           
      ASSIGN regdatum = TODAY.
   END.
   
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.      
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
   IF  Guru.Konstanter:globforetag = "cSUND" THEN DO:
      FIND FIRST utsokaonr WHERE utsokaonr.AONR = "03011" AND 
      utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
      IF AVAILABLE utsokaonr THEN DO:
         ASSIGN FILL-IN-AONR = "03011" FILL-IN-DELNR = 0.
      END.           
   END.
   IF  Guru.Konstanter:globforetag = "LULE" THEN DO:
      IF bereavt BEGINS "M" THEN DO:      
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "003005" 
         AND utsokaonr.DELNR = 1 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "003005" FILL-IN-DELNR = 1.
            DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         END.         
      END.
      ELSE IF bereavt BEGINS "P" THEN DO:      
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "241" 
         AND utsokaonr.DELNR = 0 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "241" FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         END.         
      END.
      ELSE IF bereavt BEGINS "F" THEN DO:      
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "245" 
         AND utsokaonr.DELNR = 0 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "245" FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         END.         
      END.
      ELSE IF personaltemp.OMRADE = "LB70" THEN DO:         
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "940" AND 
         utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "940" FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         END.         
      END.
      ELSE DO:
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "100293" AND 
         utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "100293" FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR  FILL-IN-DELNR WITH FRAME {&FRAME-NAME}.
         END.         
      END.
   END.
   IF  Guru.Konstanter:globforetag = "cELPA" THEN DO:
      IF bereavt = "TM" THEN DO:      
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "144013" 
         AND utsokaonr.DELNR = 0 AND utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "144013" FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.   
         END.         
      END.
      ELSE DO:
         FIND FIRST utsokaonr WHERE utsokaonr.AONR = "144015" AND 
         utsokaonr.AONRAVDATUM = 01/01/91 NO-LOCK NO-ERROR.
         IF AVAILABLE utsokaonr THEN DO:
            ASSIGN FILL-IN-AONR = "144015" FILL-IN-DELNR = 0.
            DISPLAY FILL-IN-AONR WITH FRAME {&FRAME-NAME}.   
         END.         
      END.
   END.  
   IF varifran = 1 THEN DO:
      OPEN QUERY BRW_PERS 
      FOR EACH tidpers WHERE USE-INDEX PERSONALKOD NO-LOCK.  
      ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}. 
      RELEASE tidpers NO-ERROR.
   END.
   ELSE IF varifran = 2 THEN DO:
      BTN_NAAVB:HIDDEN = TRUE.
   END. 
   ASSIGN       
   CMB_AVD:HIDDEN = TRUE
   CMB_OMR:HIDDEN = TRUE
   FILL-IN-TEXT:HIDDEN = TRUE.   
   IF  Guru.Konstanter:globforetag = "LULE" THEN DO:
      musz = musz.
   END.
   ELSE DO:
      ASSIGN
      RAD_FAST:HIDDEN = TRUE
      FILL-IN-AONR:HIDDEN = TRUE 
      FILL-IN-DELNR:HIDDEN = TRUE.
   END.
   musz = FALSE.   
   ASSIGN FILL-IN_FAKTOR:HIDDEN = TRUE. 
    
   IF Guru.Konstanter:globforetag = "SUND" OR  Guru.Konstanter:globforetag = "SNAT" OR  Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "GKAL"  THEN DO:      
      ASSIGN FILL-IN_BERBEORD:HIDDEN = TRUE. 
   END.  
   IF  Guru.Konstanter:globforetag = "MISV" OR  Guru.Konstanter:globforetag = "ELPA"  THEN DO:      
      FILL-IN_BERBEORD = TRUE.
      ASSIGN FILL-IN_BERBEORD:HIDDEN = FALSE. 
      DISPLAY FILL-IN_BERBEORD WITH FRAME {&FRAME-NAME}.
   END.  
   IF personaltemp.OMRADE NE omrtemp.OMRADE THEN CMB_OMR = Guru.Konstanter:gomrk + " : alla".
   ASSIGN CMB_OMR:SCREEN-VALUE = CMB_OMR.   
   IF varifran = 1 THEN DO:
      DISPLAY BRW_PERS WITH FRAME {&FRAME-NAME}.   
      status-ok = BRW_PERS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   END.
     
   TOG_JOUR:HIDDEN = TRUE.     
           
   IF  Guru.Konstanter:globforetag = "cSNAT" AND personaltemp.BEREDSKAPSAVTAL = "T" THEN DO:   
      /*släckt 20211110  Victoria Rosengren*/
      RAD_AB = 2.
      status-ok = RAD_AB:DELETE("A-beredskap").
      status-ok = RAD_AB:DELETE("B-beredskap").      
      RAD_AB:ADD-LAST("Vanlig beredskap", 2 ). 
      RAD_AB:ADD-LAST("Bredband-beredskap", 3).      
      ASSIGN RAD_AB = 2.            
      ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.      
      ENABLE RAD_AB WITH FRAME {&FRAME-NAME}.
      DISPLAY RAD_AB WITH FRAME {&FRAME-NAME}.
      ASSIGN RAD_AB:HIDDEN = FALSE.            
   END.      
   ELSE RAD_AB:HIDDEN = TRUE.     
   ENABLE BRW_PERS WITH FRAME {&FRAME-NAME}.  
   IF varifran = 2 THEN DO:
      BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   {FRMSIZED.I} 
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).            
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN sattindex_UI IN brwproc[1] (INPUT "OMRADE").
   IF Guru.Konstanter:appcon THEN DO:      
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
   END.
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   RUN dynprogextra IN brwproc[1]  (INPUT "omrvisa_UI",INPUT THIS-PROCEDURE).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beredkoll_UI DIALOG-1 
PROCEDURE beredkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER bertyp AS INTEGER NO-UNDO. 
   /*KOLL ATT INTE BEREDSKAP FINNS SEDAN TIDIGARE */   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN BEREDKOLL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT pkod,INPUT FILL-IN-START,INPUT FILL-IN-SLUT-2,INPUT FILL-IN-STARTDAT,INPUT FILL-IN-SLUTDAT,INPUT bertyp,OUTPUT musz,OUTPUT utdatum).
   END.
   ELSE DO:
      RUN BEREDKOLL.P 
      (INPUT pkod,INPUT FILL-IN-START,INPUT FILL-IN-SLUT-2,INPUT FILL-IN-STARTDAT,INPUT FILL-IN-SLUTDAT,INPUT bertyp,OUTPUT musz,OUTPUT utdatum).
   END.
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
      
   FILL-IN-PKOD = INPUT FRAME {&FRAME-NAME} FILL-IN-PKOD. 
   IF FILL-IN-PKOD = "" THEN DO:
      MESSAGE "Enhet/Sign kan inte vara blank." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.   
   FIND FIRST tidpers WHERE tidpers.PERSONALKOD = FILL-IN-PKOD 
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.  
   IF NOT AVAILABLE personaltemp THEN DO:
      MESSAGE "Enhet/Sign" FILL-IN-PKOD "finns inte eller är inaktiv." VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-PKOD IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.
   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   FILL-IN-SLUTDAT = INPUT FILL-IN-SLUTDAT.               
   IF FILL-IN-STARTDAT > FILL-IN-SLUTDAT THEN DO:
      MESSAGE "Felaktigt angivet datum. Startdatum kan ej vara större än slutdatum."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
   END.  
   IF FILL-IN-STARTDAT + 13 < FILL-IN-SLUTDAT THEN DO:
      MESSAGE "Skall beredskap verkligen registreras för" FILL-IN-SLUTDAT - FILL-IN-STARTDAT "dagar ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
      CASE val:
         WHEN FALSE THEN DO:  
            status-mus2 = SESSION:SET-WAIT-STATE("").
            APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
            APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
         END.         
      END CASE.   
   END.
   
   /*START DATUM*/
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
         MESSAGE "Felaktigt angivet datum. Tidsedeln är godkänd till och med"
         tillochmeddatum VIEW-AS ALERT-BOX.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-STARTDAT IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.            
   END.
   /*STLUT DATUM*/
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
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "ENTRY" TO FILL-IN-SLUTDAT IN FRAME {&FRAME-NAME}.
         APPLY "ENDKEY" TO BTN_REG IN FRAME {&FRAME-NAME}.
      END.            
   END.  
   /*DATUM MELLAN START - SLUT*/
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
               status-mus2 = SESSION:SET-WAIT-STATE("").
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
      musz = FALSE.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN NO-APPLY.
   END.                              
   regdatum = FILL-IN-STARTDAT.
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN
   regdatum = FILL-IN-SLUTDAT.
   RUN REGVEC.P.
   RUN REGDAG.P.
   ASSIGN  
   FILL-IN-SLUT-2 = INPUT FILL-IN-SLUT-2    
   FILL-IN-START = INPUT FILL-IN-START   
   FILL-IN_BERBEORD = INPUT FILL-IN_BERBEORD.
   RUN reg_UI.    
   tidpers.REGKOLL = TRUE.   
   {musarrow.i}   
   
   
      
   
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
  DISPLAY FILL-IN-PKOD FILL-IN-START FILL-IN-SLUT-2 FILL-IN_BERBEORD 
          FILL-IN_FAKTOR FILL-IN-AONR FILL-IN-DELNR FILL-IN-TEXT CMB_OMR CMB_AVD 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-START FILL-IN-SLUT-2 FILL-IN_BERBEORD FILL-IN_FAKTOR 
         FILL-IN-AONR FILL-IN-DELNR BTN_REG BTN_NAAVB BTN_AVB CMB_OMR CMB_AVD 
         BTN_SNABB 
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
   IF AVAILABLE utsokaonr THEN DO:
      ASSIGN
      FILL-IN-AONR = utsokaonr.AONR
      FILL-IN-DELNR = utsokaonr.DELNR.
      DISPLAY FILL-IN-AONR FILL-IN-DELNR WITH FRAME {&FRAME-NAME}. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE form_UI DIALOG-1 
PROCEDURE form_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lulekoll_UI DIALOG-1 
PROCEDURE lulekoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF  Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "ELPA"  THEN DO:                  
      bavt = bereavt.
      IF  Guru.Konstanter:globforetag = "LULE"  THEN DO:
         IF FILL-IN-STARTDAT GE DATE(05,15,YEAR(FILL-IN-STARTDAT)) AND FILL-IN-STARTDAT < DATE(09,01,YEAR(FILL-IN-STARTDAT)) THEN DO:
            IF bereavt =  "MV" THEN bereavt = "ms".
            FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt USE-INDEX BSTART NO-LOCK NO-ERROR.
         END.
         ELSE IF personaltemp.BEREDSKAPSAVTAL = "MV" THEN DO:
            bereavt =  "MV".            
            FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt USE-INDEX BSTART NO-LOCK NO-ERROR.
         END.
      END.
      IF  Guru.Konstanter:globforetag = "elpa" THEN DO:
         IF FILL-IN-STARTDAT GE DATE(05,15,YEAR(FILL-IN-STARTDAT)) AND FILL-IN-STARTDAT < DATE(09,01,YEAR(FILL-IN-STARTDAT)) THEN DO:
            IF bereavt =  "TM" THEN bereavt = "TN".
            FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt USE-INDEX BSTART NO-LOCK NO-ERROR.
         END.
         ELSE  IF personaltemp.BEREDSKAPSAVTAL = "TM" THEN DO: 
            bereavt =  "TM".            
            FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt USE-INDEX BSTART NO-LOCK NO-ERROR.
         END.
      END.   
      IF  Guru.Konstanter:globforetag = "ELPA" AND bereavt = "tm" THEN ASSIGN berplus = 1.
      IF  Guru.Konstanter:globforetag = "ELPA" AND bereavt = "tn" THEN ASSIGN berplus = 0.
      IF  Guru.Konstanter:globforetag = "LULE" AND bereavt = "MV" THEN ASSIGN berplus = 1.
      IF  Guru.Konstanter:globforetag = "LULE" AND bereavt = "MS" THEN ASSIGN berplus = 0.            
      IF bavt NE bereavt THEN DO:      
         regdatum = FILL-IN-STARTDAT + berplus.
         RUN REGVEC.P.
         regdagnamn = beredskapstarttemp.BEREDSKAPDAGSLUT.   
         FILL-IN-SLUTDAT = regdatum.   
         RUN form_UI.               
      
         DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
         ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
         WITH FRAME {&FRAME-NAME}.   
         ASSIGN
         vart = "BER"
         FILL-IN-SLUT-2 = beredskapstarttemp.BEREDSKAPSLUT 
         FILL-IN-START = beredskapstarttemp.BEREDSKAPSTART.         
         DISPLAY FILL-IN-START FILL-IN-SLUT-2 WITH FRAME {&FRAME-NAME}.
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nastapers_UI DIALOG-1 
PROCEDURE nastapers_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   berplus = 7.
      
   IF bjour = TRUE THEN DO:
      IF TOG_JOUR = FALSE THEN ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.
      IF TOG_JOUR = TRUE THEN ASSIGN bereavt = "J".
   END.           
   ELSE IF  Guru.Konstanter:globforetag = "SUND" AND personaltemp.BEREDSKAPSAVTAL = "K" THEN DO:
      ASSIGN RAD_AB = 2.      
      FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = personaltemp.BEREDSKAPSAVTAL
      NO-LOCK NO-ERROR.
      IF AVAILABLE beredskapstarttemp AND beredskapstarttemp.BEREDSKAPSAVTAL NE "I" THEN DO:                
         ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.
      END.      
      ELSE ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.
      DISPLAY RAD_AB WITH FRAME {&FRAME-NAME}.
   END.                
   ELSE IF  Guru.Konstanter:globforetag = "SNAT" AND personaltemp.BEREDSKAPSAVTAL = "K" THEN DO:
      ASSIGN RAD_AB = 2.      
      FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = personaltemp.BEREDSKAPSAVTAL
      NO-LOCK NO-ERROR.
      IF AVAILABLE beredskapstarttemp AND beredskapstarttemp.BEREDSKAPSAVTAL NE "I" THEN DO:                
         ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.
      END.      
      ELSE ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.
      DISPLAY RAD_AB WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:   
      ASSIGN bereavt = personaltemp.BEREDSKAPSAVTAL.
      ASSIGN TOG_JOUR = FALSE.
      TOG_JOUR:HIDDEN = TRUE.        
   END.          
     
   FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt
   USE-INDEX BSTART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE beredskapstarttemp THEN DO:        
      status-mus2 = SESSION:SET-WAIT-STATE("").
      MESSAGE "Det går inte att registrera beredskap på " tidpers.PERSONALKOD VIEW-AS ALERT-BOX.    
      RUN refreshbrw_UI IN brwproc[2].
      musz = TRUE.
      RETURN.
   END.      
       
   IF bjour = TRUE THEN DO:      
      regdatum = TODAY.
      IF MONTH(regdatum) = regmnr AND YEAR(regdatum) = regar THEN regar = regar.
      ELSE DO:
         IF regmnr = 12 THEN regdatum = DATE(12,31,regar) - 6.
         ELSE regdatum = DATE((regmnr + 1),01,regar) - 7.
      END.
      RUN REGVEC.P.
      regdagnamn = beredskapstarttemp.BEREDSKAPDAGSTART.
      RUN VECODAT.P.
      ASSIGN FILL-IN-STARTDAT = regdatum.               
   END.      
   IF FILL-IN-STARTDAT = ? THEN DO:
      ASSIGN regdatum = TODAY.
      IF MONTH(regdatum) = regmnr AND YEAR(regdatum) = regar THEN regar = regar.
      ELSE DO:
        IF regmnr = 12 THEN regdatum = DATE(12,31,regar) - 6.
         ELSE regdatum = DATE((regmnr + 1),01,regar) - 7.
      END.
      RUN REGVEC.P.
      regdagnamn = beredskapstarttemp.BEREDSKAPDAGSTART.
      RUN VECODAT.P.
      ASSIGN FILL-IN-STARTDAT = regdatum.                     
      IF  Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "ELPA"  THEN DO:                  
         IF  Guru.Konstanter:globforetag = "LULE"  THEN DO:
            IF FILL-IN-STARTDAT GE DATE(05,15,YEAR(FILL-IN-STARTDAT)) AND FILL-IN-STARTDAT < DATE(09,01,YEAR(FILL-IN-STARTDAT)) THEN DO:
               IF bereavt =  "MV" THEN bereavt = "ms".
               FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt USE-INDEX BSTART NO-LOCK NO-ERROR.
            END.
            ELSE IF personaltemp.BEREDSKAPSAVTAL = "MV" THEN bereavt =  "MV".            
         END.
         IF  Guru.Konstanter:globforetag = "elpa" THEN DO:
            IF FILL-IN-STARTDAT GE DATE(05,15,YEAR(FILL-IN-STARTDAT)) AND FILL-IN-STARTDAT < DATE(09,01,YEAR(FILL-IN-STARTDAT)) THEN DO:
               IF bereavt =  "TM" THEN bereavt = "TN".
               FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = bereavt USE-INDEX BSTART NO-LOCK NO-ERROR.
            END.
            ELSE IF personaltemp.BEREDSKAPSAVTAL = "TM" THEN bereavt =  "TM".            
         END.   
         IF  Guru.Konstanter:globforetag = "ELPA" AND bereavt = "tm" THEN ASSIGN berplus = 1.
         IF  Guru.Konstanter:globforetag = "ELPA" AND bereavt = "tn" THEN ASSIGN berplus = 0.
         IF  Guru.Konstanter:globforetag = "LULE" AND bereavt = "MV" THEN ASSIGN berplus = 1.
         IF  Guru.Konstanter:globforetag = "LULE" AND bereavt = "MS" THEN ASSIGN berplus = 0.            
      END.
   END.   
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
   regdatum = FILL-IN-STARTDAT + berplus.
   RUN REGVEC.P.
   regdagnamn = beredskapstarttemp.BEREDSKAPDAGSLUT.   
   FILL-IN-SLUTDAT = regdatum.   
   RUN form_UI.               

   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.   
   ASSIGN
   vart = "BER"
   FILL-IN-SLUT-2 = beredskapstarttemp.BEREDSKAPSLUT 
   FILL-IN-START = beredskapstarttemp.BEREDSKAPSTART
   FILL-IN-PKOD = personaltemp.PERSONALKOD.
   DISPLAY FILL-IN-START FILL-IN-SLUT-2 WITH FRAME {&FRAME-NAME}.
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
   ASSIGN    
   FILL-IN-TEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-SOKPA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   RECT-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_PERS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE    
   CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_AONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-TEXT:HIDDEN = TRUE
   RAD_FAST:HIDDEN = TRUE
   FILL-IN_AONRS:HIDDEN = TRUE
   FILL-IN_ORTS:HIDDEN = TRUE. 
   IF valgubbe = FALSE THEN DO:         
      IF NOT AVAILABLE tidpers THEN DO:
         FIND FIRST tidpers WHERE tidpers.PERSONALKOD = pkod NO-LOCK NO-ERROR.
         persrec = tidpers.TIDPERSREC.
      END.
      ELSE DO:
         pkod = tidpers.PERSONALKOD.
         FIND NEXT tidpers WHERE tidpers.REGKOLL = FALSE USE-INDEX PERSONALKOD NO-ERROR.
         IF AVAILABLE tidpers THEN DO:
            IF pkod = tidpers.PERSONALKOD THEN DO:
               FIND NEXT tidpers WHERE tidpers.REGKOLL = FALSE USE-INDEX PERSONALKOD NO-ERROR.
            END.
         END.
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
   persrec = tidpers.TIDPERSREC
   persrec2 = RECID(tidpers).
   RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(tidpers)).
   FIND personaltemp WHERE personaltemp.PERSONALKOD = pkod 
   NO-LOCK NO-ERROR.
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = personaltemp.OMRADE 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF CMB_OMR = Guru.Konstanter:gomrk + " : alla" THEN musz = musz.
   ELSE DO:
      IF tidpers.OMRADE NE omrtemp.OMRADE THEN DO:   
         CMB_OMR = Guru.Konstanter:gomrk + " : alla".
         ASSIGN CMB_OMR:SCREEN-VALUE = CMB_OMR.      
      END.
   END. 
   RUN nastapers_UI.
   IF musz = TRUE THEN RETURN.
   ELSE IF varifran = 1 THEN DO:
      DISPLAY BRW_PERS WITH FRAME {&FRAME-NAME}.
      RUN lastselectdyn_UI IN brwproc[2].
   END.
   {musarrow.i}  
   DISPLAY FILL-IN-PKOD FILL-IN-START FILL-IN-SLUT-2 WITH FRAME {&FRAME-NAME}.
   RUN form_UI.      
   DISPLAY FILL-IN-STARTDAT FILL-IN-SLUTDAT WITH FRAME {&FRAME-NAME}.
   ENABLE FILL-IN-STARTDAT FILL-IN-SLUTDAT BTN_FVE-3 BTN_NVE-3 BTN_FVE-4 BTN_NVE-4 
   WITH FRAME {&FRAME-NAME}.
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
   {NYCOL.I}
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redanmed_UI DIALOG-1 
PROCEDURE redanmed_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   MESSAGE 'Det finns redan beredskap registrerad.'
   'Du måste ta bort den först. Beredskapen startar den '  
   utdatum
   VIEW-AS ALERT-BOX.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reg_UI DIALOG-1 
PROCEDURE reg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST beredskapstarttemp WHERE beredskapstarttemp.BEREDSKAPSAVTAL = personaltemp.BEREDSKAPSAVTAL
   USE-INDEX BSTART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE beredskapstarttemp THEN DO:           
      status-mus2 = SESSION:SET-WAIT-STATE("").
      MESSAGE "Det går inte att registrera beredskap på denna person." VIEW-AS ALERT-BOX.
      RETURN.     
   END.   
   IF musz = TRUE THEN DO:      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      musz = FALSE.
      RETURN.
   END.        
   IF FILL-IN-START > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.      
   IF SUBSTRING(STRING(FILL-IN-START,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.      
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.         
   IF FILL-IN-SLUT-2 > 24.00 THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END. 
   IF SUBSTRING(STRING(FILL-IN-SLUT-2,"99.99"),4 ,2) > "59" THEN DO:
      MESSAGE "Orimligt klockslag." VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN.
   END.    
   
   IF  Guru.Konstanter:globforetag = "LULE" THEN DO:
      /*Luleå energi får bara skriva beredskap på föjlande*/
      IF jid = "01" THEN DO:
         IF FILL-IN-AONR = "241" THEN.
         ELSE IF FILL-IN-AONR = "245" THEN.
         ELSE DO:
            MESSAGE "Felaktigt projektnummer. Projekt skall vara 241 eller 245" VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
      /* Luleå Elnät får bara skriva beredskap på nedan. Det är bara lunet som får använda 940*/
      ELSE IF jid = "03" THEN DO:
         IF FILL-IN-AONR = "100293" THEN.
         ELSE IF FILL-IN-AONR= "003005" AND FILL-IN-DELNR = 1 THEN.
         ELSE DO:
            MESSAGE "Felaktigt projektnummer. Projekt skall vara 100293 003005 " VIEW-AS ALERT-BOX.
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
      END.
   END.
   IF  Guru.Konstanter:globforetag = "LULE" THEN DO:
      ASSIGN
      FILL-IN-AONR = INPUT FRAME {&FRAME-NAME} FILL-IN-AONR  
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
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         ELSE IF soktemp.SOKDATE[1] = 01/01/1991 THEN musz = musz.
         ELSE IF soktemp.SOKDATE[1] < regdatum  THEN DO:
            MESSAGE Guru.Konstanter:gaol FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är avslutat." VIEW-AS ALERT-BOX.      
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
            RETURN.
         END.         
      END.
      ELSE DO:                    
         {AOKOLLERS.I}
         IF utsokaonr.AONRAVDATUM = 01/01/1991 OR
         utsokaonr.AONRAVDATUM >= regdatum THEN FILL-IN-DELNR = FILL-IN-DELNR.
         ELSE DO:
            MESSAGE Guru.Konstanter:gaok FILL-IN-AONR STRING(FILL-IN-DELNR,Guru.Konstanter:varforetypchar[1]) "är redan avslutat." VIEW-AS ALERT-BOX.           
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.
         END.
         IF utsokaonr.PRISTYP = "FRÅNVARO." THEN DO:      
            MESSAGE "Beredskap kan inte registreras på frånvaro " VIEW-AS ALERT-BOX.             
            status-mus2 = SESSION:SET-WAIT-STATE("").
            RETURN.    
         END.    
     
      END.
   END.
   ELSE DO:
      musz = musz.
      
   END.   
   ASSIGN
   bdatum = FILL-IN-STARTDAT
   avdatum = FILL-IN-SLUTDAT.
   RUN beredkoll_UI (INPUT RAD_AB).      
   IF musz = TRUE THEN DO:
      RUN redanmed_UI.
      musz = FALSE.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN NO-APPLY.
   END.
   ELSE DO:   
      EMPTY TEMP-TABLE bertemp NO-ERROR.       
      CREATE bertemp.      
      ASSIGN
      bertemp.FORETAG =  Guru.Konstanter:globforetag
      bertemp.ANVANDARE = Guru.Konstanter:globanv     
      bertemp.PKOD = FILL-IN-PKOD 
      bertemp.SLUT2 = FILL-IN-SLUT-2       
      bertemp.START = FILL-IN-START      
      bertemp.BERBEORD = FILL-IN_BERBEORD
      bertemp.DATUM1 = FILL-IN-STARTDAT
      bertemp.DATUM2 = FILL-IN-SLUTDAT
      bertemp.AONR = FILL-IN-AONR
      bertemp.DELNR = FILL-IN-DELNR
      bertemp.BDATUM = bdatum
      bertemp.AVDATUM = avdatum
      bertemp.JOUR = TOG_JOUR
      bertemp.FAKTOR = FILL-IN_FAKTOR
      bertemp.BERALT = RAD_AB.     
      regdatum = FILL-IN-STARTDAT.
      RUN REGVEC.P.
      RUN REGDAG.P.
      ASSIGN
      bertemp.DAG = regdagnamn
      bertemp.VECKA = regvnr
      regdatum = FILL-IN-SLUTDAT.
      RUN REGVEC.P.
      RUN REGDAG.P.
      ASSIGN
      bertemp.DAG2 = regdagnamn
      bertemp.VECKA2 = regvnr.          
      {SKABERIN.I}
      {FELTEXINUT.I}
      IF musz = TRUE THEN DO:      
         status-mus2 = SESSION:SET-WAIT-STATE("").
         musz = FALSE.
         RETURN NO-APPLY.
      END.                                     
      MESSAGE "Beredskapen är nu registrerad." VIEW-AS ALERT-BOX.        
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

