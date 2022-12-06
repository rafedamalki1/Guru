&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win



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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{PERSONALTEMP.I}
{LOGGNING.I}


DEFINE TEMP-TABLE pmpersonaltemp NO-UNDO LIKE personaltemp
   FIELD FORETAG AS CHARACTER
   FIELD E-POST AS CHARACTER   
   FIELD SISTA AS INTEGER
   INDEX FORETAG FORETAG ANVANDARE.
   
DEFINE TEMP-TABLE exppmpersonaltemp NO-UNDO LIKE pmpersonaltemp.
DEFINE TEMP-TABLE supportdagbok NO-UNDO
   FIELD SUPPORTID AS INTEGER
   FIELD ANVANDARE AS CHARACTER
   FIELD STARTDATUM  AS DATE
   FIELD STARTKLOCKAN AS DECIMAL
   FIELD SLUTDATUM AS DATE
   FIELD SLUTKLOCKAN AS DECIMAL
   FIELD KATEGORI AS INTEGER
   FIELD FRAGA AS CHARACTER
   FIELD SVAR AS CHARACTER
   FIELD FORETAG AS CHARACTER
   INDEX FORETAG FORETAG SUPPORTID.
DEFINE TEMP-TABLE esupportdagbok NO-UNDO LIKE supportdagbok.
{VALDBTEMP.I}
         
                              
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE suppapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE sistavar AS INTEGER NO-UNDO.



DEFINE VARIABLE templogph AS HANDLE NO-UNDO.

DEFINE VARIABLE logprogh AS HANDLE NO-UNDO.

{VALDBALL.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_LOGNIV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES templogniv templogpost pmpersonaltemp ~
supportdagbok valdbtemp

/* Definitions for BROWSE BRW_LOGNIV                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_LOGNIV templogniv.LOGNIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOGNIV 
&Scoped-define QUERY-STRING-BRW_LOGNIV FOR EACH templogniv NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LOGNIV OPEN QUERY BRW_LOGNIV FOR EACH templogniv NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LOGNIV templogniv
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOGNIV templogniv


/* Definitions for BROWSE BRW_LOGPOST                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_LOGPOST templogpost.LOGNIV ~
templogpost.TYP templogpost.DATUM templogpost.TID templogpost.ANVANDARE ~
templogpost.DATORNAMN templogpost.PRONAMN templogpost.SOKBEGREPP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOGPOST 
&Scoped-define QUERY-STRING-BRW_LOGPOST FOR EACH templogpost NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LOGPOST OPEN QUERY BRW_LOGPOST FOR EACH templogpost NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LOGPOST templogpost
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOGPOST templogpost


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS pmpersonaltemp.PERSONALKOD ~
pmpersonaltemp.ANVANDARE pmpersonaltemp.FORNAMN pmpersonaltemp.EFTERNAMN ~
pmpersonaltemp.TELEFON pmpersonaltemp.MOBILTEL pmpersonaltemp.E-POST 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH pmpersonaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH pmpersonaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS pmpersonaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS pmpersonaltemp


/* Definitions for BROWSE BRW_SUPPORT                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_SUPPORT supportdagbok.SUPPORTID ~
supportdagbok.ANVANDARE supportdagbok.KATEGORI supportdagbok.STARTDATUM ~
supportdagbok.STARTKLOCKAN supportdagbok.SLUTDATUM ~
supportdagbok.SLUTKLOCKAN supportdagbok.FRAGA supportdagbok.SVAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SUPPORT supportdagbok.ANVANDARE ~
supportdagbok.KATEGORI supportdagbok.STARTDATUM supportdagbok.STARTKLOCKAN ~
supportdagbok.SLUTDATUM supportdagbok.SLUTKLOCKAN supportdagbok.FRAGA ~
supportdagbok.SVAR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_SUPPORT supportdagbok
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_SUPPORT supportdagbok
&Scoped-define QUERY-STRING-BRW_SUPPORT FOR EACH supportdagbok NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SUPPORT OPEN QUERY BRW_SUPPORT FOR EACH supportdagbok NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SUPPORT supportdagbok
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SUPPORT supportdagbok


/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.GFORETAG valdbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for FRAME FRAME-LOG                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-LOG ~
    ~{&OPEN-QUERY-BRW_LOGNIV}~
    ~{&OPEN-QUERY-BRW_LOGPOST}

/* Definitions for FRAME FRAME-NYREG                                    */
&Scoped-define QUERY-STRING-FRAME-NYREG FOR EACH pmpersonaltemp SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-NYREG OPEN QUERY FRAME-NYREG FOR EACH pmpersonaltemp SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-NYREG pmpersonaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-NYREG pmpersonaltemp


/* Definitions for FRAME FRAME-SUPP                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-SUPP ~
    ~{&OPEN-QUERY-BRW_SUPPORT}~
    ~{&OPEN-QUERY-BRW_VDB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD-VAL 
&Scoped-Define DISPLAYED-OBJECTS RAD-VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE RAD-VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Support", 1,
"Logg", 2
     SIZE 32.5 BY 1.25 NO-UNDO.

DEFINE BUTTON BTN_ADDNIV 
     LABEL "Lägg Till" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB2 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LOGBORT 
     LABEL "Ta Bort" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NIVBORT 
     LABEL "Ta Bort" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-LOGNIV AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_SPARA 
     LABEL "Spara" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_ANVANDARE AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13.25 BY 1.

DEFINE VARIABLE FILL-IN_E-POST AS CHARACTER FORMAT "X(256)" 
     LABEL "E-post" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "x(256)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

DEFINE VARIABLE FILL-IN_MOBILTEL AS CHARACTER FORMAT "x(11)" 
     LABEL "Mobiltel" 
     VIEW-AS FILL-IN 
     SIZE 12.25 BY 1.

DEFINE VARIABLE FILL-IN_PERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6.25 BY 1.

DEFINE VARIABLE FILL-IN_TELEFON AS CHARACTER FORMAT "x(11)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 12.25 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXP 
     LABEL "Export anv" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMP 
     LABEL "Import anv" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NYANV 
     LABEL "Ny användare" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SUP 
     LABEL "Ny support" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LOGNIV FOR 
      templogniv SCROLLING.

DEFINE QUERY BRW_LOGPOST FOR 
      templogpost SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      pmpersonaltemp SCROLLING.

DEFINE QUERY BRW_SUPPORT FOR 
      supportdagbok SCROLLING.

DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.

DEFINE QUERY FRAME-NYREG FOR 
      pmpersonaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_LOGNIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOGNIV C-Win _STRUCTURED
  QUERY BRW_LOGNIV NO-LOCK DISPLAY
      templogniv.LOGNIV COLUMN-LABEL "Logniv" FORMAT "->>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 25.75 BY 8.92
         TITLE "Lognivar" EXPANDABLE.

DEFINE BROWSE BRW_LOGPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOGPOST C-Win _STRUCTURED
  QUERY BRW_LOGPOST NO-LOCK DISPLAY
      templogpost.LOGNIV COLUMN-LABEL "Logniv" FORMAT "->>>>>>9":U
      templogpost.TYP COLUMN-LABEL "Typ" FORMAT "X(8)":U
      templogpost.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      templogpost.TID COLUMN-LABEL "Tid" FORMAT "->>>>>>9":U
            WIDTH 7
      templogpost.ANVANDARE COLUMN-LABEL "Användare" FORMAT "X(8)":U
      templogpost.DATORNAMN COLUMN-LABEL "Datornamn" FORMAT "X(8)":U
      templogpost.PRONAMN COLUMN-LABEL "Pronamn" FORMAT "X(8)":U
            WIDTH 5
      templogpost.SOKBEGREPP COLUMN-LABEL "Sokbegrepp" FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 91 BY 23.25
         TITLE "Logposter" ROW-HEIGHT-CHARS .63 EXPANDABLE.

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      pmpersonaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      pmpersonaltemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      pmpersonaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 13
      pmpersonaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 19.5
      pmpersonaltemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "x(11)":U
      pmpersonaltemp.MOBILTEL COLUMN-LABEL "Mobiltele" FORMAT "x(11)":U
      pmpersonaltemp.E-POST COLUMN-LABEL "E-post" FORMAT "X(256)":U
            WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 103.5 BY 6.25
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_SUPPORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SUPPORT C-Win _STRUCTURED
  QUERY BRW_SUPPORT NO-LOCK DISPLAY
      supportdagbok.SUPPORTID COLUMN-LABEL "SupportId" FORMAT ">>>>>>9":U
      supportdagbok.ANVANDARE COLUMN-LABEL "Användare" FORMAT "X(8)":U
      supportdagbok.KATEGORI COLUMN-LABEL "Kategori" FORMAT "->>>>>>9":U
      supportdagbok.STARTDATUM COLUMN-LABEL "Startdatum" FORMAT "99/99/99":U
      supportdagbok.STARTKLOCKAN COLUMN-LABEL "Starttid" FORMAT ">9.99":U
      supportdagbok.SLUTDATUM COLUMN-LABEL "Slutdatum" FORMAT "99/99/99":U
      supportdagbok.SLUTKLOCKAN COLUMN-LABEL "Sluttid" FORMAT ">9.99":U
      supportdagbok.FRAGA COLUMN-LABEL "Fråga" FORMAT "X(256)":U
            WIDTH 25
      supportdagbok.SVAR COLUMN-LABEL "Svar" FORMAT "X(256)":U
  ENABLE
      supportdagbok.ANVANDARE
      supportdagbok.KATEGORI
      supportdagbok.STARTDATUM
      supportdagbok.STARTKLOCKAN
      supportdagbok.SLUTDATUM
      supportdagbok.SLUTKLOCKAN
      supportdagbok.FRAGA
      supportdagbok.SVAR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 103.5 BY 19.25
         TITLE "Supportärenden" EXPANDABLE.

DEFINE BROWSE BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED
  QUERY BRW_VDB DISPLAY
      valdbtemp.GFORETAG COLUMN-LABEL "Företag" FORMAT "X(5)":U
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 18.5 BY 23.5
         FONT 4
         TITLE "Databaser" TOOLTIP "Välj databas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD-VAL AT ROW 1 COL 1.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-NYREG
     FILL-IN_PERSONALKOD AT ROW 2.5 COL 15.5 COLON-ALIGNED
     FILL-IN_ANVANDARE AT ROW 3.63 COL 15.5 COLON-ALIGNED
     FILL-IN_FORNAMN AT ROW 4.75 COL 15.5 COLON-ALIGNED
     FILL-IN_EFTERNAMN AT ROW 5.88 COL 15.5 COLON-ALIGNED
     FILL-IN_TELEFON AT ROW 7 COL 15.5 COLON-ALIGNED HELP
          "HEMTELE"
     FILL-IN_MOBILTEL AT ROW 8.13 COL 15.5 COLON-ALIGNED
     FILL-IN_E-POST AT ROW 9.33 COL 15.5 COLON-ALIGNED
     BTN_SPARA AT ROW 12 COL 19
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 33.75 ROW 4
         SIZE 56 BY 13.5.

DEFINE FRAME FRAME-LOG
     BRW_LOGPOST AT ROW 3 COL 32.5
     BRW_LOGNIV AT ROW 3.08 COL 5.63
     BTN_NIVBORT AT ROW 12.5 COL 17
     FILL-IN-LOGNIV AT ROW 14.25 COL 5.5 NO-LABEL
     BTN_ADDNIV AT ROW 14.25 COL 17
     BTN_LOGBORT AT ROW 26.96 COL 68.5
     BTN_AVB2 AT ROW 27 COL 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2
         SIZE 123.5 BY 27.25.

DEFINE FRAME FRAME-SUPP
     BRW_VDB AT ROW 1.5 COL 1.5
     BRW_PERS AT ROW 1.5 COL 21.13
     BRW_SUPPORT AT ROW 8 COL 21.13
     BTN_NYANV AT ROW 25.17 COL 2.75
     BTN_IMP AT ROW 26.25 COL 2.75
     BTN_EXP AT ROW 27.38 COL 2.75
     BTN_SUP AT ROW 27.42 COL 59.5
     BTN_AVB AT ROW 27.42 COL 110.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2
         SIZE 125 BY 27.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: pmpersonaltemp T "?" NO-UNDO temp-db pmpersonaltemp
      TABLE: supportdagbok T "?" NO-UNDO temp-db supportdagbok
      TABLE: templogniv T "?" NO-UNDO temp-db templogniv
      TABLE: templogpost T "?" NO-UNDO temp-db templogpost
      TABLE: valdbtemp T "?" NO-UNDO temp-db valdbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Support"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
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
ASSIGN FRAME FRAME-LOG:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-NYREG:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-SUPP:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-SUPP:MOVE-AFTER-TAB-ITEM (RAD-VAL:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-LOG:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-NYREG:HANDLE)
       XXTABVALXX = FRAME FRAME-SUPP:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-LOG:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-LOG
                                                                        */
/* BROWSE-TAB BRW_LOGPOST 1 FRAME-LOG */
/* BROWSE-TAB BRW_LOGNIV BRW_LOGPOST FRAME-LOG */
ASSIGN 
       FRAME FRAME-LOG:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-LOGNIV IN FRAME FRAME-LOG
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-NYREG
                                                                        */
/* SETTINGS FOR FRAME FRAME-SUPP
                                                                        */
/* BROWSE-TAB BRW_VDB 1 FRAME-SUPP */
/* BROWSE-TAB BRW_PERS BRW_VDB FRAME-SUPP */
/* BROWSE-TAB BRW_SUPPORT BRW_PERS FRAME-SUPP */
ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-SUPP         = 1000
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-SUPP = TRUE
       BRW_PERS:COLUMN-RESIZABLE IN FRAME FRAME-SUPP       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOGNIV
/* Query rebuild information for BROWSE BRW_LOGNIV
     _TblList          = "Temp-Tables.templogniv"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.templogniv.LOGNIV
"templogniv.LOGNIV" "Logniv" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_LOGNIV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOGPOST
/* Query rebuild information for BROWSE BRW_LOGPOST
     _TblList          = "Temp-Tables.templogpost"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.templogpost.LOGNIV
"templogpost.LOGNIV" "Logniv" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.templogpost.TYP
"templogpost.TYP" "Typ" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.templogpost.DATUM
"templogpost.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.templogpost.TID
"templogpost.TID" "Tid" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.templogpost.ANVANDARE
"templogpost.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.templogpost.DATORNAMN
"templogpost.DATORNAMN" "Datornamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.templogpost.PRONAMN
"templogpost.PRONAMN" "Pronamn" ? "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.templogpost.SOKBEGREPP
"templogpost.SOKBEGREPP" "Sokbegrepp" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_LOGPOST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.pmpersonaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.pmpersonaltemp.PERSONALKOD
"pmpersonaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.pmpersonaltemp.ANVANDARE
"pmpersonaltemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.pmpersonaltemp.FORNAMN
"pmpersonaltemp.FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.pmpersonaltemp.EFTERNAMN
"pmpersonaltemp.EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.pmpersonaltemp.TELEFON
"pmpersonaltemp.TELEFON" "Telefon" ? "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.pmpersonaltemp.MOBILTEL
"pmpersonaltemp.MOBILTEL" "Mobiltele" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.pmpersonaltemp.E-POST
"pmpersonaltemp.E-POST" "E-post" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SUPPORT
/* Query rebuild information for BROWSE BRW_SUPPORT
     _TblList          = "Temp-Tables.supportdagbok"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.supportdagbok.SUPPORTID
"supportdagbok.SUPPORTID" "SupportId" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.supportdagbok.ANVANDARE
"supportdagbok.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.supportdagbok.KATEGORI
"supportdagbok.KATEGORI" "Kategori" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.supportdagbok.STARTDATUM
"supportdagbok.STARTDATUM" "Startdatum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.supportdagbok.STARTKLOCKAN
"supportdagbok.STARTKLOCKAN" "Starttid" ">9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.supportdagbok.SLUTDATUM
"supportdagbok.SLUTDATUM" "Slutdatum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.supportdagbok.SLUTKLOCKAN
"supportdagbok.SLUTKLOCKAN" "Sluttid" ">9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.supportdagbok.FRAGA
"supportdagbok.FRAGA" "Fråga" "X(256)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.supportdagbok.SVAR
"supportdagbok.SVAR" "Svar" "X(256)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_SUPPORT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _FldNameList[1]   > Temp-Tables.valdbtemp.GFORETAG
"valdbtemp.GFORETAG" "Företag" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VDB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-NYREG
/* Query rebuild information for FRAME FRAME-NYREG
     _TblList          = "Temp-Tables.pmpersonaltemp"
     _Query            is OPENED
*/  /* FRAME FRAME-NYREG */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Support */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Support */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SUPPORT
&Scoped-define FRAME-NAME FRAME-SUPP
&Scoped-define SELF-NAME BRW_SUPPORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SUPPORT C-Win
ON ROW-LEAVE OF BRW_SUPPORT IN FRAME FRAME-SUPP /* Supportärenden */
DO:
   DISPLAY 
   supportdagbok.ANVANDARE 
   supportdagbok.STARTDATUM 
   supportdagbok.STARTKLOCKAN 
   supportdagbok.SLUTDATUM 
   supportdagbok.SLUTKLOCKAN 
   supportdagbok.FRAGA 
   supportdagbok.SVAR WITH BROWSE BRW_SUPPORT NO-ERROR.

   ASSIGN 
   supportdagbok.ANVANDARE     INPUT BROWSE BRW_SUPPORT   supportdagbok.ANVANDARE   
   supportdagbok.STARTDATUM    INPUT BROWSE BRW_SUPPORT   supportdagbok.STARTDATUM  
   supportdagbok.STARTKLOCKAN  INPUT BROWSE BRW_SUPPORT   supportdagbok.STARTKLOCKAN
   supportdagbok.SLUTDATUM     INPUT BROWSE BRW_SUPPORT   supportdagbok.SLUTDATUM   
   supportdagbok.SLUTKLOCKAN   INPUT BROWSE BRW_SUPPORT   supportdagbok.SLUTKLOCKAN 
   supportdagbok.FRAGA         INPUT BROWSE BRW_SUPPORT   supportdagbok.FRAGA       
   supportdagbok.SVAR          INPUT BROWSE BRW_SUPPORT   supportdagbok.SVAR.        
   


   DISPLAY 
   supportdagbok.ANVANDARE 
   supportdagbok.STARTDATUM 
   supportdagbok.STARTKLOCKAN 
   supportdagbok.SLUTDATUM 
   supportdagbok.SLUTKLOCKAN 
   supportdagbok.FRAGA 
   supportdagbok.SVAR WITH BROWSE BRW_SUPPORT NO-ERROR.
   EMPTY TEMP-TABLE esupportdagbok NO-ERROR. 
   CREATE esupportdagbok.
   BUFFER-COPY supportdagbok TO esupportdagbok.
   RUN sparasup_UI IN suppapph (INPUT TABLE esupportdagbok). 
   EMPTY TEMP-TABLE esupportdagbok NO-ERROR. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SUPPORT C-Win
ON VALUE-CHANGED OF BRW_SUPPORT IN FRAME FRAME-SUPP /* Supportärenden */
DO:
    status-ok = BRW_SUPPORT:SELECT-FOCUSED-ROW() IN FRAME FRAME-SUPP.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.ANVANDARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.ANVANDARE BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.ANVANDARE IN BROWSE BRW_SUPPORT /* Användare */
DO:
   status-ok = BRW_SUPPORT:SELECT-FOCUSED-ROW() IN FRAME FRAME-SUPP NO-ERROR.  
   DISPLAY supportdagbok.ANVANDARE WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.ANVANDARE BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.ANVANDARE IN BROWSE BRW_SUPPORT /* Användare */
DO:
   status-ok = BRW_SUPPORT:SELECT-FOCUSED-ROW() IN FRAME FRAME-SUPP NO-ERROR.  
   supportdagbok.ANVANDARE = INPUT BROWSE BRW_SUPPORT supportdagbok.ANVANDARE.    
   DISPLAY supportdagbok.ANVANDARE WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.KATEGORI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.KATEGORI BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.KATEGORI IN BROWSE BRW_SUPPORT /* Kategori */
DO:
   status-ok = BRW_SUPPORT:SELECT-FOCUSED-ROW() IN FRAME FRAME-SUPP NO-ERROR.  
   DISPLAY supportdagbok.KATEGORI WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.KATEGORI BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.KATEGORI IN BROWSE BRW_SUPPORT /* Kategori */
DO:
   status-ok = BRW_SUPPORT:SELECT-FOCUSED-ROW() IN FRAME FRAME-SUPP NO-ERROR.  
   supportdagbok.KATEGORI = INPUT BROWSE BRW_SUPPORT supportdagbok.KATEGORI.    
   DISPLAY supportdagbok.KATEGORI WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.STARTDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.STARTDATUM BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.STARTDATUM IN BROWSE BRW_SUPPORT /* Startdatum */
DO:
   DISPLAY supportdagbok.STARTDATUM WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.STARTDATUM BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.STARTDATUM IN BROWSE BRW_SUPPORT /* Startdatum */
DO:
   supportdagbok.STARTDATUM = INPUT BROWSE BRW_SUPPORT supportdagbok.STARTDATUM.    
   DISPLAY supportdagbok.STARTDATUM WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.STARTKLOCKAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.STARTKLOCKAN BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.STARTKLOCKAN IN BROWSE BRW_SUPPORT /* Starttid */
DO:
   DISPLAY supportdagbok.STARTKLOCKAN WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.STARTKLOCKAN BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.STARTKLOCKAN IN BROWSE BRW_SUPPORT /* Starttid */
DO:
   supportdagbok.STARTKLOCKAN = INPUT BROWSE BRW_SUPPORT supportdagbok.STARTKLOCKAN.    
   DISPLAY supportdagbok.STARTKLOCKAN WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.SLUTDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.SLUTDATUM BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.SLUTDATUM IN BROWSE BRW_SUPPORT /* Slutdatum */
DO:
   DISPLAY supportdagbok.SLUTDATUM WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.SLUTDATUM BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.SLUTDATUM IN BROWSE BRW_SUPPORT /* Slutdatum */
DO:
   supportdagbok.SLUTDATUM = INPUT BROWSE BRW_SUPPORT supportdagbok.SLUTDATUM.    
   DISPLAY supportdagbok.SLUTDATUM WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.SLUTKLOCKAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.SLUTKLOCKAN BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.SLUTKLOCKAN IN BROWSE BRW_SUPPORT /* Sluttid */
DO:
   DISPLAY supportdagbok.SLUTKLOCKAN WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.SLUTKLOCKAN BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.SLUTKLOCKAN IN BROWSE BRW_SUPPORT /* Sluttid */
DO:
   supportdagbok.SLUTKLOCKAN = INPUT BROWSE BRW_SUPPORT supportdagbok.SLUTKLOCKAN.    
   DISPLAY supportdagbok.SLUTKLOCKAN WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.FRAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.FRAGA BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.FRAGA IN BROWSE BRW_SUPPORT /* Fråga */
DO:
   DISPLAY supportdagbok.FRAGA supportdagbok.SVAR WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.FRAGA BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.FRAGA IN BROWSE BRW_SUPPORT /* Fråga */
DO:
   IF supportdagbok.FRAGA = INPUT BROWSE BRW_SUPPORT supportdagbok.FRAGA THEN.
   ELSE DO:
      ASSIGN
      supportdagbok.SLUTDATUM = TODAY
      supportdagbok.SLUTKLOCKAN = DECIMAL(REPLACE(STRING(TIME,"HH:MM"),":",".")).
   END.
   supportdagbok.FRAGA = INPUT BROWSE BRW_SUPPORT supportdagbok.FRAGA.    
   DISPLAY supportdagbok.FRAGA supportdagbok.SVAR supportdagbok.SLUTDATUM 
   supportdagbok.SLUTKLOCKAN   
   WITH BROWSE BRW_SUPPORT.
   EMPTY TEMP-TABLE esupportdagbok NO-ERROR. 
   CREATE esupportdagbok.
   BUFFER-COPY supportdagbok TO esupportdagbok.
   RUN sparasup_UI IN suppapph (INPUT TABLE esupportdagbok). 
   EMPTY TEMP-TABLE esupportdagbok NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME supportdagbok.SVAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.SVAR BRW_SUPPORT _BROWSE-COLUMN C-Win
ON ENTRY OF supportdagbok.SVAR IN BROWSE BRW_SUPPORT /* Svar */
DO:
   DISPLAY supportdagbok.FRAGA supportdagbok.SVAR WITH BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL supportdagbok.SVAR BRW_SUPPORT _BROWSE-COLUMN C-Win
ON LEAVE OF supportdagbok.SVAR IN BROWSE BRW_SUPPORT /* Svar */
DO:
   supportdagbok.SVAR = INPUT BROWSE BRW_SUPPORT supportdagbok.SVAR.    
   DISPLAY supportdagbok.SVAR supportdagbok.FRAGA WITH BROWSE BRW_SUPPORT.
   EMPTY TEMP-TABLE esupportdagbok NO-ERROR. 
   CREATE esupportdagbok.
   BUFFER-COPY supportdagbok TO esupportdagbok.
   RUN sparasup_UI IN suppapph (INPUT TABLE esupportdagbok). 
   EMPTY TEMP-TABLE esupportdagbok NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VDB
&Scoped-define SELF-NAME BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VDB IN FRAME FRAME-SUPP /* Databaser */
DO:
   
   status-ok = BRW_VDB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   EMPTY TEMP-TABLE pmpersonaltemp NO-ERROR. 
   RUN hmtpers_UI IN suppapph (INPUT valdbtemp.GFORETAG, OUTPUT TABLE pmpersonaltemp).
   valford = 'pmpersonaltemp.FORETAG = "' + valdbtemp.GFORETAG + '"'.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT valford).
   RUN openbdynspec_UI IN brwproc[2]. 
   RUN getfirst_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2]. 
   EMPTY TEMP-TABLE supportdagbok NO-ERROR. 
   RUN hmtsupp_UI IN suppapph (INPUT valdbtemp.GFORETAG, OUTPUT TABLE supportdagbok).
   valford = 'supportdagbok.FORETAG = "' + valdbtemp.GFORETAG + '"'.
   RUN setcolsortvar_UI IN brwproc[3] (INPUT valford).
   RUN openbdynspec_UI IN brwproc[3]. 
   RUN getfirst_UI IN brwproc[3].
   RUN lastselectdyn_UI IN brwproc[3]. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME FRAME-SUPP /* Databaser */
DO:
   status-ok = BRW_VDB:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.   
   IF AVAILABLE valdbtemp THEN DO:
      ASSIGN
      BTN_NYANV:HIDDEN IN FRAME FRAME-SUPP = FALSE  
      BTN_IMP:HIDDEN IN FRAME FRAME-SUPP = FALSE 
      BTN_EXP:HIDDEN IN FRAME FRAME-SUPP = FALSE 
      BTN_SUP:HIDDEN IN FRAME FRAME-SUPP = FALSE.    
   END.
   ELSE DO:
      ASSIGN
      BTN_NYANV:HIDDEN IN FRAME FRAME-SUPP = TRUE  
      BTN_IMP:HIDDEN IN FRAME FRAME-SUPP = TRUE 
      BTN_EXP:HIDDEN IN FRAME FRAME-SUPP = TRUE 
      BTN_SUP:HIDDEN IN FRAME FRAME-SUPP = TRUE.    
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-LOG
&Scoped-define SELF-NAME BTN_ADDNIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ADDNIV C-Win
ON CHOOSE OF BTN_ADDNIV IN FRAME FRAME-LOG /* Lägg Till */
DO:
   FILL-IN-LOGNIV = INPUT FILL-IN-LOGNIV.
   RUN nivskap_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SUPP
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME FRAME-SUPP /* Avsluta */
DO:   
   {BORTBRWPROC.I}    
   IF VALID-HANDLE(suppapph) THEN DELETE PROCEDURE suppapph NO-ERROR.
   APPLY "END-ERROR":U TO SELF.           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-LOG
&Scoped-define SELF-NAME BTN_AVB2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB2 C-Win
ON CHOOSE OF BTN_AVB2 IN FRAME FRAME-LOG /* Avsluta */
DO:
  {BORTBRWPROC.I}    
   IF VALID-HANDLE(suppapph) THEN DELETE PROCEDURE suppapph NO-ERROR.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SUPP
&Scoped-define SELF-NAME BTN_EXP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXP C-Win
ON CHOOSE OF BTN_EXP IN FRAME FRAME-SUPP /* Export anv */
DO:
   {muswait.i}   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   RUN export_UI IN suppapph (OUTPUT TABLE exppmpersonaltemp).
   fildir = fildir + valdbtemp.GFORETAG + "supportpers.d".
   {AMERICANEUROPEAN.I}
   OUTPUT TO VALUE(fildir).
   FOR EACH exppmpersonaltemp:
      EXPORT exppmpersonaltemp.
   END.
   OUTPUT CLOSE.
   {EUROPEANAMERICAN.I}
   {musarrow.i}   
   MESSAGE "Data finns nu i filen " fildir VIEW-AS ALERT-BOX.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMP C-Win
ON CHOOSE OF BTN_IMP IN FRAME FRAME-SUPP /* Import anv */
DO:
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den fil som Ni vill koppla"
   FILTERS        "All Files (*.d)"  "*.d"   
   INITIAL-DIR    fildir
   UPDATE OKvald.
   {muswait.i} 
   {AMERICANEUROPEAN.I}  
   IF OKvald = TRUE THEN DO:
      INPUT FROM VALUE(fildir).
      REPEAT:
         CREATE exppmpersonaltemp.
         ASSIGN.
         IMPORT exppmpersonaltemp.
      END.
      RUN import_UI IN suppapph (INPUT TABLE exppmpersonaltemp).
      OS-DELETE VALUE(fildir). 
   END.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   {EUROPEANAMERICAN.I}
   APPLY "MOUSE-SELECT-DBLCLICK" TO BRW_VDB.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-LOG
&Scoped-define SELF-NAME BTN_LOGBORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LOGBORT C-Win
ON CHOOSE OF BTN_LOGBORT IN FRAME FRAME-LOG /* Ta Bort */
DO:
 RUN logbort_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NIVBORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NIVBORT C-Win
ON CHOOSE OF BTN_NIVBORT IN FRAME FRAME-LOG /* Ta Bort */
DO:
  RUN nivbort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SUPP
&Scoped-define SELF-NAME BTN_NYANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYANV C-Win
ON CHOOSE OF BTN_NYANV IN FRAME FRAME-SUPP /* Ny användare */
DO:
   ASSIGN
   FRAME FRAME-SUPP:HIDDEN = TRUE
   FRAME FRAME-LOG:HIDDEN = TRUE
   FRAME FRAME-NYREG:HIDDEN = FALSE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-NYREG
&Scoped-define SELF-NAME BTN_SPARA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPARA C-Win
ON CHOOSE OF BTN_SPARA IN FRAME FRAME-NYREG /* Spara */
DO:
   
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   ASSIGN
   FILL-IN_PERSONALKOD  = INPUT FILL-IN_PERSONALKOD  
   FILL-IN_ANVANDARE    = INPUT FILL-IN_ANVANDARE    
   FILL-IN_FORNAMN      = INPUT FILL-IN_FORNAMN      
   FILL-IN_EFTERNAMN    = INPUT FILL-IN_EFTERNAMN    
   FILL-IN_TELEFON      = INPUT FILL-IN_TELEFON      
   FILL-IN_MOBILTEL     = INPUT FILL-IN_MOBILTEL     
   FILL-IN_E-POST       = INPUT FILL-IN_E-POST.       
   IF FILL-IN_PERSONALKOD = "" THEN.
   ELSE DO:
      {muswait.i}   
      CREATE exppmpersonaltemp.
      ASSIGN
      exppmpersonaltemp.FORETAG =     valdbtemp.GFORETAG      
      exppmpersonaltemp.PERSONALKOD = FILL-IN_PERSONALKOD    
      exppmpersonaltemp.ANVANDARE   = FILL-IN_ANVANDARE      
      exppmpersonaltemp.FORNAMN     = FILL-IN_FORNAMN        
      exppmpersonaltemp.EFTERNAMN   = FILL-IN_EFTERNAMN      
      exppmpersonaltemp.TELEFON     = FILL-IN_TELEFON        
      exppmpersonaltemp.MOBILTEL    = FILL-IN_MOBILTEL       
      exppmpersonaltemp.E-POST      = FILL-IN_E-POST.        
      RUN sparapers_UI IN suppapph (INPUT TABLE exppmpersonaltemp,OUTPUT musz).
      {musarrow.i}   
      IF musz = TRUE THEN DO:
         EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
         musz = FALSE.
         MESSAGE "Enhet/Sign finns redan!"  VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      CREATE pmpersonaltemp.
      BUFFER-COPY exppmpersonaltemp TO pmpersonaltemp.
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(pmpersonaltemp)).
      RUN openbdynspec_UI IN brwproc[2]. 
      RUN lastselectdyn_UI IN brwproc[2]. 
   END.
   EMPTY TEMP-TABLE exppmpersonaltemp NO-ERROR. 
   ASSIGN
   FRAME FRAME-SUPP:HIDDEN = FALSE
   FRAME FRAME-NYREG:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-SUPP
&Scoped-define SELF-NAME BTN_SUP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SUP C-Win
ON CHOOSE OF BTN_SUP IN FRAME FRAME-SUPP /* Ny support */
DO:
   RUN sistasup_UI IN suppapph (INPUT valdbtemp.GFORETAG,OUTPUT sistavar).
   CREATE supportdagbok.
   ASSIGN
   supportdagbok.FORETAG = valdbtemp.GFORETAG
   supportdagbok.SUPPORTID = sistavar
   supportdagbok.STARTDATUM = TODAY 
   supportdagbok.STARTKLOCKAN = DECIMAL(REPLACE(STRING(TIME,"HH:MM"),":","."))
   supportdagbok.SLUTDATUM = TODAY
   supportdagbok.SLUTKLOCKAN = DECIMAL(REPLACE(STRING(TIME,"HH:MM"),":",".")).
   IF AVAILABLE pmpersonaltemp THEN DO:
      supportdagbok.ANVANDARE = pmpersonaltemp.ANVANDARE.   
   END.
   RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(supportdagbok)).
   RUN openbdynspec_UI IN brwproc[3]. 
   RUN lastselectdyn_UI IN brwproc[3]. 
   IF supportdagbok.ANVANDAR = "" THEN APPLY "ENTRY" TO supportdagbok.ANVANDAR IN BROWSE BRW_SUPPORT.
   ELSE APPLY "ENTRY" TO supportdagbok.FRAGA IN BROWSE BRW_SUPPORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-NYREG
&Scoped-define SELF-NAME FILL-IN_E-POST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_E-POST C-Win
ON LEAVE OF FILL-IN_E-POST IN FRAME FRAME-NYREG /* E-post */
DO:
   FILL-IN_E-POST = INPUT FILL-IN_E-POST.
   IF FILL-IN_E-POST NE "" THEN DO:   
      musz = FALSE.
      RUN EPOSTKOLL.P (INPUT FILL-IN_E-POST,OUTPUT musz).   
      IF musz = FALSE THEN DO:
         APPLY "ENTRY" TO FILL-IN_E-POST IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.      
      musz = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME RAD-VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD-VAL C-Win
ON VALUE-CHANGED OF RAD-VAL IN FRAME DEFAULT-FRAME
DO:

   RAD-VAL = INPUT RAD-VAL.

  IF RAD-VAL = 1 THEN DO:
     FRAME FRAME-SUPP:HIDDEN = FALSE.
     FRAME FRAME-LOG:HIDDEN = TRUE.     
     /*RUN testlog_UI.*/
  END.
  ELSE IF RAD-VAL = 2 THEN DO:
     FRAME FRAME-SUPP:HIDDEN = TRUE.
     FRAME FRAME-LOG:HIDDEN = FALSE.

     RUN hamtalogniv_UI.
     RUN hamtalogpost_UI.

     /*RUN testlog_UI.*/
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOGNIV
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
   IF VALID-HANDLE(suppapph) THEN DELETE PROCEDURE suppapph NO-ERROR.
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
   RUN enable_UI.
   {FRMSIZEF.I}  
   ASSIGN
   FRAME FRAME-SUPP:HIDDEN = FALSE
   FRAME FRAME-NYREG:HIDDEN = TRUE
   BTN_NYANV:HIDDEN IN FRAME FRAME-SUPP = TRUE  
   BTN_IMP:HIDDEN IN FRAME FRAME-SUPP = TRUE 
   BTN_EXP:HIDDEN IN FRAME FRAME-SUPP = TRUE 
   BTN_SUP:HIDDEN IN FRAME FRAME-SUPP = TRUE.    
   {musarrow.i}   
   {WIN_M_SLUT.I}
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_VDB:HANDLE IN FRAME FRAME-SUPP).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_PERS:HANDLE IN FRAME FRAME-SUPP).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_SUPPORT:HANDLE IN FRAME FRAME-SUPP).
   RUN setcolindex_UI IN brwproc[3] (INPUT "FORETAG BY SUPPORTID DESCENDING").

   RUN DYNBRW.P PERSISTENT SET brwproc[4] (INPUT BRW_LOGNIV:HANDLE IN FRAME FRAME-LOG).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_LOGPOST:HANDLE IN FRAME FRAME-LOG).

   IF Guru.Konstanter:appcon THEN DO:
      RUN SUPPORTAPP.P PERSISTENT SET suppapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SUPPORTAPP.P PERSISTENT SET suppapph.
   END. 
      
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
  DISPLAY RAD-VAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD-VAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BRW_VDB BRW_PERS BRW_SUPPORT BTN_NYANV BTN_IMP BTN_EXP BTN_SUP BTN_AVB 
      WITH FRAME FRAME-SUPP IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-SUPP}
  DISPLAY FILL-IN-LOGNIV 
      WITH FRAME FRAME-LOG IN WINDOW C-Win.
  ENABLE BRW_LOGPOST BRW_LOGNIV BTN_NIVBORT FILL-IN-LOGNIV BTN_ADDNIV 
         BTN_LOGBORT BTN_AVB2 
      WITH FRAME FRAME-LOG IN WINDOW C-Win.
  VIEW FRAME FRAME-LOG IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LOG}

  {&OPEN-QUERY-FRAME-NYREG}
  GET FIRST FRAME-NYREG.
  DISPLAY FILL-IN_PERSONALKOD FILL-IN_ANVANDARE FILL-IN_FORNAMN 
          FILL-IN_EFTERNAMN FILL-IN_TELEFON FILL-IN_MOBILTEL FILL-IN_E-POST 
      WITH FRAME FRAME-NYREG IN WINDOW C-Win.
  ENABLE FILL-IN_PERSONALKOD FILL-IN_ANVANDARE FILL-IN_FORNAMN 
         FILL-IN_EFTERNAMN FILL-IN_TELEFON FILL-IN_MOBILTEL FILL-IN_E-POST 
         BTN_SPARA 
      WITH FRAME FRAME-NYREG IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-NYREG}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtalogniv_UI C-Win 
PROCEDURE hamtalogniv_UI :
/*status-ok = BRW_LOGNIV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. */
   EMPTY TEMP-TABLE templogniv NO-ERROR. 
   RUN hmtlogniv_UI IN suppapph (OUTPUT TABLE templogniv).
   /*valford = 'pmpersonaltemp.FORETAG = "' + valdbtemp.GFORETAG + '"'.*/
   /*RUN setcolsortvar_UI IN brwproc[4] (INPUT valford).*/
   RUN openbdynspec_UI IN brwproc[4]. 
   RUN getfirst_UI IN brwproc[4].
   RUN lastselectdyn_UI IN brwproc[4].


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtalogpost_UI C-Win 
PROCEDURE hamtalogpost_UI :
/*status-ok = BRW_LOGPOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. */
   EMPTY TEMP-TABLE templogpost NO-ERROR. 
   RUN hmtlogpost_UI IN suppapph (OUTPUT TABLE templogpost).
   /*valford = 'pmpersonaltemp.FORETAG = "' + valdbtemp.GFORETAG + '"'.*/
   /*RUN setcolsortvar_UI IN brwproc[4] (INPUT valford).*/
   RUN openbdynspec_UI IN brwproc[5]. 
   RUN getfirst_UI IN brwproc[5].
   RUN lastselectdyn_UI IN brwproc[5]. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE logbort_UI C-Win 
PROCEDURE logbort_UI :
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
   antal_raknare = 1.

   EMPTY TEMP-TABLE templogpost2 NO-ERROR.

   DO WHILE antal_raknare LE BRW_LOGPOST:NUM-SELECTED-ROWS IN FRAME FRAME-LOG:
      BRW_LOGPOST:FETCH-SELECTED-ROW(antal_raknare).
      CREATE templogpost2.
      BUFFER-COPY templogpost TO templogpost2. 
      /*IF antal_raknare = BRW_LOGPOST:NUM-SELECTED-ROWS THEN DO:
         RUN selnextprevrow_UI IN brwproc[5].
      END.*/
      DELETE templogpost.
      antal_raknare = antal_raknare + 1.
      
   END.
   RUN selnextprevrow_UI IN brwproc[5].
   RUN openbdyn_UI IN brwproc[5] (INPUT "").            
   RUN lastselectdyn_UI IN brwproc[5].

   RUN bortlogpost_UI IN suppapph (INPUT TABLE templogpost2). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nivbort_UI C-Win 
PROCEDURE nivbort_UI :
EMPTY TEMP-TABLE templogniv2 NO-ERROR.
   
   BRW_LOGNIV:FETCH-SELECTED-ROW(1) IN FRAME FRAME-LOG.
   CREATE templogniv2.
   BUFFER-COPY templogniv TO templogniv2.
      
   DELETE templogniv.
   RUN selnextprevrow_UI IN brwproc[4].
   RUN openbdyn_UI IN brwproc[4] (INPUT "").            
   RUN lastselectdyn_UI IN brwproc[4].

   RUN bortlogniv_UI IN suppapph (INPUT TABLE templogniv2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nivskap_UI C-Win 
PROCEDURE nivskap_UI :
FIND FIRST templogniv WHERE templogniv.LOGNIV = FILL-IN-LOGNIV NO-ERROR.
   EMPTY TEMP-TABLE templogniv2 NO-ERROR.
   
   IF NOT AVAILABLE templogniv AND FILL-IN-LOGNIV NE 0 THEN DO:
      CREATE templogniv2.
      ASSIGN 
      templogniv2.LOGNIV = FILL-IN-LOGNIV.

      RUN sparlogniv_UI IN suppapph (INPUT TABLE templogniv2).
      
      CREATE templogniv.
      BUFFER-COPY templogniv2 TO templogniv.

      RUN selnextprevrow_UI IN brwproc[4].
      RUN openbdyn_UI IN brwproc[4] (INPUT "").            
      RUN lastselectdyn_UI IN brwproc[4].

      
   END.
   ELSE DO:
      MESSAGE "Logniv finns redan // Logniv får inte vara 0" VIEW-AS ALERT-BOX.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE testlog_UI C-Win 
PROCEDURE testlog_UI :
RUN BORTLOGG.P PERSISTENT SET logprogh.
   FILE-INFO:FILE-NAME = TRIM(SUBSTRING(PROGRAM-NAME(1),INDEX(PROGRAM-NAME(1),"_UI") + 3)).
   RUN loggskap_UI IN logprogh (INPUT 2,INPUT SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)), INPUT "WCCHB1", INPUT "").
   IF VALID-HANDLE(logprogh) THEN DELETE PROCEDURE logprogh NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

