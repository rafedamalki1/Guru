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
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER varifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fastighbet AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER korvar AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{FASTIGHET.I}  

{BRWSOK.I}
DEFINE NEW SHARED VARIABLE faktplanrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE markvrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE faktrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE fastrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE markrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE radvar AS INTEGER NO-UNDO.
DEFINE VARIABLE vardben AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpvardrec AS RECID NO-UNDO.
DEFINE VARIABLE tmpmarkrec AS RECID NO-UNDO.
DEFINE VARIABLE fastvrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE markrec2 AS RECID NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE tot% AS INTEGER NO-UNDO.  
DEFINE VARIABLE val1 AS LOGICAL NO-UNDO.
DEFINE VARIABLE varsenfak AS DATE NO-UNDO. 
DEFINE VARIABLE varsentid AS DATE NO-UNDO.
DEFINE VARIABLE varsenvecko AS CHARACTER NO-UNDO.
DEFINE VARIABLE ortkalks AS CHARACTER NO-UNDO.
DEFINE VARIABLE marknrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE markagareapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE persnr AS INTEGER EXTENT 10 FORMAT "99" NO-UNDO.
DEFINE VARIABLE fragvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE returnr AS INTEGER NO-UNDO.
DEFINE VARIABLE brwhjvar AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define BROWSE-NAME BRW_MAGA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES markagaretemp valmarkfast valfastvard

/* Definitions for BROWSE BRW_MAGA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MAGA markagaretemp.MARKNR ~
markagaretemp.MARKAGARE markagaretemp.PERSONNUMMER markagaretemp.PNR2 ~
markagaretemp.GATUADRESS markagaretemp.POSTNUMMER markagaretemp.POSTADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MAGA markagaretemp.MARKNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MAGA markagaretemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MAGA markagaretemp
&Scoped-define QUERY-STRING-BRW_MAGA FOR EACH markagaretemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_MAGA OPEN QUERY BRW_MAGA FOR EACH markagaretemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_MAGA markagaretemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MAGA markagaretemp


/* Definitions for BROWSE BRW_VMAGA                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VMAGA valmarkfast.MARKNR ~
valmarkfast.PROCENT valmarkfast.MARKAGARE valmarkfast.PERSONNUMMER ~
valmarkfast.PNR2 valmarkfast.GATUADRESS valmarkfast.POSTNUMMER ~
valmarkfast.POSTADRESS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VMAGA valmarkfast.PROCENT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VMAGA valmarkfast
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VMAGA valmarkfast
&Scoped-define QUERY-STRING-BRW_VMAGA FOR EACH valmarkfast NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VMAGA OPEN QUERY BRW_VMAGA FOR EACH valmarkfast NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VMAGA valmarkfast
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VMAGA valmarkfast


/* Definitions for BROWSE BRW_VVARD                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VVARD valfastvard.VARDNR ~
valfastvard.OMRADE valfastvard.BENAMNING valfastvard.AONR valfastvard.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VVARD valfastvard.VARDNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VVARD valfastvard
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VVARD valfastvard
&Scoped-define QUERY-STRING-BRW_VVARD FOR EACH valfastvard NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VVARD OPEN QUERY BRW_VVARD FOR EACH valfastvard NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VVARD valfastvard
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VVARD valfastvard


/* Definitions for FRAME FRAME-MARKAGARE                                */

/* Definitions for FRAME FRAME-VARDNR                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-52 RAD_MARKVARD BTN_OK BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FABH RAD_MARKVARD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-FABH AS CHARACTER FORMAT "X(256)":U INITIAL "FASTIGHETSBETECKNING" 
     LABEL "Fastighetsbeteckning" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1.25
     FGCOLOR 12 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_BETECKNING AS CHARACTER FORMAT "X(50)" 
     VIEW-AS FILL-IN 
     SIZE 20.88 BY 1.25
     FONT 17.

DEFINE VARIABLE RAD_MARKVARD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Markägare", 1,
"Värderingsnummer kopplade till vald fastighet", 2
     SIZE 119 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 56 BY 1.5.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Koppla från markägare från fastighet".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Koppla markägare till fastighet".

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_HMARKNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Markägarnr" 
     VIEW-AS FILL-IN 
     SIZE 9.75 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN_HPERSONNUMMER AS CHARACTER FORMAT "XXXXXX-XXXX" INITIAL "0000000000" 
     LABEL "Personnummer" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .92.

DEFINE VARIABLE FILL-IN_MARKNAMN AS CHARACTER FORMAT "x(40)" 
     LABEL "Markägare" 
     VIEW-AS FILL-IN 
     SIZE 20.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_MARKNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Markägarnr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PERSONNUMMER AS CHARACTER FORMAT "XXXXXX-XXXX" INITIAL "0000000000" 
     LABEL "Personnummer" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63.5 BY 2.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MAGA FOR 
      markagaretemp SCROLLING.

DEFINE QUERY BRW_VMAGA FOR 
      valmarkfast SCROLLING.

DEFINE QUERY BRW_VVARD FOR 
      valfastvard SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MAGA C-Win _STRUCTURED
  QUERY BRW_MAGA NO-LOCK DISPLAY
      markagaretemp.MARKNR COLUMN-LABEL "Mark-!ägarnr" FORMAT ">>>9":U
      markagaretemp.MARKAGARE COLUMN-LABEL "Markägare" FORMAT "X(256)":U
            WIDTH 25
      markagaretemp.PERSONNUMMER COLUMN-LABEL "Personnummer" FORMAT "999999-9999":U
      markagaretemp.PNR2 COLUMN-LABEL "Make/Maka" FORMAT "999999-9999":U
            WIDTH 12
      markagaretemp.GATUADRESS COLUMN-LABEL "Gatuadress" FORMAT "X(256)":U
            WIDTH 25
      markagaretemp.POSTNUMMER COLUMN-LABEL "Postnummer" FORMAT "XXX XX":U
      markagaretemp.POSTADRESS COLUMN-LABEL "Postadress" FORMAT "X(256)":U
            WIDTH 19
  ENABLE
      markagaretemp.MARKNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 63.5 BY 15.5
         TITLE "Markägarförteckning".

DEFINE BROWSE BRW_VMAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VMAGA C-Win _STRUCTURED
  QUERY BRW_VMAGA NO-LOCK DISPLAY
      valmarkfast.MARKNR COLUMN-LABEL "Mark-!ägarnr" FORMAT ">>>9":U
      valmarkfast.PROCENT COLUMN-LABEL "Pro!cent" FORMAT ">>9":U
      valmarkfast.MARKAGARE COLUMN-LABEL "Markägare" FORMAT "X(256)":U
            WIDTH 25
      valmarkfast.PERSONNUMMER COLUMN-LABEL "Personnummer" FORMAT "999999-9999":U
      valmarkfast.PNR2 COLUMN-LABEL "Make/Maka" FORMAT "999999-9999":U
            WIDTH 12
      valmarkfast.GATUADRESS COLUMN-LABEL "Gatuadress" FORMAT "X(256)":U
            WIDTH 25
      valmarkfast.POSTNUMMER COLUMN-LABEL "Postnummer" FORMAT "XXX XX":U
      valmarkfast.POSTADRESS COLUMN-LABEL "Postadress" FORMAT "X(256)":U
            WIDTH 19
  ENABLE
      valmarkfast.PROCENT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 54.5 BY 15.5
         TITLE "Valda markägare".

DEFINE BROWSE BRW_VVARD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VVARD C-Win _STRUCTURED
  QUERY BRW_VVARD NO-LOCK DISPLAY
      valfastvard.VARDNR COLUMN-LABEL "Värdering!nr" FORMAT ">>>>>9":U
      valfastvard.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      valfastvard.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
      valfastvard.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valfastvard.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
  ENABLE
      valfastvard.VARDNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 57.5 BY 11
         TITLE "Valda värderingar" ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BARABREDD
     FILL-IN-FABH AT ROW 1.25 COL 22.5 COLON-ALIGNED
     FILL-IN_BETECKNING AT ROW 1.25 COL 77 COLON-ALIGNED NO-LABEL
     RAD_MARKVARD AT ROW 2.58 COL 1.5 NO-LABEL
     BTN_OK AT ROW 28 COL 94.63
     BTN_AVSL AT ROW 28 COL 109.63
     RECT-52 AT ROW 11.25 COL 2.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.38 BY 28.42.

DEFINE FRAME FRAME-MARKAGARE
     FILL-IN_HMARKNR AT ROW 1.13 COL 22 COLON-ALIGNED
     FILL-IN_HPERSONNUMMER AT ROW 2.21 COL 22 COLON-ALIGNED
     BTN_HAMT AT ROW 3.25 COL 17
     BRW_MAGA AT ROW 4.63 COL 1.5
     BRW_VMAGA AT ROW 4.63 COL 69.63
     BTN_OVER AT ROW 9.92 COL 65
     BTN_BACK AT ROW 12.42 COL 65
     BTN_NY AT ROW 20.63 COL 12
     BTN_UPP AT ROW 20.63 COL 24.63
     BTN_BORT AT ROW 20.63 COL 37.25
     FILL-IN_MARKNR AT ROW 22.58 COL 21.5 COLON-ALIGNED
     FILL-IN_MARKNAMN AT ROW 22.58 COL 41.75 COLON-ALIGNED
     FILL-IN_PERSONNUMMER AT ROW 23.58 COL 21.5 COLON-ALIGNED
     "Koppla markägare till fastighet" VIEW-AS TEXT
          SIZE 40.5 BY 1.04 AT ROW 3.5 COL 78.25
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 7.5 BY .83 AT ROW 22.54 COL 2
     RECT-53 AT ROW 22.33 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.71
         SIZE 124 BY 24.

DEFINE FRAME FRAME-VARDNR
     BRW_VVARD AT ROW 1.5 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 6.33
         SIZE 62 BY 13.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: markagaretemp T "?" NO-UNDO temp-db markagaretemp
      TABLE: valfastvard T "?" NO-UNDO temp-db valfastvard
      TABLE: valmarkfast T "?" NO-UNDO temp-db valmarkfast
      TABLE: varderingtemp T "?" NO-UNDO temp-db varderingtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Registrera ändra markägare, koppla markägare till fastighet"
         HEIGHT             = 28.42
         WIDTH              = 124.38
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
ASSIGN FRAME FRAME-MARKAGARE:FRAME = FRAME FRAME-BARABREDD:HANDLE
       FRAME FRAME-VARDNR:FRAME = FRAME FRAME-BARABREDD:HANDLE.

/* SETTINGS FOR FRAME FRAME-BARABREDD
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-FABH IN FRAME FRAME-BARABREDD
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_BETECKNING IN FRAME FRAME-BARABREDD
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_BETECKNING:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

/* SETTINGS FOR FRAME FRAME-MARKAGARE
                                                                        */
/* BROWSE-TAB BRW_MAGA BTN_HAMT FRAME-MARKAGARE */
/* BROWSE-TAB BRW_VMAGA BRW_MAGA FRAME-MARKAGARE */
ASSIGN 
       BRW_MAGA:MAX-DATA-GUESS IN FRAME FRAME-MARKAGARE         = 2000
       BRW_MAGA:COLUMN-RESIZABLE IN FRAME FRAME-MARKAGARE       = TRUE.

ASSIGN 
       BRW_VMAGA:COLUMN-RESIZABLE IN FRAME FRAME-MARKAGARE       = TRUE.

/* SETTINGS FOR FRAME FRAME-VARDNR
                                                                        */
/* BROWSE-TAB BRW_VVARD 1 FRAME-VARDNR */
ASSIGN 
       BRW_VVARD:COLUMN-RESIZABLE IN FRAME FRAME-VARDNR       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MAGA
/* Query rebuild information for BROWSE BRW_MAGA
     _TblList          = "Temp-Tables.markagaretemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.markagaretemp.MARKNR
"markagaretemp.MARKNR" "Mark-!ägarnr" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.markagaretemp.MARKAGARE
"markagaretemp.MARKAGARE" "Markägare" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.markagaretemp.PERSONNUMMER
"markagaretemp.PERSONNUMMER" "Personnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.markagaretemp.PNR2
"markagaretemp.PNR2" "Make/Maka" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.markagaretemp.GATUADRESS
"markagaretemp.GATUADRESS" "Gatuadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.markagaretemp.POSTNUMMER
"markagaretemp.POSTNUMMER" "Postnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.markagaretemp.POSTADRESS
"markagaretemp.POSTADRESS" "Postadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MAGA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VMAGA
/* Query rebuild information for BROWSE BRW_VMAGA
     _TblList          = "Temp-Tables.valmarkfast"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valmarkfast.MARKNR
"valmarkfast.MARKNR" "Mark-!ägarnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valmarkfast.PROCENT
"valmarkfast.PROCENT" "Pro!cent" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valmarkfast.MARKAGARE
"valmarkfast.MARKAGARE" "Markägare" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valmarkfast.PERSONNUMMER
"valmarkfast.PERSONNUMMER" "Personnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valmarkfast.PNR2
"valmarkfast.PNR2" "Make/Maka" ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.valmarkfast.GATUADRESS
"valmarkfast.GATUADRESS" "Gatuadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.valmarkfast.POSTNUMMER
"valmarkfast.POSTNUMMER" "Postnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.valmarkfast.POSTADRESS
"valmarkfast.POSTADRESS" "Postadress" "X(256)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VMAGA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VVARD
/* Query rebuild information for BROWSE BRW_VVARD
     _TblList          = "Temp-Tables.valfastvard"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valfastvard.VARDNR
"valfastvard.VARDNR" "Värdering!nr" ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valfastvard.OMRADE
"valfastvard.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valfastvard.BENAMNING
"valfastvard.BENAMNING" "Benämning" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valfastvard.AONR
"valfastvard.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valfastvard.DELNR
"valfastvard.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VVARD */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrera ändra markägare, koppla markägare till fastighet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrera ändra markägare, koppla markägare till fastighet */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MAGA
&Scoped-define FRAME-NAME FRAME-MARKAGARE
&Scoped-define SELF-NAME BRW_MAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MAGA C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_MAGA IN FRAME FRAME-MARKAGARE /* Markägarförteckning */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MAGA C-Win
ON VALUE-CHANGED OF BRW_MAGA IN FRAME FRAME-MARKAGARE /* Markägarförteckning */
DO:
   IF brwhjvar = 0 THEN brwhjvar = 1.
   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
   IF AVAILABLE markagaretemp THEN DO:
       
      FIND FIRST valmarkfast WHERE valmarkfast.MARKNR = markagaretemp.MARKNR NO-LOCK NO-ERROR.
      IF AVAILABLE valmarkfast THEN DO:
         IF brwhjvar = 1 THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(valmarkfast)).
            RUN lastselectdyn_UI IN brwproc[3].              
         END.
      END.            
      ELSE DO:
         status-ok = BRW_VMAGA:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
      END.
   END.   
   brwhjvar = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VMAGA
&Scoped-define SELF-NAME BRW_VMAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VMAGA C-Win
ON ROW-DISPLAY OF BRW_VMAGA IN FRAME FRAME-MARKAGARE /* Valda markägare */
DO:
   ASSIGN
   valmarkfast.MARKNR:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valmarkfast.PROCENT:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valmarkfast.MARKAGARE:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valmarkfast.PERSONNUMMER:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valmarkfast.GATUADRESS:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valmarkfast.POSTNUMMER:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
   valmarkfast.POSTADRESS:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VMAGA C-Win
ON VALUE-CHANGED OF BRW_VMAGA IN FRAME FRAME-MARKAGARE /* Valda markägare */
DO:
   IF brwhjvar = 0 THEN brwhjvar = 2.
   status-ok = BRW_VMAGA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF AVAILABLE valmarkfast THEN DO:
       
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = valmarkfast.MARKNR NO-LOCK NO-ERROR.
      IF AVAILABLE markagaretemp THEN DO:
         IF brwhjvar = 2 THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
            RUN lastselectdyn_UI IN brwproc[1].  
         END.
      END.            
      ELSE DO:
         status-ok = BRW_MAGA:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
      END.
   END.  
   brwhjvar = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VVARD
&Scoped-define FRAME-NAME FRAME-VARDNR
&Scoped-define SELF-NAME BRW_VVARD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VVARD C-Win
ON ROW-DISPLAY OF BRW_VVARD IN FRAME FRAME-VARDNR /* Valda värderingar */
DO:
   ASSIGN
   valfastvard.VARDNR:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valfastvard.OMRADE:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valfastvard.BENAMNING:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12
   valfastvard.AONR:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
   valfastvard.DELNR:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL C-Win
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-BARABREDD /* Avbryt */
DO:              
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-MARKAGARE
&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME FRAME-MARKAGARE
DO:        
   status-ok = BRW_VMAGA:SELECT-FOCUSED-ROW() IN FRAME FRAME-MARKAGARE NO-ERROR.
   IF AVAILABLE valmarkfast THEN DO:
      markvrec = RECID(valmarkfast).  
   END.
   ELSE RETURN NO-APPLY.
   FIND FIRST valmarkfast WHERE RECID(valmarkfast) = markvrec NO-LOCK NO-ERROR.
   IF AVAILABLE valmarkfast THEN DO:                    
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = valmarkfast.MARKNR USE-INDEX MARKNR NO-LOCK NO-ERROR.
      DELETE valmarkfast.
      RUN selnextprevrow_UI IN brwproc[3].
   END.           
   RUN valdamark_UI. 
   IF AVAILABLE markagaretemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
      RUN lastselectdyn_UI IN brwproc[1].   
   END.
   RUN lastselectdyn_UI IN brwproc[3].      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME FRAME-MARKAGARE /* Ta bort */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   FIND FIRST valmarkfast WHERE valmarkfast.MARKNR = markagaretemp.MARKNR USE-INDEX FAST NO-LOCK NO-ERROR.
   IF AVAILABLE valmarkfast THEN DO:
      MESSAGE "Det finns fastigheter som har denna markägare kopplad." SKIP
      "Ta bort kopplingen till fastigheten:" SUBSTRING(valmarkfast.BETECKNING,1,40) " först." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.      
   ELSE DO:
      RUN bort_UI.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT C-Win
ON CHOOSE OF BTN_HAMT IN FRAME FRAME-MARKAGARE /* Hämta och visa urval */
DO:   
   FILL-IN_HMARKNR = INPUT FILL-IN_HMARKNR.
   FILL-IN_HPERSONNUMMER = INPUT FILL-IN_HPERSONNUMMER.
   tthandle = TEMP-TABLE markagaretemp:HANDLE. 
   IF FILL-IN_HMARKNR = 0 AND FILL-IN_HPERSONNUMMER = "0000000000" THEN DO:
      fragvar = "".
   END.
   ELSE IF FILL-IN_HMARKNR = 0 THEN DO:
        fragvar = " WHERE PERSONNUMMER = " + "'" + FILL-IN_HPERSONNUMMER + "'".
   END.
   ELSE DO:
      fragvar = " WHERE MARKNR = " + STRING(FILL-IN_HMARKNR).
   END.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "MARKAGARE", INPUT fragvar).
   FOR EACH valmarkfast:      
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = valmarkfast.MARKNR
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE markagaretemp THEN DO: 
         fragvar = " WHERE MARKNR = " + STRING(valmarkfast.MARKNR).
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle APPEND, INPUT "MARKAGARE", INPUT fragvar).
      END.
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   IF FILL-IN_HMARKNR = 0 AND FILL-IN_HPERSONNUMMER = "000000000000" THEN DO:
      FIND FIRST markagaretemp NO-LOCK NO-ERROR.
   END.
   ELSE IF FILL-IN_HMARKNR = 0 THEN DO:
      FIND FIRST markagaretemp WHERE markagaretemp.PERSONNUMMER = FILL-IN_HPERSONNUMMER
      NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = FILL-IN_HMARKNR
      NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE markagaretemp THEN DO: 
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
      RUN lastselectdyn_UI IN brwproc[1].    
   END.   
   APPLY "VALUE-CHANGED" TO BRW_MAGA.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME FRAME-MARKAGARE /* Ny */
DO:
   marknrvar = ?.
   {muswait.i} 
   tthandle = TEMP-TABLE markagaretemp:HANDLE. 
   vart = "NYA".  
   RUN ANDMARKV.W (INPUT-OUTPUT marknrvar,OUTPUT returnr,OUTPUT FILL-IN_HPERSONNUMMER).      
   IF returnr = 3 THEN DO:
      musz = FALSE.
      FIND FIRST markagaretemp WHERE markagaretemp.PERSONNUMMER = FILL-IN_HPERSONNUMMER
      NO-LOCK NO-ERROR.
      IF AVAILABLE markagaretemp THEN DO: 
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
         RUN lastselectdyn_UI IN brwproc[1].    
      END.   
      ELSE DO:
         fragvar = " WHERE PERSONNUMMER = " + "'" + FILL-IN_HPERSONNUMMER + "'".
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle APPEND, INPUT "MARKAGARE", INPUT fragvar).
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         FIND FIRST markagaretemp WHERE markagaretemp.PERSONNUMMER = FILL-IN_HPERSONNUMMER
         NO-LOCK NO-ERROR.
         IF AVAILABLE markagaretemp THEN DO: 
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
            RUN lastselectdyn_UI IN brwproc[1].    
         END. 
         APPLY "VALUE-CHANGED" TO BRW_MAGA.
      END.
   END.
   ELSE IF returnr = 33 THEN DO:
      musz = FALSE.
      FIND FIRST markagaretemp WHERE markagaretemp.PNR2 = FILL-IN_HPERSONNUMMER
      NO-LOCK NO-ERROR.
      IF AVAILABLE markagaretemp THEN DO: 
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
         RUN lastselectdyn_UI IN brwproc[1].    
      END.   
      ELSE DO:
         fragvar = " WHERE PNR2 = " + "'" + FILL-IN_HPERSONNUMMER + "'".
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle APPEND, INPUT "MARKAGARE", INPUT fragvar).
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         FIND FIRST markagaretemp WHERE markagaretemp.PNR2 = FILL-IN_HPERSONNUMMER
         NO-LOCK NO-ERROR.
         IF AVAILABLE markagaretemp THEN DO: 
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
            RUN lastselectdyn_UI IN brwproc[1].    
         END. 
         APPLY "VALUE-CHANGED" TO BRW_MAGA.
      END.
   END.
   ELSE DO:
      IF musz = TRUE OR returnr = 4 THEN DO:
         musz = FALSE.
         RETURN NO-APPLY.
      END.
      RUN mark_UI.   
   END.
   {musarrow.i} 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME FRAME-BARABREDD /* Ok */
DO: 
   tot% = 0.
   FOR EACH valmarkfast  WHERE valmarkfast.BETECKNING = fastighettemp.BETECKNING USE-INDEX FAST NO-LOCK:
      tot% = tot% + valmarkfast.PROCENT.      
   END.   
   IF tot% = 0 THEN tot% = tot%.
   ELSE IF tot% < 99 THEN DO:   
      MESSAGE "Du har bara registrerat att fastigheten ägs till" tot% " %." SKIP
      "Är detta rätt tryck JA, då kommer enbart " tot% "% betalas ut." SKIP         
      "Tryck Nej om du vill komplettera upp till 100 %."
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val5 AS LOGICAL.        
      IF val5 = FALSE THEN DO:  
         RETURN NO-APPLY.
      END.         
   END.         
   ELSE IF tot% > 100 THEN DO:   
      MESSAGE "Fastigheten kan inte ägas till mer än 100 %" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
      
   END.         
   korvar = "".
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN DO:
      FIND FIRST valfastvard WHERE valfastvard.BETECKNING = fastighettemp.BETECKNING
      AND valfastvard.VARDNR = varderingtemp.VARDNR USE-INDEX FAST NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valfastvard THEN DO: 
         FIND LAST valfastvard WHERE valfastvard.BETECKNING = fastighettemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.         
      END.
   END.
   ELSE DO:
      FIND LAST valfastvard WHERE valfastvard.BETECKNING = fastighettemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.       
   END.
   IF AVAILABLE valfastvard THEN DO:
      FIND FIRST valmarkfast WHERE valmarkfast.BETECKNING = fastighettemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.  
      IF NOT AVAILABLE valmarkfast THEN DO: 
         MESSAGE "För att kunna markvärdera måste markägare vara kopplad."
         "Det finns ingen markägare kopplad till denna fastighet." 
         "Vill du lägga upp markägare innan du går vidare?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.        
         IF val3 = FALSE THEN DO:  
            RUN btnok IN markagareapph (INPUT fastighbet,INPUT valvardnr,
                                        INPUT TABLE valfastvard,INPUT TABLE valmarkfast).
            APPLY "GO" TO BTN_OK IN FRAME FRAME-BARABREDD.
         END.
      END.
      ELSE DO:
         /*värdera?*/
         IF AVAILABLE varderingtemp AND varifran = 1 THEN DO:                     
            IF valfastvard.VARDNR = varderingtemp.VARDNR THEN musz = musz.
            ELSE DO :
               /*man ska inte kunna markvärdera på en värdering som inte är vald*/
               FIND LAST varderingtemp WHERE varderingtemp.VARDNR = valfastvard.VARDNR 
               AND varderingtemp.VARDNR = valvardnr  USE-INDEX VARDNR NO-LOCK NO-ERROR.
            END.
            IF AVAILABLE varderingtemp  THEN DO:                
               RUN btnok IN markagareapph (INPUT fastighbet,INPUT valvardnr,
                                           INPUT TABLE valfastvard,INPUT TABLE valmarkfast).
               MESSAGE "Vill du markvärdera för fastighet:" SUBSTRING(fastighettemp.BETECKNING,1,40) SKIP
                    "på värderingsnummer" varderingtemp.VARDNR varderingtemp.BENAMNING "direkt?"
   
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.        
               IF val4 = TRUE THEN DO:  
                  tmpvardrec = RECID(varderingtemp).
                  korvar = "VARDSPEC.W".               
               END.
               ELSE korvar = "".
            END.
         END.
         RUN btnok IN markagareapph (INPUT fastighbet,INPUT valvardnr,
                                     INPUT TABLE valfastvard,INPUT TABLE valmarkfast).
         IF korvar NE "" THEN DO: 
            {AVBGOM.I}
            RUN VALUE(korvar) (INPUT valvardnr,INPUT fastighettemp.BETECKNING,INPUT ?).
            {AVBFRAM.I}   
         END.
         APPLY "GO" TO BTN_OK IN FRAME FRAME-BARABREDD.
      END.
   END.
   ELSE DO:
      RUN btnok IN markagareapph (INPUT fastighbet,INPUT valvardnr,
                                  INPUT TABLE valfastvard,INPUT TABLE valmarkfast).
      APPLY "GO" TO BTN_OK IN FRAME FRAME-BARABREDD.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON GO OF BTN_OK IN FRAME FRAME-BARABREDD /* Ok */
DO:
      
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-MARKAGARE
&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER C-Win
ON CHOOSE OF BTN_OVER IN FRAME FRAME-MARKAGARE
DO:  
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() IN FRAME FRAME-MARKAGARE NO-ERROR.
   FIND FIRST valmarkfast WHERE valmarkfast.BETECKNING = fastighettemp.BETECKNING AND
   valmarkfast.MARKNR = markagaretemp.MARKNR USE-INDEX FAST NO-LOCK NO-ERROR.
   IF AVAILABLE valmarkfast THEN DO:
       MESSAGE "Markägare" markagaretemp.MARKNR "finns redan upplagd på denna fastighet." VIEW-AS ALERT-BOX.        
   END.
   ELSE DO:  
      ASSIGN     
      fastighbet = fastighettemp.BETECKNING. 
      marknrvar = markagaretemp.MARKNR.
      RUN MARKPROCENT.W (INPUT valvardnr,INPUT fastighbet,INPUT marknrvar).     
   END.                                   
   RUN valdamark_UI.
   FIND FIRST valmarkfast WHERE valmarkfast.BETECKNING = fastighettemp.BETECKNING
   AND valmarkfast.MARKNR = marknrvar USE-INDEX FAST NO-LOCK NO-ERROR.
   IF AVAILABLE valmarkfast THEN DO:
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(valmarkfast)).
      RUN lastselectdyn_UI IN brwproc[3].   
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP C-Win
ON CHOOSE OF BTN_UPP IN FRAME FRAME-MARKAGARE /* Ändra */
DO:
   IF brwval() = TRUE THEN RETURN NO-APPLY.
   RUN andra_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_MARKNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MARKNAMN C-Win
ON ANY-KEY OF FILL-IN_MARKNAMN IN FRAME FRAME-MARKAGARE /* Markägare */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_MARKNAMN IN FRAME FRAME-MARKAGARE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MARKNAMN C-Win
ON LEAVE OF FILL-IN_MARKNAMN IN FRAME FRAME-MARKAGARE /* Markägare */
DO:
   FILL-IN_MARKNAMN = INPUT FILL-IN_MARKNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MARKNAMN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_MARKNAMN IN FRAME FRAME-MARKAGARE /* Markägare */
DO:
   FILL-IN_MARKNAMN = INPUT FILL-IN_MARKNAMN.
   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() NO-ERROR.
   tmpmarkrec = RECID(markagaretemp).
   IF FILL-IN_MARKNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_MARKNAMN IN FRAME FRAME-MARKAGARE.
      RETURN NO-APPLY.
   END.      
   ortkalks = '*' + FILL-IN_MARKNAMN + '*'.
   FIND markagaretemp WHERE RECID(markagaretemp) = tmpmarkrec NO-LOCK NO-ERROR. 
   FIND NEXT markagaretemp WHERE markagaretemp.MARKAGARE MATCHES ortkalks 
   USE-INDEX MARKNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE markagaretemp THEN DO:
      FIND FIRST markagaretemp WHERE markagaretemp.MARKAGARE MATCHES ortkalks     
      USE-INDEX MARKNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE markagaretemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_MARKNAMN IN FRAME FRAME-MARKAGARE.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE markagaretemp THEN DO: 
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
      RUN lastselectdyn_UI IN brwproc[1].    
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_MARKNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MARKNR C-Win
ON ANY-KEY OF FILL-IN_MARKNR IN FRAME FRAME-MARKAGARE /* Markägarnr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_MARKNR IN FRAME FRAME-MARKAGARE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MARKNR C-Win
ON LEAVE OF FILL-IN_MARKNR IN FRAME FRAME-MARKAGARE /* Markägarnr */
DO:
   FILL-IN_MARKNR = INPUT FILL-IN_MARKNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MARKNR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_MARKNR IN FRAME FRAME-MARKAGARE /* Markägarnr */
DO:
   FILL-IN_MARKNR = INPUT FILL-IN_MARKNR.
   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() NO-ERROR.
   tmpmarkrec = RECID(markagaretemp).
   IF FILL-IN_MARKNR = 0 OR FILL-IN_MARKNR = ? THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt eller 0." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_MARKNR IN FRAME FRAME-MARKAGARE.
      RETURN NO-APPLY.
   END.        
   FIND markagaretemp WHERE RECID(markagaretemp) = markrec NO-LOCK NO-ERROR.
   FIND NEXT markagaretemp WHERE markagaretemp.MARKNR = FILL-IN_MARKNR  
   USE-INDEX MARKNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE markagaretemp THEN DO:
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = FILL-IN_MARKNR
      USE-INDEX MARKNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE markagaretemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_MARKNR IN FRAME FRAME-MARKAGARE.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE markagaretemp THEN DO:      
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
      RUN lastselectdyn_UI IN brwproc[1].  
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PERSONNUMMER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PERSONNUMMER C-Win
ON ANY-KEY OF FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE /* Personnummer */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PERSONNUMMER C-Win
ON LEAVE OF FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE /* Personnummer */
DO:
      
   FILL-IN_PERSONNUMMER = INPUT FILL-IN_PERSONNUMMER.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PERSONNUMMER C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE /* Personnummer */
DO:
   FILL-IN_PERSONNUMMER = INPUT FILL-IN_PERSONNUMMER.
   IF LENGTH(FILL-IN_PERSONNUMMER) < 10 THEN DO:   
      MESSAGE "Fyll i hela personnumret." VIEW-AS ALERT-BOX.      
      APPLY "ENTRY" TO FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE.
      RETURN NO-APPLY.   
   END. 

   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() NO-ERROR.
   tmpmarkrec = RECID(markagaretemp).
   IF FILL-IN_PERSONNUMMER = "" OR FILL-IN_PERSONNUMMER = ? THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt eller 0." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE.
      RETURN NO-APPLY.
   END.        
   FIND markagaretemp WHERE RECID(markagaretemp) = markrec NO-LOCK NO-ERROR.
   FIND NEXT markagaretemp WHERE markagaretemp.PERSONNUMMER = FILL-IN_PERSONNUMMER  
   USE-INDEX MARKNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE markagaretemp THEN DO:
      FIND FIRST markagaretemp WHERE markagaretemp.PERSONNUMMER = FILL-IN_PERSONNUMMER
      USE-INDEX MARKNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE markagaretemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_PERSONNUMMER IN FRAME FRAME-MARKAGARE.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE markagaretemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
      RUN lastselectdyn_UI IN brwproc[1].  
   END.   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-BARABREDD
&Scoped-define SELF-NAME RAD_MARKVARD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_MARKVARD C-Win
ON VALUE-CHANGED OF RAD_MARKVARD IN FRAME FRAME-BARABREDD
DO:
   RAD_MARKVARD = INPUT RAD_MARKVARD.
   IF RAD_MARKVARD = 1 THEN DO:
      FRAME FRAME-MARKAGARE:HIDDEN = FALSE.
      FRAME FRAME-VARDNR:HIDDEN = TRUE.
      FRAME FRAME-MARKAGARE:MOVE-TO-TOP().
   END.
   IF RAD_MARKVARD = 2 THEN DO:
      FRAME FRAME-MARKAGARE:HIDDEN = TRUE.
      FRAME FRAME-VARDNR:HIDDEN = FALSE.
      FRAME FRAME-MARKAGARE:MOVE-TO-BOTTOM().
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MAGA
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(markagareapph) THEN DELETE PROCEDURE markagareapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
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
   
   &Scoped-define FRAME-NAME FRAME-VARDNR
   &Scoped-define FORMATNAMN valfastvard.AONR
   &Scoped-define BROWSE-NAME BRW_VVARD
   {AOFORMAT1.I}
   &Scoped-define FRAME-NAME FRAME-BARABREDD
   FRAME FRAME-MARKAGARE:HIDDEN = FALSE.
   FRAME FRAME-MARKAGARE:MOVE-TO-TOP().
   EMPTY TEMP-TABLE markagaretemp NO-ERROR. 
   RUN ladda IN markagareapph (INPUT fastighbet,OUTPUT TABLE valfastvard,   
                              OUTPUT TABLE valmarkfast).
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR. 
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   IF AVAILABLE varderingtemp THEN DO:
      ASSIGN vardben = varderingtemp.BENAMNING.
   END.
   ASSIGN FILL-IN_BETECKNING = fastighettemp.BETECKNING.
   ASSIGN FILL-IN-FABH = fastighettemp.BETECKNING.
   RUN setorgtitle_UI IN brwproc[3] (INPUT "Valda markägare för fastighet : " + fastighbet).            
   RUN setorgtitle_UI IN brwproc[4] (INPUT "Valda värderingsnr för fastighet : " + fastighbet).
   
   RUN enable_UI.       
   {FRMSIZE.I}
  
   RUN valdavard_UI.
   tthandle = TEMP-TABLE markagaretemp:HANDLE. 
   FOR EACH valmarkfast:      
      FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = valmarkfast.MARKNR
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE markagaretemp THEN DO: 
         fragvar = " WHERE MARKNR = " + STRING(valmarkfast.MARKNR).
         RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle APPEND, INPUT "MARKAGARE", INPUT fragvar).
      END.
   END.
   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   ASSIGN
   valfastvard.AONR:LABEL IN BROWSE BRW_VVARD = Guru.Konstanter:gaok
   valfastvard.OMRADE:LABEL IN BROWSE BRW_VVARD = Guru.Konstanter:gomrk.
   RUN valdamark_UI. 
   IF AVAILABLE markagaretemp THEN DO:
       
      FIND FIRST valmarkfast WHERE valmarkfast.MARKNR = markagaretemp.MARKNR NO-LOCK NO-ERROR.
      IF AVAILABLE valmarkfast THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(valmarkfast)).
         RUN lastselectdyn_UI IN brwproc[3].                       
      END.            
      ELSE DO:
         status-ok = BRW_VMAGA:DESELECT-FOCUSED-ROW() IN FRAME FRAME-MARKAGARE NO-ERROR.     
      END.
   END.   
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
   ASSIGN
   markagaretemp.MARKNR:READ-ONLY IN BROWSE BRW_MAGA = TRUE
   valmarkfast.PROCENT:READ-ONLY IN BROWSE BRW_VMAGA = FALSE
   valfastvard.VARDNR:READ-ONLY IN BROWSE BRW_VVARD = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MAGA:HANDLE IN FRAME FRAME-MARKAGARE).  
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_VMAGA:HANDLE IN FRAME FRAME-MARKAGARE).  
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_VVARD:HANDLE IN FRAME FRAME-VARDNR).  
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MARKAGAREAPP.P PERSISTENT SET markagareapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MARKAGAREAPP.P PERSISTENT SET markagareapph.
   END.  
   tthandle = TEMP-TABLE varderingtemp:HANDLE.
   valford = ' WHERE VARDERING.VARDNR = ' + STRING(valvardnr).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT valford).
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "VARDERING", INPUT valford).
   END.
   
   /*
   FIND FIRST markagaretemp WHERE NO-LOCK NO-ERROR.

   IF NOT AVAILABLE markagaretemp THEN DO:
      tthandle = TEMP-TABLE markagaretemp:HANDLE. 
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "MARKAGARE", INPUT "").
   END.
   */    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI C-Win 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() IN FRAME  FRAME-MARKAGARE NO-ERROR.
   markrec = RECID(markagaretemp).
   marknrvar = markagaretemp.MARKNR.
   {muswait.i}
   vart = "AND".
   RUN ANDMARKV.W (INPUT-OUTPUT marknrvar,OUTPUT returnr,OUTPUT fragvar).      
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   RUN mark_UI.   
   {musarrow.i}
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI C-Win 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_MAGA:SELECT-FOCUSED-ROW() IN FRAME FRAME-MARKAGARE NO-ERROR.
   markrec = RECID(markagaretemp).
   markrec2 = markrec.
   MESSAGE "Vill du verkligen ta bort denna markägare?"
   markagaretemp.MARKNR markagaretemp.MARKAGARE   "?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av markägare"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:      
      RUN bort IN markagareapph (INPUT markagaretemp.MARKNR).
      FIND FIRST markagaretemp WHERE RECID(markagaretemp) = markrec NO-LOCK NO-ERROR.
      IF AVAILABLE markagaretemp THEN DELETE markagaretemp.
      RUN selnextprevrow_UI IN brwproc[1].
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   ELSE DO:
      RETURN.
   END.

   musz = FALSE.     
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
  DISPLAY FILL-IN-FABH RAD_MARKVARD 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  ENABLE RECT-52 RAD_MARKVARD BTN_OK BTN_AVSL 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BARABREDD}
  DISPLAY FILL-IN_HMARKNR FILL-IN_HPERSONNUMMER FILL-IN_MARKNR FILL-IN_MARKNAMN 
          FILL-IN_PERSONNUMMER 
      WITH FRAME FRAME-MARKAGARE IN WINDOW C-Win.
  ENABLE RECT-53 FILL-IN_HMARKNR FILL-IN_HPERSONNUMMER BTN_HAMT BRW_MAGA 
         BRW_VMAGA BTN_OVER BTN_BACK BTN_NY BTN_UPP BTN_BORT FILL-IN_MARKNR 
         FILL-IN_MARKNAMN FILL-IN_PERSONNUMMER 
      WITH FRAME FRAME-MARKAGARE IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-MARKAGARE}
  ENABLE BRW_VVARD 
      WITH FRAME FRAME-VARDNR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VARDNR}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mark_UI C-Win 
PROCEDURE mark_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   FIND FIRST markagaretemp WHERE markagaretemp.MARKNR = marknrvar USE-INDEX MARKNR NO-LOCK NO-ERROR. 
   IF AVAILABLE markagaretemp THEN DO:   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(markagaretemp)).
      RUN lastselectdyn_UI IN brwproc[1].         
   END.    
   ENABLE BRW_MAGA WITH FRAME FRAME-MARKAGARE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdamark_UI C-Win 
PROCEDURE valdamark_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
   valford = 'valmarkfast.BETECKNING = "' + fastighettemp.BETECKNING + '"'.
   RUN setcolsortvar_UI IN brwproc[3] (INPUT valford).
   RUN openbdynspec_UI IN brwproc[3].   
   ENABLE BRW_VMAGA WITH FRAME FRAME-MARKAGARE.
   status-ok = BRW_VMAGA:DESELECT-FOCUSED-ROW() IN FRAME FRAME-MARKAGARE NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdavard_UI C-Win 
PROCEDURE valdavard_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
   valford = 'valfastvard.BETECKNING = "' + fastighettemp.BETECKNING + '"'.
   RUN setcolsortvar_UI IN brwproc[4] (INPUT valford).
   RUN openbdynspec_UI IN brwproc[4].   
   FIND FIRST valfastvard  WHERE valfastvard.BETECKNING = fastighettemp.BETECKNING
   USE-INDEX FAST NO-LOCK NO-ERROR.
   ENABLE BRW_VVARD WITH FRAME FRAME-VARDNR.
   status-ok = BRW_VVARD:DESELECT-FOCUSED-ROW() IN FRAME FRAME-VARDNR NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

   IF  BRW_MAGA:NUM-SELECTED-ROWS IN FRAME FRAME-MARKAGARE = 0 THEN RETURN TRUE.
   ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

