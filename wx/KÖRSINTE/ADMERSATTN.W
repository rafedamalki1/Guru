&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: används inte


  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/15/96 -  1:50 pm

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

/* Local Variable Definitions ---                                       */

{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE amtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE VARIABLE namnvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollev AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrben AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_lev2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE entryanv AS LOGICAL NO-UNDO.
DEFINE VARIABLE levinlast AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE menuh AS HANDLE NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE kolllkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokstring AS CHARACTER FORMAT "x(40)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE nyaquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE appqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE valdkommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbufmerah AS HANDLE NO-UNDO.
{EXTRADATA.I}
{DEFSOK.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{SOKDEF.I}
&Scoped-define NEW 
&Scoped-define SHARED 
{MTRLTEMP.I}
{LEVTEMP.I}
{ERSATTTEMP.I}
DEFINE TEMP-TABLE mtrltemp2 LIKE mtrltemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp mtrltemp2 ersattemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_HLEV-2                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV-2 mtrltemp2.Enr mtrltemp2.Benamning ~
mtrltemp2.Enhet mtrltemp2.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV-2 
&Scoped-define QUERY-STRING-BRW_HLEV-2 FOR EACH mtrltemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV-2 OPEN QUERY BRW_HLEV-2 FOR EACH mtrltemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV-2 mtrltemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV-2 mtrltemp2


/* Definitions for BROWSE BRW_VLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VLEV ersattemp.NAMN ersattemp.ANVANDARE ~
ersattemp.BEN1 ersattemp.BESTID ersattemp.OMRADE ersattemp.BEN2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VLEV ersattemp.NAMN ~
ersattemp.ANVANDARE ersattemp.BESTID ersattemp.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VLEV ersattemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VLEV ersattemp
&Scoped-define QUERY-STRING-BRW_VLEV FOR EACH ersattemp NO-LOCK ~
    BY ersattemp.NAMN
&Scoped-define OPEN-QUERY-BRW_VLEV OPEN QUERY BRW_VLEV FOR EACH ersattemp NO-LOCK ~
    BY ersattemp.NAMN.
&Scoped-define TABLES-IN-QUERY-BRW_VLEV ersattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VLEV ersattemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-4 IMAGE-16 IMAGE-17 BRW_VLEV BTN_NY ~
BTN_BORT FILL-IN-FRANLEV FILL-IN-ENR FILL-IN-TILLLEV FILL-IN-BEN FBTN_IEXC ~
RAD_SOK TOG_TRAFF TOG_EJTRAFF BRW_HLEV BRW_HLEV-2 BTN_UPP CMB_LEV CMB_LEV-2 ~
FILL-IN-KOD FILL-IN-KOD-2 FILL-IN-BEN-2 FILL-IN-BEN-3 RAD_SOK-2 RAD_SOK-3 ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-FRANLEV FILL-IN-ENR ~
FILL-IN-TILLLEV FILL-IN-BEN RAD_SOK TOG_TRAFF TOG_EJTRAFF CMB_LEV CMB_LEV-2 ~
FILL-IN-KOD FILL-IN-KOD-2 RAD_SOK-2 RAD_SOK-3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_IEXC 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Från lev i första kolumnen, Från Enr i andra, Till lev i tredje, Till Enr i fjärde".

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV-2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(11)":U 
     LABEL "Till enr" 
     VIEW-AS FILL-IN 
     SIZE 12.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN-2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 27.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN-3 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 27.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Från enr" 
     VIEW-AS FILL-IN 
     SIZE 12.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRANLEV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Från Lev" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD-2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILLLEV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Till Lev" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-16
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-17
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-4
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .79 NO-UNDO.

DEFINE VARIABLE RAD_SOK-2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .79 NO-UNDO.

DEFINE VARIABLE RAD_SOK-3 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .79 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 3.29
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41.5 BY 3.29
     BGCOLOR 8 .

DEFINE RECTANGLE RECT_STOR
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83.5 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE TOG_EJTRAFF AS LOGICAL INITIAL no 
     LABEL "Ej träff" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_TRAFF AS LOGICAL INITIAL no 
     LABEL "Enbart träff" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_HLEV-2 FOR 
      mtrltemp2 SCROLLING.

DEFINE QUERY BRW_VLEV FOR 
      ersattemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 27
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 9.75
         TITLE "Materiel".

DEFINE BROWSE BRW_HLEV-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV-2 WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV-2 NO-LOCK DISPLAY
      mtrltemp2.Enr FORMAT "X(11)":U
      mtrltemp2.Benamning FORMAT "x(256)":U WIDTH 27
      mtrltemp2.Enhet FORMAT "x(5)":U
      mtrltemp2.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 54 BY 9.75
         TITLE "Materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-1 _STRUCTURED
  QUERY BRW_VLEV NO-LOCK DISPLAY
      ersattemp.NAMN COLUMN-LABEL "Från lev" FORMAT "x(8)":U
      ersattemp.ANVANDARE COLUMN-LABEL "Från Enr" FORMAT "x(11)":U
      ersattemp.BEN1 COLUMN-LABEL "Benäming" FORMAT "X(256)":U
            WIDTH 20
      ersattemp.BESTID COLUMN-LABEL "Till lev" FORMAT "x(8)":U
      ersattemp.OMRADE COLUMN-LABEL "Till enr" FORMAT "x(11)":U
      ersattemp.BEN2 COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 20
  ENABLE
      ersattemp.NAMN
      ersattemp.ANVANDARE
      ersattemp.BESTID
      ersattemp.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 111.5 BY 10.5
         TITLE "Materiel för vald leverantör. Varje post sparas när du lämnar raden.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_VLEV AT ROW 1.5 COL 1.5
     BTN_NY AT ROW 12.21 COL 85.5
     BTN_BORT AT ROW 12.21 COL 100.5
     FILL-IN-FRANLEV AT ROW 12.5 COL 13 COLON-ALIGNED WIDGET-ID 22
     FILL-IN-ENR AT ROW 12.5 COL 28 COLON-ALIGNED
     FILL-IN-TILLLEV AT ROW 12.5 COL 52.38 COLON-ALIGNED WIDGET-ID 16
     FILL-IN-BEN AT ROW 12.5 COL 66.88 COLON-ALIGNED
     FBTN_IEXC AT ROW 13.38 COL 85.5
     RAD_SOK AT ROW 13.63 COL 6.38 NO-LABEL
     TOG_TRAFF AT ROW 13.75 COL 46 WIDGET-ID 18
     TOG_EJTRAFF AT ROW 13.75 COL 64.5 WIDGET-ID 20
     BRW_HLEV AT ROW 14.83 COL 1.38
     BRW_HLEV-2 AT ROW 14.83 COL 60.5 WIDGET-ID 200
     BTN_UPP AT ROW 15 COL 56 WIDGET-ID 24
     CMB_LEV AT ROW 24.75 COL 20.5 COLON-ALIGNED
     CMB_LEV-2 AT ROW 24.75 COL 75 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-KOD AT ROW 26.13 COL 14.25 COLON-ALIGNED
     FILL-IN-KOD-2 AT ROW 26.13 COL 70 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-BEN-2 AT ROW 27.04 COL 14.25 COLON-ALIGNED
     FILL-IN-BEN-3 AT ROW 27.04 COL 70 COLON-ALIGNED WIDGET-ID 4
     RAD_SOK-2 AT ROW 28.17 COL 8.25 NO-LABEL
     RAD_SOK-3 AT ROW 28.17 COL 60.13 NO-LABEL WIDGET-ID 8
     BTN_AVB AT ROW 28.25 COL 101
     RECT_STOR AT ROW 12.25 COL 1.5
     RECT-22 AT ROW 25.96 COL 1.5
     IMAGE-4 AT ROW 26.25 COL 58.75
     IMAGE-16 AT ROW 12.54 COL 2
     IMAGE-17 AT ROW 26.25 COL 2
     RECT-23 AT ROW 25.96 COL 58.25 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.5 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: ersattemp T "?" NO-UNDO temp-db ersattemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: mtrltemp2 T "?" NO-UNDO temp-db mtrltemp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Uppdatera materielkatalog"
         HEIGHT             = 28.42
         WIDTH              = 114.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 116
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 116
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VLEV RECT-23 FRAME-A */
/* BROWSE-TAB BRW_HLEV TOG_EJTRAFF FRAME-A */
/* BROWSE-TAB BRW_HLEV-2 BRW_HLEV FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_HLEV-2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV-2:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_HLEV-2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV-2:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_VLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_VLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV-2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN-2 IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BEN-2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN-3 IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BEN-3:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-KOD-2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-23 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT_STOR IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "27" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV-2
/* Query rebuild information for BROWSE BRW_HLEV-2
     _TblList          = "Temp-Tables.mtrltemp2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp2.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp2.Benamning
"mtrltemp2.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "27" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp2.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp2.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VLEV
/* Query rebuild information for BROWSE BRW_VLEV
     _TblList          = "Temp-Tables.ersattemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.ersattemp.NAMN|yes"
     _FldNameList[1]   > Temp-Tables.ersattemp.NAMN
"ersattemp.NAMN" "Från lev" "x(8)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ersattemp.ANVANDARE
"ersattemp.ANVANDARE" "Från Enr" "x(11)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ersattemp.BEN1
"ersattemp.BEN1" "Benäming" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ersattemp.BESTID
"ersattemp.BESTID" "Till lev" "x(8)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.ersattemp.OMRADE
"ersattemp.OMRADE" "Till enr" "x(11)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.ersattemp.BEN2
"ersattemp.BEN2" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VLEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_VLEV
&Scoped-define SELF-NAME BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ENTRY OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ROW-LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:
   
  IF AVAILABLE ersattemp THEN DO:
     RUN visa_UI.       
     EMPTY TEMP-TABLE bersattemp NO-ERROR.
     IF TRIM(ersattemp.NAMN) = "NY" THEN DO:
        MESSAGE "Vill du ta bort posten?"  
        VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
        IF svar THEN DO:
           DELETE ersattemp.        
           RUN selnextprevrow_UI IN brwproc[1].  
           RUN openbdynspec_UI IN brwproc[1].
           RUN lastselectdyn_UI IN brwproc[1].           
        END.        
        ELSE RETURN NO-APPLY.
     END.
     ELSE IF ersattemp.NAMN = "" OR ersattemp.ANVANDARE = "" OR ersattemp.BESTID = "" OR ersattemp.OMRADE = "" THEN DO:
        MESSAGE "Inget av fälten får vara blankt"  VIEW-AS ALERT-BOX.
        MESSAGE "Vill du ta bort posten?"  
        VIEW-AS ALERT-BOX
        QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar2 AS LOGICAL.         
        IF svar2 THEN DO:
           DELETE ersattemp.        
           RUN selnextprevrow_UI IN brwproc[1].  
           RUN openbdynspec_UI IN brwproc[1].
           RUN lastselectdyn_UI IN brwproc[1].           
        END.        
        ELSE RETURN NO-APPLY.        
     END.
     ELSE DO:
        CREATE bersattemp.          
        BUFFER-COPY ersattemp TO bersattemp. 
        RUN checkmtrl_UI IN amtrlbapph ( INPUT TABLE bersattemp,OUTPUT kollfel ).
        FIND FIRST bersattemp NO-ERROR.
        IF kollfel = TRUE THEN DO:                                          
           MESSAGE "Detta enr finns redan i översättningslistan från vald leverantör till vald leverantör.Vill du ta bort posten?"  
           VIEW-AS ALERT-BOX
           QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar1 AS LOGICAL.         
           IF svar1 THEN DO:
              DELETE ersattemp.        
              RUN selnextprevrow_UI IN brwproc[1].  
              RUN openbdynspec_UI IN brwproc[1].
              RUN lastselectdyn_UI IN brwproc[1].           
           END.        
           ELSE RETURN NO-APPLY.                         
        END.
        ELSE DO:          
           RUN admsparmtrl_UI IN amtrlbapph ( INPUT-OUTPUT TABLE bersattemp ). 
           FIND FIRST bersattemp NO-ERROR.
           IF AVAILABLE bersattemp THEN DO:
               FIND FIRST ersattemp WHERE ersattemp.NAMN = bersattemp.NAMN AND   
               ersattemp.ANVANDARE = bersattemp.ANVANDARE AND  ersattemp.BESTID = bersattemp.BESTID NO-LOCK NO-ERROR.
               IF AVAILABLE ersattemp THEN DO:
                  ersattemp.MTRLROW = bersattemp.MTRLROW.          
                  DELETE bersattemp.
               END.
           END.           
        END.
     END.
     RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON VALUE-CHANGED OF BRW_VLEV IN FRAME FRAME-A /* Materiel för vald leverantör. Varje post sparas när du lämnar raden. */
DO:
   ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = FALSE.   
   status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ersattemp.NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.NAMN BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF ersattemp.NAMN IN BROWSE BRW_VLEV /* Från lev */
DO: 

   IF AVAILABLE ersattemp THEN DO:
      RUN visa_UI.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.NAMN BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF ersattemp.NAMN IN BROWSE BRW_VLEV /* Från lev */
DO:
   IF AVAILABLE ersattemp THEN DO:  
      IF TRIM(INPUT BROWSE BRW_VLEV ersattemp.NAMN) = "NY" THEN DO:
         MESSAGE "Vill du ta bort posten?"  
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar1 AS LOGICAL.         
         IF svar1 THEN DO:
            DELETE ersattemp.        
            RUN selnextprevrow_UI IN brwproc[1].  
            RUN openbdynspec_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].           
         END.        
         ELSE RETURN NO-APPLY.
      END.
      IF AVAILABLE ersattemp THEN DO: 
         IF ersattemp.NAMN NE INPUT BROWSE BRW_VLEV ersattemp.NAMN THEN DO:
            namnvar = INPUT BROWSE BRW_VLEV ersattemp.NAMN.
            IF namnvar NE "" THEN DO:
            
               RUN kolllev_UI IN amtrlbapph ( INPUT namnvar,OUTPUT kollev ). 
               IF kollev = TRUE THEN DO:
                  ersattemp.NAMN = INPUT BROWSE BRW_VLEV ersattemp.NAMN.
                  RUN visa_UI.               
               END.
               ELSE DO:
                  MESSAGE "Det finns ingen leverantör med lev-id: " namnvar VIEW-AS ALERT-BOX.
                  APPLY "ENTRY" TO ersattemp.NAMN IN BROWSE BRW_VLEV.      
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.NAMN BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF ersattemp.NAMN IN BROWSE BRW_VLEV /* Från lev */
DO:
   APPLY "ENTRY" TO ersattemp.NAMN IN BROWSE BRW_VLEV.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ersattemp.ANVANDARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.ANVANDARE BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF ersattemp.ANVANDARE IN BROWSE BRW_VLEV /* Från Enr */
DO:
   IF AVAILABLE ersattemp THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.ANVANDARE BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF ersattemp.ANVANDARE IN BROWSE BRW_VLEV /* Från Enr */
DO:   
   IF AVAILABLE ersattemp THEN DO:      
      IF ersattemp.ANVANDARE NE INPUT BROWSE BRW_VLEV ersattemp.ANVANDARE THEN DO:
         enrvar = INPUT BROWSE BRW_VLEV ersattemp.ANVANDARE.
         IF ersattemp.NAMN NE "" AND enrvar NE ""  THEN DO:
            RUN kollenrn_UI IN amtrlbapph ( INPUT ersattemp.NAMN,INPUT enrvar ,OUTPUT kollev,OUTPUT enrben ).             
            IF kollev = TRUE THEN DO:            
                ersattemp.BEN1 = enrben. 
                ersattemp.ANVANDARE = INPUT BROWSE BRW_VLEV ersattemp.ANVANDARE.           
                RUN visa_UI.                
            END.
            ELSE DO:               
               
               MESSAGE  "Det finns inget enr: " enrvar " hos leverantör med lev-id: " ersattemp.NAMN " Ska översättningen göras ändå?"   VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
               IF svar = TRUE THEN DO:
                  /*släpp igenom så att nyss borttagna går att styra*/
                  ersattemp.ANVANDARE = INPUT BROWSE BRW_VLEV ersattemp.ANVANDARE.           
                  RUN visa_UI.
               END.          
               IF svar = FALSE THEN DO:
                  APPLY "ENTRY" TO ersattemp.ANVANDARE IN BROWSE BRW_VLEV.      
                  RETURN NO-APPLY.
               END.   
            END.
         END.
      END.
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.ANVANDARE BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF ersattemp.ANVANDARE IN BROWSE BRW_VLEV /* Från Enr */
DO:
   APPLY "ENTRY" TO ersattemp.ANVANDARE IN BROWSE BRW_VLEV.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ersattemp.BESTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.BESTID BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF ersattemp.BESTID IN BROWSE BRW_VLEV /* Till lev */
DO:
   IF AVAILABLE ersattemp THEN RUN visa_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.BESTID BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF ersattemp.BESTID IN BROWSE BRW_VLEV /* Till lev */
DO:     
   IF AVAILABLE ersattemp THEN DO:
      IF ersattemp.BESTID NE INPUT BROWSE BRW_VLEV ersattemp.BESTID THEN DO:
         namnvar = INPUT BROWSE BRW_VLEV ersattemp.BESTID.
         IF namnvar NE "" THEN DO:
            RUN kolllev_UI IN amtrlbapph ( INPUT namnvar,OUTPUT kollev ). 
            IF kollev = TRUE THEN DO:
               ersattemp.BESTID = INPUT BROWSE BRW_VLEV ersattemp.BESTID.           
               RUN visa_UI.       
            END.
            ELSE DO:
               MESSAGE "Det finns ingen leverantör med lev-id: " namnvar VIEW-AS ALERT-BOX.
               APPLY "ENTRY" TO ersattemp.BESTID IN BROWSE BRW_VLEV.      
               RETURN NO-APPLY.                   
            END.
         END.
      END.
   END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.BESTID BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF ersattemp.BESTID IN BROWSE BRW_VLEV /* Till lev */
DO:
   APPLY "ENTRY" TO ersattemp.BESTID IN BROWSE BRW_VLEV.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ersattemp.OMRADE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.OMRADE BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF ersattemp.OMRADE IN BROWSE BRW_VLEV /* Till enr */
DO:
   IF AVAILABLE ersattemp THEN RUN visa_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.OMRADE BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF ersattemp.OMRADE IN BROWSE BRW_VLEV /* Till enr */
DO:   
   IF AVAILABLE ersattemp THEN DO:
      IF ersattemp.OMRADE NE INPUT BROWSE BRW_VLEV ersattemp.OMRADE THEN DO:
         enrvar = INPUT BROWSE BRW_VLEV ersattemp.OMRADE.
         IF ersattemp.BESTID NE "" AND enrvar NE ""  THEN DO:
            RUN kollenrn_UI IN amtrlbapph ( INPUT ersattemp.BESTID,INPUT enrvar ,OUTPUT kollev,OUTPUT enrben ). 
            IF kollev = TRUE THEN DO:             
               ersattemp.BEN2 = enrben.
               ersattemp.OMRADE = INPUT BROWSE BRW_VLEV ersattemp.OMRADE.           
               RUN visa_UI.               
            END.
            ELSE DO:               
               MESSAGE  "Det finns inget enr: " enrvar " hos leverantör med lev-id: " ersattemp.BESTID " Ska översättningen göras ändå?"   VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
               IF svar = TRUE THEN DO:
                  /*Mat Johansson vill kunna lägga upp översättning till ett enr som leverantören är sen med att få in i listan. Lena 20191114  */
                  ersattemp.OMRADE = INPUT BROWSE BRW_VLEV ersattemp.OMRADE.           
                  RUN visa_UI.
               END.          
               IF svar = FALSE THEN DO:
                  APPLY "ENTRY" TO ersattemp.OMRADE IN BROWSE BRW_VLEV.      
                  RETURN NO-APPLY.
               END.   
            END.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ersattemp.OMRADE BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF ersattemp.OMRADE IN BROWSE BRW_VLEV /* Till enr */
DO:
   APPLY "ENTRY" TO ersattemp.OMRADE IN BROWSE BRW_VLEV.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:  
  IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
  APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:                   
   ASSIGN
   antal_valda = BRW_VLEV:NUM-SELECTED-ROWS
   antal_raknare = 1.
   IF antal_valda > 1 THEN DO:
      MESSAGE "Vill du ta bort alla markerade ersättningsrader ?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar2 AS LOGICAL.         
      IF svar2 = TRUE  THEN DO TRANSACTION:
         DO WHILE antal_raknare LE antal_valda:
            ASSIGN                                   
            status-ok = BRW_VLEV:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE ersattemp THEN DO:
               mtrl_rowid = ROWID(ersattemp).                                       
               EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
               EMPTY TEMP-TABLE bersattemp.
               CREATE bersattemp.
               BUFFER-COPY ersattemp TO bersattemp.
               sprow = ROWID(ersattemp).
               RUN admbortmtrl_UI IN amtrlbapph (INPUT-OUTPUT TABLE bersattemp,OUTPUT TABLE felmeddtemp ). 
               FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
               IF AVAILABLE felmeddtemp THEN DO:
                  MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
                  DELETE felmeddtemp.                        
               END.            
               ELSE DO:
                  EMPTY TEMP-TABLE bersattemp.
                  FIND FIRST ersattemp WHERE ROWID(ersattemp) = sprow NO-ERROR.                                    
                  DELETE ersattemp.          
                  IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].                          
               END.                           
            END.
            antal_raknare = antal_raknare + 1.   
         END.   
      END.         
   END.
   ELSE  DO:
      DO WHILE antal_raknare LE antal_valda:
         ASSIGN                                   
         status-ok = BRW_VLEV:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE ersattemp THEN DO:
            mtrl_rowid = ROWID(ersattemp).                        
            MESSAGE "Vill du ta bort mtrl - " + ersattemp.NAMN + " " + ersattemp.ANVANDARE VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
            IF svar THEN DO TRANSACTION:
               EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
               EMPTY TEMP-TABLE bersattemp.
               CREATE bersattemp.
               BUFFER-COPY ersattemp TO bersattemp.
               sprow = ROWID(ersattemp).
               RUN admbortmtrl_UI IN amtrlbapph (INPUT-OUTPUT TABLE bersattemp,OUTPUT TABLE felmeddtemp ). 
               FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
               IF AVAILABLE felmeddtemp THEN DO:
                  MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
                  DELETE felmeddtemp.                        
               END.            
               ELSE DO:
                  EMPTY TEMP-TABLE bersattemp.
                  FIND FIRST ersattemp WHERE ROWID(ersattemp) = sprow NO-ERROR.                                    
                  DELETE ersattemp.          
                  IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].                          
               END.         
            END.   
         END.
         antal_raknare = antal_raknare + 1.   
      END.
   END.    
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " ").
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:                                        
   {muswait.i}   
   CREATE ersattemp.
   ASSIGN   
   ersattemp.NAMN = " NY"
   ersattemp.MTRLROW = ?.
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(ersattemp)).
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   {musarrow.i}                                
      
   APPLY "ENTRY" TO ersattemp.NAMN IN BROWSE BRW_VLEV.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A
DO:
  EMPTY TEMP-TABLE bersattemp NO-ERROR. 
  status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  status-ok2 = BRW_HLEV-2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  IF status-ok THEN DO: 
     IF status-ok2 THEN DO: 
        IF AVAILABLE mtrltemp THEN DO:
           IF AVAILABLE mtrltemp2 THEN DO:
              CREATE ersattemp.
              ASSIGN 
              ersattemp.NAMN =  mtrltemp.LEVKOD
              ersattemp.ANVANDARE =   mtrltemp.Enr             
              ersattemp.bestid     = mtrltemp2.LEVKO
              ersattemp.omrade = mtrltemp2.Enr.
           END.           
           CREATE bersattemp.          
           BUFFER-COPY ersattemp TO bersattemp.            
           RUN checkmtrl_UI IN amtrlbapph ( INPUT TABLE bersattemp,OUTPUT kollfel ).
           FIND FIRST bersattemp NO-ERROR.           
           IF kollfel = TRUE THEN DO:                                               
              MESSAGE "Detta enr finns redan i översättningslistan från vald leverantör till vald leverantör."  
              VIEW-AS ALERT-BOX.     
              DELETE ersattemp.                         
              RETURN NO-APPLY.                         
           END.
           ELSE DO:
              RUN hmtben_UI IN amtrlbapph ( INPUT-OUTPUT TABLE bersattemp ).          
              RUN admsparmtrl_UI IN amtrlbapph ( INPUT-OUTPUT TABLE bersattemp ). 
              FIND FIRST bersattemp NO-ERROR.
              IF AVAILABLE bersattemp THEN DO:
                  FIND FIRST ersattemp WHERE ersattemp.NAMN = bersattemp.NAMN AND   
                  ersattemp.ANVANDARE = bersattemp.ANVANDARE AND  ersattemp.BESTID = bersattemp.BESTID NO-LOCK NO-ERROR.
                  IF AVAILABLE ersattemp THEN DO:
                     ASSIGN 
                     ersattemp.MTRLROW = bersattemp.MTRLROW          
                     ersattemp.BEN1 = bersattemp.BEN1
                     ersattemp.BEN2 = bersattemp.BEN2.
                     DELETE bersattemp.
                  END.
              END.           
           END.
           RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
           RUN openbdynspec_UI IN brwproc[1].  
           RUN title_UI IN brwproc[1].
        END.      
        
     END.
  END.
  IF status-ok = FALSE OR status-ok2 = FALSE THEN DO:
  
     MESSAGE "Du måste välja en artikel för respektive levererantör i de två nedre vallistorna"
     VIEW-AS ALERT-BOX.
  END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:
   CMB_LEV = INPUT CMB_LEV.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV 
   USE-INDEX LEV NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD. 
   levinlast = "".
   RUN levinlast_UI (INPUT vald_lev,OUTPUT levinlast).
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + CMB_LEV + " " + vald_lev  + levinlast).
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE KALKNR = 0 AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX ENR ").
   RUN openbdynspec_UI IN brwproc[2].
   RUN getmenuitem_UI IN brwproc[2] (INPUT BRW_HLEV:HANDLE,INPUT "Saldo Onninen via Internet", OUTPUT menuh).
   IF levtemp.LEVNAMN = "Onninen" OR levtemp.LEVNAMN = "Onninen VSAB" THEN DO:      
      RUN sensitivem_UI IN brwproc[2] (INPUT menuh, INPUT TRUE ).      
   END.
   ELSE RUN sensitivem_UI IN brwproc[2] (INPUT menuh, INPUT FALSE).         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV-2 WINDOW-1
ON VALUE-CHANGED OF CMB_LEV-2 IN FRAME FRAME-A /* Leverantörer */
DO:
   CMB_LEV-2 = INPUT CMB_LEV-2.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV-2 
   USE-INDEX LEV NO-LOCK NO-ERROR.
   vald_lev2 = levtemp.LEVKOD. 
   levinlast = "".
   RUN levinlast2_UI (INPUT vald_lev2,OUTPUT levinlast).
   RUN setorgtitle_UI IN brwproc[3] (INPUT "Materiellista " + CMB_LEV-2 + " " + vald_lev2  + levinlast).
   RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE KALKNR = 0 AND LEVKOD = '" + STRING(vald_lev2) + "' USE-INDEX ENR ").
   RUN openbdynspec_UI IN brwproc[3].
   RUN getmenuitem_UI IN brwproc[3] (INPUT BRW_HLEV:HANDLE,INPUT "Saldo Onninen via Internet", OUTPUT menuh).
   IF levtemp.LEVNAMN = "Onninen" OR levtemp.LEVNAMN = "Onninen VSAB" THEN DO:      
      RUN sensitivem_UI IN brwproc[3] (INPUT menuh, INPUT TRUE ).      
   END.
   ELSE RUN sensitivem_UI IN brwproc[3] (INPUT menuh, INPUT FALSE).         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IEXC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IEXC WINDOW-1
ON CHOOSE OF FBTN_IEXC IN FRAME FRAME-A /* Import Exel */
DO:   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE eersattemp NO-ERROR. 
   EMPTY TEMP-TABLE felemex NO-ERROR. 
   fildir = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den excelfil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                      
      {muswait.i}     
      EMPTY TEMP-TABLE felemex NO-ERROR.    
      RUN OSATEXELIN.P (INPUT fildir,INPUT vald_lev, OUTPUT TABLE eersattemp ).         
      RUN enrvalexc_UI.
      
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].  
      RUN title_UI IN brwproc[1].
      FIND FIRST felemex NO-ERROR.
      IF AVAILABLE felemex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".  
         {AMERICANEUROPEAN.I}     
         OUTPUT TO VALUE(felfil).         
         PUT "Dessa översättningar blev ej inlästa från excel . Var god kontrollera!" AT 6.         
         PUT SKIP.         
         FOR EACH felemex:           
            PUT UNFORMATTED felemex.NAMN AT 6 felemex.ANVANDARE AT 10 felemex.BESTID AT 20 felemex.OMRADE AT 24.  
            PUT SKIP.
         END.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
   END.               
   {musarrow.i}  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Till enr */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
   ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Till enr */
DO:
   {muswait.i}
   {BENHMT2.I}   
   FILL-IN-TILLLEV = INPUT FILL-IN-TILLLEV.
   RUN initsok_UI (INPUT 2,INPUT aosok).
   RUN hmtben_UI IN amtrlbapph ( INPUT-OUTPUT TABLE ersattemp ). 
   IF TOG_TRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN DELETE ersattemp.          
      END.
   END.
   ELSE IF TOG_EJTRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN.
         ELSE DELETE ersattemp.
      END.
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " "). 
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = TRUE. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN-2 WINDOW-1
ON ANY-KEY OF FILL-IN-BEN-2 IN FRAME FRAME-A /* Benämning */
DO:
   {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN-2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN-2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN-2 IN FRAME FRAME-A /* Benämning */
DO:
  {muswait.i}
   ASSIGN
   RAD_SOK-2 = INPUT FRAME {&FRAME-NAME} RAD_SOK-2
   FILL-IN-BEN-2 = INPUT FILL-IN-BEN-2.
   IF FILL-IN-BEN-2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN-2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN-2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   IF RAD_SOK-2 = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN-2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK-2 = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN-2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK-2 = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN-2.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 4,INPUT aosok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN-3 WINDOW-1
ON ANY-KEY OF FILL-IN-BEN-3 IN FRAME FRAME-A /* Benämning */
DO:
  {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN-3 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN-3 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN-3 IN FRAME FRAME-A /* Benämning */
DO:
  {muswait.i}
   ASSIGN
   RAD_SOK-3 = INPUT FRAME {&FRAME-NAME} RAD_SOK-3
   FILL-IN-BEN-3 = INPUT FILL-IN-BEN-3.
   IF FILL-IN-BEN-3 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN-3 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN-3,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK-3 = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN-3.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK-3 = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN-3 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK-3 = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN-3.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 6,INPUT aosok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-A /* Från enr */
DO:
   {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
    ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Från enr */
DO:
   {muswait.i}   
   {ENRHMT2.I}
   FILL-IN-FRANLEV = INPUT FILL-IN-FRANLEV.
   RUN initsok_UI (INPUT 1,INPUT posok).
   RUN hmtben_UI IN amtrlbapph ( INPUT-OUTPUT TABLE ersattemp ). 
   IF TOG_TRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN DELETE ersattemp.          
      END.
   END.
   ELSE IF TOG_EJTRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN.
         ELSE DELETE ersattemp.
      END.
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " "). 
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].  
   RUN title_UI IN brwproc[1].
   ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = TRUE.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FRANLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRANLEV WINDOW-1
ON ANY-KEY OF FILL-IN-FRANLEV IN FRAME FRAME-A /* Från Lev */
DO:
  {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FRANLEV IN FRAME {&FRAME-NAME}.
   END.
    ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FRANLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FRANLEV IN FRAME FRAME-A /* Från Lev */
DO:
   {muswait.i}   
   ASSIGN
   FILL-IN-FRANLEV = INPUT  FILL-IN-FRANLEV.
   FILL-IN-TILLLEV = INPUT  FILL-IN-TILLLEV.
   IF FILL-IN-FRANLEV = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-FRANLEV IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   
   RUN initsok_UI (INPUT 7,INPUT FILL-IN-FRANLEV).     
   RUN hmtben_UI IN amtrlbapph ( INPUT-OUTPUT TABLE ersattemp ).
   IF TOG_TRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN DELETE ersattemp.          
      END.
   END.
   ELSE IF TOG_EJTRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN. 
         ELSE DELETE ersattemp.
      END.
   END.       
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " "). 
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].  
   RUN title_UI IN brwproc[1].
   ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = TRUE.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON ANY-KEY OF FILL-IN-KOD IN FRAME FRAME-A /* Enr */
DO:
  {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME FRAME-A /* Enr */
DO:
  {muswait.i}   
   ASSIGN
   RAD_SOK-2 = INPUT FRAME {&FRAME-NAME} RAD_SOK-2
   FILL-IN-KOD = INPUT  FILL-IN-KOD.
   IF FILL-IN-KOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-KOD,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK-2 = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-KOD.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK-2 = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK-2 = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 3,INPUT posok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD-2 WINDOW-1
ON ANY-KEY OF FILL-IN-KOD-2 IN FRAME FRAME-A /* Enr */
DO:
  {TRYCKS.I}    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD-2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD-2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD-2 IN FRAME FRAME-A /* Enr */
DO:
  {muswait.i}   
   ASSIGN
   RAD_SOK-3 = INPUT FRAME {&FRAME-NAME} RAD_SOK-3
   FILL-IN-KOD-2 = INPUT  FILL-IN-KOD-2.
   IF FILL-IN-KOD-2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD-2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-KOD-2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK-3 = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-KOD-2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK-3 = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD-2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK-3 = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD-2.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 5,INPUT posok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TILLLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TILLLEV WINDOW-1
ON ANY-KEY OF FILL-IN-TILLLEV IN FRAME FRAME-A /* Till Lev */
DO:
  IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TILLLEV IN FRAME {&FRAME-NAME}.
   END.
    ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = FALSE. 


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TILLLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-TILLLEV IN FRAME FRAME-A /* Till Lev */
DO:
  {muswait.i}   
   ASSIGN  
   FILL-IN-TILLLEV = INPUT  FILL-IN-TILLLEV.
   FILL-IN-FRANLEV = INPUT  FILL-IN-FRANLEV.
   IF FILL-IN-TILLLEV = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-FRANLEV IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.     
   RUN initsok_UI (INPUT 8,INPUT FILL-IN-TILLLEV).
   RUN hmtben_UI IN amtrlbapph ( INPUT-OUTPUT TABLE ersattemp ). 
   IF TOG_TRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN DELETE ersattemp.          
      END.
   END.
   ELSE IF TOG_EJTRAFF = TRUE THEN DO:
      FOR EACH ersattemp  :
         IF ersattemp.BEN1 = "" OR ersattemp.BEN2 = "" THEN.
         ELSE DELETE ersattemp.
      END.
   END.       
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " "). 
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].  
   RUN title_UI IN brwproc[1].
   ersattemp.NAMN:READ-ONLY IN BROWSE BRW_VLEV = TRUE.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EJTRAFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EJTRAFF WINDOW-1
ON VALUE-CHANGED OF TOG_EJTRAFF IN FRAME FRAME-A /* Ej träff */
DO:
  TOG_EJTRAFF = INPUT TOG_EJTRAFF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TRAFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TRAFF WINDOW-1
ON VALUE-CHANGED OF TOG_TRAFF IN FRAME FRAME-A /* Enbart träff */
DO:
  TOG_TRAFF = INPUT TOG_TRAFF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(amtrlbapph) THEN DELETE PROCEDURE amtrlbapph.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
   EMPTY TEMP-TABLE levtemp NO-ERROR. 
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   {ALLSTARTDYN.I}   
   
   ASSIGN WINDOW-1:TITLE = "Ersättningslista".         
   RUN enable_UI.   
   {FRMSIZE.I}           
   ersattemp.NAMN:LABEL IN BROWSE BRW_VLEV = "Från lev.".
   ersattemp.ANVANDARE:LABEL IN BROWSE BRW_VLEV = "Från " + Guru.Konstanter:genk.
   ersattemp.BESTID:LABEL IN BROWSE BRW_VLEV = "Till lev.".
   ersattemp.OMRADE:LABEL IN BROWSE BRW_VLEV = "Till " + Guru.Konstanter:genk.
   FILL-IN-ENR:LABEL = "Från " + Guru.Konstanter:genk.   
   FILL-IN-BEN:LABEL = "Till " + Guru.Konstanter:genk.   
   status-ok = CMB_LEV:DELETE("0").
   status-ok = CMB_LEV-2:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   CMB_LEV = levtemp.LEVNAMN.   
   status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN).
   CMB_LEV-2:SCREEN-VALUE = levtemp.LEVNAMN.
   CMB_LEV-2 = levtemp.LEVNAMN.
   vald_lev = levtemp.LEVKOD.    
   vald_lev2 = levtemp.LEVKOD.    
   RUN getmenuitem_UI IN brwproc[2] (INPUT BRW_HLEV:HANDLE,INPUT "Saldo Onninen via Internet", OUTPUT menuh).
   IF levtemp.LEVNAMN = "Onninen" OR levtemp.LEVNAMN = "Onninen VSAB" THEN DO:      
      RUN sensitivem_UI IN brwproc[2] (INPUT menuh, INPUT TRUE ).      
   END.
   ELSE RUN sensitivem_UI IN brwproc[2] (INPUT menuh, INPUT FALSE).
   levinlast = "".
   RUN levinlast_UI (INPUT vald_lev,OUTPUT levinlast).
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + CMB_LEV + " " + vald_lev  + levinlast).
   RUN levinlast2_UI (INPUT vald_lev2,OUTPUT levinlast).
   RUN setorgtitle_UI IN brwproc[3] (INPUT "Materiellista " + CMB_LEV + " " + vald_lev2  + levinlast).   
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN).      
   END.
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      status-ok = CMB_LEV-2:ADD-LAST(levtemp.LEVNAMN).      
   END.   
   FBTN_IEXC:LABEL = "Import Excel".
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
         (INPUT BRW_VLEV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
         (INPUT BRW_HLEV-2:HANDLE IN FRAME {&FRAME-NAME}).      

   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).
   RUN dynprogextra IN brwproc[3] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[3] (INPUT TRUE).   
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).   
   RUN setcolindex_UI IN brwproc[1] (INPUT "NAMN BY BESTID BY ANVANDARE BY OMRADE"). 
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).
   RUN StartSokEnrLev_UI IN brwproc[3] (INPUT THIS-PROCEDURE).
  
   IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      RUN addmenuitem_UI IN brwproc[2] (INPUT BRW_HLEV:HANDLE,INPUT "Saldo Onninen via Internet",INPUT "infosal_UI").      
   END.            
   IF Guru.Konstanter:appcon THEN DO:
      RUN ADMERSATTAPP.P PERSISTENT SET amtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ADMERSATTAPP.P PERSISTENT SET amtrlbapph.
   END.   
   tthandle = TEMP-TABLE levtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmark_UI WINDOW-1 
PROCEDURE avmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
    status-ok = BRW_HLEV:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
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
  DISPLAY FILL-IN-FRANLEV FILL-IN-ENR FILL-IN-TILLLEV FILL-IN-BEN RAD_SOK 
          TOG_TRAFF TOG_EJTRAFF CMB_LEV CMB_LEV-2 FILL-IN-KOD FILL-IN-KOD-2 
          RAD_SOK-2 RAD_SOK-3 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE IMAGE-4 IMAGE-16 IMAGE-17 BRW_VLEV BTN_NY BTN_BORT FILL-IN-FRANLEV 
         FILL-IN-ENR FILL-IN-TILLLEV FILL-IN-BEN FBTN_IEXC RAD_SOK TOG_TRAFF 
         TOG_EJTRAFF BRW_HLEV BRW_HLEV-2 BTN_UPP CMB_LEV CMB_LEV-2 FILL-IN-KOD 
         FILL-IN-KOD-2 FILL-IN-BEN-2 FILL-IN-BEN-3 RAD_SOK-2 RAD_SOK-3 BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrvalexc_UI WINDOW-1 
PROCEDURE enrvalexc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   
   FOR EACH eersattemp:        
      
      EMPTY TEMP-TABLE kersattemp NO-ERROR. 
      CREATE kersattemp.
      BUFFER-COPY eersattemp TO kersattemp .
      RUN kollenrke_UI IN amtrlbapph ( INPUT-OUTPUT TABLE kersattemp,OUTPUT kollev ).       
      FIND FIRST kersattemp NO-LOCK NO-ERROR.
      IF AVAILABLE kersattemp THEN DO:
         ASSIGN
         eersattemp.BEN1 = kersattemp.BEN1
         eersattemp.BEN2 = kersattemp.BEN2.
      END.
      IF kollev = FALSE THEN DO:      
         FIND FIRST ersattemp WHERE ersattemp.NAMN = eersattemp.NAMN AND ersattemp.ANVANDARE = eersattemp.ANVANDARE AND
         ersattemp.BESTID = eersattemp.BESTID AND ersattemp.OMRADE = eersattemp.OMRADE NO-LOCK NO-ERROR.            
         IF NOT AVAILABLE ersattemp  THEN DO:
            FIND FIRST ersattemp WHERE ersattemp.NAMN = eersattemp.NAMN AND ersattemp.ANVANDARE = eersattemp.ANVANDARE AND
            ersattemp.BESTID = eersattemp.BESTID NO-LOCK NO-ERROR.            
            IF NOT AVAILABLE ersattemp  THEN DO:
               CREATE ersattemp.               
            END.            
         END.
         BUFFER-COPY eersattemp TO ersattemp .         
         EMPTY TEMP-TABLE bersattemp NO-ERROR.
         CREATE bersattemp.          
         BUFFER-COPY ersattemp TO bersattemp.                   
         RUN checkmtrl_UI IN amtrlbapph ( INPUT TABLE bersattemp,OUTPUT kollfel ).        
         IF kollfel = FALSE THEN DO:
            RUN admsparmtrl_UI IN amtrlbapph (INPUT-OUTPUT TABLE bersattemp ). 
         END.
         EMPTY TEMP-TABLE bersattemp NO-ERROR. 
      END.      
      ELSE DO:      
         CREATE felemex.   
         BUFFER-COPY eersattemp TO felemex.         
      END.  
   END.
   EMPTY TEMP-TABLE eersattemp NO-ERROR.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES1_UI WINDOW-1 
PROCEDURE infoES1_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES2_UI WINDOW-1 
PROCEDURE infoES2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
  
   IF brwname = "BRW_HLEV" THEN valbrw = 1.  
   IF brwname = "BRW_HLEV-2" THEN valbrw = 2.
   IF brwname = "BRW_VLEV" THEN valbrw = 3.    
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/      
   IF valbrw = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.            
         IF status-ok THEN DO:   
            levnamnvar = levtemp.LEVNAMN.
            {LEVLANK.I}
         END.
      END.   
   END. 
   IF valbrw = 2 THEN DO:    
      status-ok = BRW_HLEV-2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp2.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp2.LEVKOD
         NO-LOCK NO-ERROR.            
         IF status-ok THEN DO:   
            levnamnvar = levtemp.LEVNAMN.
            {LEVLANK.I}
         END.
       END.  
   END.
   IF valbrw = 3 THEN DO:    
      status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF status-ok THEN DO:
         valenr = ersattemp.ANVANDARE.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = ersattemp.NAMN
         NO-LOCK NO-ERROR.            
         IF status-ok THEN DO:   
            levnamnvar = levtemp.LEVNAMN.
            {LEVLANK.I}
         END.
       END.  
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infosal_UI WINDOW-1 
PROCEDURE infosal_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/            
   status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE mtrltemp THEN RETURN.
   valenr = mtrltemp.ENR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
   NO-LOCK NO-ERROR.            
   IF status-ok THEN DO:   
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANKS.I}
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-1 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:      
      IF FILL-IN-FRANLEV = "" THEN   orgfraga = " ".
      ELSE orgfraga = " WHERE NAMN = '" + STRING(FILL-IN-FRANLEV) + "'".
      tth = TEMP-TABLE ersattemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "BETPLAN"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "ANVANDARE" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:      
     IF FILL-IN-TILLLEV = "" THEN   orgfraga = " ".
     ELSE orgfraga = " WHERE BESTID = '" + STRING(FILL-IN-TILLLEV) + "'".
     
     tth = TEMP-TABLE ersattemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "BETPLAN"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "OMRADE" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
  END.  
  IF vad = 3 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "'".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp). 
  END.
  IF vad = 4 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "'".    
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp). 
   END.
   IF vad = 5 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev2) + "'".
      tth = TEMP-TABLE mtrltemp2:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[3] (INPUT TABLE valsoktemp). 
  END.
  IF vad = 6 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev2) + "'".      
      tth = TEMP-TABLE mtrltemp2:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[3] (INPUT TABLE valsoktemp). 
   END.
   IF vad = 7 THEN DO:      
      orgfraga = " ".      
      IF FILL-IN-TILLLEV = "" THEN   orgfraga = " ".
      ELSE orgfraga = " WHERE BESTID = '" + STRING(FILL-IN-TILLLEV) + "'".
      
      tth = TEMP-TABLE ersattemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "BETPLAN"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "NAMN" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 8 THEN DO:      
     orgfraga = " ".
     IF FILL-IN-FRANLEV = "" THEN   orgfraga = " ".
     ELSE orgfraga = " WHERE NAMN = '" + STRING(FILL-IN-FRANLEV) + "'".
     
     tth = TEMP-TABLE ersattemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "BETPLAN"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "BESTID" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levinlast2_UI WINDOW-1 
PROCEDURE levinlast2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER lekod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER levinlast AS CHARACTER NO-UNDO.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = lekod.                                              
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         levinlast = " Inläst " + STRING(extradatatemp.SOKDATE[1]).
         IF extradatatemp.SOKDATE[2] NE ? AND extradatatemp.SOKDATE[2] NE ?  THEN DO:      
            RUN tooltip_UI IN brwproc[3] (INPUT "Katalogen gäller från: " + STRING(extradatatemp.SOKDATE[2]) + " till: " + STRING(extradatatemp.SOKDATE[3])).          
         END.
         ELSE RUN tooltip_UI IN brwproc[3] (INPUT "").             
      END.
      ELSE RUN tooltip_UI IN brwproc[3] (INPUT "").             
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levinlast_UI WINDOW-1 
PROCEDURE levinlast_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER lekod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER levinlast AS CHARACTER NO-UNDO.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = lekod.                                              
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         IF extradatatemp.SOKDATE[1] NE ? THEN levinlast = " Inläst " + STRING(extradatatemp.SOKDATE[1]).
         IF extradatatemp.SOKDATE[2] NE ? AND extradatatemp.SOKDATE[3] NE ?  THEN DO:      
            RUN tooltip_UI IN brwproc[2] (INPUT "Katalogen gäller från: " + STRING(extradatatemp.SOKDATE[2]) + " till: " + STRING(extradatatemp.SOKDATE[3])).          
         END.
         ELSE RUN tooltip_UI IN brwproc[2] (INPUT "").             
      END.
      ELSE RUN tooltip_UI IN brwproc[2] (INPUT "").             
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-1 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.   
   IF brwh:NAME = "BRW_HLEV" THEN RUN rowdispmtrl_UI (INPUT 1). 
   IF brwh:NAME = "BRW_HLEV-2" THEN RUN rowdispmtrl_UI (INPUT 2).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI WINDOW-1 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.   
   IF vad = 1 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
          IF mtrltemp.KUND = TRUE THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28].          
         END.         
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38].          
         END.         
      END.
   END.
   IF vad = 2 THEN DO:
      IF AVAILABLE mtrltemp2 THEN DO:
          IF mtrltemp2.KUND = TRUE THEN DO: 
            ASSIGN
            mtrltemp2.ENR:BGCOLOR IN BROWSE BRW_HLEV-2 = Guru.Konstanter:varforetypval[28]
            mtrltemp2.NPRIS:BGCOLOR IN BROWSE BRW_HLEV-2 = Guru.Konstanter:varforetypval[28].          
         END.         
         ELSE IF mtrltemp2.KUND = ? THEN DO: 
            ASSIGN
            mtrltemp2.ENR:BGCOLOR IN BROWSE BRW_HLEV-2 = Guru.Konstanter:varforetypval[38]
            mtrltemp2.NPRIS:BGCOLOR IN BROWSE BRW_HLEV-2 = Guru.Konstanter:varforetypval[38].          
         END.         
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF AVAILABLE ersattemp THEN DO:           
      DISPLAY
      ersattemp.NAMN       
      ersattemp.ANVANDARE
      ersattemp.BEN1
      ersattemp.BESTID 
      ersattemp.OMRADE
      ersattemp.BEN2  WITH BROWSE BRW_VLEV.                
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

