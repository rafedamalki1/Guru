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
DEFINE INPUT PARAMETER  aonrvarorg AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER  delnrvarorg AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER  fbaonrvar AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{FAKTTYPDEF.I}
&Scoped-define SHARED 
{ANSPROJBER.I}
{DIRDEF.I}
{REGVAR.I}
{AONRDEF.I}
{SOKDEF.I}
{AVTPLANTEMP.I}

{OMRTEMPW.I}
{BERIN.I}
{EXTRATAB.I}  
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{BESTKUNDALLT.I}
{AVDTEMP.I}
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE antanurval AS INTEGER NO-UNDO.
DEFINE VARIABLE labelvar1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE labelvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE valnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE tillbakaaonr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tillbakadelnr AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr valdaao

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT utsokaonr.BEREDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR utsokaonr.AONR 
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


/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR valdaao.AONR valdaao.DELNR ~
valdaao.ORT valdaao.OMRADE valdaao.BEREDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR valdaao.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VAONR valdaao
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VAONR valdaao
&Scoped-define QUERY-STRING-BRW_VAONR FOR EACH valdaao NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH valdaao NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR valdaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR valdaao


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_AONR}~
    ~{&OPEN-QUERY-BRW_VAONR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-52 BTN_NVE-3 BTN_NVE-4 TOG_PAGA ~
TOG_AVSLUTADE FILL-IN-AVSTARTD FILL-IN-AVSLUTD BTN_FVE-3 BTN_FVE-4 ~
TOG_TILLF TOG_FASTA CMB_AVD CMB_PROJ CMB_OMR CMB_BERE FILL-IN-K1 CMB_BESORG ~
CMB_ANSV FILL-IN-K2 CMB_FAK BTN_HAMT BRW_AONR BRW_VAONR BTN_ALLOVER ~
BTN_OVER BTN_BACK BTN_ALLBACK FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR ~
FILL-IN_DELNR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_PAGA TOG_AVSLUTADE FILL-IN-MELL ~
FILL-IN-AVSTARTD FILL-IN-OCH FILL-IN-AVSLUTD TOG_TILLF TOG_FASTA CMB_AVD ~
CMB_PROJ CMB_OMR CMB_BERE FILL-IN-K1 CMB_BESORG CMB_ANSV FILL-IN-K2 CMB_FAK ~
FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR FILL-IN-AOTEXT ~
FILL-IN-KTO FILL-IN-VAL FILL-IN-SOK 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkbrw C-Win 
FUNCTION checkbrw RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_OK 
     LABEL "Koppla":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BERE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beredare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beställare/Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fakturakategori" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PROJ AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av arbetsorder" 
      VIEW-AS TEXT 
     SIZE 37.13 BY 1
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSTARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-K1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTOK1" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-K2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "KONTO" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 TOOLTIP "Det går att använda ~"*~" men endast som ex ~"32*~"." NO-UNDO.

DEFINE VARIABLE FILL-IN-KTO AS CHARACTER FORMAT "X(256)":U INITIAL "Urval konto" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK AS CHARACTER FORMAT "X(256)":U INITIAL "Sök på:" 
      VIEW-AS TEXT 
     SIZE 7.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-VAL AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta aonr:" 
      VIEW-AS TEXT 
     SIZE 10.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 17.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PAAV AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pågående", 1,
"Avslutade", 2
     SIZE 20 BY 1 TOOLTIP "Utsökningen ska gälla antingen pågående eller avslutade eller både och." NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52.5 BY 1.21
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 49.5 BY 1.21
     BGCOLOR 8 .

DEFINE VARIABLE TOG_AVSLUTADE AS LOGICAL INITIAL no 
     LABEL "Avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 TOOLTIP "Utsökningen ska gälla avslutade mellan angivna datum." NO-UNDO.

DEFINE VARIABLE TOG_FASTA AS LOGICAL INITIAL no 
     LABEL "Fasta" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_PAGA AS LOGICAL INITIAL no 
     LABEL "Pågående" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 TOOLTIP "Utsökningen ska gälla pågående." NO-UNDO.

DEFINE VARIABLE TOG_TILLF AS LOGICAL INITIAL no 
     LABEL "Tillfälliga" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_VAONR FOR 
      valdaao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR C-Win _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
            WIDTH 30
      utsokaonr.BEREDARE FORMAT "x(5)":U
  ENABLE
      utsokaonr.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 52.5 BY 13.5
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR C-Win _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      valdaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
            WIDTH 30
      valdaao.OMRADE FORMAT "x(6)":U
      valdaao.BEREDARE FORMAT "x(5)":U
  ENABLE
      valdaao.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 49.5 BY 13.5
         TITLE "Faktura beställarens".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_NVE-3 AT ROW 2.29 COL 60.88
     BTN_NVE-4 AT ROW 2.29 COL 78.25
     TOG_PAGA AT ROW 2.58 COL 1.5
     TOG_AVSLUTADE AT ROW 2.58 COL 24.75
     FILL-IN-MELL AT ROW 2.58 COL 41.75 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSTARTD AT ROW 2.58 COL 48.88 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 2.58 COL 62.25 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSLUTD AT ROW 2.58 COL 66.25 COLON-ALIGNED NO-LABEL
     BTN_FVE-3 AT ROW 3.17 COL 60.88
     BTN_FVE-4 AT ROW 3.17 COL 78.25
     TOG_TILLF AT ROW 3.67 COL 1.5
     TOG_FASTA AT ROW 3.67 COL 24.75
     CMB_AVD AT ROW 4.67 COL 7.38
     CMB_PROJ AT ROW 4.67 COL 57 COLON-ALIGNED
     CMB_OMR AT ROW 5.92 COL 10.38
     CMB_BERE AT ROW 5.92 COL 57 COLON-ALIGNED
     FILL-IN-K1 AT ROW 5.92 COL 90 COLON-ALIGNED
     CMB_BESORG AT ROW 7.17 COL 16.38 COLON-ALIGNED
     CMB_ANSV AT ROW 7.17 COL 57 COLON-ALIGNED
     FILL-IN-K2 AT ROW 7.17 COL 90 COLON-ALIGNED
     FBTN_OK AT ROW 8 COL 111.5
     CMB_FAK AT ROW 8.5 COL 57 COLON-ALIGNED
     BTN_HAMT AT ROW 9.88 COL 22.75
     BRW_AONR AT ROW 11 COL 1.5
     BRW_VAONR AT ROW 11 COL 60.13
     BTN_ALLOVER AT ROW 14.38 COL 55.25
     BTN_OVER AT ROW 16.54 COL 55.25
     BTN_BACK AT ROW 18.75 COL 55.25
     RAD_FAST AT ROW 19.96 COL 98 NO-LABEL
     RAD_PAAV AT ROW 20.46 COL 103 NO-LABEL
     BTN_ALLBACK AT ROW 20.96 COL 55.25
     FILL-IN-AR AT ROW 22.13 COL 106 COLON-ALIGNED NO-LABEL
     FILL-IN_SAONR AT ROW 24.83 COL 12.5 COLON-ALIGNED
     FILL-IN_ORT AT ROW 24.83 COL 32.63 COLON-ALIGNED
     FILL-IN_EAONR AT ROW 24.92 COL 80 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR AT ROW 24.92 COL 94 COLON-ALIGNED NO-LABEL
     BTN_AVB AT ROW 24.92 COL 111.5
     FILL-IN-AOTEXT AT ROW 1.25 COL 1.5 NO-LABEL
     FILL-IN-KTO AT ROW 4.88 COL 84.75 COLON-ALIGNED NO-LABEL
     FILL-IN-VAL AT ROW 24.83 COL 58.88 COLON-ALIGNED NO-LABEL
     FILL-IN-SOK AT ROW 24.92 COL 2.13 NO-LABEL
     RECT-50 AT ROW 24.71 COL 1.5
     RECT-52 AT ROW 24.71 COL 60.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 25.46.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 25.38
         WIDTH              = 124.75
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_AONR BTN_HAMT DEFAULT-FRAME */
/* BROWSE-TAB BRW_VAONR BRW_AONR DEFAULT-FRAME */
ASSIGN 
       BRW_AONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME DEFAULT-FRAME
   SHARED ALIGN-L                                                       */
/* SETTINGS FOR BUTTON FBTN_OK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FBTN_OK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSLUTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSTARTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KTO IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-MELL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-VAL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PAAV IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PAAV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[5]   = Temp-Tables.utsokaonr.BEREDARE
     _Query            is OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.valdaao"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valdaao.AONR
"valdaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdaao.DELNR
"valdaao.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdaao.ORT
"valdaao.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.valdaao.OMRADE
     _FldNameList[5]   = Temp-Tables.valdaao.BEREDARE
     _Query            is OPENED
*/  /* BROWSE BRW_VAONR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONR
&Scoped-define SELF-NAME BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONR C-Win
ON ENTRY OF BRW_AONR IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAONR
&Scoped-define SELF-NAME BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR C-Win
ON ENTRY OF BRW_VAONR IN FRAME DEFAULT-FRAME /* Faktura beställarens */
DO:
   IF AVAILABLE valdaao THEN BRW_VAONR:TOOLTIP = valdaao.ORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAONR C-Win
ON VALUE-CHANGED OF BRW_VAONR IN FRAME DEFAULT-FRAME /* Faktura beställarens */
DO:
   IF AVAILABLE valdaao THEN DO:
      BRW_VAONR:TOOLTIP = valdaao.ORT.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 C-Win
ON CHOOSE OF BTN_FVE-3 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.   
   FILL-IN-AVSTARTD = FILL-IN-AVSTARTD - 1.      
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 C-Win
ON CHOOSE OF BTN_FVE-4 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.   
   FILL-IN-AVSLUTD = FILL-IN-AVSLUTD - 1.      
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT C-Win
ON CHOOSE OF BTN_HAMT IN FRAME DEFAULT-FRAME /* Hämta och visa urval */
DO:   
   RUN utvald_UI.
   RUN allaao_UI.
   RUN title_UI IN brwproc[{&LEFT-BROWSE}].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 C-Win
ON CHOOSE OF BTN_NVE-3 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.   
   FILL-IN-AVSTARTD = FILL-IN-AVSTARTD + 1.        
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 C-Win
ON CHOOSE OF BTN_NVE-4 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.   
   FILL-IN-AVSLUTD = FILL-IN-AVSLUTD + 1.        
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME DEFAULT-FRAME /* Avdelning */
DO:
   CMB_AVD = INPUT CMB_AVD.              
   CMB_OMR:LIST-ITEMS = "". 
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   IF CMB_AVD = "Alla" THEN DO:
      FOR EACH omrtemp:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END. 
   END.
   ELSE DO:
      FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.     

      FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = avdtemp.AVDELNINGNR:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.      
   END.
   CMB_OMR:SCREEN-VALUE = "Alla".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESORG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESORG C-Win
ON LEAVE OF CMB_BESORG IN FRAME DEFAULT-FRAME /* Beställare/Kund */
DO:
   CMB_BESORG = INPUT CMB_BESORG.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_FAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FAK C-Win
ON VALUE-CHANGED OF CMB_FAK IN FRAME DEFAULT-FRAME /* Fakturakategori */
DO:
   CMB_FAK = INPUT CMB_FAK.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR C-Win
ON VALUE-CHANGED OF CMB_OMR IN FRAME DEFAULT-FRAME /* Område */
DO:
   CMB_OMR = INPUT CMB_OMR.                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK C-Win
ON CHOOSE OF FBTN_OK IN FRAME DEFAULT-FRAME /* Koppla */
DO: 
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.    
   FIND FIRST valdaao NO-ERROR.
   IF NOT AVAILABLE valdaao THEN DO:
      MESSAGE "Inget " LC(Guru.Konstanter:gaok) " är valt!"  VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   IF fbaonrvar = "INTAONR" THEN DO:
      RUN getnumresult_UI IN brwproc[{&RIGHT-BROWSE}] (OUTPUT antanurval).    
      IF antanurval > 1 THEN DO:
         MESSAGE "Du kan bara koppla ett " LC(Guru.Konstanter:gaok) "!"  VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   FOR EACH valdaao:
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = fbaonrvar                   
      inextrakopptemp.KOPPLACHAR1 = aonrvarorg
      inextrakopptemp.KOPPLAINT1 =  delnrvarorg      
      inextrakopptemp.KOPPLACHAR2 = valdaao.AONR            
      inextrakopptemp.KOPPLAINT2 =  valdaao.DELNR
      inextrakopptemp.SOKINT[1] = 100.
   END.
   RUN sparaextra_UI IN fbestapph (INPUT TABLE inextrakopptemp).
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AR C-Win
ON LEAVE OF FILL-IN-AR IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AR = INPUT FILL-IN-AR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVSLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSLUTD C-Win
ON LEAVE OF FILL-IN-AVSLUTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.
   IF FILL-IN-AVSLUTD < FILL-IN-AVSTARTD THEN DO:
      FILL-IN-AVSTARTD = FILL-IN-AVSLUTD.
      DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSLUTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-AVSLUTD IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-AVSLUTD.
   RUN AlmanBtn.w.
   FILL-IN-AVSLUTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVSTARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSTARTD C-Win
ON LEAVE OF FILL-IN-AVSTARTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.
   IF FILL-IN-AVSLUTD < FILL-IN-AVSTARTD THEN DO:
      FILL-IN-AVSLUTD = FILL-IN-AVSTARTD.
      DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSTARTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-AVSTARTD IN FRAME DEFAULT-FRAME
DO:
  ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-AVSTARTD.
   RUN AlmanBtn.w.
   FILL-IN-AVSTARTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DELNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON ANY-KEY OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
   {TRYCKS.I}   
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON ENTRY OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON LEAVE OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DELNR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DELNR IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   ASSIGN
   FILL-IN_EAONR = INPUT FRAME {&FRAME-NAME} FILL-IN_EAONR
   FILL-IN_DELNR = INPUT FILL-IN_DELNR
   aonrvar = FILL-IN_EAONR
   delnrvar = FILL-IN_DELNR.   
   status-ok = BRW_VAONR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
   IF AVAILABLE valdaao THEN DO:
      ASSIGN
      tillbakaaonr = valdaao.AONR
      tillbakadelnr = valdaao.DELNR.
   END.
   ELSE tillbakaaonr = ?.
   FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
   valdaao.DELNR = delnrvar NO-LOCK NO-ERROR. 
   IF AVAILABLE valdaao THEN DO:
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].        
   END.
   ELSE DO:
      RUN aonrhmtaen IN aonrapph (INPUT aonrvar,INPUT delnrvar,
      OUTPUT TABLE valdaao APPEND,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         FIND FIRST valdaao WHERE valdaao.AONR = tillbakaaonr AND 
         valdaao.DELNR = tillbakadelnr NO-LOCK NO-ERROR. 
         IF AVAILABLE valdaao THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
            RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].        
         END.
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.         
      END.
      ELSE DO:
         RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
         FIND FIRST valdaao WHERE valdaao.AONR = aonrvar AND 
         valdaao.DELNR = delnrvar NO-LOCK NO-ERROR.
         IF AVAILABLE valdaao THEN DO:      
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.  
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR C-Win
ON ANY-KEY OF FILL-IN_EAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EAONR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EAONR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   FILL-IN_EAONR = INPUT FILL-IN_EAONR.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DELNR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT C-Win
ON LEAVE OF FILL-IN_ORT IN FRAME DEFAULT-FRAME /* Ort/Benämning */
DO:
   FILL-IN_ORT = INPUT FILL-IN_ORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SAONR C-Win
ON LEAVE OF FILL-IN_SAONR IN FRAME DEFAULT-FRAME /* Aonr */
DO:
   FILL-IN_SAONR = INPUT FILL-IN_SAONR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST C-Win
ON VALUE-CHANGED OF RAD_FAST IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RAD_FAST = INPUT RAD_FAST. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PAAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PAAV C-Win
ON VALUE-CHANGED OF RAD_PAAV IN FRAME DEFAULT-FRAME
DO:
   {muswait.i}
   RAD_PAAV = INPUT RAD_PAAV. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AVSLUTADE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AVSLUTADE C-Win
ON VALUE-CHANGED OF TOG_AVSLUTADE IN FRAME DEFAULT-FRAME /* Avslutade */
DO:
   {muswait.i}                
   TOG_AVSLUTADE = INPUT TOG_AVSLUTADE. 
   RUN goma_UI.
   {musarrow.i}               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FASTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FASTA C-Win
ON VALUE-CHANGED OF TOG_FASTA IN FRAME DEFAULT-FRAME /* Fasta */
DO:
   {muswait.i}
   TOG_FASTA = INPUT TOG_FASTA. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_PAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_PAGA C-Win
ON VALUE-CHANGED OF TOG_PAGA IN FRAME DEFAULT-FRAME /* Pågående */
DO:
   {muswait.i}                
   TOG_PAGA = INPUT TOG_PAGA. 
   {musarrow.i}               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TILLF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TILLF C-Win
ON VALUE-CHANGED OF TOG_TILLF IN FRAME DEFAULT-FRAME /* Tillfälliga */
DO:
   {muswait.i}
   TOG_TILLF = INPUT TOG_TILLF. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.      
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph.      
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
   ASSIGN
   status-ok = CMB_OMR:ADD-FIRST("Alla").
   FOR EACH omrtemp:
      status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
   END.
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   FILL-IN_ORT:LABEL = Guru.Konstanter:gaonamnk.
   CMB_OMR = omrtemp.NAMN. 
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
      CMB_OMR = "Alla".
      ASSIGN CMB_OMR:SCREEN-VALUE = "Alla".
      DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      CMB_OMR = INPUT CMB_OMR.
   END.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   
   status-ok = CMB_BESORG:ADD-FIRST("Alla").
   FOR EACH bestkundallt USE-INDEX BESTNAMN NO-LOCK:
      status-ok = CMB_BESORG:ADD-LAST(bestkundallt.BESTNAMN).
   END.
   status-ok = CMB_BESORG:ADD-LAST("Samma som ovan"). 
   musz = FALSE.
   status-ok = CMB_ANSV:ADD-FIRST("Alla").
   FOR EACH ansvaraotemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_ANSV:ADD-LAST(ansvaraotemp.NAMN).
   END.
   status-ok = CMB_PROJ:ADD-FIRST("Alla").
   FOR EACH projtemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_PROJ:ADD-LAST(projtemp.NAMN).
   END.
   status-ok = CMB_BERE:ADD-FIRST("Alla").
   FOR EACH beredartemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_BERE:ADD-LAST(beredartemp.NAMN).
   END.
   FOR EACH faktyptemp : 
      status-ok = CMB_FAK:ADD-LAST(faktyptemp.VIFAKTTYP).      
   END.   
   status-ok = CMB_AVD:ADD-FIRST("Alla").
   FOR EACH avdtemp:
      status-ok = CMB_AVD:ADD-LAST(avdtemp.AVDELNINGNAMN).
   END.
   CMB_AVD:SCREEN-VALUE = "Alla". 
   RUN labelk2 IN aonrapph (OUTPUT labelvar2).
   ASSIGN
   CMB_AVD:LABEL = Guru.Konstanter:gavdk
   FILL-IN-K2:LABEL = labelvar2
   FILL-IN-AOTEXT = "Gör urval av " + LC(Guru.Konstanter:gaol) +  ":"
   BTN_ALLBACK:TOOLTIP = "Alla valda " + LC(Guru.Konstanter:gaok) + " tas bort från vallistan"   
   BTN_ALLOVER:TOOLTIP = "Alla " + LC(Guru.Konstanter:gaok) + " väljs"
   BTN_HAMT:TOOLTIP = "Dina val ovan avgör vilka " + LC(Guru.Konstanter:gaok) +  " du får i listan nedan."
   FILL-IN_EAONR:TOOLTIP = "Tryck RETURN här för välja enstaka " + LC(Guru.Konstanter:gaok) 
   FILL-IN_SAONR:LABEL = Guru.Konstanter:gaok
   FILL-IN_EAONR:LABEL = Guru.Konstanter:gaok
   CMB_PROJ:LABEL = Guru.Konstanter:gprojl
   CMB_BERE:LABEL = Guru.Konstanter:gberek 
   CMB_ANSV:LABEL = Guru.Konstanter:garbak
   CMB_OMR:LABEL = Guru.Konstanter:gomrk
   FILL-IN-VAL = "Hämta " + LC(Guru.Konstanter:gaok)
   CMB_BESORG:LABEL = RIGHT-TRIM(SUBSTRIN(Guru.Konstanter:gbestk,1,12))      
   utsokaonr.BEREDARE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gberek 
   valdaao.BEREDARE:LABEL IN BROWSE BRW_VAONR = Guru.Konstanter:gberek
   utsokaonr.OMRADE:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gomrk
   utsokaonr.AONR:LABEL IN BROWSE BRW_AONR = Guru.Konstanter:gaok 
   valdaao.AONR:LABEL IN BROWSE BRW_VAONR = Guru.Konstanter:gaok. 
   RUN labelk1 IN aonrapph (OUTPUT labelvar1).
   FILL-IN-K1:LABEL = labelvar1.
   ASSIGN
   C-Win:TITLE = "Faktura beställare".
   IF fbaonrvar = "INTAONR" THEN C-Win:TITLE = "Intäkts" + Guru.Konstanter:gaok.
   CREATE uppvaltemp.
   ASSIGN         
   uppvaltemp.ENDBEST = ?  
   uppvaltemp.DELNRKOLL = TRUE
   uppvaltemp.VISPERAR = TRUE
   uppvaltemp.STARTDATUM = DATE(01,01,YEAR(TODAY))
   uppvaltemp.SLUTDATUM  = TODAY
   uppvaltemp.AVSLUTSTART = DATE(01,01,YEAR(TODAY))
   uppvaltemp.AVSLUTSLUT = TODAY
   uppvaltemp.TILLFALLFAST = 1
   SUBSTRING(uppvaltemp.PROJEKTOR,1,20) = "ALLA"
   uppvaltemp.BEREDARE = "ALLA"
   uppvaltemp.ARBANSVARIG = "ALLA"
   uppvaltemp.BESTNAMN = "ALLA"
   uppvaltemp.OMRNAMN = CMB_OMR
   uppvaltemp.AVDNAMN = "ALLA"
   uppvaltemp.AVDNR = "ALLA"
   uppvaltemp.FAKTTYP = "ALLA"
   uppvaltemp.PAAV = 1.
   ASSIGN
   TOG_PAGA = TRUE      
   TOG_AVSLUTADE = FALSE 
   TOG_TILLF = TRUE     
   TOG_FASTA = FALSE.     

   {TOGTIFA.I}
   ASSIGN
   FILL-IN-MELL = "mellan" 
   FILL-IN-OCH = "och".    
   RUN enable_UI.
   {FRMSIZE.I}
   RUN ladda_UI.  
   GET FIRST BRW_VAONR NO-LOCK.
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN musz = musz.
   ELSE CMB_FAK:HIDDEN = TRUE.
   
   RUN goma_UI.
   ASSIGN
   FBTN_OK:HIDDEN = FALSE
   BRW_AONR:HIDDEN = FALSE.
   ENABLE FBTN_OK WITH FRAME {&FRAME-NAME}. 
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF labelvar2 = "" THEN FILL-IN-K2:HIDDEN = TRUE.
   IF labelvar1 = "" THEN FILL-IN-K1:HIDDEN = TRUE.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allaao_UI C-Win 
PROCEDURE allaao_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   
   RUN aonrhmt IN aonrapph 
   (INPUT " ",INPUT TABLE valsoktemp,
   INPUT-OUTPUT TABLE uppvaltemp,INPUT-OUTPUT TABLE utsokaonr). 

   RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   RUN fetchselrowid_UI IN brwproc[{&LEFT-BROWSE}].
   {musarrow.i}    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   utsokaonr.AONR:READ-ONLY IN BROWSE BRW_AONR = TRUE    
   valdaao.AONR:READ-ONLY IN BROWSE  BRW_VAONR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_AONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VAONR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_AONR:HANDLE, INPUT BRW_VAONR:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.            
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph.
      RUN MAONRAPP.P PERSISTENT SET aonrapph. 
   END.
   RUN anvglob_UI IN aonrapph (INPUT Guru.Konstanter:globanv).
   tthandle = TEMP-TABLE valdaao:HANDLE.
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_ORT:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ORT").
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_SAONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONR").  
   
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
  DISPLAY TOG_PAGA TOG_AVSLUTADE FILL-IN-MELL FILL-IN-AVSTARTD FILL-IN-OCH 
          FILL-IN-AVSLUTD TOG_TILLF TOG_FASTA CMB_AVD CMB_PROJ CMB_OMR CMB_BERE 
          FILL-IN-K1 CMB_BESORG CMB_ANSV FILL-IN-K2 CMB_FAK FILL-IN_SAONR 
          FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR FILL-IN-AOTEXT FILL-IN-KTO 
          FILL-IN-VAL FILL-IN-SOK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-52 BTN_NVE-3 BTN_NVE-4 TOG_PAGA TOG_AVSLUTADE 
         FILL-IN-AVSTARTD FILL-IN-AVSLUTD BTN_FVE-3 BTN_FVE-4 TOG_TILLF 
         TOG_FASTA CMB_AVD CMB_PROJ CMB_OMR CMB_BERE FILL-IN-K1 CMB_BESORG 
         CMB_ANSV FILL-IN-K2 CMB_FAK BTN_HAMT BRW_AONR BRW_VAONR BTN_ALLOVER 
         BTN_OVER BTN_BACK BTN_ALLBACK FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR 
         FILL-IN_DELNR BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_AVSLUTADE = TRUE THEN DO:
      ASSIGN
      BTN_FVE-3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BTN_FVE-4:HIDDEN = FALSE 
      BTN_NVE-3:HIDDEN = FALSE 
      BTN_NVE-4:HIDDEN = FALSE
      FILL-IN-MELL:HIDDEN = FALSE 
      FILL-IN-OCH:HIDDEN = FALSE 
      FILL-IN-AVSLUTD:HIDDEN = FALSE 
      FILL-IN-AVSTARTD:HIDDEN = FALSE.       
   END.
   ELSE DO:
      ASSIGN
      BTN_FVE-3:HIDDEN = TRUE 
      BTN_FVE-4:HIDDEN = TRUE 
      BTN_NVE-3:HIDDEN = TRUE 
      BTN_NVE-4:HIDDEN = TRUE 
      FILL-IN-MELL:HIDDEN = TRUE 
      FILL-IN-OCH:HIDDEN = TRUE 
      FILL-IN-AVSLUTD:HIDDEN = TRUE 
      FILL-IN-AVSTARTD:HIDDEN = TRUE.
   END.
   DISPLAY TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI C-Win 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND faktyptemp WHERE faktyptemp.FAKTTYP = uppvaltemp.FAKTTYP NO-ERROR.
   CMB_FAK = faktyptemp.VIFAKTTYP.
   ASSIGN     
   /*CMB_ARTAL = YEAR(uppvaltemp.STARTDATUM)*/ 
   CMB_BESORG = uppvaltemp.BESTNAMN
   CMB_OMR = uppvaltemp.OMRNAMN
   FILL-IN-AVSLUTD = uppvaltemp.AVSLUTSLUT 
   FILL-IN-AVSTARTD = uppvaltemp.AVSLUTSTART
   CMB_PROJ = uppvaltemp.PROJEKTOR
   CMB_ANSV = uppvaltemp.ARBANSVARIG 
   CMB_BERE = uppvaltemp.BESTNAMN.
   IF uppvaltemp.PAAV = 3 THEN DO:
      ASSIGN
      TOG_PAGA = TRUE     
      TOG_AVSLUTADE = TRUE.
   END.
   ELSE IF uppvaltemp.PAAV = 2 THEN DO:
      ASSIGN
      TOG_PAGA = FALSE     
      TOG_AVSLUTADE = TRUE.
   END.
   ELSE IF uppvaltemp.PAAV = 1 THEN DO:
      ASSIGN
      TOG_PAGA = TRUE     
      TOG_AVSLUTADE = FALSE.
   END.
   ELSE DO:
      ASSIGN
      TOG_PAGA = FALSE     
      TOG_AVSLUTADE = FALSE.
   END.
   IF uppvaltemp.TILLFALLFAST = 3 THEN DO:
      ASSIGN
      TOG_TILLF = TRUE     
      TOG_FASTA = TRUE.
   END.
   ELSE IF uppvaltemp.TILLFALLFAST = 2 THEN DO:
      ASSIGN
      TOG_TILLF = FALSE     
      TOG_FASTA = TRUE.
   END.
   ELSE IF uppvaltemp.TILLFALLFAST = 1 THEN DO:
      ASSIGN
      TOG_TILLF = TRUE     
      TOG_FASTA = FALSE.
   END.
   ELSE DO:
      ASSIGN
      TOG_TILLF = FALSE     
      TOG_FASTA = FALSE.
   END.   
   DISPLAY CMB_OMR CMB_BESORG CMB_FAK CMB_ANSV CMB_BERE CMB_PROJ FILL-IN-AVSLUTD
   FILL-IN-AVSTARTD   
   TOG_PAGA TOG_AVSLUTADE TOG_TILLF TOG_FASTA WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE utvald_UI C-Win 
PROCEDURE utvald_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   CMB_ANSV = INPUT FRAME {&FRAME-NAME} CMB_ANSV 
   CMB_BERE = INPUT FRAME {&FRAME-NAME} CMB_BERE  
   CMB_PROJ = INPUT FRAME {&FRAME-NAME} CMB_PROJ  
   CMB_BESORG = INPUT FRAME {&FRAME-NAME} CMB_BESORG
   CMB_FAK = INPUT FRAME {&FRAME-NAME} CMB_FAK   
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR
   FILL-IN-K2 = INPUT FILL-IN-K2
   FILL-IN-K1 = INPUT FILL-IN-K1.
   FIND FIRST uppvaltemp NO-ERROR.      
   ASSIGN         
   uppvaltemp.VISPERAR = FALSE
   uppvaltemp.AVSLUTSTART = FILL-IN-AVSTARTD
   uppvaltemp.AVSLUTSLUT = FILL-IN-AVSLUTD   
   uppvaltemp.BESTNAMN = CMB_BESORG
   uppvaltemp.BEREDARE = CMB_BERE
   uppvaltemp.PROJEKTOR = CMB_PROJ
   uppvaltemp.ARBANSVARIG = CMB_ANSV
   uppvaltemp.AVDNAMN = CMB_AVD.
   
   IF TOG_PAGA = TRUE AND TOG_AVSLUTADE = TRUE THEN DO:
      uppvaltemp.PAAV = 3.
   END.
   ELSE IF TOG_PAGA = FALSE AND TOG_AVSLUTADE = TRUE THEN DO:
      uppvaltemp.PAAV = 2.
   END.
   ELSE IF TOG_PAGA = TRUE AND TOG_AVSLUTADE = FALSE THEN DO:
      uppvaltemp.PAAV = 1.
   END.
   ELSE DO:
      ASSIGN
      TOG_PAGA = TRUE 
      TOG_AVSLUTADE = TRUE
      uppvaltemp.PAAV = 3.      
   END.
   IF TOG_TILLF = TRUE AND TOG_FASTA = TRUE THEN DO:
      uppvaltemp.TILLFALLFAST = 3.
   END.
   ELSE IF TOG_TILLF = FALSE AND TOG_FASTA = TRUE THEN DO:
      uppvaltemp.TILLFALLFAST = 2.
   END.
   ELSE IF TOG_TILLF = TRUE AND TOG_FASTA = FALSE THEN DO:
      uppvaltemp.TILLFALLFAST = 1.
   END.
   ELSE DO:
      ASSIGN
      TOG_TILLF = TRUE 
      TOG_FASTA = TRUE
      uppvaltemp.TILLFALLFAST = 3.      
   END.
   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
   IF AVAILABLE omrtemp THEN uppvaltemp.OMRADE = omrtemp.OMRADE.
   ELSE uppvaltemp.OMRADE = "Alla".

   FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_BESORG NO-LOCK NO-ERROR.
   IF AVAILABLE bestkundallt THEN uppvaltemp.BESTID = bestkundallt.BESTID.
   ELSE DO:
      IF CMB_BESORG = "Samma som ovan" THEN uppvaltemp.BESTID = uppvaltemp.OMRADE.
      ELSE uppvaltemp.BESTID = CMB_BESORG. 
   END.
   FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = CMB_FAK NO-ERROR.
   uppvaltemp.FAKTTYP = faktyptemp.FAKTTYP.   

   FIND FIRST beredartemp WHERE beredartemp.NAMN = CMB_BERE NO-ERROR.
   IF AVAILABLE beredartemp THEN uppvaltemp.BEREDARE = beredartemp.PERSONALKOD.
   ELSE uppvaltemp.BEREDARE = "ALLA".
   
   FIND FIRST projtemp WHERE projtemp.NAMN = CMB_PROJ NO-ERROR.
   IF AVAILABLE projtemp THEN uppvaltemp.PROJEKTOR = projtemp.PERSONALKOD.
   ELSE uppvaltemp.PROJEKTOR = "ALLA".
   SUBSTRING(uppvaltemp.PROJEKTOR,25,10) = FILL-IN-K2.
   SUBSTRING(uppvaltemp.PROJEKTOR,35,10) = FILL-IN-K1.
   FIND FIRST ansvaraotemp WHERE ansvaraotemp.NAMN = CMB_ANSV NO-ERROR.
   IF AVAILABLE ansvaraotemp THEN uppvaltemp.ARBANSVARIG = ansvaraotemp.PERSONALKOD.
   ELSE uppvaltemp.ARBANSVARIG = "ALLA".
   FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
   IF AVAILABLE avdtemp THEN uppvaltemp.AVDNR = STRING(avdtemp.AVDELNINGNR).
   ELSE uppvaltemp.AVDNR = "ALLA".
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
   DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   IF leftbrwakt = TRUE THEN 
      antal_valda =   BRW_AONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   ELSE antal_valda = BRW_VAONR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Inget " + LC(Guru.Konstanter:gaok) + " är valt!" VIEW-AS ALERT-BOX.
      RETURN TRUE.                
   END.
   IF leftbrwakt = TRUE THEN DO:
      EMPTY TEMP-TABLE valdaao NO-ERROR.       
      RUN sel_over_UI IN brwproc[{&ARROWS}].
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdaao)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
   END.
   ELSE BRW_VAONR:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
   RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkbrw C-Win 
FUNCTION checkbrw RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
   RUN urvalaktiv_UI IN brwproc[{&ARROWS}] (OUTPUT leftbrwakt).
   RETURN leftbrwakt.   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

