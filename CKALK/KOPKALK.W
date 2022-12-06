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

DEFINE INPUT PARAMETER vartifran AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER planraonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artaldelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kalktypvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER kalkomr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER kalkbest AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER planrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.


DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.
/*vartifran = 2 AONR koppla
vartifran = 3 PLAN koppla
vartifran = 4 AONR kopiera
vartifran = 5 PLAN kopiera
vartifran = 99 PLAN KALK TILL AONR KALK.
 
*/


{TIDUTTT.I} 
/* Local Variable Definitions ---                                       */
DEFINE NEW SHARED VARIABLE kalkrec     AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
{EJTEMP.I}
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{OMRTEMPW.I}
{KRYSSDEF.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{BESTKUNDALLT.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KALKALLTEMP.I} 
{KALKSTART.I}

{SOKDEF.I}
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
{AVDTEMP.I}
{AVDELNINGTEMP.I}
{HAMTAVDJUDEF.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
DEFINE BUFFER valdfasttempbuff FOR valdfasttemp.
 
DEFINE TEMP-TABLE raknartemp NO-UNDO
   FIELD UF AS INTEGER
   FIELD TYP1 AS INTEGER 
   FIELD TYP2 AS INTEGER 
   FIELD TYP3 AS INTEGER 
   FIELD TYP4 AS INTEGER 
   FIELD TYP5 AS INTEGER 
   FIELD TYP6 AS INTEGER 
   FIELD HUV1 AS INTEGER
   FIELD HUV2 AS INTEGER
   FIELD HUV3 AS INTEGER
   FIELD HUV4 AS INTEGER
   FIELD HUV5 AS INTEGER
   FIELD HUV6 AS INTEGER.
DEFINE TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.
DEFINE TEMP-TABLE evaldfasttemp NO-UNDO LIKE valdfasttemp.
DEFINE VARIABLE kalkproch   AS HANDLE NO-UNDO. /* KALKAPP1.P */
DEFINE VARIABLE ebrartal      AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE varfinns AS LOGICAL NO-UNDO.
DEFINE VARIABLE TOG_AKT AS LOGICAL INITIAL no 
     LABEL "Aktiva" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .88 TOOLTIP "Utsökningen ska gälla aktiva kalkyler." NO-UNDO.
DEFINE VARIABLE kalkhmth AS HANDLE NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE valdutf AS INTEGER NO-UNDO.
DEFINE VARIABLE nyttaoapph2 AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_UKALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utvaldfasttemp valdfasttemp

/* Definitions for BROWSE BRW_UKALK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_UKALK utvaldfasttemp.OMRADE ~
utvaldfasttemp.KALKNR utvaldfasttemp.BENAMNING utvaldfasttemp.TYPCHAR ~
utvaldfasttemp.VIKATAR utvaldfasttemp.AONR utvaldfasttemp.DELNR ~
utvaldfasttemp.PLANNR utvaldfasttemp.ARTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UKALK utvaldfasttemp.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define QUERY-STRING-BRW_UKALK FOR EACH utvaldfasttemp NO-LOCK ~
    BY utvaldfasttemp.OMRADE ~
       BY utvaldfasttemp.AONR ~
        BY utvaldfasttemp.DELNR
&Scoped-define OPEN-QUERY-BRW_UKALK OPEN QUERY BRW_UKALK FOR EACH utvaldfasttemp NO-LOCK ~
    BY utvaldfasttemp.OMRADE ~
       BY utvaldfasttemp.AONR ~
        BY utvaldfasttemp.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UKALK utvaldfasttemp


/* Definitions for BROWSE BRW_VKALK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VKALK valdfasttemp.KALKNR ~
valdfasttemp.BENAMNING valdfasttemp.TYPCHAR valdfasttemp.VIKATAR ~
valdfasttemp.AONR valdfasttemp.DELNR valdfasttemp.PLANNR valdfasttemp.ARTAL ~
valdfasttemp.STATUSNIV valdfasttemp.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VKALK valdfasttemp.KALKNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VKALK valdfasttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VKALK valdfasttemp
&Scoped-define QUERY-STRING-BRW_VKALK FOR EACH valdfasttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VKALK OPEN QUERY BRW_VKALK FOR EACH valdfasttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VKALK valdfasttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VKALK valdfasttemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_UKALK}~
    ~{&OPEN-QUERY-BRW_VKALK}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-50 RECT-52 TOG_INAK CMB_JURP CMB_AVD ~
CMB_OMR CMB_UTF CMB_BESORG CMB_KANSV CMB_KTYP FBTN_KOPPLA FBTN_KOPI ~
BTN_HAMT FBTN_AKTIV BRW_UKALK BRW_VKALK BTN_ALLOVER BTN_OVER BTN_BACK ~
BTN_ALLBACK FILL-IN_KAONR FILL-IN_KALKYL FILL-IN-KPLANNR FILL-IN_KALKB ~
FILL-IN_EKALNR BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_INAK CMB_JURP CMB_AVD CMB_OMR CMB_UTF ~
CMB_BESORG CMB_KANSV CMB_KTYP FILL-IN_KAONR FILL-IN_KALKYL FILL-IN-KPLANNR ~
FILL-IN_KALKB FILL-IN_EKALNR FILL-IN-KATEXT FILL-IN-SOK FILL-IN-VALK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON BTN_AVB 
     LABEL "AvslutaR":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1.17 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_AKTIV 
     LABEL "Status" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_KOPI 
     LABEL "Koppla/Kopiera":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_KOPPLA 
     LABEL "Koppla":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Best./Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kalkylansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KTYP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kalkyl typ" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KATEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av kalkyler" 
      VIEW-AS TEXT 
     SIZE 62 BY 1
     FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-KPLANNR AS CHARACTER FORMAT "X(6)" 
     LABEL "Plannr" 
     VIEW-AS FILL-IN 
     SIZE 9.13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK AS CHARACTER FORMAT "X(256)":U INITIAL "Sök på:" 
      VIEW-AS TEXT 
     SIZE 7.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-VALK AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta kalkyl nr:" 
      VIEW-AS TEXT 
     SIZE 17.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_EKALNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     LABEL "Kalkyl nr" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka kalkyler.".

DEFINE VARIABLE FILL-IN_KALKB AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_KALKYL AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Kalkyl nr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_KAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 9.13 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54.5 BY 2.42
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.38 BY 1.5
     BGCOLOR 8 .

DEFINE VARIABLE TOG_INAK AS LOGICAL INITIAL no 
     LABEL "Inaktiva" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY .54 TOOLTIP "Utsökningen ska gälla inaktiva." NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UKALK FOR 
      utvaldfasttemp SCROLLING.

DEFINE QUERY BRW_VKALK FOR 
      valdfasttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UKALK C-Win _STRUCTURED
  QUERY BRW_UKALK NO-LOCK DISPLAY
      utvaldfasttemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      utvaldfasttemp.KALKNR COLUMN-LABEL "Kalkyl nr" FORMAT ">>>>>>9":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      utvaldfasttemp.BENAMNING COLUMN-LABEL "Ort/!Benämning" FORMAT "x(20)":U
            WIDTH 18
      utvaldfasttemp.TYPCHAR COLUMN-LABEL "Typ" FORMAT "X(3)":U
      utvaldfasttemp.VIKATAR COLUMN-LABEL "Kat.!år" FORMAT "9999":U
      utvaldfasttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      utvaldfasttemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 2
      utvaldfasttemp.PLANNR COLUMN-LABEL "Plannr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      utvaldfasttemp.ARTAL FORMAT "9999":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
  ENABLE
      utvaldfasttemp.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 55 BY 13.75
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VKALK C-Win _STRUCTURED
  QUERY BRW_VKALK NO-LOCK DISPLAY
      valdfasttemp.KALKNR COLUMN-LABEL "Kalkyl nr" FORMAT ">>>>>>9":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      valdfasttemp.BENAMNING COLUMN-LABEL "Ort/!Benämning" FORMAT "x(20)":U
            WIDTH 19
      valdfasttemp.TYPCHAR COLUMN-LABEL "Typ" FORMAT "X(3)":U
      valdfasttemp.VIKATAR COLUMN-LABEL "Kat.!år" FORMAT "9999":U
      valdfasttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      valdfasttemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      valdfasttemp.PLANNR COLUMN-LABEL "Planr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      valdfasttemp.ARTAL FORMAT "9999":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      valdfasttemp.STATUSNIV COLUMN-LABEL "Stat" FORMAT "X(3)":U
      valdfasttemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
  ENABLE
      valdfasttemp.KALKNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 48.38 BY 13.75
         TITLE "Koppla dessa kalkyler".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     TOG_INAK AT ROW 2.5 COL 16 WIDGET-ID 2
     CMB_JURP AT ROW 3.17 COL 6
     CMB_AVD AT ROW 4.25 COL 5
     CMB_OMR AT ROW 5.33 COL 8
     CMB_UTF AT ROW 5.33 COL 54.25 COLON-ALIGNED
     CMB_BESORG AT ROW 6.42 COL 14 COLON-ALIGNED
     CMB_KANSV AT ROW 6.42 COL 54.25 COLON-ALIGNED
     CMB_KTYP AT ROW 7.5 COL 14 COLON-ALIGNED
     FBTN_KOPPLA AT ROW 8 COL 111.5
     FBTN_KOPI AT ROW 8 COL 111.5
     BTN_HAMT AT ROW 8.63 COL 29.25
     FBTN_AKTIV AT ROW 9.08 COL 111.5
     BRW_UKALK AT ROW 9.88 COL 1.5
     BRW_VKALK AT ROW 9.88 COL 62.38
     BTN_ALLOVER AT ROW 13.25 COL 56.63
     BTN_OVER AT ROW 15.42 COL 56.63
     BTN_BACK AT ROW 17.63 COL 56.63
     BTN_ALLBACK AT ROW 19.83 COL 56.63
     FILL-IN_KAONR AT ROW 24.13 COL 13.75 COLON-ALIGNED
     FILL-IN_KALKYL AT ROW 24.13 COL 33.25 COLON-ALIGNED
     FILL-IN-KPLANNR AT ROW 25.21 COL 13.75 COLON-ALIGNED
     FILL-IN_KALKB AT ROW 25.21 COL 33.25 COLON-ALIGNED
     FILL-IN_EKALNR AT ROW 25.21 COL 93.88 COLON-ALIGNED AUTO-RETURN 
     BTN_AVB AT ROW 25.29 COL 111.5
     FILL-IN-KATEXT AT ROW 1.5 COL 1.5 NO-LABEL
     FILL-IN-SOK AT ROW 24.13 COL 2.25 NO-LABEL
     FILL-IN-VALK AT ROW 25.21 COL 61.38 COLON-ALIGNED NO-LABEL
     RECT-50 AT ROW 23.88 COL 1.5
     RECT-52 AT ROW 24.79 COL 62.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 25.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: utvaldfasttemp T "?" NO-UNDO temp-db utvaldfasttemp
      TABLE: valdfasttemp T "?" NO-UNDO temp-db valdfasttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kalkylering"
         HEIGHT             = 25.88
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* BROWSE-TAB BRW_UKALK FBTN_AKTIV DEFAULT-FRAME */
/* BROWSE-TAB BRW_VKALK BRW_UKALK DEFAULT-FRAME */
ASSIGN 
       BRW_UKALK:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

ASSIGN 
       BRW_VKALK:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME DEFAULT-FRAME
   SHARED ALIGN-L                                                       */
ASSIGN 
       FBTN_AKTIV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FBTN_KOPI:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FBTN_KOPPLA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-KATEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SOK IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-VALK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UKALK
/* Query rebuild information for BROWSE BRW_UKALK
     _TblList          = "Temp-Tables.utvaldfasttemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utvaldfasttemp.OMRADE|yes,Temp-Tables.utvaldfasttemp.AONR|yes,Temp-Tables.utvaldfasttemp.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utvaldfasttemp.OMRADE
"utvaldfasttemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.utvaldfasttemp.KALKNR
"utvaldfasttemp.KALKNR" "Kalkyl nr" ">>>>>>9" "integer" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.utvaldfasttemp.BENAMNING
"utvaldfasttemp.BENAMNING" "Ort/!Benämning" ? "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.utvaldfasttemp.TYPCHAR
"utvaldfasttemp.TYPCHAR" "Typ" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.utvaldfasttemp.VIKATAR
"utvaldfasttemp.VIKATAR" "Kat.!år" "9999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.utvaldfasttemp.AONR
"utvaldfasttemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.utvaldfasttemp.DELNR
"utvaldfasttemp.DELNR" "Del!nr" ">99" "integer" ? 1 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.utvaldfasttemp.PLANNR
"utvaldfasttemp.PLANNR" "Plannr" ? "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.utvaldfasttemp.ARTAL
"utvaldfasttemp.ARTAL" ? ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_UKALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VKALK
/* Query rebuild information for BROWSE BRW_VKALK
     _TblList          = "Temp-Tables.valdfasttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valdfasttemp.KALKNR
"valdfasttemp.KALKNR" "Kalkyl nr" ">>>>>>9" "integer" ? 1 ? ? 1 ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.valdfasttemp.BENAMNING
"valdfasttemp.BENAMNING" "Ort/!Benämning" ? "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.valdfasttemp.TYPCHAR
"valdfasttemp.TYPCHAR" "Typ" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.valdfasttemp.VIKATAR
"valdfasttemp.VIKATAR" "Kat.!år" "9999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.valdfasttemp.AONR
"valdfasttemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.valdfasttemp.DELNR
"valdfasttemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.valdfasttemp.PLANNR
"valdfasttemp.PLANNR" "Planr" ? "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.valdfasttemp.ARTAL
"valdfasttemp.ARTAL" ? ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.valdfasttemp.STATUSNIV
"valdfasttemp.STATUSNIV" "Stat" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.valdfasttemp.OMRADE
"valdfasttemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_VKALK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kalkylering */
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
ON WINDOW-CLOSE OF C-Win /* Kalkylering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* AvslutaR */
DO:
   musz = FALSE.
   IF BTN_AVB:LABEL = "Ok" THEN RUN ufkoll_UI (INPUT 3).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   ELSE APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT C-Win
ON CHOOSE OF BTN_HAMT IN FRAME DEFAULT-FRAME /* Hämta och visa urval */
DO:   
   RUN utvald_UI.
   RUN title_UI IN brwproc[{&LEFT-BROWSE}].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME DEFAULT-FRAME /* Avdelning */
DO:
     
   CMB_AVD = INPUT CMB_AVD.      
   
   {CMB_AVDN1.I}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESORG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESORG C-Win
ON LEAVE OF CMB_BESORG IN FRAME DEFAULT-FRAME /* Best./Kund */
DO:
   CMB_BESORG = INPUT CMB_BESORG.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP C-Win
ON VALUE-CHANGED OF CMB_JURP IN FRAME DEFAULT-FRAME /* Juridisp */
DO:
     
   CMB_JURP = INPUT CMB_JURP.      
   
   {CMB_JURP.I}
   
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


&Scoped-define SELF-NAME FBTN_AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_AKTIV C-Win
ON CHOOSE OF FBTN_AKTIV IN FRAME DEFAULT-FRAME /* Status */
DO:
   RUN ufkoll_UI (INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KOPI C-Win
ON CHOOSE OF FBTN_KOPI IN FRAME DEFAULT-FRAME /* Koppla/Kopiera */
DO:   
   DEFINE VARIABLE detgickintebra AS LOGICAL NO-UNDO.
   DEFINE VARIABLE detgickbra AS LOGICAL NO-UNDO.
   DEFINE VARIABLE aonrplanin AS LOGICAL NO-UNDO.
   /*vartifran = 2 AONR koppla
vartifran = 3 PLAN koppla
vartifran = 4 AONR kopiera
vartifran = 5 PLAN kopiera
vartifran = 99 PLAN KALK TILL AONR KALK.
*/
   detgickintebra = TRUE. 
   
   IF vartifran = 4 THEN aonrplanin = TRUE.
   IF vartifran = 5 THEN aonrplanin = FALSE.
   IF vartifran = 99 THEN aonrplanin = ?.
   FOR EACH valdfasttemp:   
      IF valdfasttemp.KALKNR = ? THEN NEXT.
      RUN kopierakoppla_UI IN KalkClasserStart (INPUT valdfasttemp.KALKNR,INPUT valdfasttemp.OMRADE,INPUT kalkomr,INPUT planraonr,INPUT artaldelnr,INPUT aonrplanin,
      OUTPUT TABLE tidut).
      DELETE valdfasttemp.
      FIND FIRST tidut WHERE NO-LOCK NO-ERROR.
      IF AVAILABLE tidut THEN DO:
         MESSAGE tidut.UT SKIP 
         "Kalkyl kopierades inte!"
         VIEW-AS ALERT-BOX.
      END. 
          
   END.                                    
   APPLY "CHOOSE" TO BTN_AVB.
   /*
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN selectbyrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdfasttemp)).
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_KOPPLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_KOPPLA C-Win
ON CHOOSE OF FBTN_KOPPLA IN FRAME DEFAULT-FRAME /* Koppla */
DO:   

   RUN koppla_UI.
   
   APPLY "CHOOSE" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_EKALNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EKALNR C-Win
ON ANY-KEY OF FILL-IN_EKALNR IN FRAME DEFAULT-FRAME /* Kalkyl nr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_EKALNR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_EKALNR C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_EKALNR IN FRAME DEFAULT-FRAME /* Kalkyl nr */
DO:
   FILL-IN_EKALNR = INPUT FILL-IN_EKALNR.
   FIND FIRST valdfasttemp WHERE valdfasttemp.KALKNR = FILL-IN_EKALNR NO-LOCK NO-ERROR.
   IF AVAILABLE valdfasttemp THEN DO:
      MESSAGE "Kalkylen är redan vald." VIEW-AS ALERT-BOX.
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdfasttemp)).              
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].     
      RETURN NO-APPLY.
   END.
   ELSE DO:
      {muswait.i}
      RUN sokkalkyl_UI IN KalkClasserStart (INPUT FILL-IN_EKALNR,OUTPUT TABLE valdfasttemp APPEND).
      FIND FIRST valdfasttemp WHERE valdfasttemp.KALKNR = FILL-IN_EKALNR NO-ERROR.
      IF AVAILABLE valdfasttemp THEN varfinns = TRUE.                                     
      IF varfinns = TRUE THEN DO:
         FIND FIRST valdfasttemp WHERE valdfasttemp.KALKNR = FILL-IN_EKALNR NO-ERROR.
         IF vartifran = 2 OR vartifran = 3 THEN DO:
            IF valdfasttemp.AKTIV = FALSE THEN DO:
               DELETE valdfasttemp.
               MESSAGE "Kalkylen är inaktiv." VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            IF valdfasttemp.AONR NE ? THEN DO:
               DELETE valdfasttemp.
               MESSAGE "Kalkylen är redan kopplad." VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            ELSE IF valdfasttemp.PLANNR NE ? THEN DO:
               DELETE valdfasttemp.
               MESSAGE "Kalkylen är redan kopplad." VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            /*
            IF valdfasttemp.OMRADE NE kalkomr THEN DO:
               DELETE valdfasttemp.
               MESSAGE "Kalkylen har fel " LC(Guru.Konstanter:gomrk) "." VIEW-AS ALERT-BOX.
               RETURN NO-APPLY.
            END.
            */
         END.
         RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
         FIND FIRST valdfasttemp WHERE valdfasttemp.KALKNR = FILL-IN_EKALNR NO-LOCK NO-ERROR.
         IF AVAILABLE valdfasttemp THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(valdfasttemp)).              
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].               
         END.
      END.
      ELSE DO:
         MESSAGE "Kalkyl finns inte!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_INAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INAK C-Win
ON VALUE-CHANGED OF TOG_INAK IN FRAME DEFAULT-FRAME /* Inaktiva */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UKALK
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(kalkhmth) THEN DELETE PROCEDURE kalkhmth.
   IF VALID-HANDLE(kalkproch) THEN DELETE PROCEDURE kalkproch.
   IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.      
   IF VALID-HANDLE(nyttaoapph2) THEN DO:
      RUN borthandle_UI IN nyttaoapph2.
      DELETE PROCEDURE nyttaoapph2.
      nyttaoapph2 = ?.
   END.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph NO-ERROR.
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
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
   DEBUGGER:SET-BREAK().
   {JURPAVDSTART2.I}      
      
   ASSIGN
   utvaldfasttemp.VIKATAR:VISIBLE IN BROWSE BRW_UKALK = TRUE 
   valdfasttemp.VIKATAR:VISIBLE IN BROWSE BRW_VKALK = TRUE. 
   TOG_AKT = TRUE.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KALKSTART.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE kalkansv).
   END.
   ELSE DO:
       RUN KALKSTART.P 
      (OUTPUT TABLE kalkansv).                
   END.
   FIND FIRST omrtemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      {OMRHMT.I}
      status-ok = CMB_OMR:ADD-FIRST("Alla").
      FOR EACH judavdtemp,         
      EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR.
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).                        
      END.
   END.
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = kalkomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      FIND FIRST omrtemp USE-INDEX OMR NO-LOCK NO-ERROR.
   END.
   CMB_OMR = omrtemp.NAMN. 
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.
   DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 2.
      {BESTHMT.I}
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
   END.
   status-ok = CMB_BESORG:ADD-FIRST("Alla").
   FOR EACH bestkundallt USE-INDEX BESTNAMN NO-LOCK:
      status-ok = CMB_BESORG:ADD-LAST(bestkundallt.BESTNAMN).
   END.
   status-ok = CMB_BESORG:ADD-LAST("Samma som ovan").
   ASSIGN CMB_BESORG:SCREEN-VALUE = "Alla".
   DISPLAY CMB_BESORG WITH FRAME {&FRAME-NAME}.
   CMB_BESORG = INPUT CMB_BESORG.
   IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 3 THEN DO:
      status-ok = CMB_KTYP:ADD-LAST("Typ 1").
      status-ok = CMB_KTYP:ADD-LAST("Typ 2").
      status-ok = CMB_KTYP:ADD-LAST("Typ 3"). 
      ASSIGN CMB_KTYP:SCREEN-VALUE = "Typ 1".
   END.
   /*kalkfor*/
   IF Guru.Konstanter:varforetypchar[10] = "1" THEN DO:
   /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "cGRAN" {GLOBVES.I} THEN DO:*/ 
      status-ok = CMB_KTYP:ADD-LAST("Sam B/F").
   END.      
   IF Guru.Konstanter:varforetypval[41] = 1 THEN DO:
      IF vartifran = 3 OR vartifran = 5 THEN CMB_KTYP:ADD-LAST("Nätreg N1").
   END.  
   
   
    IF Guru.Konstanter:varforetypval[1] = 1 OR Guru.Konstanter:varforetypval[1] = 2 THEN status-ok = CMB_KTYP:ADD-LAST("Typ fri").
      IF Guru.Konstanter:varforetypval[1] =  2 THEN DO: 
         ASSIGN CMB_KTYP:SCREEN-VALUE = "Typ fri".
      END.
   status-ok = CMB_KTYP:ADD-LAST("Alla").
   IF kalktypvar = 1 THEN ASSIGN CMB_KTYP:SCREEN-VALUE = "Typ 1".
   IF kalktypvar = 2 THEN ASSIGN CMB_KTYP:SCREEN-VALUE = "Typ 2".
   IF kalktypvar = 3 THEN ASSIGN CMB_KTYP:SCREEN-VALUE = "Typ 3".
   IF kalktypvar = 5 THEN ASSIGN CMB_KTYP:SCREEN-VALUE = "Sam B/F".
   IF kalktypvar = 6 THEN ASSIGN CMB_KTYP:SCREEN-VALUE = "Typ fri".
   IF kalktypvar = 7 THEN ASSIGN CMB_KTYP:SCREEN-VALUE = "Nätreg N1".
   DISPLAY CMB_KTYP WITH FRAME {&FRAME-NAME}.
   CMB_KTYP = INPUT CMB_KTYP.
   valdutf = 0.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FAVO"                   
   inextradatatemp.HUVUDCH = Guru.Konstanter:globanv              
   inextradatatemp.HUVUDINT =  ?.   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      valdutf = extradatatemp.SOKINT[2].      
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   status-ok = CMB_UTF:ADD-FIRST("Alla").
   status-ok = CMB_UTF:ADD-LAST(Guru.Konstanter:globanv).
   IF valdutf = 0 THEN ASSIGN CMB_UTF:SCREEN-VALUE = "Alla".   
   ELSE ASSIGN CMB_UTF:SCREEN-VALUE = Guru.Konstanter:globanv.

/*   ASSIGN CMB_UTF:SCREEN-VALUE = "Alla".*/
   DISPLAY CMB_UTF WITH FRAME {&FRAME-NAME}.
   CMB_UTF = INPUT CMB_UTF.
   status-ok = CMB_KANSV:ADD-FIRST("Alla").
   FOR EACH kalkansv USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_KANSV:ADD-LAST(kalkansv.NAMN).
   END.
   ASSIGN CMB_KANSV:SCREEN-VALUE = "Alla".
   DISPLAY CMB_KANSV WITH FRAME {&FRAME-NAME}.
   CMB_KANSV = INPUT CMB_KANSV.
   ASSIGN      
   BTN_ALLBACK:TOOLTIP = "Alla valda kalkyler tas bort från vallistan"   
   BTN_ALLOVER:TOOLTIP = "Alla kalkyler väljs"
   BTN_HAMT:TOOLTIP = "Dina val ovan avgör vilka kalkyler du får i listan nedan."
   FILL-IN_EKALNR:TOOLTIP = "Tryck RETURN här för välja enstaka kalkyler" 
   FILL-IN_KAONR:LABEL = Guru.Konstanter:gaok
   FILL-IN_EKALNR:LABEL = "Kalkyl nr"
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL= Guru.Konstanter:gavdk
   CMB_OMR:LABEL = Guru.Konstanter:gomrk
   CMB_BESORG:LABEL = RIGHT-TRIM(SUBSTRIN(Guru.Konstanter:gbestk,1,12)).
   
   /*vartifran = 2 AONR koppla
vartifran = 3 PLAN koppla
vartifran = 4 AONR kopiera
vartifran = 5 PLAN kopiera
vartifran = 99 PLAN KALK TILL AONR KALK.
FILL-IN-KATEXT
*/  
   TOG_INAK = FALSE.
   IF vartifran = 2 OR vartifran = 3 THEN DO:
      TOG_INAK:HIDDEN = TRUE.    
      FILL-IN-KATEXT = FILL-IN-KATEXT + " för att koppla". 
   END.
   IF vartifran = 4 OR vartifran = 5 THEN DO:
      TOG_INAK:HIDDEN = FALSE.    
      FILL-IN-KATEXT = FILL-IN-KATEXT + " för att kopiera" .
   END.
   
   &Scoped-define FORMATNAMN FILL-IN_KAONR      
   {AOFORMAT3.I}
   RUN enable_UI.  
   IF vartifran = 4 OR vartifran = 5 THEN TOG_INAK:HIDDEN = FALSE.
   ELSE TOG_INAK:HIDDEN = TRUE.
   
   IF juidraknare < 2 THEN CMB_JURP:HIDDEN = TRUE.
   IF avdraknare < 2 THEN CMB_AVD:HIDDEN = TRUE.
   RUN goma_UI.
   {FRMSIZE.I}  
   ASSIGN
   valdfasttemp.STATUSNIV:VISIBLE IN BROWSE BRW_VKALK = FALSE.   
   IF vartifran <= 3 THEN DO:
      /*DISABLE  CMB_OMR WITH FRAME {&FRAME-NAME}.*/
      
      ASSIGN
      CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      ASSIGN 
      FBTN_KOPPLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_KOPI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      ASSIGN
      FBTN_KOPPLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_KOPI:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   IF vartifran = 99 THEN DO:
      /*CCC*/
      RUN koppplanaonr_UI IN kalkhmth (INPUT planrvar,INPUT artalvar, OUTPUT TABLE valdfasttemp). 
      RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
      RUN deleteleftbrw_UI IN brwproc[{&ARROWS}] (INPUT TRUE).
      ASSIGN
      RECT-50:HIDDEN = TRUE 
      RECT-52:HIDDEN = TRUE
      BTN_HAMT:HIDDEN = TRUE 
      CMB_AVD:HIDDEN = TRUE 
      CMB_BESORG:HIDDEN = TRUE 
      CMB_JURP:HIDDEN = TRUE 
      CMB_KANSV:HIDDEN = TRUE 
      CMB_KTYP:HIDDEN = TRUE 
      CMB_OMR:HIDDEN = TRUE 
      CMB_UTF:HIDDEN = TRUE 
      FBTN_AKTIV:HIDDEN = TRUE 
      FBTN_KOPPLA:HIDDEN = TRUE 
      FILL-IN_EKALNR:HIDDEN = TRUE 
      FILL-IN_KALKB:HIDDEN = TRUE 
      FILL-IN_KALKYL:HIDDEN = TRUE 
      FILL-IN_KAONR:HIDDEN = TRUE 
      FILL-IN-KATEXT:HIDDEN = TRUE 
      FILL-IN-KPLANNR:HIDDEN = TRUE 
      FILL-IN-SOK:HIDDEN = TRUE 
      FILL-IN-VALK:HIDDEN = TRUE.
   END.
   
   /*
   APPLY "CHOOSE" TO BTN_HAMT.
   */
   {musarrow.i}   
   {WIN_M_SLUT.I}   
   {KRYSSBORT.I}          
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.
   /*
   IF jid NE "" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   */
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
   ASSIGN
   utvaldfasttemp.OMRADE:READ-ONLY IN BROWSE BRW_UKALK = TRUE    
   valdfasttemp.KALKNR:READ-ONLY IN BROWSE   BRW_VKALK = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_UKALK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VKALK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_UKALK:HANDLE, INPUT BRW_VKALK:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE). 
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   IF Guru.Konstanter:appcon THEN DO:
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.  
      RUN KALKAPP1.P PERSISTENT SET kalkproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN KALKHMT1.P PERSISTENT SET kalkhmth ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2 ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN MAONRAPP.P PERSISTENT SET aonrapph.  
      RUN KALKAPP1.P PERSISTENT SET kalkproch.
      RUN KALKHMT1.P PERSISTENT SET kalkhmth.
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph2.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
/*   ccc*/
   RUN anvglob_UI IN kalkhmth (INPUT Guru.Konstanter:globanv).
   RUN anvglob_UI IN kalkproch (INPUT Guru.Konstanter:globanv).
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN-KPLANNR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "PLANNR").
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_KALKB:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").  
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_KALKYL:HANDLE IN FRAME {&FRAME-NAME}, INPUT "KALKNR").
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN_KAONR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AONR").  
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
  DISPLAY TOG_INAK CMB_JURP CMB_AVD CMB_OMR CMB_UTF CMB_BESORG CMB_KANSV 
          CMB_KTYP FILL-IN_KAONR FILL-IN_KALKYL FILL-IN-KPLANNR FILL-IN_KALKB 
          FILL-IN_EKALNR FILL-IN-KATEXT FILL-IN-SOK FILL-IN-VALK 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-50 RECT-52 TOG_INAK CMB_JURP CMB_AVD CMB_OMR CMB_UTF CMB_BESORG 
         CMB_KANSV CMB_KTYP FBTN_KOPPLA FBTN_KOPI BTN_HAMT FBTN_AKTIV BRW_UKALK 
         BRW_VKALK BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK FILL-IN_KAONR 
         FILL-IN_KALKYL FILL-IN-KPLANNR FILL-IN_KALKB FILL-IN_EKALNR BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma2_UI C-Win 
PROCEDURE goma2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   FBTN_AKTIV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   valdfasttemp.STATUSNIV:VISIBLE IN BROWSE BRW_VKALK = TRUE
   valdfasttemp.PLANNR:VISIBLE IN BROWSE BRW_VKALK = FALSE
   valdfasttemp.ARTAL:VISIBLE IN BROWSE BRW_VKALK = FALSE
   valdfasttemp.AONR:VISIBLE IN BROWSE BRW_VKALK = FALSE
   valdfasttemp.DELNR:VISIBLE IN BROWSE BRW_VKALK = FALSE
   BRW_UKALK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_HAMT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_ALLBACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_ALLOVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE    
   FBTN_KOPI:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_KOPPLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_BESORG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   CMB_KANSV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   CMB_KTYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   CMB_JURP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_AVD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   CMB_UTF:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_EKALNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_KALKB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_KALKYL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN_KAONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-KATEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-KPLANNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-VALK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   RECT-50:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   RECT-52:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.    
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
   FBTN_AKTIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   /*
   IF vartifran = 2 OR vartifran = 4 THEN DO:
      ASSIGN
      utvaldfasttemp.PLANNR:VISIBLE IN BROWSE BRW_UKALK = FALSE
      utvaldfasttemp.ARTAL :VISIBLE IN BROWSE BRW_UKALK = FALSE
      valdfasttemp.PLANNR:VISIBLE IN BROWSE BRW_VKALK = FALSE
      valdfasttemp.ARTAL :VISIBLE IN BROWSE BRW_VKALK = FALSE
      FILL-IN-KPLANNR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF vartifran = 3 OR vartifran = 5 THEN DO:
      ASSIGN
      utvaldfasttemp.AONR :VISIBLE IN BROWSE BRW_UKALK = FALSE
      utvaldfasttemp.DELNR:VISIBLE IN BROWSE BRW_UKALK = FALSE
      valdfasttemp.AONR :VISIBLE IN BROWSE BRW_VKALK = FALSE
      valdfasttemp.DELNR:VISIBLE IN BROWSE BRW_VKALK = FALSE.
      FILL-IN_KAONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopikalkfri_UI C-Win 
PROCEDURE kopikalkfri_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopikalk_UI C-Win 
PROCEDURE kopikalk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   musz = FALSE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koppla_UI C-Win 
PROCEDURE koppla_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}           
   IF Guru.Konstanter:appcon THEN DO:
      RUN KALKRUTINK1.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vartifran, INPUT planraonr,INPUT artaldelnr,INPUT-OUTPUT TABLE valdfasttemp).
   END.                       
   ELSE DO:
      RUN KALKRUTINK1.P 
      (INPUT vartifran, INPUT planraonr,INPUT artaldelnr,INPUT-OUTPUT TABLE valdfasttemp).
   END.                     
   RUN goma2_UI.
   {musarrow.i}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ufkoll_UI C-Win 
PROCEDURE ufkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   {muswait.i}  
   IF vad = 3 THEN DO:
      FIND FIRST raknartemp NO-ERROR.
      IF AVAILABLE raknartemp THEN DELETE raknartemp.
      CREATE raknartemp.
      FOR EACH valdfasttemp:
         IF valdfasttemp.STATUSNIV = "UF" THEN raknartemp.UF = raknartemp.UF + 1.
         IF valdfasttemp.TYP = 1 THEN DO:
            raknartemp.TYP1 = raknartemp.TYP1 + 1.
            IF valdfasttemp.STATUSNIV = "HUV" OR valdfasttemp.STATUSNIV = "UF" THEN
            raknartemp.HUV1 = raknartemp.HUV1 + 1.
         END.
         IF valdfasttemp.TYP = 2 THEN DO:
            raknartemp.TYP2 = raknartemp.TYP2 + 1.
            IF valdfasttemp.STATUSNIV = "HUV" OR valdfasttemp.STATUSNIV = "UF" THEN
            raknartemp.HUV2 = raknartemp.HUV2 + 1.
         END.
         IF valdfasttemp.TYP = 3 THEN DO:
            raknartemp.TYP3 = raknartemp.TYP3 + 1.
            IF valdfasttemp.STATUSNIV = "HUV" OR valdfasttemp.STATUSNIV = "UF" THEN
            raknartemp.HUV3 = raknartemp.HUV3 + 1.
         END.
         IF valdfasttemp.TYP = 5 THEN DO:
            raknartemp.TYP6 = raknartemp.TYP6 + 1.
            IF valdfasttemp.STATUSNIV = "HUV" OR valdfasttemp.STATUSNIV = "UF" THEN
            raknartemp.HUV6 = raknartemp.HUV6 + 1.
         END.
      END.     
      IF raknartemp.UF = 0 THEN DO:
         MESSAGE "En kalkyl måste vara utförandekalkyl" 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.      
      IF raknartemp.UF > 1 THEN DO:
         MESSAGE "Bara en kalkyl kan vara utförandekalkyl" 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP1 >= 1 AND raknartemp.HUV1 = 0 THEN DO:
         MESSAGE "En kalkyl måste vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP1 >= 1 AND raknartemp.HUV1 > 1 THEN DO:
         MESSAGE "Bara en kalkyl kan vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP2 >= 1 AND raknartemp.HUV2 = 0 THEN DO:
         MESSAGE "En kalkyl måste vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP2 >= 1 AND raknartemp.HUV2 > 1 THEN DO:
         MESSAGE "Bara en kalkyl kan vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP3 >= 1 AND raknartemp.HUV3 = 0 THEN DO:
         MESSAGE "En kalkyl måste vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP3 >= 1 AND raknartemp.HUV3 > 1 THEN DO:
         MESSAGE "Bara en kalkyl kan vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP5 >= 1 AND raknartemp.HUV5 = 0 THEN DO:
         MESSAGE "En kalkyl måste vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP5 >= 1 AND raknartemp.HUV5 > 1 THEN DO:
         MESSAGE "Bara en kalkyl kan vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP6 >= 1 AND raknartemp.HUV6 = 0 THEN DO:
         MESSAGE "En kalkyl måste vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF raknartemp.TYP6 >= 1 AND raknartemp.HUV6 > 1 THEN DO:
         MESSAGE "Bara en kalkyl kan vara huvudkalkyl för varje typ." 
         VIEW-AS ALERT-BOX.
         status-mus = SESSION:SET-WAIT-STATE("").
         musz = TRUE.
         RETURN.
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN KALKRUTINK1.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 1, INPUT planraonr,INPUT artaldelnr,INPUT-OUTPUT TABLE valdfasttemp).
      END.                       
      ELSE DO:
         RUN KALKRUTINK1.P 
         (INPUT 1, INPUT planraonr,INPUT artaldelnr,INPUT-OUTPUT TABLE valdfasttemp).
      END.                     
   END.
   IF vad = 4 THEN DO:
      DEFINE VARIABLE kalknralt AS INTEGER NO-UNDO.
      DEFINE VARIABLE statusnivallt AS CHARACTER NO-UNDO.
      statusnivallt = "".
      status-ok = BRW_VKALK:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.  
      IF valdfasttemp.STATUSNIV = "UF" THEN DO:
         MESSAGE "Detta är en kalkyl för utförande!" SKIP
         "Välj den kalkyl som skall vara utförandekalkyl"
         VIEW-AS ALERT-BOX.
         RETURN.
      END.
      MESSAGE "Vill du att kalkylen skall vara en kalkyl för utförande? " 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE kval AS LOGICAL.
      CASE kval:
         WHEN TRUE THEN DO:
            FIND FIRST valdfasttempbuff WHERE valdfasttempbuff.STATUSNIV = "UF" 
            NO-ERROR.      
            IF AVAILABLE valdfasttempbuff THEN DO:
               IF valdfasttempbuff.TYP = valdfasttemp.TYP  THEN valdfasttempbuff.STATUSNIV = "ALT".
               ELSE valdfasttempbuff.STATUSNIV = "HUV".               
            END.
            valdfasttemp.STATUSNIV = "UF".
            FIND FIRST valdfasttempbuff WHERE valdfasttempbuff.TYP = valdfasttemp.TYP AND valdfasttempbuff.STATUSNIV = "HUV"
            NO-ERROR.      
            IF AVAILABLE valdfasttempbuff THEN DO:
               valdfasttempbuff.STATUSNIV = "ALT".            
            END.
         END.
         WHEN FALSE THEN DO:
            IF valdfasttemp.STATUSNIV = "HUV" THEN DO:
               FIND FIRST valdfasttempbuff WHERE valdfasttempbuff.TYP = valdfasttemp.TYP AND valdfasttempbuff.STATUSNIV = "ALT"
               NO-ERROR.      
               IF AVAILABLE valdfasttempbuff THEN DO:
                  ASSIGN
                  valdfasttempbuff.STATUSNIV = "HUV"
                  valdfasttemp.STATUSNIV = "ALT".
               END.
            END.
            ELSE DO:
               FIND FIRST valdfasttempbuff WHERE valdfasttempbuff.TYP = valdfasttemp.TYP AND valdfasttempbuff.STATUSNIV = "HUV"
               NO-ERROR.      
               IF AVAILABLE valdfasttempbuff THEN DO:
                  ASSIGN
                  valdfasttempbuff.STATUSNIV = "ALT"
                  valdfasttemp.STATUSNIV = "HUV".               
               END.
               FIND FIRST valdfasttempbuff WHERE valdfasttempbuff.TYP = valdfasttemp.TYP AND valdfasttempbuff.STATUSNIV = "UF"
               NO-ERROR.      
               IF NOT AVAILABLE valdfasttempbuff THEN DO:
                  valdfasttemp.STATUSNIV = "HUV".          
               END.
            END.
         END.
      END CASE.
      IF Guru.Konstanter:appcon THEN DO:
         RUN KALKRUTINK1.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT 1, INPUT planraonr,INPUT artaldelnr,INPUT-OUTPUT TABLE valdfasttemp).
      END.
      ELSE DO:
         RUN KALKRUTINK1.P 
         (INPUT 1, INPUT planraonr,INPUT artaldelnr,INPUT-OUTPUT TABLE valdfasttemp).
      END.                      
   END.
   RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   {musarrow.i}   
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
   CMB_AVD = INPUT FRAME {&FRAME-NAME} CMB_AVD 
   CMB_KANSV = INPUT FRAME {&FRAME-NAME} CMB_KANSV 
   CMB_KTYP = INPUT FRAME {&FRAME-NAME} CMB_KTYP  
   CMB_UTF = INPUT FRAME {&FRAME-NAME} CMB_UTF  
   CMB_BESORG = INPUT FRAME {&FRAME-NAME} CMB_BESORG
   CMB_OMR = INPUT FRAME {&FRAME-NAME} CMB_OMR.  
   TOG_INAK = INPUT FRAME {&FRAME-NAME} TOG_INAK.   
   IF jid = "" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "celpa" THEN CMB_JURP = INPUT FRAME {&FRAME-NAME} CMB_JURP. 
   FIND FIRST uppkalktemp NO-ERROR.      
   IF NOT AVAILABLE uppkalktemp THEN CREATE uppkalktemp.
   FIND FIRST uppavdjud NO-ERROR.      
   IF NOT AVAILABLE uppavdjud THEN CREATE uppavdjud.
   FIND FIRST avdtemp WHERE avdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.   
   IF AVAILABLE avdtemp THEN uppavdjud.AVDNR = STRING(avdtemp.AVDELNINGNR).
   ELSE uppavdjud.AVDNR = "ALLA".
   FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.        
   IF AVAILABLE jurperstemp THEN uppavdjud.JUDID = jurperstemp.JUDID.
   ELSE uppavdjud.JUDID = "ALLA".
   ASSIGN         
   uppkalktemp.UTFARD = CMB_UTF.
   IF CMB_KTYP = "ALLA" THEN uppkalktemp.TYP = 0.
   ELSE IF CMB_KTYP = "Sam B/F" THEN uppkalktemp.TYP = 5.
   ELSE IF CMB_KTYP = "Typ fri" THEN uppkalktemp.TYP = 6.
   ELSE IF CMB_KTYP = "Nätreg N1" THEN uppkalktemp.TYP = 7.
   ELSE uppkalktemp.TYP = INTEGER(SUBSTRING(CMB_KTYP,5,1)). 
   
   uppkalktemp.AKIN = 1.    
   IF TOG_INAK:HIDDEN = FALSE THEN DO:
      IF TOG_INAK = TRUE THEN uppkalktemp.AKIN = 2.
      ELSE uppkalktemp.AKIN = 1.
   END.
   
                
   IF CMB_OMR NE "ALLA" THEN DO:
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
      IF AVAILABLE omrtemp THEN uppkalktemp.OMRADE = omrtemp.OMRADE.
      ELSE uppkalktemp.OMRADE = CMB_OMR.    
   END.
   ELSE uppkalktemp.OMRADE = CMB_OMR.    
   IF CMB_BESORG NE "ALLA" THEN DO:
      FIND FIRST bestkundallt WHERE bestkundallt.BESTNAMN = CMB_BESORG NO-LOCK NO-ERROR.
      IF AVAILABLE bestkundallt THEN uppkalktemp.BESTID = bestkundallt.BESTID.
      ELSE DO:
          IF CMB_BESORG = "Samma som ovan" THEN uppkalktemp.BESTID = uppkalktemp.OMRADE.
          ELSE uppkalktemp.BESTID = CMB_BESORG.
      END.
   END.
   ELSE uppkalktemp.BESTID = CMB_BESORG. 
   
   IF CMB_KANSV NE "ALLA" THEN DO:
      FIND FIRST kalkansv WHERE kalkansv.NAMN = CMB_KANSV NO-ERROR.
      IF AVAILABLE kalkansv THEN uppkalktemp.KALKANSVARIG = kalkansv.PERSONALKOD.
      ELSE uppkalktemp.KALKANSVARIG = CMB_KANSV.
   END.
   ELSE uppkalktemp.KALKANSVARIG = CMB_KANSV.    
        
   {muswait.i}
   EMPTY TEMP-TABLE utvaldfasttemp NO-ERROR.
   status-mus2 = SESSION:SET-WAIT-STATE("").         
   EMPTY TEMP-TABLE utvaldfasttemp NO-ERROR. 
   RUN urvalkalkyl_UI IN KalkClasserStart (INPUT TABLE uppkalktemp,INPUT TABLE uppavdjud,OUTPUT TABLE utvaldfasttemp).                                                
   IF vartifran = 2 OR vartifran = 3 THEN DO:
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "utvaldfasttemp.AONR = ? AND utvaldfasttemp.PLANNR = ?").
   END.
   ELSE IF vartifran = 4 OR vartifran = 5 THEN DO:

      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   END.   
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

