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
{DIRDEF.I}
{AVTAONRTEMP.I}
{AVDTEMP.I}
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
{BRWSOK.I}
{OMRTEMPW.I}
{AVTPLANTEMP.I}
{JURPERST.I}
{REGVAR.I}
&Scoped-define SHARED SHARED
&Scoped-define NEW
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE VARIABLE aonrapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{TIDUTTTNEW.I}    
{VISUPPTMP.I}
DEFINE BUFFER valdtidslagtempbuff FOR valdtidslagtemp.
DEFINE TEMP-TABLE valtemp
   FIELD VALDLISTA AS CHARACTER
   FIELD BAVAL AS INTEGER
   FIELD ALLTID AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE.
 
DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE TEMP-TABLE omravdJUD NO-UNDO
FIELD OMRADE AS CHARACTER.

DEFINE INPUT-OUTPUT PARAMETER vallista AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR omrtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR avdtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TLAGE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidslagtemp valdtidslagtemp

/* Definitions for BROWSE BRW_TLAGE                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_TLAGE tidslagtemp.TIDLAGE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TLAGE 
&Scoped-define QUERY-STRING-BRW_TLAGE FOR EACH tidslagtemp NO-LOCK ~
    BY tidslagtemp.TIDLAGE
&Scoped-define OPEN-QUERY-BRW_TLAGE OPEN QUERY BRW_TLAGE FOR EACH tidslagtemp NO-LOCK ~
    BY tidslagtemp.TIDLAGE.
&Scoped-define TABLES-IN-QUERY-BRW_TLAGE tidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TLAGE tidslagtemp


/* Definitions for BROWSE BRW_VTLAGE                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_VTLAGE valdtidslagtemp.TIDLAGE ~
valdtidslagtemp.AKTIVITET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VTLAGE 
&Scoped-define QUERY-STRING-BRW_VTLAGE FOR EACH valdtidslagtemp NO-LOCK ~
    BY valdtidslagtemp.TIDLAGE
&Scoped-define OPEN-QUERY-BRW_VTLAGE OPEN QUERY BRW_VTLAGE FOR EACH valdtidslagtemp NO-LOCK ~
    BY valdtidslagtemp.TIDLAGE.
&Scoped-define TABLES-IN-QUERY-BRW_VTLAGE valdtidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VTLAGE valdtidslagtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_UPP CMB_ARTAL BTN_NVE BTN_NVE-2 ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 CMB_JURP FBTN_VISA ~
CMB_AVD CMB_OMR BRW_TLAGE BRW_VTLAGE BTN_ALLOVER BTN_OVER BTN_BACK ~
BTN_ALLBACK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_UPP CMB_ARTAL FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT CMB_JURP CMB_AVD CMB_OMR FILL-IN-AOTEXT 

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
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort".

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UPP AS CHARACTER FORMAT "X(256)":U 
     LABEL "Val av lista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 45.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval :" 
      VIEW-AS TEXT 
     SIZE 22.5 BY .83
     FGCOLOR 2 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Visa allt", 3,
"Visning per år", 1,
"Visning per period", 2
     SIZE 55.5 BY 1 TOOLTIP "Vising av kostnader och intäkter." NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TLAGE FOR 
      tidslagtemp SCROLLING.

DEFINE QUERY BRW_VTLAGE FOR 
      valdtidslagtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TLAGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TLAGE C-Win _STRUCTURED
  QUERY BRW_TLAGE NO-LOCK DISPLAY
      tidslagtemp.TIDLAGE FORMAT "X(256)":U WIDTH 25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 46.5 BY 16.67
         TITLE "Tidlägen" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE BRW_VTLAGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VTLAGE C-Win _STRUCTURED
  QUERY BRW_VTLAGE NO-LOCK DISPLAY
      valdtidslagtemp.TIDLAGE FORMAT "X(35)":U
      valdtidslagtemp.AKTIVITET FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 46.5 BY 16.67
         TITLE "Valda Tidlägen" ROW-HEIGHT-CHARS .54.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_UPP AT ROW 2.5 COL 14.38 COLON-ALIGNED WIDGET-ID 6
     CMB_ARTAL AT ROW 2.5 COL 65.38
     BTN_NVE AT ROW 3.63 COL 73.25
     BTN_NVE-2 AT ROW 3.63 COL 92.75
     RAD_PERIOD AT ROW 3.83 COL 2 NO-LABEL WIDGET-ID 2
     FILL-IN-STARTDAT AT ROW 3.83 COL 61.25 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 3.83 COL 80.75 COLON-ALIGNED
     BTN_FVE AT ROW 4.46 COL 73.25
     BTN_FVE-2 AT ROW 4.46 COL 92.75
     CMB_JURP AT ROW 6.63 COL 66
     FBTN_VISA AT ROW 6.63 COL 111
     CMB_AVD AT ROW 7.71 COL 65
     CMB_OMR AT ROW 8.79 COL 68
     BRW_TLAGE AT ROW 11 COL 3 WIDGET-ID 100
     BRW_VTLAGE AT ROW 11 COL 57.5 WIDGET-ID 200
     BTN_ALLOVER AT ROW 14.5 COL 51
     BTN_OVER AT ROW 16.67 COL 51
     BTN_BACK AT ROW 18.92 COL 51
     BTN_ALLBACK AT ROW 21.08 COL 51
     BTN_AVB AT ROW 28.21 COL 111
     FILL-IN-AOTEXT AT ROW 1.25 COL 2.25 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: tidslagtemp T "?" NO-UNDO temp-db tidslagtemp
      TABLE: valdtidslagtemp T "?" NO-UNDO temp-db valdtidslagtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.42
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
/* BROWSE-TAB BRW_TLAGE CMB_OMR DEFAULT-FRAME */
/* BROWSE-TAB BRW_VTLAGE BRW_TLAGE DEFAULT-FRAME */
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PERIOD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TLAGE
/* Query rebuild information for BROWSE BRW_TLAGE
     _TblList          = "Temp-Tables.tidslagtemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tidslagtemp.TIDLAGE|yes"
     _FldNameList[1]   > Temp-Tables.tidslagtemp.TIDLAGE
"tidslagtemp.TIDLAGE" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TLAGE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VTLAGE
/* Query rebuild information for BROWSE BRW_VTLAGE
     _TblList          = "Temp-Tables.valdtidslagtemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.valdtidslagtemp.TIDLAGE|yes"
     _FldNameList[1]   > Temp-Tables.valdtidslagtemp.TIDLAGE
"valdtidslagtemp.TIDLAGE" ? "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.valdtidslagtemp.AKTIVITET
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VTLAGE */
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


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE C-Win
ON CHOOSE OF BTN_FVE IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 C-Win
ON CHOOSE OF BTN_FVE-2 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE C-Win
ON CHOOSE OF BTN_NVE IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 C-Win
ON CHOOSE OF BTN_NVE-2 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME DEFAULT-FRAME /* Avdelning */
DO:
   CMB_AVD = INPUT CMB_AVD.
    {CMB_AVDN1.I}
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


&Scoped-define SELF-NAME CMB_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_UPP C-Win
ON VALUE-CHANGED OF CMB_UPP IN FRAME DEFAULT-FRAME /* Val av lista */
DO:
   CMB_UPP = INPUT FRAME {&FRAME-NAME} CMB_UPP.
   FIND FIRST visaupp WHERE visaupp.UT = CMB_UPP NO-ERROR.
   vallista = visaupp.UPPFOLJVAL.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   visvalvar = 1.
   {AMERICANEUROPEAN.I}  
   RUN btnok_UI.  
   {EUROPEANAMERICAN.I} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   DEFINE VARIABLE status-musdia AS LOGICAL NO-UNDO. 
   status-musdia = SESSION:SET-WAIT-STATE("").
   RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT  FILL-IN-STARTDAT 
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STOPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STOPPDAT.
   RUN AlmanBtn.w.
   FILL-IN-STOPPDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD C-Win
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME DEFAULT-FRAME
DO:
   RAD_PERIOD = INPUT RAD_PERIOD.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TLAGE
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
    IF VALID-HANDLE(aonrapph) THEN DELETE PROCEDURE aonrapph NO-ERROR.   
   {BORTBRWPROC.I}
   musz = TRUE.
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
   tidslagtemp.TIDLAGE:LABEL IN BROWSE BRW_TLAGE = Guru.Konstanter:gtidlk
   BRW_TLAGE:TITLE = Guru.Konstanter:gtidlk
   valdtidslagtemp.TIDLAGE:LABEL IN BROWSE BRW_VTLAGE = Guru.Konstanter:gtidlk
   BRW_VTLAGE:TITLE = "Valda " + Guru.Konstanter:gtidlk
   CMB_JURP:LABEL = Guru.Konstanter:gjuk
   CMB_AVD:LABEL = Guru.Konstanter:gavdk
   CMB_OMR:LABEL = Guru.Konstanter:gomrk. 
   CMB_OMR:SCREEN-VALUE = "Alla".
   CMB_OMR = "Alla".  
   {JURPAVDSTART2.I}
   status-ok = CMB_ARTAL:DELETE("0"). 
   status-ok = CMB_ARTAL:ADD-LAST(STRING(YEAR(TODAY),"9999")).      
   CMB_ARTAL:SCREEN-VALUE = STRING(YEAR(TODAY),"9999").
   FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY)).
   FILL-IN-STOPPDAT = TODAY.
   DEFINE VARIABLE iar AS INTEGER NO-UNDO.
   iar = 1. 
   REPEAT:
      status-ok = CMB_ARTAL:ADD-LAST(STRING(YEAR(TODAY) - iar,"9999")).
      iar = iar + 1.
      IF iar = 10 THEN LEAVE.
   END.    
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   CMB_UPP:ADD-LAST(visaupp.UT).
   FOR EACH visaupp WHERE visaupp.KUURVAL = TRUE USE-INDEX ORDNING:
      CMB_UPP:ADD-LAST(visaupp.UT).
   END.    
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   CMB_UPP:SCREEN-VALUE = visaupp.UT.
   RUN enable_UI.
   {FRMSIZE.I}
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   DEBUGGER:SET-BREAK().
         
   Guru.GlobalaVariabler:collefth = ?.
   FBTN_VISA:HIDDEN = FALSE.
   Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).      
   ASSIGN C-Win:TITLE = visaupp.UT.  
   {musarrow.i}   
   {WIN_M_SLUT.I}
   RAD_PERIOD:HIDDEN = TRUE.   
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   FIND FIRST jurperstemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE jurperstemp THEN CMB_JURP:HIDDEN = TRUE.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_ui C-Win 
PROCEDURE allstartbrw_ui :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}]
      (INPUT BRW_TLAGE:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}]
      (INPUT BRW_VTLAGE:HANDLE IN FRAME {&FRAME-NAME}).        
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}]
      (INPUT BRW_TLAGE:HANDLE, INPUT BRW_VTLAGE:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).   
       
   RUN overextra_UI IN brwproc[{&ARROWS}]  (INPUT "seltidextra_UI", INPUT THIS-PROCEDURE).
   RUN aoverextra_UI IN brwproc[{&ARROWS}] (INPUT "seltidextra_UI", INPUT THIS-PROCEDURE).      
   tthandle = TEMP-TABLE tidslagtemp:HANDLE.       
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN DYNLADDATEMP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "TIDSLAGEN", INPUT "").
      RUN MAONRAPP.P PERSISTENT SET aonrapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "TIDSLAGEN", INPUT "").
      RUN MAONRAPP.P PERSISTENT SET aonrapph.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aseltidextra_UI C-Win 
PROCEDURE aseltidextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
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
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   CMB_AVD = INPUT FRAME {&FRAME-NAME} CMB_AVD.
   CMB_OMR = INPUT CMB_OMR.
   CMB_JURP = INPUT CMB_JURP.
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT.
   EMPTY TEMP-TABLE omravdJUD NO-ERROR. 
   IF CMB_OMR NE "ALLA" THEN DO:
      FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
      CREATE omravdJUD.
      omravdJUD.OMRADE = omrtemp.OMRADE.      
   END.   
   ELSE DO:
      IF CMB_AVD NE "Alla" THEN DO:
         FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNAMN = CMB_AVD NO-LOCK NO-ERROR.
         FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR NO-LOCK:
            CREATE omravdJUD.
            omravdJUD.OMRADE = omrtemp.OMRADE.
         END.
      END.
      ELSE DO:
         IF CMB_JURP NE  "Alla" THEN DO:
            FIND FIRST jurperstemp WHERE jurperstemp.NAMN = CMB_JURP NO-LOCK NO-ERROR.
            FOR EACH judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID NO-LOCK:
               FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR NO-LOCK:
                  CREATE omravdJUD.
                  omravdJUD.OMRADE = omrtemp.OMRADE.
               END.  
            END.
         END.
         ELSE DO:
            FOR EACH jurperstemp WHERE NO-LOCK:
               FOR EACH judavdtemp WHERE judavdtemp.JUDID = jurperstemp.JUDID NO-LOCK:
                  FOR EACH omrtemp WHERE omrtemp.AVDELNINGNR = judavdtemp.AVDELNINGNR NO-LOCK:
                     CREATE omravdJUD.
                     omravdJUD.OMRADE = omrtemp.OMRADE.
                  END.
               END.     
            END.
         END.      
      END.               
   END.   
   RUN TIDLAGEUTF.W (INPUT vallista, INPUT FILL-IN-STARTDAT, INPUT FILL-IN-STOPPDAT, INPUT TABLE omravdJUD, INPUT TABLE valdtidslagtemp, OUTPUT TABLE evaldaao).
   IF vallista NE 335 THEN RUN uppfolj_UI.
   {musarrow.i}
   APPLY "GO" TO FBTN_VISA IN FRAME {&FRAME-NAME}.    
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
  DISPLAY CMB_UPP CMB_ARTAL FILL-IN-STARTDAT FILL-IN-STOPPDAT CMB_JURP CMB_AVD 
          CMB_OMR FILL-IN-AOTEXT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_UPP CMB_ARTAL BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT FILL-IN-STOPPDAT 
         BTN_FVE BTN_FVE-2 CMB_JURP FBTN_VISA CMB_AVD CMB_OMR BRW_TLAGE 
         BRW_VTLAGE BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE seltidextra_UI C-Win 
PROCEDURE seltidextra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   FIND FIRST valdtidslagtempbuff WHERE valdtidslagtempbuff.IDTIDLAG = valdtidslagtemp.IDTIDLAG AND  valdtidslagtempbuff.AKTIVITET1 =  valdtidslagtemp.AKTIVITET1 NO-LOCK NO-ERROR.
   IF AVAILABLE valdtidslagtempbuff THEN DO:
      IF valdtidslagtempbuff.AKTIVITET = "" THEN valdtidslagtempbuff.AKTIVITET = valdtidslagtemp.AKTIVITET1.
   END.
   IF valdtidslagtemp.AKTIVITET2 NE "" THEN DO:
      FIND FIRST valdtidslagtempbuff WHERE valdtidslagtempbuff.IDTIDLAG = valdtidslagtemp.IDTIDLAG AND  valdtidslagtempbuff.AKTIVITET2 =  valdtidslagtemp.AKTIVITET2 NO-LOCK NO-ERROR.
      IF AVAILABLE valdtidslagtempbuff THEN DO:
         IF valdtidslagtempbuff.AKTIVITET = "" THEN valdtidslagtempbuff.AKTIVITET = valdtidslagtemp.AKTIVITET2.
         ELSE DO:
            CREATE valdtidslagtempbuff.
            BUFFER-COPY valdtidslagtemp TO valdtidslagtempbuff.
            valdtidslagtempbuff.AKTIVITET = valdtidslagtemp.AKTIVITET2.
         END.   
      END.
   END.
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   FOR EACH valdtidslagtemp WHERE valdtidslagtemp.AKTIVITET = "" NO-LOCK:
      FIND FIRST valdtidslagtempbuff WHERE valdtidslagtempbuff.IDTIDLAG = valdtidslagtemp.IDTIDLAG AND  valdtidslagtempbuff.AKTIVITET1 =  valdtidslagtemp.AKTIVITET1 NO-LOCK NO-ERROR.
      IF AVAILABLE valdtidslagtempbuff THEN DO:
         IF valdtidslagtempbuff.AKTIVITET = "" THEN valdtidslagtempbuff.AKTIVITET = valdtidslagtemp.AKTIVITET1.
      END.
      IF valdtidslagtemp.AKTIVITET2 NE "" THEN DO:
         FIND FIRST valdtidslagtempbuff WHERE valdtidslagtempbuff.IDTIDLAG = valdtidslagtemp.IDTIDLAG AND  valdtidslagtempbuff.AKTIVITET2 =  valdtidslagtemp.AKTIVITET2 NO-LOCK NO-ERROR.
         IF AVAILABLE valdtidslagtempbuff THEN DO:
            IF valdtidslagtempbuff.AKTIVITET = "" THEN valdtidslagtempbuff.AKTIVITET = valdtidslagtemp.AKTIVITET2.
            ELSE DO:
               CREATE valdtidslagtempbuff.
               BUFFER-COPY valdtidslagtemp TO valdtidslagtempbuff.
               valdtidslagtempbuff.AKTIVITET = valdtidslagtemp.AKTIVITET2.
            END.   
         END.
      END.
   END.   
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uppfolj_UI C-Win 
PROCEDURE uppfolj_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
   FIND FIRST visaupp WHERE visaupp.UPPFOLJVAL = vallista NO-LOCK NO-ERROR.
   FIND FIRST uppvaltemp NO-ERROR.
   uppvaltemp.VALDLIST = visaupp.UT.
                                   
   ASSIGN     
   uppvaltemp.VISPERAR = FALSE
   uppvaltemp.STARTDATUM = FILL-IN-STARTDAT
   uppvaltemp.SLUTDATUM  = FILL-IN-STOPPDAT
   uppvaltemp.OMRNAMN = CMB_OMR
   uppvaltemp.AVDNAMN = CMB_AVD.

   FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.      
   IF AVAILABLE omrtemp THEN uppvaltemp.OMRADE = omrtemp.OMRADE.
   ELSE uppvaltemp.OMRADE = "Alla".
   uppvaltemp.BESTID = uppvaltemp.OMRADE.
   SUBSTRING(uppvaltemp.PROJEKTOR,1,24) = "ALLA". 
   uppvaltemp.ARBANSVARIG = "ALLA".
   uppvaltemp.BESTID = "ALLA".
   uppvaltemp.OMRADE = "ALLA".   
   uppvaltemp.AVDNR = "ALLA".   
   IF vallista = 21 THEN DO:
      {AVBGOM.I}
      excellista = 6.
      IF Guru.Konstanter:appcon THEN DO:
         RUN AOMEAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao, 
         OUTPUT TABLE tidut). /*,OUTPUT str, OUTPUT str2, OUTPUT str3)*/
      END.
      ELSE DO:                   
         RUN AOMEAPP.P 
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao, 
         OUTPUT TABLE tidut). /*,OUTPUT str, OUTPUT str2, OUTPUT str3)*/         
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
      {AVBFRAM.I}
   END.
   ELSE IF vallista = 41 THEN DO:
      {AVBGOM.I} 
      RUN BEFVAL.W.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         {AVBFRAM.I}
         RETURN NO-APPLY.         
      END.
      RUN DIRUTFUC.W (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao).
      {AVBFRAM.I}
   END.
   ELSE IF vallista = 4 OR  vallista = 28 THEN DO: 
      {AVBGOM.I}
                                       
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:
         RUN extraval_UI.
         RUN DIRUTFUC.W (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao).
         
      END.
      ELSE DO:
         RUN extraval_UI.
         RUN DIRUTFUC.W (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao).
         
      END.
      {AVBFRAM.I}
   END.
   ELSE IF vallista = 34 THEN DO:       
      {AVBGOM.I}
      RUN DIRUTFNU2C.W (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao).
      
      {AVBFRAM.I}
   END.
   
   ELSE IF vallista = 38 THEN DO:
      {AVBGOM.I}
      excellista = 19.
      IF Guru.Konstanter:appcon THEN DO:
         RUN MIAOMEAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao, OUTPUT TABLE tidut). 
      END.
      ELSE DO:                   
         RUN MIAOMEAPP.P 
         (INPUT Guru.Konstanter:globanv,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao, OUTPUT TABLE tidut).          
      END.
      RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).       
      {AVBFRAM.I}
   END.
   ELSE IF vallista = 401 THEN DO:
      {AVBGOM.I}
      RUN ProjKosttUpp.w (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao).      
      {AVBFRAM.I}
   END.   
   ELSE DO:
      {AVBGOM.I}
      RUN DIRUTFUC.W (INPUT vallista,INPUT TABLE uppvaltemp, INPUT TABLE evaldaao).      
      {AVBFRAM.I}
   END.   
   {musarrow.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

