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

DEFINE INPUT PARAMETER planraonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER artaldelnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE btnberh AS HANDLE NO-UNDO.
 
/* Local Variable Definitions ---                                       */
{WHANDLTEMP.I}
   
DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
{BERIN.I}
{ALLDEF.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
&Scoped-define NEW 
&Scoped-define SHARED SHARED
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{FORETEMP.I}
&Scoped-define SHARED SHARED
{HUVLEVTEMP.I}
{LEVTEMP.I}  
{HOPPSEK2W.I}
{OMRTEMPW.I}
{AVDTEMP.I}
{AVDELNINGTEMP.I}
{BERTEMPTAB.I}
{SOKDEF.I}
{KONVALTEMP.I}  

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-BER
&Scoped-define BROWSE-NAME BRW_URBER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES urberedningtemp valberedningtemp

/* Definitions for BROWSE BRW_URBER                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URBER urberedningtemp.OMRADE ~
urberedningtemp.BERNR urberedningtemp.BENAMNING urberedningtemp.AONR ~
urberedningtemp.DELNR urberedningtemp.AKTIV urberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URBER 
&Scoped-define QUERY-STRING-BRW_URBER FOR EACH urberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URBER OPEN QUERY BRW_URBER FOR EACH urberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URBER urberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URBER urberedningtemp


/* Definitions for BROWSE BRW_VBER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VBER valberedningtemp.OMRADE ~
valberedningtemp.BERNR valberedningtemp.BENAMNING valberedningtemp.AONR ~
valberedningtemp.DELNR valberedningtemp.AKTIV valberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VBER 
&Scoped-define QUERY-STRING-BRW_VBER FOR EACH valberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VBER OPEN QUERY BRW_VBER FOR EACH valberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VBER valberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VBER valberedningtemp


/* Definitions for FRAME FRAME-BER                                      */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-7 RECT-22 RECT-62 TOG_AKT-2 ~
TOG_INAKT-2 TOG_BERAO TOG_SCH CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 CMB_UTF-2 ~
BTN_HAMT-2 BRW_URBER BRW_VBER TOG_BERNY BTN_KOPI-2 FILL-IN-BERNR ~
FILL-IN_AONR FILL-IN-BEN FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 ~
BTN_AVB-2 TOG_ALLABER BTN_BACK-2 BTN_OVER-2 
&Scoped-Define DISPLAYED-OBJECTS TOG_AKT-2 TOG_INAKT-2 TOG_BERAO TOG_SCH ~
CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 CMB_UTF-2 TOG_BERNY FILL-IN-BERNR ~
FILL-IN_AONR FILL-IN-BEN FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 ~
TOG_ALLABER FILL-IN-AOTEXT-B FILL-IN-HTEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB-2 
     LABEL "AvslutaR":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-2 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_HAMT-2 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_KOPI-2 
     LABEL "Kopiera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER-2 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE VARIABLE CMB_AVD-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-B AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av beredning" 
      VIEW-AS TEXT 
     SIZE 26.5 BY 1.21
     FGCOLOR 5 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY .83
     FGCOLOR 5  NO-UNDO.

DEFINE VARIABLE FILL-IN-HBERNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     LABEL "Beredningnr" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka beredningar."
     FGCOLOR 5 .

DEFINE VARIABLE FILL-IN-HTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta:" 
      VIEW-AS TEXT 
     SIZE 6.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(8)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR-2 AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR-2 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE IMAGE IMAGE-7
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.5 BY 1.21.

DEFINE VARIABLE TOG_AKT-2 AS LOGICAL INITIAL yes 
     LABEL "Aktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_ALLABER AS LOGICAL INITIAL no 
     LABEL "Ej delnr" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.75 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_BERAO AS LOGICAL INITIAL no 
     LABEL "Endast ej kopplade" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .75 NO-UNDO.

DEFINE VARIABLE TOG_BERNY AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 1.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_INAKT-2 AS LOGICAL INITIAL no 
     LABEL "Inaktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_SCH AS LOGICAL INITIAL no 
     LABEL "Endast schakt" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.13 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_URBER FOR 
      urberedningtemp SCROLLING.

DEFINE QUERY BRW_VBER FOR 
      valberedningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_URBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URBER C-Win _STRUCTURED
  QUERY BRW_URBER NO-LOCK DISPLAY
      urberedningtemp.OMRADE FORMAT "x(6)":U
      urberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 5.5 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      urberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 16.5
      urberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      urberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 49.5 BY 15.46
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VBER C-Win _STRUCTURED
  QUERY BRW_VBER NO-LOCK DISPLAY
      valberedningtemp.OMRADE FORMAT "x(6)":U
      valberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 7 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      valberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 19
      valberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      valberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 53.5 BY 15.46
         FONT 4
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BER
     TOG_AKT-2 AT ROW 2.17 COL 3
     TOG_INAKT-2 AT ROW 2.17 COL 11.25
     TOG_BERAO AT ROW 2.17 COL 19.63
     TOG_SCH AT ROW 2.17 COL 35.13
     CMB_JURP-2 AT ROW 3 COL 3.25
     CMB_AVD-2 AT ROW 4.08 COL 2.25
     CMB_OMR-2 AT ROW 5.17 COL 11.25 COLON-ALIGNED
     CMB_UTF-2 AT ROW 6.29 COL 11.25 COLON-ALIGNED
     BTN_HAMT-2 AT ROW 7.58 COL 18.5
     BRW_URBER AT ROW 8.79 COL 1.5
     BRW_VBER AT ROW 8.79 COL 56.13
     TOG_BERNY AT ROW 9.08 COL 109.38
     BTN_KOPI-2 AT ROW 13.46 COL 111
     FILL-IN-BERNR AT ROW 24.71 COL 21.88 COLON-ALIGNED
     FILL-IN_AONR AT ROW 24.71 COL 39.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 25.83 COL 21.88 COLON-ALIGNED
     FILL-IN-HBERNR AT ROW 25.83 COL 69.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_EAONR-2 AT ROW 25.83 COL 86.13 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR-2 AT ROW 25.83 COL 95 COLON-ALIGNED NO-LABEL
     BTN_AVB-2 AT ROW 25.83 COL 111
     TOG_ALLABER AT ROW 25.88 COL 101.63
     BTN_BACK-2 AT ROW 17.08 COL 51.5
     BTN_OVER-2 AT ROW 14.79 COL 51.5
     FILL-IN-AOTEXT-B AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-HTEXT AT ROW 25.83 COL 55 COLON-ALIGNED NO-LABEL
     IMAGE-7 AT ROW 24.75 COL 2.25
     RECT-22 AT ROW 24.5 COL 1.5
     RECT-62 AT ROW 25.63 COL 56.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: urberedningtemp T "?" NO-UNDO temp-db urberedningtemp
      TABLE: utvaldfasttemp T "?" NO-UNDO temp-db utvaldfasttemp
      TABLE: valberedningtemp T "?" NO-UNDO temp-db valberedningtemp
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
/* SETTINGS FOR FRAME FRAME-BER
                                                                        */
/* BROWSE-TAB BRW_URBER BTN_HAMT-2 FRAME-BER */
/* BROWSE-TAB BRW_VBER BRW_URBER FRAME-BER */
ASSIGN 
       BRW_URBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BER = TRUE
       BRW_URBER:COLUMN-RESIZABLE IN FRAME FRAME-BER       = TRUE.

ASSIGN 
       BRW_VBER:HIDDEN  IN FRAME FRAME-BER                = TRUE
       BRW_VBER:MAX-DATA-GUESS IN FRAME FRAME-BER         = 1000
       BRW_VBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BER = TRUE
       BRW_VBER:COLUMN-RESIZABLE IN FRAME FRAME-BER       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD-2 IN FRAME FRAME-BER
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP-2 IN FRAME FRAME-BER
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-B IN FRAME FRAME-BER
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-HTEXT IN FRAME FRAME-BER
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URBER
/* Query rebuild information for BROWSE BRW_URBER
     _TblList          = "Temp-Tables.urberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.urberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.urberedningtemp.BERNR
"urberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "5.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.urberedningtemp.BENAMNING
"urberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "16.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.urberedningtemp.AONR
"urberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.urberedningtemp.DELNR
"urberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.urberedningtemp.AKTIV
"urberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.urberedningtemp.ANVANDARE
"urberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VBER
/* Query rebuild information for BROWSE BRW_VBER
     _TblList          = "Temp-Tables.valberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.valberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.valberedningtemp.BERNR
"valberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.valberedningtemp.BENAMNING
"valberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.valberedningtemp.AONR
"valberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.valberedningtemp.DELNR
"valberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.valberedningtemp.AKTIV
"valberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.valberedningtemp.ANVANDARE
"valberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BER
/* Query rebuild information for FRAME FRAME-BER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BER */
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


&Scoped-define BROWSE-NAME BRW_URBER
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
   IF VALID-HANDLE(btnberh) THEN DELETE PROCEDURE btnberh.
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
   IF Guru.Konstanter:hoppsekvar[2] = TRUE THEN DO:
      IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
         RUN btnberstart_UI (INPUT 0, INPUT "", INPUT 0, INPUT TABLE berintemp).                  
      END. 
      ELSE LEAVE MAIN-BLOCK.      
   END.
   ELSE DO:
      LEAVE MAIN-BLOCK. 
   END. 
   RUN enable_UI.  
   
   {FRMSIZE.I}  
   {musarrow.i}   
   {WIN_M_SLUT.I}   
   {KRYSSBORT.I}          
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
  
  /*
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_UKALK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VKALK:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_UKALK:HANDLE, INPUT BRW_VKALK:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE). 
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnberstart_UI C-Win 
PROCEDURE btnberstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER beraonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER berdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR berintemp.     
   HIDE FRAME FRAME-START.
   HIDE FRAME FRAME-WEBB.      
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN goma_UI (INPUT TRUE,INPUT ""). 
   IF NOT VALID-HANDLE(btnberh) THEN DO:
      RUN btnber_UI  (INPUT franvart, INPUT beraonr, INPUT berdelnr, INPUT TABLE berintemp).       
   END.  
   ELSE RUN franstart_UI IN btnberh (INPUT franvart, INPUT beraonr, INPUT berdelnr, INPUT TABLE berintemp). 
   RUN frame_UI (INPUT "BER").
   /*direkt här*/
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN repo_UI IN btnberh.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnber_UI C-Win 
PROCEDURE btnber_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER beraonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER berdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR berintemp.
   EMPTY TEMP-TABLE whandltemp NO-ERROR.
   ordningnr = 1.
   CREATE whandltemp.
   RUN whandle_UI (INPUT-OUTPUT ordningnr, C-WIN:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_AKT-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_INAKT-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_JURP-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_AVD-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_OMR-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BACK-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,CMB_UTF-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_UPP-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_HAMT-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_OVER-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ALLOVER-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ALLBACK-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KALK-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_URBER:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_VBER:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LIST:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_INK:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ATG:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_KOPI-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_INAKTIV:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ADM:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_LAS:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_EXP:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_IMP:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BA:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_ATT:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_AVB-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BERNR:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN_AONR:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_NY-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BTN_BORT-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BEN:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-HBERNR:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN_EAONR-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN_DELNR-2:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-AOTEXT-B:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,IMAGE-7:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-HTEXT:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RECT-22:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RECT-62:HANDLE IN FRAME FRAME-BER).     
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_AOF-2:HANDLE IN FRAME FRAME-BER). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME FRAME-BER:HANDLE). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_HAOF-2:HANDLE IN FRAME FRAME-BER). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FRAME AMULTISTART-FRAME:HANDLE). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_BERNY:HANDLE IN FRAME FRAME-BER). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr,BRW_MAXMALL:HANDLE IN FRAME FRAME-KALK).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_BERAO:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, TOG_ALLABER:HANDLE IN FRAME FRAME-BER). 
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_SCHAKTP:HANDLE IN FRAME FRAME-BER).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,TOG_SCH:HANDLE IN FRAME FRAME-BER). 
   RUN BERMENY.P PERSISTENT SET btnberh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,
                                         INPUT franvart,INPUT beraonr,INPUT berdelnr,INPUT TABLE berintemp).
  
   FRAME FRAME-BER:HIDDEN = FALSE.   
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
  DISPLAY TOG_AKT-2 TOG_INAKT-2 TOG_BERAO TOG_SCH CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 
          CMB_UTF-2 TOG_BERNY FILL-IN-BERNR FILL-IN_AONR FILL-IN-BEN 
          FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 TOG_ALLABER 
          FILL-IN-AOTEXT-B FILL-IN-HTEXT 
      WITH FRAME FRAME-BER IN WINDOW C-Win.
  ENABLE IMAGE-7 RECT-22 RECT-62 TOG_AKT-2 TOG_INAKT-2 TOG_BERAO TOG_SCH 
         CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 CMB_UTF-2 BTN_HAMT-2 BRW_URBER BRW_VBER 
         TOG_BERNY BTN_KOPI-2 FILL-IN-BERNR FILL-IN_AONR FILL-IN-BEN 
         FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 BTN_AVB-2 TOG_ALLABER 
         BTN_BACK-2 BTN_OVER-2 
      WITH FRAME FRAME-BER IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BER}
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
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER kopivar AS LOGICAL NO-UNDO.
   kopivar = FALSE.
   EMPTY TEMP-TABLE extravaldfasttemp NO-ERROR. 
   CREATE extravaldfasttemp.
   ASSIGN
   extravaldfasttemp.BESTID = kalkbest
   extravaldfasttemp.TYP = valdfasttemp.TYP
   extravaldfasttemp.OMRADE = kalkomr
   extravaldfasttemp.KALKNR = ?
   extravaldfasttemp.AONR = ?
   extravaldfasttemp.DELNR = ?
   extravaldfasttemp.PLANNR = ?
   extravaldfasttemp.ARTAL = ?   
   extravaldfasttemp.AKTIV = TRUE
   extravaldfasttemp.KATAR = valdfasttemp.KATAR
   extravaldfasttemp.BENAMNING = "Kopierad från " + STRING(valdfasttemp.KALKNR) + " " + valdfasttemp.BENAMNING 
   extravaldfasttemp.STATUSNIV = ""
   extravaldfasttemp.KALKANV = valdfasttemp.KALKANV
   extravaldfasttemp.ANVANDARE = Guru.Konstanter:globanv.
   ASSIGN 
   extravaldfasttemp.STARTVNR = valdfasttemp.STARTVNR
   extravaldfasttemp.SLUTVNR = valdfasttemp.SLUTVNR
   extravaldfasttemp.STARTDAG = valdfasttemp.STARTDAG
   extravaldfasttemp.SLUTDAG = valdfasttemp.SLUTDAG.      

   RUN btnokkalkhuv_UI IN kalkproch (INPUT "", INPUT-OUTPUT TABLE extravaldfasttemp, OUTPUT felmedd). 
   IF felmedd = "OMR" THEN DO:
      MESSAGE "Det går inte att lägga upp kalkyler på detta " + LC(Guru.Konstanter:gomrk) + 
            ". Nummerserie saknas eller är fylld." VIEW-AS ALERT-BOX.
      kopivar = TRUE.
      RETURN.
   END.
  {muswait.i}      
   FIND FIRST extravaldfasttemp NO-LOCK NO-ERROR.
   extravaldfasttemp.TYPCHAR = "Fri".
   RUN kopkalkfri_UI IN kalkproch (INPUT valdfasttemp.KALKNR, INPUT extravaldfasttemp.KALKNR).                               
   /* tas bort sen!!!!!!!!     */
   RUN hamtakalkrec_UI IN kalkproch (INPUT extravaldfasttemp.TYP, INPUT extravaldfasttemp.KALKNR, OUTPUT kalkrec).
   /* Slut på tabort...     */
   FIND FIRST extravaldfasttemp NO-LOCK NO-ERROR.
   CREATE evaldfasttemp.
   BUFFER-COPY extravaldfasttemp TO evaldfasttemp.  
         
  
   
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
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER kopivar AS LOGICAL NO-UNDO.
   kopivar = FALSE.
    /*KALKÅR*/
   {KALKKAT4.I}
   ELSE DO:

      RUN ebrpris_UI IN kalkproch (OUTPUT ebrartal).
      IF valdfasttemp.KATAR NE ebrartal THEN DO:
         IF valdfasttemp.KATAR = 1998 THEN DO:
            MESSAGE "Kalkylen går ej att kopiera då den är gjord enligt " STRING(valdfasttemp.KATAR) 
            " års kostnadskatalog. Formatet har ändrats."VIEW-AS ALERT-BOX.
            kopivar = TRUE.
         END.
         ELSE DO:
            IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
               IF ebrartal = 2001 AND valdfasttemp.KATAR = 1999 THEN DO:
                  kopivar = FALSE.
               END.
               ELSE DO:
                  IF ebrartal - valdfasttemp.KATAR > 3 THEN DO:
                     MESSAGE "Kalkylen går ej att kopiera då den är bygger på en för gammal kostnadskatalog."
                     VIEW-AS ALERT-BOX.
                     kopivar = TRUE.
                  END.
               END.
            END.
            ELSE DO:
               IF ebrartal - valdfasttemp.KATAR > 3 THEN DO:
                  MESSAGE "Kalkylen går ej att kopiera då den är bygger på en för gammal kostnadskatalog."
                  VIEW-AS ALERT-BOX.
                  kopivar = TRUE.
               END.
            END.
         END.
      END.
   END.
   IF kopivar = FALSE THEN DO:
      EMPTY TEMP-TABLE extravaldfasttemp NO-ERROR. 
      CREATE extravaldfasttemp.
      ASSIGN
      extravaldfasttemp.BESTID = kalkbest
      extravaldfasttemp.TYP = valdfasttemp.TYP
      extravaldfasttemp.OMRADE = kalkomr
      extravaldfasttemp.KALKNR = ?
      extravaldfasttemp.AONR = ?
      extravaldfasttemp.DELNR = ?
      extravaldfasttemp.PLANNR = ?
      extravaldfasttemp.ARTAL = ?   
      extravaldfasttemp.AKTIV = TRUE
      extravaldfasttemp.KATAR = valdfasttemp.KATAR
      extravaldfasttemp.BENAMNING = "Kopierad från " + STRING(valdfasttemp.KALKNR) + " " + valdfasttemp.BENAMNING 
      extravaldfasttemp.STATUSNIV = ""
      extravaldfasttemp.KALKANV = valdfasttemp.KALKANV
      extravaldfasttemp.ANVANDARE = Guru.Konstanter:globanv.
      /*Kalkår*/

      IF extravaldfasttemp.TYP = 6 THEN DO:
         ASSIGN 
         extravaldfasttemp.STARTVNR = valdfasttemp.STARTVNR
         extravaldfasttemp.SLUTVNR = valdfasttemp.SLUTVNR
         extravaldfasttemp.STARTDAG = valdfasttemp.STARTDAG
         extravaldfasttemp.SLUTDAG = valdfasttemp.SLUTDAG.      
      END.
      ELSE DO:               
         extravaldfasttemp.STARTDAG = valdfasttemp.STARTDAG.
      END.

      
      RUN btnokkalkhuv_UI IN kalkproch (INPUT "", INPUT-OUTPUT TABLE extravaldfasttemp, OUTPUT felmedd). 
      IF felmedd = "OMR" THEN DO:
         MESSAGE "Det går inte att lägga upp kalkyler på detta " + LC(Guru.Konstanter:gomrk) + 
               ". Nummerserie saknas eller är fylld." VIEW-AS ALERT-BOX.
         kopivar = TRUE.
         RETURN.
      END.
      FIND FIRST extravaldfasttemp NO-LOCK NO-ERROR.
      IF extravaldfasttemp.TYP = 5 THEN extravaldfasttemp.TYPCHAR = "Sam".
      ELSE IF extravaldfasttemp.TYP = 6 THEN extravaldfasttemp.TYPCHAR = "Fri".
      ELSE IF extravaldfasttemp.TYP = 7 THEN extravaldfasttemp.TYPCHAR = "Nät".
      ELSE extravaldfasttemp.TYPCHAR = STRING(extravaldfasttemp.TYP).
      FIND FIRST extravaldfasttemp NO-LOCK NO-ERROR.
      RUN kopkalk_UI IN kalkproch (INPUT valdfasttemp.KALKNR, INPUT extravaldfasttemp.KALKNR, 
                                   OUTPUT TABLE ejkopi, OUTPUT TABLE ejmtrl).
      /*
      FIND FIRST ejmtrl NO-LOCK NO-ERROR.
      FIND FIRST ejkopi NO-LOCK NO-ERROR.
      IF AVAILABLE ejmtrl OR AVAILABLE ejkopi THEN RUN VKOPIA.W.      
      */
      /* tas bort sen!!!!!!!!     */
      RUN hamtakalkrec_UI IN kalkproch (INPUT extravaldfasttemp.TYP, INPUT extravaldfasttemp.KALKNR, OUTPUT kalkrec).
      FIND FIRST extravaldfasttemp NO-LOCK NO-ERROR.
      CREATE evaldfasttemp.
      BUFFER-COPY extravaldfasttemp TO evaldfasttemp.  
      
   END.
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
   IF classerkoll NE "" THEN DO:  
      status-mus2 = SESSION:SET-WAIT-STATE("").         
      EMPTY TEMP-TABLE utvaldfasttemp NO-ERROR. 
      RUN urvalkalkyl_UI IN KalkClasserStart (INPUT TABLE uppkalktemp,INPUT TABLE uppavdjud,OUTPUT TABLE utvaldfasttemp).                                                
   END. 
   ELSE RUN skapvaldan_UI IN kalkhmth (INPUT TABLE uppkalktemp,INPUT TABLE uppavdjud, OUTPUT TABLE utvaldfasttemp). 
  
  
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

