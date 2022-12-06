&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
     
     
     
        
  KNAPPEN -rx GURU.DF     , BTN_ADM-RX,      Läser in en ny df fil GURU.DF I WTID MAPPEN
  
  DEFINE VARIABLE TOG_ON AS LOGICAL INITIAL yes 
     LABEL "On/off Line" 
  KRYSSRUTAN On/off Line  , TOG_ON           ANGER OM DATABASEN KÖRS I SINGEL ELLER MULTID
  KNAPPEN Kommando        , BTN_KOMMANDO,    KÖR DET SOM STÅR I FÄLTET Kommando
  KNAPPEN MULTIDB Kommando, BTN_MUKOMMAND,   KÖR DET SOM STÅR I FÄLTET  Kommando FÖR FLERA DATABASER
                                             ex. Kommando används för -C tablemove, GURUADD.ST mm 
                                               
                  
   KNAPPEN Program        , BTN_PROGRAM      KÖR DET som står i FILL-IN-programkor               
   KNAPPEN MULTIDB Program, BTN_MULTPROGRAM  KÖR DET som står i FILL-IN-programkor för flera databaser              
                                            ex STARTADMMEDSKAP.P STARTADMVisaAvSekApp.P
                                            
                                            
                                            
   FÄLTET SINGEL          , FILL-IN-SINGEL  Guru.Konstanter:AppSpringSet[14] behövs inte längre
   
   KNAPPEN Byt lösen mm!  , BTN_Bytlosen    KÖR STARTDBMELLAN.P där man kan lägga in olika program som ska utnyttja befintliga program för t.ex lägga upp användre från xml fil         
                 
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: STARTADMDB.w

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

{ALLDEF2.I}
{APPCONDEF.I}
{VALDBDEF.I}
&Scoped-define NEW NEW

Guru.Konstanter:SaltData().
DEFINE VARIABLE kolllosen  AS CHARACTER NO-UNDO CASE-SENSITIVE.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 30 NO-UNDO.
DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.
DEFINE VARIABLE logprogh AS HANDLE NO-UNDO.
DEFINE VARIABLE okvar AS LOGICAL NO-UNDO.

  
DEFINE TEMP-TABLE jobbtt NO-UNDO
   FIELD BENAMNING AS CHARACTER
   FIELD BENAMNINGSLUT AS CHARACTER
   FIELD ORDNING AS INTEGER
   FIELD SUBORDNING AS INTEGER
   FIELD ANM AS CHARACTER
   FIELD MK AS CHARACTER
   INDEX ORDNING ORDNING SUBORDNING.
DEFINE VARIABLE progkopia AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnvarhj AS CHARACTER NO-UNDO.

DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE computername  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE complength    AS INTEGER     NO-UNDO INITIAL 128.
DEFINE VARIABLE retvalue      AS INTEGER     NO-UNDO.


{Computer_LanIP.I}
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_JOBB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jobbtt valdbtemp

/* Definitions for BROWSE BRW_JOBB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_JOBB jobbtt.MK jobbtt.ORDNING ~
jobbtt.BENAMNING jobbtt.BENAMNINGSLUT jobbtt.ANM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_JOBB 
&Scoped-define QUERY-STRING-BRW_JOBB FOR EACH jobbtt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_JOBB OPEN QUERY BRW_JOBB FOR EACH jobbtt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_JOBB jobbtt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_JOBB jobbtt


/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.FORETAG valdbtemp.VALDB ~
valdbtemp.DBCON 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_JOBB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-3 RECT-4 BTN_ADM ~
BTN_ADM-RX TOG_DB BRW_VDB BRW_JOBB FORETAGIN DBNANAMN PORTnr TOG_ON DBPLATS ~
DBPLATS-2 FILL-IN-2 FILL-IN-KOMANDO FILL-IN-singel BTN_PROGRAM BTN_KOMMANDO ~
FILL-IN-ANVBYTLOSEN FILL-IN-1 FILL-IN-programkor FILL-IN-3 BTN_MULTPROGRAM ~
BTN_MUKOMMAND CMB_STMELLAN FILL-IN-5 BTN_Bytlosen FILL-IN-4 
&Scoped-Define DISPLAYED-OBJECTS TOG_DB FORETAGIN DBNANAMN PORTnr TOG_ON ~
DBPLATS DBPLATS-2 FILL-IN-2 FILL-IN-KOMANDO FILL-IN-singel ~
FILL-IN-ANVBYTLOSEN FILL-IN-1 FILL-IN-programkor FILL-IN-3 CMB_STMELLAN ~
FILL-IN-5 FILL-IN-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ADM AUTO-GO 
     LABEL "Data Admin" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_ADM-RX AUTO-GO 
     LABEL "-rx GURU.DF" 
     SIZE 18 BY 1.5 TOOLTIP "LÄGG GURU.DF I WTID MAPPEN".

DEFINE BUTTON BTN_Bytlosen AUTO-GO 
     LABEL "Byt lösen mm!" 
     SIZE 18 BY 1.29 TOOLTIP "Byta lösen lägga upp personal för ber-kalk-mark se STARTDBMELLAN.P".

DEFINE BUTTON BTN_KOMMANDO AUTO-GO 
     LABEL "Kommando" 
     SIZE 18 BY 1.5 TOOLTIP "Kör för ansluten databas".

DEFINE BUTTON BTN_MUKOMMAND AUTO-GO 
     LABEL "MULTIDB Kommando" 
     SIZE 18 BY 1.5 TOOLTIP "Kör för flera databaser".

DEFINE BUTTON BTN_MULTPROGRAM AUTO-GO 
     LABEL "MULTIDB Program" 
     SIZE 18 BY 1.5 TOOLTIP "Kör för flera databaser".

DEFINE BUTTON BTN_PROGRAM AUTO-GO 
     LABEL "Program" 
     SIZE 18 BY 1.5 TOOLTIP "Kör för ansluten databas".

DEFINE VARIABLE CMB_STMELLAN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "ANGE PROGRAM" 
     DROP-DOWN-LIST
     SIZE 21.5 BY 1 NO-UNDO.

DEFINE VARIABLE DBNANAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Databasnamn" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE DBPLATS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Databassökväg" 
     VIEW-AS FILL-IN 
     SIZE 68.5 BY 1 NO-UNDO.

DEFINE VARIABLE DBPLATS-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Databassökväg tillfälligt" 
     VIEW-AS FILL-IN 
     SIZE 72 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "ADMdynbrowse.w" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .79 TOOLTIP "Granska Data i databasen!" NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\pro11~\guru~\db~\" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "GURUSTC.P" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .79 TOOLTIP "Skapa nya GURUSUPPORT användare" NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "AVTUPP.P" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .79 TOOLTIP "För upplägg av Avtal tidskrivning" NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "XLABEL.W" 
     VIEW-AS FILL-IN 
     SIZE 24 BY .79 TOOLTIP "Ny sekretess till databas" NO-UNDO.

DEFINE VARIABLE FILL-IN-ANVBYTLOSEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .75 TOOLTIP "Din input till STARTDBMELLAN.P" NO-UNDO.

DEFINE VARIABLE FILL-IN-KOMANDO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommando" 
     VIEW-AS FILL-IN 
     SIZE 104.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-programkor AS CHARACTER FORMAT "X(256)":U 
     LABEL "Program" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-singel AS CHARACTER FORMAT "X(256)":U 
     LABEL "Singel" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FORETAGIN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE PORTnr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Port" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 25 BY 6.29
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.5 BY 6.75
     FGCOLOR 12 .

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 26.5 BY 6.21
     BGCOLOR 12 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 31 BY 6.21
     BGCOLOR 12 .

DEFINE VARIABLE TOG_DB AS LOGICAL INITIAL no 
     LABEL "Databaser på denna dator" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_ON AS LOGICAL INITIAL yes 
     LABEL "On/off Line" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.38 BY .79 NO-UNDO.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 18 BY 1.5.

DEFINE VARIABLE FILL-IN-anv AS CHARACTER FORMAT "X(256)":U 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lösen" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_JOBB FOR 
      jobbtt SCROLLING.

DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_JOBB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_JOBB C-Win _STRUCTURED
  QUERY BRW_JOBB NO-LOCK DISPLAY
      jobbtt.MK FORMAT "x(8)":U WIDTH 3
      jobbtt.ORDNING COLUMN-LABEL "Ordning" FORMAT "->,>>>,>>9":U
            WIDTH 7.5
      jobbtt.BENAMNING COLUMN-LABEL "Del ett av jobbsträng" FORMAT "x(256)":U
            WIDTH 25
      jobbtt.BENAMNINGSLUT COLUMN-LABEL "Del två av jobbsträng" FORMAT "x(256)":U
            WIDTH 25
      jobbtt.ANM FORMAT "x(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 116 BY 14.5
         TITLE "Vad ska göras?".

DEFINE BROWSE BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED
  QUERY BRW_VDB DISPLAY
      valdbtemp.FORETAG FORMAT "X(5)":U
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(40)":U WIDTH 36
      valdbtemp.DBCON FORMAT "x(256)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 51 BY 21.54
         TITLE "Databaser" ROW-HEIGHT-CHARS .54 TOOLTIP "Välj databas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-START
     FILL-IN-anv AT ROW 1.04 COL 69 COLON-ALIGNED WIDGET-ID 4
     FILL-IN-LOS AT ROW 1.04 COL 95.5 COLON-ALIGNED WIDGET-ID 6 BLANK 
     BTN_AVB AT ROW 31.71 COL 158.5 WIDGET-ID 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 178.25 BY 33.21
         TITLE "SKRIV IN ANV OCH LÖSEN" WIDGET-ID 200.

DEFINE FRAME DEFAULT-FRAME
     BTN_ADM AT ROW 1.25 COL 140 WIDGET-ID 12
     BTN_ADM-RX AT ROW 1.25 COL 160 WIDGET-ID 24
     TOG_DB AT ROW 1.96 COL 41 WIDGET-ID 30
     BRW_VDB AT ROW 2.71 COL 4.5
     BRW_JOBB AT ROW 2.71 COL 61 WIDGET-ID 100
     FORETAGIN AT ROW 17.63 COL 75.63 COLON-ALIGNED WIDGET-ID 2
     DBNANAMN AT ROW 17.63 COL 116 COLON-ALIGNED WIDGET-ID 4
     PORTnr AT ROW 17.71 COL 141 COLON-ALIGNED WIDGET-ID 14
     TOG_ON AT ROW 18 COL 159.5 WIDGET-ID 28
     DBPLATS AT ROW 19.17 COL 75.63 COLON-ALIGNED WIDGET-ID 6
     DBPLATS-2 AT ROW 20.71 COL 75.63 COLON-ALIGNED WIDGET-ID 8
     FILL-IN-2 AT ROW 20.92 COL 149.5 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     FILL-IN-KOMANDO AT ROW 22.21 COL 64.25 COLON-ALIGNED
     FILL-IN-singel AT ROW 24.96 COL 80.63 COLON-ALIGNED WIDGET-ID 34
     BTN_PROGRAM AT ROW 25.17 COL 111 WIDGET-ID 16
     BTN_KOMMANDO AT ROW 25.17 COL 154.5
     FILL-IN-ANVBYTLOSEN AT ROW 25.75 COL 17.5 COLON-ALIGNED WIDGET-ID 38
     FILL-IN-1 AT ROW 25.75 COL 44.63 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     FILL-IN-programkor AT ROW 26.46 COL 80 COLON-ALIGNED WIDGET-ID 18
     FILL-IN-3 AT ROW 26.83 COL 44.63 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     BTN_MULTPROGRAM AT ROW 27.04 COL 111 WIDGET-ID 32
     BTN_MUKOMMAND AT ROW 27.04 COL 154.5 WIDGET-ID 26
     CMB_STMELLAN AT ROW 27.08 COL 7.5 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     FILL-IN-5 AT ROW 27.92 COL 44.63 COLON-ALIGNED NO-LABEL WIDGET-ID 64
     BTN_Bytlosen AT ROW 28.33 COL 9.5 WIDGET-ID 36
     FILL-IN-4 AT ROW 29 COL 44.63 COLON-ALIGNED NO-LABEL WIDGET-ID 62
     "Kör det som står i Program" VIEW-AS TEXT
          SIZE 23.5 BY 1 AT ROW 23.75 COL 108 WIDGET-ID 48
          FONT 4
     "Copy and Paste Hjälp!" VIEW-AS TEXT
          SIZE 23.5 BY .83 AT ROW 24.75 COL 46.5 WIDGET-ID 54
          FONT 4
     "Kör det som står Kommando" VIEW-AS TEXT
          SIZE 23.5 BY 1.25 AT ROW 23.75 COL 152 WIDGET-ID 50
          FONT 4
     "För att köra special program" VIEW-AS TEXT
          SIZE 23.5 BY .83 AT ROW 24.75 COL 6.5 WIDGET-ID 60
          FONT 4
     "Välj databas för ADMIN av DB!" VIEW-AS TEXT
          SIZE 35.25 BY 1.25 AT ROW 1.5 COL 6
          FONT 17
     RECT-1 AT ROW 23.25 COL 107.5 WIDGET-ID 44
     RECT-2 AT ROW 23.25 COL 151.5 WIDGET-ID 46
     RECT-3 AT ROW 24.33 COL 46 WIDGET-ID 52
     RECT-4 AT ROW 24.33 COL 6 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.13 ROW 2.96
         SIZE 177 BY 29.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: jobbtt T "?" NO-UNDO temp-db jobbtt
      TABLE: ? T "?" NO-UNDO temp-db valdbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Start av Guru"
         HEIGHT             = 33.33
         WIDTH              = 178.5
         MAX-HEIGHT         = 42.42
         MAX-WIDTH          = 240
         VIRTUAL-HEIGHT     = 42.42
         VIRTUAL-WIDTH      = 240
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
/* BROWSE-TAB BRW_VDB TOG_DB DEFAULT-FRAME */
/* BROWSE-TAB BRW_JOBB BRW_VDB DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN-1:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-3:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-4:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-5:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FRAME FRAME-START
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_JOBB
/* Query rebuild information for BROWSE BRW_JOBB
     _TblList          = "Temp-Tables.jobbtt"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.jobbtt.MK
"jobbtt.MK" ? ? "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.jobbtt.ORDNING
"jobbtt.ORDNING" "Ordning" ? "integer" ? ? ? ? ? ? no ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.jobbtt.BENAMNING
"jobbtt.BENAMNING" "Del ett av jobbsträng" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.jobbtt.BENAMNINGSLUT
"jobbtt.BENAMNINGSLUT" "Del två av jobbsträng" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.jobbtt.ANM
"jobbtt.ANM" ? "x(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_JOBB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(40)" "character" ? ? ? ? ? ? no ? no no "36" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.valdbtemp.DBCON
"valdbtemp.DBCON" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VDB */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Start av Guru */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Start av Guru */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_JOBB
&Scoped-define SELF-NAME BRW_JOBB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_JOBB C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_JOBB IN FRAME DEFAULT-FRAME /* Vad ska göras? */
DO:
   RUN kommando_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VDB
&Scoped-define SELF-NAME BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   RUN fyllfalt_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ADM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ADM C-Win
ON CHOOSE OF BTN_ADM IN FRAME DEFAULT-FRAME /* Data Admin */
DO:  
   RUN pkoll_UI.
   IF okvar = FALSE THEN RETURN NO-APPLY.
   
   RUN  _admin.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ADM-RX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ADM-RX C-Win
ON CHOOSE OF BTN_ADM-RX IN FRAME DEFAULT-FRAME /* -rx GURU.DF */
DO:  
   RUN pkoll_UI.
   IF okvar = FALSE THEN RETURN NO-APPLY.
   MESSAGE "DU STARTAR LÄGG TILL TABELLER MM AV SAMTLIGA DATABASER"  SKIP 
   "OBS lägg sista posten dubbel i " Guru.Konstanter:wtidvar  "guru.df" SKIP
   
      VIEW-AS ALERT-BOX
   QUESTION BUTTONS OK-CANCEL TITLE "Köra?" UPDATE svar2 AS LOGICAL.          
   IF svar2 THEN RUN serverUPDATE.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-START
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME FRAME-START /* Avsluta */
DO:       
  /* IF Guru.Konstanter:globforetag = "SOLE" THEN QUIT.*/
   {BORTBRWPROC.I}
   SESSION:PRINTER-CONTROL-HANDLE = 0.
  /* RUN val_UI.*/
 /*  DEFAULT-WINDOW:HIDDEN = FALSE.*/
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON ENDKEY OF BTN_AVB IN FRAME FRAME-START /* Avsluta */
DO:
  SESSION:PRINTER-CONTROL-HANDLE = 0.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BTN_Bytlosen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_Bytlosen C-Win
ON CHOOSE OF BTN_Bytlosen IN FRAME DEFAULT-FRAME /* Byt lösen mm! */
DO:
   FILL-IN-ANVBYTLOSEN = INPUT FILL-IN-ANVBYTLOSEN.
   DISPLAY FILL-IN-ANVBYTLOSEN WITH FRAME DEFAULT-FRAME IN WINDOW C-Win. 
   RUN pkoll_UI. 
  
   IF okvar = FALSE THEN RETURN NO-APPLY.
   
   CMB_STMELLAN = INPUT CMB_STMELLAN.
   IF CMB_STMELLAN = "Byt lösen" THEN DO:
      RUN STARTDBMELLAN.P (INPUT 1, INPUT FILL-IN-ANVBYTLOSEN).
   END. 
   IF CMB_STMELLAN = "Skapa PERS.XSD" THEN DO:
      RUN STARTDBMELLAN.P (INPUT 2, INPUT "PXSD").
   END.
   IF CMB_STMELLAN = "Skapa Personer och Avändare" THEN DO:
      RUN STARTDBMELLAN.P (INPUT 2, INPUT "PERS").
   END.    

   MESSAGE "klart " STRING(time,"hh:mm:ss") VIEW-AS ALERT-BOX.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOMMANDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOMMANDO C-Win
ON CHOOSE OF BTN_KOMMANDO IN FRAME DEFAULT-FRAME /* Kommando */
DO:  
   RUN pkoll_UI.
   IF okvar = FALSE THEN RETURN NO-APPLY.
   FILL-IN-KOMANDO = INPUT FILL-IN-KOMANDO.
   FILL-IN-SINGEL = Guru.Konstanter:AppSpringSet[14].
   DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
   RUN kor_UI.      
   MESSAGE "klart " STRING(time,"hh:mm:ss") VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MUKOMMAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MUKOMMAND C-Win
ON CHOOSE OF BTN_MUKOMMAND IN FRAME DEFAULT-FRAME /* MULTIDB Kommando */
DO: 
   DEFINE VARIABLE pdbvar AS CHARACTER NO-UNDO.
   Guru.Konstanter:AppSpringSet[14] = "".
   FILL-IN-SINGEL = Guru.Konstanter:AppSpringSet[14].
   DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
   FILL-IN-programkor = "".
   RUN pkoll_UI. 
   IF okvar = FALSE THEN RETURN NO-APPLY.
   IF okvar = TRUE THEN DO:  
     
      FILL-IN-KOMANDO = INPUT FILL-IN-KOMANDO.
         pdbvar = FILL-IN-KOMANDO.
         IF pdbvar = "" THEN DO:
            MESSAGE "Inget körs"
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY. 
         END.         
      MESSAGE "Startar" FILL-IN-programkor pdbvar
      "FÖR SAMTLIGA DATABASER"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS OK-CANCEL TITLE "Köra?" UPDATE svar2 AS LOGICAL. 
      IF svar2 THEN DO:  
         RUN startup_UI IN logprogh (INPUT FILL-IN-programkor, INPUT pdbvar, INPUT TOG_ON).
         RUN server_UI IN logprogh.
         RUN acction_UI IN logprogh.
      END.        
   END.
   
   ELSE RETURN NO-APPLY. 
   MESSAGE "klart " STRING(time,"hh:mm:ss") VIEW-AS ALERT-BOX.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MULTPROGRAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MULTPROGRAM C-Win
ON CHOOSE OF BTN_MULTPROGRAM IN FRAME DEFAULT-FRAME /* MULTIDB Program */
DO: 
   DEFINE VARIABLE pdbvar AS CHARACTER NO-UNDO.
   Guru.Konstanter:AppSpringSet[14] = "".
   FILL-IN-SINGEL = Guru.Konstanter:AppSpringSet[14].
   DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
   RUN pkoll_UI. 
   IF okvar = FALSE THEN RETURN NO-APPLY.
   IF okvar = TRUE THEN DO:   
      FILL-IN-programkor = INPUT FILL-IN-programkor.
      IF FILL-IN-programkor NE "" THEN DO:
         pdbvar = "".
         IF SEARCH(FILL-IN-programkor) = ? THEN FILL-IN-programkor = REPLACE(FILL-IN-programkor,".P",".r").
         IF SEARCH(FILL-IN-programkor) = ? THEN FILL-IN-programkor = REPLACE(FILL-IN-programkor,".r",".P").
         IF SEARCH(FILL-IN-programkor) = ? THEN DO:
            MESSAGE "Hittar inte " FILL-IN-programkor 
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY. 
         END.
      END.  
      
               
      MESSAGE "Startar" FILL-IN-programkor pdbvar
      "FÖR SAMTLIGA DATABASER"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS OK-CANCEL TITLE "Köra?" UPDATE svar2 AS LOGICAL. 
      IF svar2 THEN DO:  
         RUN startup_UI IN logprogh (INPUT FILL-IN-programkor, INPUT pdbvar, INPUT TOG_ON).
         RUN server_UI IN logprogh.
         RUN acction_UI IN logprogh.
      END.        
   END.
   
   ELSE RETURN NO-APPLY. 
   MESSAGE "klart " STRING(time,"hh:mm:ss") VIEW-AS ALERT-BOX.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PROGRAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PROGRAM C-Win
ON CHOOSE OF BTN_PROGRAM IN FRAME DEFAULT-FRAME /* Program */
DO:
   FILL-IN-SINGEL = INPUT FILL-IN-singel.
   DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win. 
   RUN pkoll_UI. 
   IF okvar = FALSE THEN RETURN NO-APPLY.
   IF okvar = TRUE THEN DO:   
      FILL-IN-programkor = INPUT FILL-IN-programkor.
      FILL-IN-KOMANDO = INPUT FILL-IN-KOMANDO.
      IF FILL-IN-programkor = "STARTAVT.P" OR FILL-IN-programkor = "STARTAVT.r" THEN DO:
         RUN VALUE(FILL-IN-programkor) (INPUT FALSE).
      END.
      ELSE DO:
         IF SEARCH(FILL-IN-programkor) = ? THEN FILL-IN-programkor = REPLACE(FILL-IN-programkor,".P",".r").
         IF SEARCH(FILL-IN-programkor) = ? THEN FILL-IN-programkor = REPLACE(FILL-IN-programkor,".W",".r").
         IF SEARCH(FILL-IN-programkor) = ? THEN FILL-IN-programkor = REPLACE(FILL-IN-programkor,".r",".P").
         IF SEARCH(FILL-IN-programkor) = ? THEN FILL-IN-programkor = REPLACE(FILL-IN-programkor,".r",".W").
         IF SEARCH(FILL-IN-programkor) = ? THEN DO:
            MESSAGE "Hittar inte " FILL-IN-programkor 
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY. 
         END.     
         MESSAGE "Startar" FILL-IN-programkor
         VIEW-AS ALERT-BOX.
         RUN VALUE(FILL-IN-programkor).
      END.    
   END.
   ELSE RETURN NO-APPLY. 
   Guru.Konstanter:AppSpringSet[14] = FILL-IN-SINGEL.
   DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
   MESSAGE "klart " STRING(time,"hh:mm:ss") VIEW-AS ALERT-BOX.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_STMELLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_STMELLAN C-Win
ON VALUE-CHANGED OF CMB_STMELLAN IN FRAME DEFAULT-FRAME
DO:
   CMB_STMELLAN = INPUT CMB_STMELLAN.
   BTN_Bytlosen:LABEL = CMB_STMELLAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-2 C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-2 IN FRAME DEFAULT-FRAME
DO:
   DBPLATS-2 = INPUT FILL-IN-2.
   DISPLAY DBPLATS-2
   WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.

   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-START
&Scoped-define SELF-NAME FILL-IN-LOS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LOS C-Win
ON RETURN OF FILL-IN-LOS IN FRAME FRAME-START /* Lösen */
DO:
   RUN pkoll_ui.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME FILL-IN-singel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-singel C-Win
ON LEAVE OF FILL-IN-singel IN FRAME DEFAULT-FRAME /* Singel */
DO:
   FILL-IN-SINGEL = INPUT FILL-IN-singel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DB C-Win
ON VALUE-CHANGED OF TOG_DB IN FRAME DEFAULT-FRAME /* Databaser på denna dator */
DO:
   TOG_DB = INPUT TOG_DB. 
   IF TOG_DB = FALSE THEN DO:
      kommandosortquery = "for each valdbtemp".
   END.
   ELSE DO:
      
      FIND FIRST valdbtemp WHERE valdbtemp.DBCON MATCHES "*" + Computer_LanIP + "*" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valdbtemp THEN DO:
         computername = FILL(" ", complength).
         RUN GetComputerNameA (INPUT-OUTPUT computername, OUTPUT complength, OUTPUT retvalue).
         computername = RIGHT-TRIM(computername).
         Computer_LanIP = computername.
      END.
      kommandosortquery = "for each valdbtemp where valdbtemp.DBCON MATCHES '*" + Computer_LanIP + "*'".
      
   END. 
    
   RUN openbdynspec_UI.  
   Computer_LanIP = Ipcheck:checkIp().   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ON C-Win
ON VALUE-CHANGED OF TOG_ON IN FRAME DEFAULT-FRAME /* On/off Line */
DO:
   TOG_ON = INPUT TOG_ON.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_JOBB
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   DEFINE VARIABLE subo AS INTEGER NO-UNDO.   
   /*{WIN_M_START.I}*/   
   /*CMB_DB:SCREEN-VALUE = "Energiadministration". */
   {VALDBALL.I} 
    
   IF NOT VALID-HANDLE(logprogh) THEN RUN MultiDBUPDATE.p PERSISTENT SET logprogh. 
   SESSION:DEBUG-ALERT = YES.
   C-Win:TITLE = "Start av Guru " + PROVERSION + " " + SESSION:CLIENT-TYPE .
   FILL-IN-3 = "ALLMENBYTSTART.W".
   IF SESSION:TEMP-DIRECTORY BEGINS "D" THEN DO:
      FILL-IN-2 = "D:\pro11\GURU\DB\".
   END.
   RUN jobb_UI (INPUT 1,INPUT "K" ,INPUT subo,INPUT "PROREST ",INPUT "", INPUT "v10 OBS om du konvertera starta i rätt version! Har du ingen st fil kommer delar av db hamna i " + SESSION:TEMP-DIRECTORY).
   RUN jobb_UI (INPUT 2,INPUT "MK" ,INPUT subo,INPUT "PROUTIL ",INPUT " -C TRUNCATE BI ", INPUT "v10 OBS om du konvertera starta i rätt version!").
   RUN jobb_UI (INPUT 3,INPUT "MK" ,INPUT subo,INPUT "PROUTIL ",INPUT " -C conv1011 ", INPUT "v11 OBS om du konvertera starta i rätt version!").
   RUN jobb_UI (INPUT 4,INPUT "" ,INPUT subo,INPUT "steg4skapaendelta.df",INPUT "", INPUT "v11 Anslut databasen och skapa en delta.df eller läs IN en redan befintlig").
   RUN jobb_UI (INPUT 5,INPUT "K" ,INPUT subo,INPUT "PROBKUP ",INPUT " ", INPUT "v11 Ta backup och flytta db!").
   RUN jobb_UI (INPUT 6,INPUT "K" ,INPUT subo,INPUT "PROBKUP ",INPUT " ", INPUT "multibackup och flytta db!").
   RUN jobb_UI (INPUT 7,INPUT "MK" ,INPUT subo,INPUT "rfutil ",INPUT " -C aimage end", INPUT "Stoppa afterimage").
   RUN jobb_UI (INPUT 8,INPUT "MK" ,INPUT subo,INPUT "rfutil ",INPUT " -C aimage begin", INPUT "Starta afterimage").
   RUN jobb_UI (INPUT 8,INPUT "MK" ,INPUT 1,INPUT "rfutil ",INPUT " -C aiarchiver enable", INPUT " Enable After-Imaging (AI) while the database is offline:").
   RUN jobb_UI (INPUT 9,INPUT "MK" ,INPUT subo,INPUT "rfutil ",INPUT " -C aimage empty 1", INPUT "Töm afterimage nr 1").
   RUN jobb_UI (INPUT 10,INPUT "MK" ,INPUT subo,INPUT "rfutil ",INPUT " -C aimage empty 2", INPUT "Töm afterimage nr 2").
   RUN jobb_UI (INPUT 11,INPUT "MK" ,INPUT subo,INPUT "rfutil ",INPUT " -C aimage empty 3", INPUT "Töm afterimage nr 3").
   RUN jobb_UI (INPUT 12,INPUT "K" ,INPUT subo,INPUT "PROSHUT ",INPUT " ", INPUT "Stoppa db och slänga ut anv").
   RUN jobb_UI (INPUT 13,INPUT "K" ,INPUT subo,INPUT "CONNECT",INPUT " ", INPUT "Koppla upp dbplats SINGEL").
   RUN jobb_UI (INPUT 14,INPUT "K" ,INPUT subo,INPUT "CONNECT",INPUT " ", INPUT "Koppla upp dbconn MULTI").
   RUN jobb_UI (INPUT 14,INPUT "K" ,INPUT 1,INPUT "DISCONNECT",INPUT " ", INPUT "Koppla ifrån databas.").
   
   RUN jobb_UI (INPUT 15,INPUT "MK" ,INPUT subo,INPUT "proutil",INPUT "-C enablenewvsttables", INPUT "Starta NYA VST TABES 11.5 OCH HÖGRE").
   RUN jobb_UI (INPUT 16,INPUT "MK" ,INPUT subo,INPUT "proutil",INPUT "-C updatevst", INPUT "Uppdatera till 11.4 OCH LÄGRE").
   RUN jobb_UI (INPUT 17,INPUT "MK" ,INPUT subo,INPUT "proutil",INPUT "-C disablenewvsttables", INPUT "Stopp NYA VST TABES 11.5 OCH HÖGRE").
   RUN jobb_UI (INPUT 18,INPUT "MK" ,INPUT subo,INPUT "proutil",INPUT "-C tablemove di", INPUT "Flyttar TABBELLER OCH INDEX till data och index").
   RUN jobb_UI (INPUT 19,INPUT "MK" ,INPUT subo,INPUT "proutil",INPUT "-C tablemove", INPUT "Flyttar till data").
   RUN jobb_UI (INPUT 20,INPUT "MK" ,INPUT subo,INPUT "PROSTRCT",INPUT "repair ", INPUT "Om db är flyttad").
   RUN jobb_UI (INPUT 21,INPUT "MK" ,INPUT subo,INPUT "PROSTRCT",INPUT "ADD", INPUT "SKAPAR  AREA DATA INDEX LÄGG guruadd.st i db").
   RUN jobb_UI (INPUT 26,INPUT "MK" ,INPUT subo,INPUT "proutil",INPUT "-C idxmove", INPUT "Flyttar Kalkyl Beredning Index").
   /*
   RUN jobb_UI (INPUT 22,INPUT subo,INPUT "protc",INPUT "start", INPUT "Start av Tomcat").
   RUN jobb_UI (INPUT 23,INPUT subo,INPUT "protc",INPUT "stop", INPUT "Stopp av Tomcat").
   */
   /*
   RUN jobb_UI (INPUT 25,INPUT "proutil",INPUT "-C idxmove", INPUT "Flyttar till INDEX ").
   */
   CMB_STMELLAN:ADD-LAST("Byt lösen").
   CMB_STMELLAN:ADD-LAST("Skapa PERS.XSD").
   CMB_STMELLAN:ADD-LAST("Skapa Personer och Avändare").
   CMB_STMELLAN:SCREEN-VALUE = "ANGE PROGRAM".
   
   FILL-IN-3 = "GURUSUPPORTANV.W".
   
   Guru.Konstanter:globanv = FILL-IN-anv. 
   BRW_JOBB:COLUMN-RESIZABLE       = TRUE.
   BRW_VDB:COLUMN-RESIZABLE       = TRUE.
   RUN enable_UI.
   FRAME DEFAULT-FRAME:HIDDEN = TRUE.
   dynbrwh = BRW_VDB:HANDLE. 
   dynqueh = dynbrwh:QUERY.
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   kommandosortquery = "for each valdbtemp".
   RUN openbdynspec_UI .
   status-ok = BRW_VDB:DESELECT-FOCUSED-ROW() NO-ERROR.
   RELEASE valdbtemp.
   {SLUTWIN.I}
   IF NOT VALID-HANDLE(brwproc[1]) THEN RUN DYNBRWLIGHT.P PERSISTENT SET brwproc[1] (INPUT BRW_VDB:HANDLE).
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN-anv FILL-IN-LOS 
      WITH FRAME FRAME-START IN WINDOW C-Win.
  ENABLE FILL-IN-anv FILL-IN-LOS BTN_AVB 
      WITH FRAME FRAME-START IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-START}
  DISPLAY TOG_DB FORETAGIN DBNANAMN PORTnr TOG_ON DBPLATS DBPLATS-2 FILL-IN-2 
          FILL-IN-KOMANDO FILL-IN-singel FILL-IN-ANVBYTLOSEN FILL-IN-1 
          FILL-IN-programkor FILL-IN-3 CMB_STMELLAN FILL-IN-5 FILL-IN-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-3 RECT-4 BTN_ADM BTN_ADM-RX TOG_DB BRW_VDB BRW_JOBB 
         FORETAGIN DBNANAMN PORTnr TOG_ON DBPLATS DBPLATS-2 FILL-IN-2 
         FILL-IN-KOMANDO FILL-IN-singel BTN_PROGRAM BTN_KOMMANDO 
         FILL-IN-ANVBYTLOSEN FILL-IN-1 FILL-IN-programkor FILL-IN-3 
         BTN_MULTPROGRAM BTN_MUKOMMAND CMB_STMELLAN FILL-IN-5 BTN_Bytlosen 
         FILL-IN-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttaBerMtrl C-Win 
PROCEDURE FlyttaBerMtrl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Flyttakalk_UI C-Win 
PROCEDURE Flyttakalk_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fyllfalt_UI C-Win 
PROCEDURE fyllfalt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   DBNANAMN = valdbtemp.DBNAMN 
   DBPLATS = valdbtemp.DBPLATS
   FORETAGIN = valdbtemp.FORETAG.
   {AppSprinSet.I}
   IF INDEX(valdbtemp.DBCON,"-S") > 0 THEN PORTNR = TRIM(SUBSTRING(valdbtemp.DBCON,INDEX(valdbtemp.DBCON,"-S") + 3,5)).
   DISPLAY FORETAGIN DBPLATS DBNANAMN PORTNR 
   WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE jobb_UI C-Win 
PROCEDURE jobb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordv AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER mkin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sordv AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER jobbv AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER jobbsv AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER anmv AS CHARACTER NO-UNDO.
   CREATE jobbtt.
   ASSIGN 
   jobbtt.ORDNING = ordv
   jobbtt.MK = mkin
   jobbtt.SUBORDNING = sordv
   jobbtt.BENAMNING = jobbv
   jobbtt.BENAMNINGSLUT = jobbsv
   jobbtt.ANM = anmv.
   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kommando_UI C-Win 
PROCEDURE kommando_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dbvar AS CHARACTER NO-UNDO.
  DEFINE VARIABLE DBPLATSKOPIA AS CHARACTER NO-UNDO.
  DISPLAY FILL-IN-KOMANDO WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  Guru.Konstanter:AppSpringSet[14] = "".
  FILL-IN-SINGEL = Guru.Konstanter:AppSpringSet[14].
  DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  DBPLATS-2  = INPUT DBPLATS-2.
  IF DBPLATS-2 = "" THEN dbvar = DBPLATS.
  ELSE dbvar = DBPLATS-2.
  DBPLATSKOPIA = REPLACE(dbvar,"DB\","DBKOPIA\").
  IF jobbtt.BENAMNING = "CONNECT" THEN DO:
     IF AVAILABLE valdbtemp THEN DO:
        Guru.Konstanter:globforetag = valdbtemp.FORETAG.
        IF jobbtt.ANM MATCHES "*SINGEL*" THEN DO:
           Guru.Konstanter:AppSpringSet[14] = "singel".
           FILL-IN-SINGEL = Guru.Konstanter:AppSpringSet[14].
           DISPLAY FILL-IN-SINGEL WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
           FILL-IN-KOMANDO = jobbtt.BENAMNING + " " + dbvar + valdbtemp.DBNAMN + " -1".
           
        END.  
        ELSE DO:
           FILL-IN-KOMANDO = jobbtt.BENAMNING + " " + valdbtemp.DBCON.
           FILL-IN-KOMANDO = REPLACE(FILL-IN-KOMANDO,"www.guruonweb.se","webguru") NO-ERROR.
           FILL-IN-KOMANDO = REPLACE(FILL-IN-KOMANDO,"www2.guruonweb.se","webguru") NO-ERROR.
        END.   
     END.   
  END.
  ELSE IF jobbtt.BENAMNING = "DISCONNECT" THEN DO:
     FILL-IN-KOMANDO = jobbtt.BENAMNING + " " + DBNANAMN + " NO-ERROR".
     Guru.Konstanter:globforetag = "".
  END.
  
  ELSE IF jobbtt.BENAMNING = "PROSTRCT" THEN DO:
     FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + jobbtt.BENAMNINGSLUT + " " + dbvar + DBNANAMN .
     IF jobbtt.BENAMNINGSLUT = "ADD"  OR jobbtt.BENAMNINGSLUT = "ADDONLINE" THEN DO: 
        IF TOG_ON = TRUE THEN DO:
           IF INDEX(FILL-IN-KOMANDO,"ADDONLINE") = 0 THEN FILL-IN-KOMANDO = REPLACE(FILL-IN-KOMANDO,"ADD","ADDONLINE"). 
        END.   
        FILL-IN-KOMANDO =  FILL-IN-KOMANDO + " " + dbvar + "GURUADD.ST".
     END.
     FILL-IN-KOMANDO = Guru.Konstanter:dlcvar + FILL-IN-KOMANDO.
     
  END.
  ELSE IF jobbtt.BENAMNING = "proutil" THEN DO:
     FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + dbvar + DBNANAMN + " " + jobbtt.BENAMNINGSLUT .
     IF jobbtt.BENAMNINGSLUT = "-C tablemove"  THEN DO:
/*        FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + jobbtt.BENAMNINGSLUT + " ".*/
        FILL-IN-KOMANDO =  FILL-IN-KOMANDO + " data".
     END.
     IF jobbtt.BENAMNINGSLUT = "-C tablemove di"  THEN DO:
        /*
        FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + "-C tablemove" + " " .
        */
        FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + dbvar + DBNANAMN + " " + "-C tablemove".
        FILL-IN-KOMANDO =  FILL-IN-KOMANDO + " data index".
     END.   
     IF jobbtt.BENAMNINGSLUT = "-C idxmove"  THEN DO:
        FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + dbvar + DBNANAMN + " " + jobbtt.BENAMNINGSLUT.
     END.    
     FILL-IN-KOMANDO = Guru.Konstanter:dlcvar + FILL-IN-KOMANDO.
     
     
  END.
  ELSE IF jobbtt.BENAMNING = "protc" THEN DO:
     FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + jobbtt.BENAMNINGSLUT.
     FILL-IN-KOMANDO = Guru.Konstanter:dlcvar + FILL-IN-KOMANDO.
  END.
  
  ELSE IF jobbtt.anm BEGINS  "multibackup" THEN DO:
      
     FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + "online" + " " + dbvar + DBNANAMN + " " + jobbtt.BENAMNINGSLUT.
     IF SUBSTRING(FILL-IN-KOMANDO,1,7) =  "PROBKUP" THEN FILL-IN-KOMANDO = FILL-IN-KOMANDO  + dbvar + DBNANAMN + ".BCK -aiarcdir " +
     DBPLATSKOPIA +  " -com".
     FILL-IN-KOMANDO = Guru.Konstanter:dlcvar + FILL-IN-KOMANDO.
  END.   
  ELSE DO:
     FILL-IN-KOMANDO =  jobbtt.BENAMNING + " " + dbvar + DBNANAMN + " " + jobbtt.BENAMNINGSLUT.
     IF SUBSTRING(FILL-IN-KOMANDO,1,7) = "PROREST" THEN FILL-IN-KOMANDO = FILL-IN-KOMANDO  + dbvar + DBNANAMN + ".BCK".
     IF SUBSTRING(FILL-IN-KOMANDO,1,7) =  "PROBKUP" THEN FILL-IN-KOMANDO = FILL-IN-KOMANDO  + dbvar + DBNANAMN + ".BCK".
     FILL-IN-KOMANDO = Guru.Konstanter:dlcvar + FILL-IN-KOMANDO.
  END.
  
  DISPLAY FILL-IN-KOMANDO WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kor_UI C-Win 
PROCEDURE kor_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE koppla AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dbkorvar AS CHARACTER NO-UNDO.
   DISPLAY FILL-IN-KOMANDO WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.   
   DBNANAMN = INPUT DBNANAMN.
   dbkorvar =  FILL-IN-KOMANDO.   
   MESSAGE dbkorvar     
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS OK-CANCEL TITLE "Köra?" UPDATE svar AS LOGICAL.          
   IF svar THEN DO:
      IF dbkorvar BEGINS "CONNECT" THEN DO:
         /*
         dbkorvar = dbkorvar + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
         CONNECT VALUE(SUBSTRING(dbkorvar,8)).
         MESSAGE "SÄTTA ALIAS"     VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Alias?" UPDATE svar.
         IF svar THEN  {VERALIAS.I}
          */
         MESSAGE "SÄTTA ALIAS"     VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Alias?" UPDATE svar.
         IF svar THEN dbkorvar = dbkorvar + " -ld rt9" + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
         ELSE  dbkorvar = dbkorvar + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
         CONNECT VALUE(SUBSTRING(dbkorvar,8)).  
      END.
      ELSE IF dbkorvar BEGINS "DISCONNECT" THEN DO:
          DISCONNECT VALUE(LDBNAME(1)) NO-ERROR. 
      END. 
         
      ELSE DO:
         
         IF jobbtt.BENAMNINGSLUT BEGINS "-C tablemove"  THEN DO:
            IF LDBNAME(1) NE valdbtemp.DBNAMN THEN DO:
               IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
            END.
            IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO :
               DBPLATS = INPUT DBPLATS.
               DBPLATS-2  = INPUT DBPLATS-2.
               IF DBPLATS-2 = "" THEN DBPLATS-2 = DBPLATS.
               
               IF TOG_ON = TRUE THEN koppla = valdbtemp.DBCON + " -ld rt9" + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
               ELSE koppla = DBPLATS-2 + valdbtemp.DBNAMN + " -1 -ld rt9" + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
                CONNECT VALUE(koppla) .                 
            END.
            IF CONNECTED(LDBNAME(1))   THEN DO:
               RUN startup_UI IN logprogh (INPUT "", INPUT dbkorvar, INPUT TOG_ON).
               RUN tmove_UI IN logprogh (INPUT "").
               DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.
               
               RUN ttkoruppdate_UI IN logprogh.
            END.
            ELSE DO:
               MESSAGE "Ingen db"
               VIEW-AS ALERT-BOX.
            END.     
         END.
         ELSE IF jobbtt.BENAMNINGSLUT BEGINS "-C idxmove"  THEN DO:
            IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
            RUN IdxMoveKalk_UI IN logprogh (INPUT FILL-IN-KOMANDO).
            RUN IdxMoveBer_UI  IN logprogh (INPUT FILL-IN-KOMANDO).
         END.
         ELSE DO:
            OS-COMMAND  VALUE(dbkorvar). 
            IF dbkorvar MATCHES "*GURUADD.ST*" THEN DO:
               DBPLATS = INPUT DBPLATS.
               DBPLATS-2  = INPUT DBPLATS-2.
               IF DBPLATS-2 = "" THEN dbkorvar = DBPLATS.
               ELSE dbkorvar = DBPLATS-2.  
               
               progkopia = dbkorvar + DBNANAMN + ".ST". 
               prognamnvarhj = dbkorvar + "GURUADD.ST".
               OS-APPEND VALUE(prognamnvarhj) VALUE(progkopia).
            END.
         END.   
      END.  
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openbdynspec_UI C-Win 
PROCEDURE openbdynspec_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
   
   dynok = dynqueh:QUERY-PREPARE(kommandosortquery).
   IF dynok = TRUE THEN dynok = dynqueh:QUERY-OPEN() NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pkoll_ui C-Win 
PROCEDURE pkoll_ui :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   FILL-IN-ANV = INPUT FRAME FRAME-START FILL-IN-ANV .
   FILL-IN-LOS = INPUT FRAME FRAME-START FILL-IN-LOS.
   TOG_ON = INPUT FRAME DEFAULT-FRAME TOG_ON . 
   okvar = FALSE.
   kolllosen = CHR(75) + CHR(65) + CHR(71) + CHR(71) + CHR(69) + CHR(78) . 
   IF FILL-IN-ANV = CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111) AND kolllosen = FILL-IN-LOS THEN Okvar = TRUE.
   IF Okvar = TRUE THEN FRAME DEFAULT-FRAME:HIDDEN = FALSE.
   ELSE QUIT.
END PROCEDURE.

PROCEDURE GetComputerNameA EXTERNAL "kernel32":
   DEFINE INPUT-OUTPUT PARAMETER lpszName AS CHAR.
   DEFINE OUTPUT PARAMETER lpdwcBuffer AS LONG.
   DEFINE RETURN PARAMETER ReturnValue AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

