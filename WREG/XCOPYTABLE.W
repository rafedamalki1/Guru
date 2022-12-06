&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 
XCOPYTABLE.W  KOPIERA TABELLER
 
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
DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.
{FRAMSIZETEMP.I}
/* DEFINE VARIABLE framesizedh AS HANDLE NO-UNDO. */
DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.
/*
DEFINE NEW SHARED VARIABLE hpApi AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.*/
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE handproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE apphandass AS HANDLE NO-UNDO.
DEFINE VARIABLE apphandklar AS HANDLE NO-UNDO.
DEFINE VARIABLE appprogok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE LEFT-BROWSE 1
&GLOBAL-DEFINE RIGHT-BROWSE 2
&GLOBAL-DEFINE ARROWS 3
DEFINE VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.
{SLVALDBRW.I}

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{GLOBVAR2DEL1.I}
/*{EGENBVAR.I}*/
{VALDBDEF.I}
DEFINE VARIABLE conappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
DEFINE VARIABLE frandb AS CHARACTER NO-UNDO.
DEFINE VARIABLE franforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE gforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgdb AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgfieldh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgqh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempkommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE tillcon AS CHARACTER NO-UNDO.
DEFINE VARIABLE francon AS CHARACTER NO-UNDO.
DEFINE VARIABLE tilldb AS CHARACTER NO-UNDO.
DEFINE VARIABLE tillforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE koptabproch AS HANDLE NO-UNDO.
DEFINE VARIABLE tabnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE klar AS LOGICAL NO-UNDO.
DEFINE VARIABLE stoppa AS LOGICAL NO-UNDO.
DEFINE VARIABLE counter AS INTEGER NO-UNDO.
DEFINE VARIABLE counter2 AS INTEGER NO-UNDO.
DEFINE VARIABLE asynch AS HANDLE NO-UNDO.
DEFINE VARIABLE fellog AS LOGICAL NO-UNDO.

/* DEFINE NEW SHARED VARIABLEgplk AS CHARACTER NO-UNDO.     /*PLANNR KORT*/   */
/* DEFINE NEW SHARED VARIABLEgaok AS CHARACTER NO-UNDO.       /*AONR KORT*/   */
/* DEFINE NEW SHARED VARIABLE gomrk AS CHARACTER NO-UNDO.      /*OMRÅDE KORT*/ */

DEFINE TEMP-TABLE filetemp NO-UNDO
   FIELD FILENAME    AS CHARACTER   FORMAT "x(32)"
   FIELD FILENUMBER  AS INTEGER     FORMAT "->>>>9"
   FIELD NUMKEY      AS INTEGER     FORMAT ">>9"
   FIELD NUMKFLD     AS INTEGER     FORMAT ">>9"
   FIELD NUMKCOMP    AS INTEGER     FORMAT ">>9"
   FIELD NUMFLD      AS INTEGER     FORMAT ">>9"
   FIELD LASTCHANGED AS INTEGER     FORMAT "->>>>>>>>>9"
   INDEX NAMN  FILENAME.
DEFINE TEMP-TABLE valfiletemp NO-UNDO LIKE filetemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TAB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES filetemp valdbtemp valfiletemp

/* Definitions for BROWSE BRW_TAB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TAB filetemp.FILENAME 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TAB filetemp.FILENAME 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TAB filetemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TAB filetemp
&Scoped-define QUERY-STRING-BRW_TAB FOR EACH filetemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TAB OPEN QUERY BRW_TAB FOR EACH filetemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TAB filetemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TAB filetemp


/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.FORETAG valdbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for BROWSE BRW_VTAB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VTAB valfiletemp.FILENAME 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VTAB valfiletemp.FILENAME 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VTAB valfiletemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VTAB valfiletemp
&Scoped-define QUERY-STRING-BRW_VTAB FOR EACH valfiletemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VTAB OPEN QUERY BRW_VTAB FOR EACH valfiletemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VTAB valfiletemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VTAB valfiletemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_VDB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-4 RECT-5 RECT-6 CMB_VALDB ~
BRW_VDB BTN_FRAN FILL-FRAN BTN_TILL FILL-TILL BRW_TAB BRW_VTAB EDT-LOG ~
BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK BTN_OK BTN_AVB FILL-IN-TAB ~
FILL-IN-LOG 
&Scoped-Define DISPLAYED-OBJECTS CMB_VALDB FILL-FRAN FILL-TILL EDT-LOG ~
FILL-IN-TAB FILL-IN-LOG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

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
     LABEL "Avsluta" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_FRAN 
     LABEL "Från" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_OK 
     LABEL "Kopiera" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_STOP 
     LABEL "Stoppa" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_TILL 
     LABEL "Till" 
     SIZE 12 BY 1.5.

DEFINE VARIABLE CMB_VALDB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Välj grundb" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 50 BY .96 NO-UNDO.

DEFINE VARIABLE EDT-LOG AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 28.75 BY 10.54 NO-UNDO.

DEFINE VARIABLE FILL-FRAN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOG AS CHARACTER FORMAT "X(256)":U INITIAL "01:12:12 (Tot. 03:12:54)" 
     LABEL "Log" 
      VIEW-AS TEXT 
     SIZE 23.75 BY .71 NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tabellnamn" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-TILL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 99 BY 27.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 3.25.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.88 BY 3.29.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 31.5 BY 12.25.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 6.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TAB FOR 
      filetemp SCROLLING.

DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.

DEFINE QUERY BRW_VTAB FOR 
      valfiletemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TAB C-Win _STRUCTURED
  QUERY BRW_TAB NO-LOCK DISPLAY
      filetemp.FILENAME COLUMN-LABEL "Tabellnamn" FORMAT "x(25)":U
  ENABLE
      filetemp.FILENAME
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 27.88 BY 12
         TITLE "Tabeller".

DEFINE BROWSE BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED
  QUERY BRW_VDB NO-LOCK DISPLAY
      valdbtemp.FORETAG FORMAT "X(5)":U
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 36.13 BY 9.79
         TITLE "Databaser" EXPANDABLE TOOLTIP "Välj databas".

DEFINE BROWSE BRW_VTAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VTAB C-Win _STRUCTURED
  QUERY BRW_VTAB NO-LOCK DISPLAY
      valfiletemp.FILENAME COLUMN-LABEL "Tabellnamn" FORMAT "x(25)":U
  ENABLE
      valfiletemp.FILENAME
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 28 BY 12
         TITLE "Valda tabeller".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_VALDB AT ROW 1.75 COL 22 COLON-ALIGNED
     BRW_VDB AT ROW 3.58 COL 2.5
     BTN_FRAN AT ROW 10.04 COL 41
     FILL-FRAN AT ROW 10.29 COL 53.25 COLON-ALIGNED NO-LABEL
     BTN_TILL AT ROW 11.88 COL 41
     FILL-TILL AT ROW 12.13 COL 53.25 COLON-ALIGNED NO-LABEL
     BRW_TAB AT ROW 13.96 COL 2.5
     BRW_VTAB AT ROW 13.96 COL 38.75
     EDT-LOG AT ROW 15.25 COL 68.75 NO-LABEL
     BTN_ALLOVER AT ROW 16.96 COL 31.88
     BTN_OVER AT ROW 19.13 COL 31.88
     BTN_BACK AT ROW 21.33 COL 31.88
     BTN_ALLBACK AT ROW 23.54 COL 31.88
     BTN_OK AT ROW 27.75 COL 51
     BTN_STOP AT ROW 27.75 COL 66.75
     BTN_AVB AT ROW 27.75 COL 82.5
     FILL-IN-TAB AT ROW 28.25 COL 15 COLON-ALIGNED
     FILL-IN-LOG AT ROW 14.33 COL 71.75 COLON-ALIGNED
     "2. Pila över de tabeller som ska kopieras" VIEW-AS TEXT
          SIZE 49.38 BY .63 AT ROW 6 COL 42.5
     "1. Välj databaser, (Från och Till)" VIEW-AS TEXT
          SIZE 49.38 BY .63 AT ROW 5.17 COL 42.5
     "Info:" VIEW-AS TEXT
          SIZE 6.88 BY 1 AT ROW 3.96 COL 42.13
          FONT 17
     "4. Välj Kopiera och se händelseförloppet i Log:" VIEW-AS TEXT
          SIZE 49.38 BY .63 AT ROW 7.67 COL 42.5
     "3. Tips! Använd  Sök: T*, *tab" VIEW-AS TEXT
          SIZE 49.38 BY .63 TOOLTIP "?" AT ROW 6.83 COL 42.5
     "5. Avbryt kopiering genom att trycka på Stoppa-knappen" VIEW-AS TEXT
          SIZE 55.5 BY .63 AT ROW 8.5 COL 42.5
     "Funktioner:" VIEW-AS TEXT
          SIZE 11.5 BY .63 AT ROW 26.83 COL 40.13
     "Sök:" VIEW-AS TEXT
          SIZE 5.38 BY .63 AT ROW 26.79 COL 3.63
     RECT-1 AT ROW 3 COL 1
     RECT-2 AT ROW 26.5 COL 39
     RECT-4 AT ROW 26.46 COL 2.5
     RECT-5 AT ROW 14 COL 67.5
     RECT-6 AT ROW 3.63 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104 BY 29.21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: filetemp T "?" NO-UNDO temp-db filetemp
      TABLE: valdbtemp T "?" NO-UNDO temp-db valdbtemp
      TABLE: valfiletemp T "?" NO-UNDO temp-db valfiletemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopiera tabell"
         HEIGHT             = 29.21
         WIDTH              = 104
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_VDB CMB_VALDB DEFAULT-FRAME */
/* BROWSE-TAB BRW_TAB FILL-TILL DEFAULT-FRAME */
/* BROWSE-TAB BRW_VTAB BRW_TAB DEFAULT-FRAME */
/* SETTINGS FOR BUTTON BTN_STOP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDT-LOG:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-FRAN:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-LOG:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-TILL:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB
/* Query rebuild information for BROWSE BRW_TAB
     _TblList          = "Temp-Tables.filetemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.filetemp.FILENAME
"filetemp.FILENAME" "Tabellnamn" "x(25)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VDB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VTAB
/* Query rebuild information for BROWSE BRW_VTAB
     _TblList          = "Temp-Tables.valfiletemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.valfiletemp.FILENAME
"valfiletemp.FILENAME" "Tabellnamn" "x(25)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VTAB */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 4.75
       COLUMN          = 93.5
       HEIGHT          = 1.5
       WIDTH           = 5
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(BRW_VDB:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kopiera tabell */
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
ON WINDOW-CLOSE OF C-Win /* Kopiera tabell */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VDB
&Scoped-define SELF-NAME BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
/*    MESSAGE valdbtemp.ORDNING valdbtemp.FORETAG valdbtemp.GFORETAG. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRAN C-Win
ON CHOOSE OF BTN_FRAN IN FRAME DEFAULT-FRAME /* Från */
DO:
   BRW_VDB:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE valdbtemp THEN DO:
      ASSIGN
      francon = valdbtemp.DBCON
      frandb = valdbtemp.DBNAMN
      conappvar = valdbtemp.APPCON
      gforetag = valdbtemp.GFORETAG
      franforetag = valdbtemp.FORETAG
      FILL-FRAN:SCREEN-VALUE = valdbtemp.VALDB.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
   RUN btnok_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_STOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_STOP C-Win
ON CHOOSE OF BTN_STOP IN FRAME DEFAULT-FRAME /* Stoppa */
DO:
   IF stoppa = FALSE THEN DO:
      MESSAGE "Är du säker på att du vill stoppa kopieringen?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
      IF svar = TRUE THEN stoppa = TRUE.      
      EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Håller på att avsluta kopiering!" + CHR(10).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TILL C-Win
ON CHOOSE OF BTN_TILL IN FRAME DEFAULT-FRAME /* Till */
DO:
   BRW_VDB:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE valdbtemp THEN DO:
      ASSIGN
      tillcon = valdbtemp.DBCON
      tilldb = valdbtemp.DBNAMN
      tillforetag = valdbtemp.FORETAG
      FILL-TILL:SCREEN-VALUE = valdbtemp.VALDB.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VALDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALDB C-Win
ON LEAVE OF CMB_VALDB IN FRAME DEFAULT-FRAME /* Välj grundb */
DO:
   DEFINE VARIABLE convar AS CHARACTER NO-UNDO.
   {muswait.i}
   FIND FIRST valdbtemp WHERE valdbtemp.VALDB = CMB_VALDB NO-ERROR.
   IF NOT AVAILABLE valdbtemp THEN DO:
   END.
   ELSE DO:
      EMPTY TEMP-TABLE filetemp NO-ERROR. 
      IF LDBNAME(1) NE valdbtemp.DBNAMN THEN DO:
         IF LDBNAME(1) NE ? THEN DISCONNECT VALUE(LDBNAME(1)) NO-ERROR.         
      END.
      IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO :
         convar = valdbtemp.DBCON + " -P " + QUOTER({setpwd.I}) +  " -U " + QUOTER({setuser.I}).
         CONNECT VALUE(convar) NO-ERROR.
      END.
      orgdb = valdbtemp.DBNAMN.
      RELEASE valdbtemp.
      tempkommando = orgdb + "._FILE".
      CREATE BUFFER orgbufh FOR TABLE tempkommando.
      CREATE QUERY orgqh.
      tempkommando = "FOR EACH " + orgdb + "._FILE WHERE " + orgdb + "._FILE._FROZEN = FALSE " + " NO-LOCK  BY _File-Name". 
      orgqh:SET-BUFFERS(orgbufh).
      orgqh:QUERY-PREPARE(tempkommando).
      orgqh:QUERY-OPEN.
      orgqh:GET-FIRST(NO-LOCK).
      DO WHILE orgqh:QUERY-OFF-END = FALSE:
         orgfieldh = orgbufh:BUFFER-FIELD("_File-Name").
         IF SUBSTRING(orgfieldh:BUFFER-VALUE,1,1) NE "_" THEN DO:
            CREATE filetemp.
            filetemp.FILENAME = orgfieldh:BUFFER-VALUE.
         END.
         orgqh:GET-NEXT(NO-LOCK).
      END.
      orgqh:QUERY-CLOSE.
      DELETE OBJECT orgqh.
      DELETE OBJECT orgbufh.   
      DISCONNECT VALUE(orgdb) NO-ERROR.
      /*
      DELETE ALIAS VALUE(orgdb).
      */
      CREATE SERVER Guru.Konstanter:apphand.
      
      RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALDB C-Win
ON VALUE-CHANGED OF CMB_VALDB IN FRAME DEFAULT-FRAME /* Välj grundb */
DO:
   CMB_VALDB = INPUT CMB_VALDB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
   IF klar = FALSE THEN DO:
      FILL-IN-LOG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(counter2, "HH:MM:SS") 
      + " (Tot. " + STRING(counter, "HH:MM:SS") + ")".
   END.
   counter = counter + 1.
   counter2 = counter2 + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TAB
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF klar = FALSE THEN RETURN NO-APPLY.
   IF VALID-HANDLE(koptabproch) THEN DELETE PROCEDURE koptabproch.
   IF VALID-HANDLE(Guru.Konstanter:apphand) THEN DO:
      IF Guru.Konstanter:apphand:CONNECTED() = TRUE THEN Guru.Konstanter:apphand:DISCONNECT().
      DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
   END.   
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
    
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {VALDBALL.I}  
   {ALLSTARTDYN.I}
   FOR EACH valdbtemp:
      status-ok = CMB_VALDB:ADD-LAST(valdbtemp.VALDB).
   END.
   RUN enable_UI.
   ASSIGN
   klar = TRUE
   FILL-IN-LOG:SCREEN-VALUE = "".
   stoppa = FALSE.
   
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
   filetemp.FILENAME:READ-ONLY IN BROWSE BRW_TAB = TRUE
   valfiletemp.FILENAME:READ-ONLY IN BROWSE BRW_VTAB = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}]
      (INPUT BRW_TAB:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}]
      (INPUT BRW_VTAB:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_TAB:HANDLE, INPUT BRW_VTAB:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).    
   RUN addfillin_UI IN brwproc[{&ARROWS}] 
      (INPUT FILL-IN-TAB:HANDLE IN FRAME {&FRAME-NAME}, INPUT "FILENAME").    
   RUN settitlenum_UI IN brwproc[{&ARROWS}] (INPUT TRUE).      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN 
   FILL-FRAN = INPUT FRAME {&FRAME-NAME} FILL-FRAN
   FILL-TILL = INPUT FRAME {&FRAME-NAME} FILL-TILL.
   FIND FIRST valfiletemp NO-LOCK NO-ERROR.
   IF FILL-FRAN = "" OR FILL-TILL = "" THEN DO:
      MESSAGE "Du måste välja någon databas!" VIEW-AS ALERT-BOX.
   END.
   ELSE IF NOT AVAILABLE valfiletemp THEN DO:
      MESSAGE "Du måste välja någon tabell!" VIEW-AS ALERT-BOX.
   END.
   ELSE IF tillforetag = tillforetag THEN DO:
      MESSAGE "Vill du kopiera data från databas " + FILL-FRAN +
         " till databas " + FILL-TILL + "?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
      IF svar = TRUE THEN DO:
         MESSAGE "Vill du radera data från databas " + FILL-TILL + "?" 
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar2 AS LOGICAL.
         MESSAGE "Vill du köra app:at ?" VIEW-AS ALERT-BOX QUESTION BUTTONS 
         YES-NO UPDATE val1 AS LOGICAL.
         CASE val1:
            WHEN TRUE THEN DO:
               RUN conappserver_UI.                                          
            END.
            WHEN FALSE THEN DO:
               
            END.
         END.   
         
         IF Guru.Konstanter:appcon = TRUE THEN DO:                                     
            RUN KOPTABDB.P PERSISTENT SET koptabproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT       
            (INPUT frandb, INPUT tilldb, INPUT TABLE valfiletemp, INPUT tillcon, INPUT francon,
             INPUT svar2, OUTPUT felmed, OUTPUT fellog).                              
         END.
         ELSE DO:
            RUN KOPTABDB.P PERSISTENT SET koptabproch        
            (INPUT frandb, INPUT tilldb, INPUT TABLE valfiletemp, INPUT tillcon, INPUT francon,
             INPUT svar2, OUTPUT felmed, OUTPUT fellog).                              
         END.
         EDT-LOG:SCREEN-VALUE = felmed.
         IF fellog = FALSE THEN DO:
            EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + CHR(10) + "Kopierar tabeller!" + CHR(10) + CHR(10).
            EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Kop-tid: Tabellnamn:" + CHR(10).
            klar = FALSE.
            RUN visagom_UI.
            counter = 0.
            RUN kopieratab_UI.
         END.         
      END.
   END.
   ELSE DO:
      MESSAGE "Båda databaserna måste ligga på samma server.".
   END. 
   DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.
   Guru.Konstanter:apphand = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE conappserver_UI C-Win 
PROCEDURE conappserver_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}          
   IF Guru.Konstanter:apphand:CONNECTED() = FALSE THEN DO:
      Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
      IF Guru.Konstanter:appcon = FALSE THEN DO:
         MESSAGE "Det går ej att ansluta appserver." SKIP "Kontakta Mikael Eriksson!" VIEW-AS ALERT-BOX.      
      END.   
   END.
   ELSE DO:                    
      IF VALID-HANDLE(koptabproch) THEN DELETE PROCEDURE koptabproch.
      IF ENTRY(3,Guru.Konstanter:apphand:NAME,":") NE ENTRY(2,conappvar," ") THEN DO:
         Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
         IF Guru.Konstanter:appcon = FALSE THEN DO:
            MESSAGE "Kan ej koppla från appserver" Guru.Konstanter:apphand:NAME.
         END.
         ELSE DO:
            Guru.Konstanter:appcon = Guru.Konstanter:apphand:CONNECT(conappvar,{APPCON1.i},{APPCON2.i},gforetag) NO-ERROR.
            IF Guru.Konstanter:appcon = FALSE THEN DO:
               MESSAGE "Det går ej att ansluta appserver." SKIP 
               "Kontakta Mikael Eriksson!" VIEW-AS ALERT-BOX.      
            END.   
         END.
      END.
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.
&ENDIF

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
  RUN control_load.
  DISPLAY CMB_VALDB FILL-FRAN FILL-TILL EDT-LOG FILL-IN-TAB FILL-IN-LOG 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-4 RECT-5 RECT-6 CMB_VALDB BRW_VDB BTN_FRAN 
         FILL-FRAN BTN_TILL FILL-TILL BRW_TAB BRW_VTAB EDT-LOG BTN_ALLOVER 
         BTN_OVER BTN_BACK BTN_ALLBACK BTN_OK BTN_AVB FILL-IN-TAB FILL-IN-LOG 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kopieratab_UI C-Win 
PROCEDURE kopieratab_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   counter2 = 0.
   IF Guru.Konstanter:appcon = TRUE THEN RUN koptab_UI IN koptabproch ASYNCHRONOUS SET asynch EVENT-PROCEDURE "tabevent_UI" IN THIS-PROCEDURE (OUTPUT felmed).   
   ELSE RUN koptabe_UI IN koptabproch ASYNCHRONOUS SET asynch EVENT-PROCEDURE "tabevent_UI" IN THIS-PROCEDURE (OUTPUT felmed).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabevent_UI C-Win 
PROCEDURE tabevent_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER rettext AS CHARACTER NO-UNDO.
   IF rettext NE "" THEN DO:
      EDT-LOG:SCREEN-VALUE IN FRAME {&FRAME-NAME} = EDT-LOG:SCREEN-VALUE + STRING(counter2, "HH:MM:SS") + " " + rettext + CHR(10).
      IF stoppa = FALSE THEN RUN kopieratab_UI.
      ELSE DO: 
         ASSIGN
         EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Kopiering avbruten!" + CHR(10)
         stoppa = FALSE
         klar = TRUE.
         RUN visagom_UI.
      END.
   END.
   ELSE DO:
      klar = TRUE.
      RUN visagom_UI.
      EDT-LOG:SCREEN-VALUE = EDT-LOG:SCREEN-VALUE + "Klar!" + CHR(10).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visagom_UI C-Win 
PROCEDURE visagom_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF klar = TRUE THEN DO:
      ENABLE BTN_OK BTN_AVB WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_STOP WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ENABLE BTN_STOP WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_OK BTN_AVB WITH FRAME {&FRAME-NAME}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

