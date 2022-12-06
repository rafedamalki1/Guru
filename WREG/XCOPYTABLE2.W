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
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.

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
   FIELD FILEREC     AS RECID. 
/* DEFINE TEMP-TABLE valfiletemp NO-UNDO LIKE filetemp. */

DEFINE TEMP-TABLE fieldtemp NO-UNDO
   FIELD FIELDNAME   AS CHARACTER   FORMAT "x(32)"
   FIELD FILEREC     AS RECID.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_FALT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fieldtemp filetemp valdbtemp

/* Definitions for BROWSE BRW_FALT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FALT fieldtemp.FIELDNAME 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FALT 
&Scoped-define QUERY-STRING-BRW_FALT FOR EACH fieldtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FALT OPEN QUERY BRW_FALT FOR EACH fieldtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FALT fieldtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FALT fieldtemp


/* Definitions for BROWSE BRW_TAB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TAB filetemp.FILENAME 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TAB 
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


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_VDB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VDB EDT-LOG BTN_FRAN FILL-FRAN BTN_TILL ~
FILL-TILL BRW_TAB BRW_FALT BTN_FORST FILL-IN_FORSTA BTN_ANDRA FILL-IN_ANDRA ~
BTN_OK BTN_AVB FILL-IN-LOG RECT-1 RECT-2 RECT-5 
&Scoped-Define DISPLAYED-OBJECTS EDT-LOG FILL-FRAN FILL-TILL FILL-IN_FORSTA ~
FILL-IN_ANDRA FILL-IN-LOG 

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
DEFINE BUTTON BTN_ANDRA 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_FORST 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_FRAN 
     LABEL "Från" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_OK 
     LABEL "Kopiera" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_STOP 
     LABEL "Stoppa" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_TILL 
     LABEL "Till" 
     SIZE 12 BY 1.5.

DEFINE VARIABLE EDT-LOG AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 32.75 BY 10.54 NO-UNDO.

DEFINE VARIABLE FILL-FRAN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOG AS CHARACTER FORMAT "X(256)":U INITIAL "01:12:12 (Tot. 03:12:54)" 
     LABEL "Log" 
      VIEW-AS TEXT 
     SIZE 23.75 BY .71 NO-UNDO.

DEFINE VARIABLE FILL-IN_ANDRA AS CHARACTER FORMAT "X(256)":U INITIAL "?" 
     LABEL "Andra Fält" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORSTA AS CHARACTER FORMAT "X(256)":U INITIAL "?" 
     LABEL "Första Fält" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-TILL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38.25 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 97.5 BY 23.5.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 3.25.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 35.5 BY 12.25.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FALT FOR 
      fieldtemp SCROLLING.

DEFINE QUERY BRW_TAB FOR 
      filetemp SCROLLING.

DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FALT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FALT C-Win _STRUCTURED
  QUERY BRW_FALT NO-LOCK DISPLAY
      fieldtemp.FIELDNAME COLUMN-LABEL "Fältnamn" FORMAT "X(16)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 21.5 BY 8.96
         TITLE "Fält".

DEFINE BROWSE BRW_TAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TAB C-Win _STRUCTURED
  QUERY BRW_TAB NO-LOCK DISPLAY
      filetemp.FILENAME COLUMN-LABEL "Tabellnamn" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 27.88 BY 9
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


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_VDB AT ROW 1.58 COL 2.5
     EDT-LOG AT ROW 3.5 COL 59.75 NO-LABEL
     BTN_FRAN AT ROW 11.58 COL 2.5
     FILL-FRAN AT ROW 11.83 COL 14.88 COLON-ALIGNED NO-LABEL
     BTN_TILL AT ROW 13.42 COL 2.5
     FILL-TILL AT ROW 13.67 COL 14.88 COLON-ALIGNED NO-LABEL
     BRW_TAB AT ROW 15.17 COL 2.5
     BRW_FALT AT ROW 15.21 COL 32
     BTN_FORST AT ROW 15.75 COL 56.25
     FILL-IN_FORSTA AT ROW 16.25 COL 75.5 COLON-ALIGNED NO-TAB-STOP 
     BTN_ANDRA AT ROW 17.75 COL 56.25
     FILL-IN_ANDRA AT ROW 18 COL 75.5 COLON-ALIGNED
     BTN_OK AT ROW 21.75 COL 56.13
     BTN_STOP AT ROW 21.75 COL 70.13
     BTN_AVB AT ROW 21.75 COL 84
     FILL-IN-LOG AT ROW 2.58 COL 62.75 COLON-ALIGNED
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 20.5 COL 54
     RECT-5 AT ROW 2.25 COL 58.5
     "Funktioner:" VIEW-AS TEXT
          SIZE 11.5 BY .63 AT ROW 20.83 COL 55.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.38 BY 23.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: fieldtemp T "?" NO-UNDO temp-db fieldtemp
      TABLE: filetemp T "?" NO-UNDO temp-db filetemp
      TABLE: valdbtemp T "?" NO-UNDO temp-db valdbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopiera tabell"
         HEIGHT             = 23.67
         WIDTH              = 98.13
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
/* BROWSE-TAB BRW_VDB 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_TAB FILL-TILL DEFAULT-FRAME */
/* BROWSE-TAB BRW_FALT BRW_TAB DEFAULT-FRAME */
ASSIGN 
       BRW_FALT:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_TAB:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BUTTON BTN_STOP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       EDT-LOG:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-FRAN:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-LOG:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_ANDRA:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_FORSTA:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-TILL:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FALT
/* Query rebuild information for BROWSE BRW_FALT
     _TblList          = "Temp-Tables.fieldtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.fieldtemp.FIELDNAME
"fieldtemp.FIELDNAME" "Fältnamn" "X(16)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FALT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TAB
/* Query rebuild information for BROWSE BRW_TAB
     _TblList          = "Temp-Tables.filetemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.filetemp.FILENAME
"filetemp.FILENAME" "Tabellnamn" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 2.5
       COLUMN          = 43
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


&Scoped-define BROWSE-NAME BRW_FALT
&Scoped-define SELF-NAME BRW_FALT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FALT C-Win
ON VALUE-CHANGED OF BRW_FALT IN FRAME DEFAULT-FRAME /* Fält */
DO:
/*    IF AVAILABLE fieldtemp THEN DO:                                                                        */
/* /*       RUN setcolsortvar_UI IN brwproc[4] (INPUT "fieldtemp.FILEREC = " + STRING(filetemp.FILEREC)). */ */
/*       MESSAGE fieldtemp.FILEREC.                                                                          */
/*    END.                                                                                                   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TAB
&Scoped-define SELF-NAME BRW_TAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TAB C-Win
ON VALUE-CHANGED OF BRW_TAB IN FRAME DEFAULT-FRAME /* Tabeller */
DO:
   IF AVAILABLE filetemp THEN DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT "fieldtemp.FILEREC = " + STRING(filetemp.FILEREC)).
      RUN openbdynspec_UI IN brwproc[2].
      ASSIGN
      FILL-IN_ANDRA:SCREEN-VALUE = "?"
      FILL-IN_FORSTA:SCREEN-VALUE = "?".      
   END.
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


&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA C-Win
ON CHOOSE OF BTN_ANDRA IN FRAME DEFAULT-FRAME
DO:
  /* andra knapp */
   BRW_FALT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE fieldtemp THEN DO:
      IF FILL-IN_FORSTA:SCREEN-VALUE = fieldtemp.FIELDNAME THEN DO:
         MESSAGE "Första och andra kan ej ha samma värde!" VIEW-AS ALERT-BOX TITLE "Meddelande!".
      END.
      ELSE DO: 
         FILL-IN_ANDRA:SCREEN-VALUE = fieldtemp.FIELDNAME.                  
      END.
   END.           
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


&Scoped-define SELF-NAME BTN_FORST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FORST C-Win
ON CHOOSE OF BTN_FORST IN FRAME DEFAULT-FRAME
DO:
  /* första knapp */
   BRW_FALT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE fieldtemp THEN DO:
      IF FILL-IN_ANDRA:SCREEN-VALUE = fieldtemp.FIELDNAME THEN DO:
         MESSAGE "Första och andra kan ej ha samma värde!" VIEW-AS ALERT-BOX TITLE "Meddelande!".
      END.
      ELSE DO: 
         FILL-IN_FORSTA:SCREEN-VALUE = fieldtemp.FIELDNAME.         
      END.
   END.
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


&Scoped-define BROWSE-NAME BRW_FALT
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
   RUN enable_UI.
   RUN start_UI.
/*    ASSIGN                                                                */
/*    orgdb = "rt9"                                                         */
/*    klar = TRUE                                                           */
/*    FILL-IN-LOG:SCREEN-VALUE = "".                                        */
/*    stoppa = FALSE.                                                       */
/*    IF NOT CONNECTED(orgdb) THEN DO:                                      */
/*       FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = orgdb NO-ERROR.      */
/*       CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.                           */
/*    END.                                                                  */
/*    status-ok = BRW_VDB:DESELECT-FOCUSED-ROW().                           */
/*    RELEASE valdbtemp.                                                    */
/*    tempkommando = orgdb + "._FILE".                                      */
/*    CREATE BUFFER orgbufh FOR TABLE tempkommando.                         */
/*    CREATE QUERY orgqh.                                                   */
/*    tempkommando = "FOR EACH " + orgdb + "._FILE NO-LOCK BY _File-Name.". */
/*    orgqh:SET-BUFFERS(orgbufh).                                           */
/*    orgqh:QUERY-PREPARE(tempkommando).                                    */
/*    orgqh:QUERY-OPEN.                                                     */
/*    orgqh:GET-FIRST(NO-LOCK).                                             */
/*    DO WHILE orgqh:QUERY-OFF-END = FALSE:                                 */
/*       orgfieldh = orgbufh:BUFFER-FIELD("_File-Name").                    */
/*       IF SUBSTRING(orgfieldh:BUFFER-VALUE,1,1) NE "_" THEN DO:           */
/*          CREATE filetemp.                                                */
/*          filetemp.FILENAME = orgfieldh:BUFFER-VALUE.                     */
/*       END.                                                               */
/*       orgqh:GET-NEXT(NO-LOCK).                                           */
/*    END.                                                                  */
/*    orgqh:QUERY-CLOSE.                                                    */
/*    DELETE OBJECT orgqh.                                                  */
/*    DELETE OBJECT orgbufh.                                                */
/* /*    DISCONNECT VALUE(orgdb) NO-ERROR. */                               */
/* /*    DELETE ALIAS VALUE(orgdb).        */                               */
/*    CREATE SERVER Guru.Konstanter:apphand.                                                */
/*    RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].                       */
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
/*    ASSIGN                                                */
/*    filetemp.FILENAME:READ-ONLY IN BROWSE BRW_TAB = TRUE. */
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TAB:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_FALT:HANDLE IN FRAME {&FRAME-NAME}).  
/*    RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] */
/*       (INPUT BRW_VTAB:HANDLE IN FRAME {&FRAME-NAME}).   */
/*    RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}]      */
/*       (INPUT BRW_TAB:HANDLE, INPUT BRW_VTAB:HANDLE ,     */
/*        INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE , */
/*        INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE). */
/*    RUN addfillin_UI IN brwproc[{&ARROWS}]                                  */
/*       (INPUT FILL-IN-TAB:HANDLE IN FRAME {&FRAME-NAME}, INPUT "FILENAME"). */
/*    RUN settitlenum_UI IN brwproc[{&ARROWS}] (INPUT TRUE).                  */
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
   FILL-TILL = INPUT FRAME {&FRAME-NAME} FILL-TILL
   FILL-IN_FORSTA = INPUT FRAME {&FRAME-NAME} FILL-IN_FORSTA
   FILL-IN_ANDRA = INPUT FRAME {&FRAME-NAME} FILL-IN_ANDRA.
/*    FIND FIRST valfiletemp NO-LOCK NO-ERROR. */
   IF FILL-IN_FORSTA = "?" THEN DO:
      MESSAGE "Första fältet får inte vara '?'" VIEW-AS ALERT-BOX TITLE "Meddelande!".
   END.
   ELSE IF FILL-FRAN = "" OR FILL-TILL = "" THEN DO:
      MESSAGE "Du måste välja någon databas!" VIEW-AS ALERT-BOX.
   END.
   ELSE IF tillforetag = franforetag THEN DO:
      MESSAGE "Vill du kopiera data från databas " + FILL-FRAN +
         " till databas " + FILL-TILL + "?" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.
      IF svar = TRUE THEN DO:
         MESSAGE "Vill du radera data från databas " + FILL-TILL + "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svar2 AS LOGICAL.
         RUN conappserver_UI.
         IF Guru.Konstanter:appcon = TRUE THEN DO:
            RUN KOPTABDB2.P PERSISTENT SET koptabproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT FILL-IN_FORSTA, INPUT FILL-IN_ANDRA, INPUT frandb, INPUT tilldb, 
             INPUT filetemp.FILENAME, INPUT tillcon, INPUT svar2, OUTPUT felmed, OUTPUT fellog).
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
   END.
   ELSE DO:
      MESSAGE "Båda databaserna måste ligga på samma server.".
   END.
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
  DISPLAY EDT-LOG FILL-FRAN FILL-TILL FILL-IN_FORSTA FILL-IN_ANDRA FILL-IN-LOG 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_VDB EDT-LOG BTN_FRAN FILL-FRAN BTN_TILL FILL-TILL BRW_TAB BRW_FALT 
         BTN_FORST FILL-IN_FORSTA BTN_ANDRA FILL-IN_ANDRA BTN_OK BTN_AVB 
         FILL-IN-LOG RECT-1 RECT-2 RECT-5 
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
   RUN koptab_UI IN koptabproch ASYNCHRONOUS SET asynch EVENT-PROCEDURE "tabevent_UI" IN THIS-PROCEDURE (OUTPUT felmed).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE start_UI C-Win 
PROCEDURE start_UI :
/*------------------------------------------------------------------------------
  Purpose:     btnok
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   orgdb = "rt9"    
   klar = TRUE
   stoppa = FALSE.
   IF NOT CONNECTED(orgdb) THEN DO:
      FIND FIRST valdbtemp WHERE valdbtemp.DBNAMN = orgdb NO-ERROR.
      CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.
   END.
   BRW_VDB:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
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
         ASSIGN
         filetemp.FILENAME = orgfieldh:BUFFER-VALUE
         filetemp.FILEREC = orgbufh:RECID.
      END.      
      orgqh:GET-NEXT(NO-LOCK).
   END.
   orgqh:QUERY-CLOSE.
   DELETE OBJECT orgqh.
   DELETE OBJECT orgbufh.   
   tempkommando = orgdb + "._FIELD".
   CREATE BUFFER orgbufh FOR TABLE tempkommando.
   CREATE QUERY orgqh.
   tempkommando = "FOR EACH " + orgdb + "._FIELD NO-LOCK BY _File-recid.". 
   orgqh:SET-BUFFERS(orgbufh).
   orgqh:QUERY-PREPARE(tempkommando).
   orgqh:QUERY-OPEN.
   orgqh:GET-FIRST(NO-LOCK).
   DO WHILE orgqh:QUERY-OFF-END = FALSE:
      CREATE fieldtemp.
      orgfieldh = orgbufh:BUFFER-FIELD("_Field-Name").
      fieldtemp.FIELDNAME = orgfieldh:BUFFER-VALUE.   
      orgfieldh = orgbufh:BUFFER-FIELD("_File-recid").
      fieldtemp.FILEREC = orgfieldh:BUFFER-VALUE.
      orgqh:GET-NEXT(NO-LOCK).
   END.
   orgqh:QUERY-CLOSE.
   DELETE OBJECT orgqh.
   DELETE OBJECT orgbufh.   
   
/*    DISCONNECT VALUE(orgdb) NO-ERROR. */
/*    DELETE ALIAS VALUE(orgdb).        */
   CREATE SERVER Guru.Konstanter:apphand.
/*    RUN openbdynspec_UI IN brwproc[1]. */
   RUN openbdynspec_UI IN brwproc[1].
   APPLY "VALUE-CHANGED" TO BRW_TAB IN FRAME {&FRAME-NAME}.
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

