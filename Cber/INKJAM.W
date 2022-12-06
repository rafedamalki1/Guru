&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}     
&Scoped-define SHARED
{JAMTEMP.I}   
{MTRLJAM.I}  
DEFINE INPUT PARAMETER lev1 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lev2 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER lev3 AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER optsatt AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER vejt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER vhu AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR summtrljam.
DEFINE INPUT PARAMETER TABLE FOR saknastmp.
DEFINE VARIABLE excllbred AS INTEGER NO-UNDO.
DEFINE VARIABLE ordvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE jamexh AS HANDLE NO-UNDO.
DEFINE VARIABLE tpris1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tpris2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tpris3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE ejsum AS LOGICAL NO-UNDO.
DEFINE VARIABLE brwfrgnr AS INTEGER NO-UNDO.
{TIDUTTTNEW.I}
DEFINE VARIABLE vald_kundlev AS CHARACTER NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER NO-UNDO.
DEFINE VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE bytlev3apph AS HANDLE NO-UNDO.

{JMFLEVTEP.I}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_JAMF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jamtemp

/* Definitions for BROWSE BRW_JAMF                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_JAMF jamtemp.BENAMNING jamtemp.ANTAL ~
jamtemp.ENR1 jamtemp.ENHET1 jamtemp.LEV1 jamtemp.TOTPRIS jamtemp.ENR2 ~
jamtemp.ENHET2 jamtemp.LEV2 jamtemp.TOTPRIS2 jamtemp.TP1TP2 jamtemp.ENR3 ~
jamtemp.ENHET3 jamtemp.LEV3 jamtemp.TOTPRIS3 jamtemp.TP1TP3 jamtemp.UPPLAG ~
jamtemp.FRI2 jamtemp.FRI3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_JAMF 
&Scoped-define QUERY-STRING-BRW_JAMF FOR EACH jamtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_JAMF OPEN QUERY BRW_JAMF FOR EACH jamtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_JAMF jamtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_JAMF jamtemp


/* Definitions for FRAME FRAME-VINST                                    */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_JAMF BTN_SKRIV BTN_EXCEL BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LEV1 FILL-IN-LEV2 FILL-IN-LEV3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-LEV1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE FILL-IN-LEV2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN-LEV3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     FGCOLOR 4  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_JAMF FOR 
      jamtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_JAMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_JAMF WINDOW-2 _STRUCTURED
  QUERY BRW_JAMF NO-LOCK DISPLAY
      jamtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 15
      jamtemp.ANTAL COLUMN-LABEL "Antal" FORMAT "X(256)":U WIDTH 7
      jamtemp.ENR1 COLUMN-LABEL "Enr 1" FORMAT "X(256)":U WIDTH 20
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      jamtemp.ENHET1 COLUMN-LABEL "Enh 1" FORMAT "x(5)":U COLUMN-FGCOLOR 1
            LABEL-FGCOLOR 1
      jamtemp.LEV1 COLUMN-LABEL "Pris 1" FORMAT ">>>>>>9.99":U
            WIDTH 8 COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      jamtemp.TOTPRIS COLUMN-LABEL "Totpris 1" FORMAT ">>>>>>9.99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      jamtemp.ENR2 COLUMN-LABEL "Enr 2" FORMAT "X(256)":U WIDTH 20
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      jamtemp.ENHET2 COLUMN-LABEL "Enh 2" FORMAT "x(5)":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      jamtemp.LEV2 COLUMN-LABEL "Pris 2" FORMAT ">>>>>>9.99":U
            WIDTH 8 COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      jamtemp.TOTPRIS2 COLUMN-LABEL "Totpris 2" FORMAT ">>>>>>9.99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      jamtemp.TP1TP2 COLUMN-LABEL "Tp1-Tp2" FORMAT "->>>>9.99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      jamtemp.ENR3 FORMAT "x(256)":U WIDTH 20 COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      jamtemp.ENHET3 COLUMN-LABEL "Enh 3" FORMAT "x(5)":U COLUMN-FGCOLOR 4
            LABEL-FGCOLOR 4
      jamtemp.LEV3 COLUMN-LABEL "Pris 3" FORMAT ">>>>>>9.99":U
            WIDTH 8 COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      jamtemp.TOTPRIS3 COLUMN-LABEL "Totpris 3" FORMAT ">>>>>>9.99":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      jamtemp.TP1TP3 COLUMN-LABEL "Tp1-Tp3" FORMAT "->>>>9.99":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      jamtemp.UPPLAG COLUMN-LABEL "Upplag" FORMAT "X(256)":U WIDTH 5
      jamtemp.FRI2 COLUMN-LABEL "Id" FORMAT "X(256)":U WIDTH 6
      jamtemp.FRI3 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 10
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 123.5 BY 25.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     FILL-IN-LEV1 AT ROW 1.08 COL 37.25 COLON-ALIGNED NO-LABEL
     FILL-IN-LEV2 AT ROW 1.08 COL 62.63 COLON-ALIGNED NO-LABEL
     FILL-IN-LEV3 AT ROW 1.08 COL 91.5 COLON-ALIGNED NO-LABEL
     BRW_JAMF AT ROW 2.25 COL 1.5
     BTN_SKRIV AT ROW 28 COL 77
     BTN_EXCEL AT ROW 28 COL 92
     BTN_AVS AT ROW 28 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: jamtemp T "?" NO-UNDO temp-db jamtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Lista materiel/artikel"
         HEIGHT             = 28.46
         WIDTH              = 125
         MAX-HEIGHT         = 28.46
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.46
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-VINST
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_JAMF FILL-IN-LEV3 FRAME-VINST */
/* SETTINGS FOR FILL-IN FILL-IN-LEV1 IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LEV2 IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-LEV3 IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_JAMF
/* Query rebuild information for BROWSE BRW_JAMF
     _TblList          = "Temp-Tables.jamtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.jamtemp.BENAMNING
"BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.jamtemp.ANTAL
"ANTAL" "Antal" "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.jamtemp.ENR1
"ENR1" "Enr 1" "X(256)" "character" ? 1 ? ? 1 ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.jamtemp.ENHET1
"ENHET1" "Enh 1" "x(5)" "character" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.jamtemp.LEV1
"LEV1" "Pris 1" ">>>>>>9.99" "character" ? 1 ? ? 1 ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.jamtemp.TOTPRIS
"TOTPRIS" "Totpris 1" ">>>>>>9.99" "character" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.jamtemp.ENR2
"ENR2" "Enr 2" "X(256)" "character" ? 2 ? ? 2 ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.jamtemp.ENHET2
"ENHET2" "Enh 2" "x(5)" "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.jamtemp.LEV2
"LEV2" "Pris 2" ">>>>>>9.99" "character" ? 2 ? ? 2 ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.jamtemp.TOTPRIS2
"TOTPRIS2" "Totpris 2" ">>>>>>9.99" "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.jamtemp.TP1TP2
"TP1TP2" "Tp1-Tp2" ? "decimal" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.jamtemp.ENR3
"ENR3" ? "x(256)" "character" ? 4 ? ? 4 ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.jamtemp.ENHET3
"ENHET3" "Enh 3" "x(5)" "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.jamtemp.LEV3
"LEV3" "Pris 3" ">>>>>>9.99" "character" ? 4 ? ? 4 ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.jamtemp.TOTPRIS3
"TOTPRIS3" "Totpris 3" ">>>>>>9.99" "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.jamtemp.TP1TP3
"TP1TP3" "Tp1-Tp3" ? "decimal" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.jamtemp.UPPLAG
"UPPLAG" "Upplag" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > Temp-Tables.jamtemp.FRI2
"FRI2" "Id" "X(256)" "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > Temp-Tables.jamtemp.FRI3
"FRI3" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_JAMF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Visa i Excel */
DO: 
   {muswait.i}
   RUN skaptidut_UI (INPUT TRUE).
   RUN jamfarg_ui IN jamexh (INPUT jamtemp.LEV1:COLUMN-FGCOLOR  IN BROWSE BRW_JAMF, INPUT jamtemp.LEV2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF,INPUT jamtemp.LEV3:COLUMN-FGCOLOR  IN BROWSE BRW_JAMF).          
   RUN jamexcel_UI IN jamexh (INPUT excllbred,INPUT TABLE tidut,INPUT lev1,INPUT lev2,INPUT lev3,INPUT optsatt).         
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   RUN skaptidut_UI (INPUT FALSE).
   RUN SKRIVVAL.W (INPUT TRUE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
     RUN ut_UI.      
   END.
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_JAMF
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
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
   {ALLSTARTDYN.I}
   RUN skapalista_UI IN bytlev3apph (OUTPUT TABLE jmflev_temp, OUTPUT vald_kundlev).   
   IF Guru.Konstanter:varforetypval[28] = 11 OR Guru.Konstanter:varforetypval[38] = 11 THEN brwfrgnr = 11.
   ELSE brwfrgnr = 13.   
   {&WINDOW-NAME}:TITLE = "Jämför priser i inköp för valda konstruktioner".         
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE jamtemp NO-ERROR.     
   IF jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = Guru.Konstanter:varforetypval[28] OR jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = Guru.Konstanter:varforetypval[38] THEN DO:       
      jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = brwfrgnr.
      ASSIGN
      jamtemp.LEV1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TOTPRIS:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENR1:LABEL-FGCOLOR IN BROWSE BRW_JAMF =  jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.LEV1:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TOTPRIS:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENHET1:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR1:COLUMN-FGCOLOR IN BROWSE BRW_JAMF.      
   END.  
   IF jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = Guru.Konstanter:varforetypval[28] OR jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = Guru.Konstanter:varforetypval[38] THEN DO:
      jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = brwfrgnr.
      ASSIGN
      jamtemp.LEV2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TOTPRIS2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENHET2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TP1TP2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENR2:LABEL-FGCOLOR IN BROWSE BRW_JAMF =  jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.LEV2:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TOTPRIS2:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENHET2:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TP1TP2:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR2:COLUMN-FGCOLOR IN BROWSE BRW_JAMF.                 
   END. 
   IF jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = Guru.Konstanter:varforetypval[28] OR jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = Guru.Konstanter:varforetypval[38] THEN DO:
      jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = brwfrgnr.
      ASSIGN
      jamtemp.LEV3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TOTPRIS3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENHET3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TP1TP3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENR3:LABEL-FGCOLOR IN BROWSE BRW_JAMF =  jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.LEV3:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TOTPRIS3:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.ENHET3:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF
      jamtemp.TP1TP3:LABEL-FGCOLOR IN BROWSE BRW_JAMF = jamtemp.ENR3:COLUMN-FGCOLOR IN BROWSE BRW_JAMF.
   END.         
   IF optsatt = 1 OR optsatt = 3 THEN DO:   
      rrakn = 0.      
      FOR EACH summtrljam WHERE summtrljam.ANTAL > 0 NO-LOCK:
         /*one vill inte ha med Draka i prisjämförelser Mats Johansson 20171020 Lena*/
         IF Guru.Konstanter:globforetag = "GRAN" AND summtrljam.LEVKODINK = "32" THEN .
         ELSE DO:
            CREATE jamtemp.
            ASSIGN
            rrakn = rrakn + 1
            jamtemp.RADRAKN = rrakn
            jamtemp.KTYPKOD = summtrljam.KTYPKOD
            jamtemp.FRI1 = STRING(summtrljam.FRI1)
            jamtemp.FRI2 = summtrljam.FRI2
            jamtemp.NUM = STRING(summtrljam.NUM)
            jamtemp.ENR = summtrljam.ENR
            jamtemp.ENR1 = summtrljam.ENR1
            jamtemp.ENR2 = summtrljam.ENR2
            jamtemp.ENR3 = summtrljam.ENR3            
            jamtemp.LEV1 = summtrljam.PRIS
            jamtemp.LEV2 = summtrljam.PRIS2
            jamtemp.LEV3 = summtrljam.PRIS3
            jamtemp.TOTPRIS = summtrljam.TOTPRIS
            jamtemp.TOTPRIS2 = summtrljam.TOTPRIS2
            jamtemp.TOTPRIS3 = summtrljam.TOTPRIS3
            jamtemp.TP1TP2 = summtrljam.TP1TP2
            jamtemp.TP1TP3 = summtrljam.TP1TP3
            jamtemp.NETTO1 = summtrljam.NETTO1
            jamtemp.NETTO2 = summtrljam.NETTO2
            jamtemp.NETTO3 = summtrljam.NETTO3
            jamtemp.ENHET1 = summtrljam.ENHET1
            jamtemp.ENHET2 = summtrljam.ENHET2
            jamtemp.ENHET3 = summtrljam.ENHET3
            jamtemp.ENHET = summtrljam.ENHET
            jamtemp.BENAMNING = summtrljam.BENAMNING
            jamtemp.ANTAL = STRING(summtrljam.ANTAL).
         END.
      END.   
      IF optsatt = 1 THEN DO:   
         ASSIGN
         tpris1 = 0
         tpris2 = 0
         tpris3 = 0.      
         CREATE jamtemp.
         ASSIGN                         
         jamtemp.BENAMNING = "Summa"
         rrakn = rrakn + 1
         jamtemp.RADRAKN = rrakn.
         FOR EACH summtrljam :
            /*one vill inte ha med Draka i prisjämförelser Mats Johansson 20171020 Lena*/
            IF Guru.Konstanter:globforetag = "GRAN" AND summtrljam.LEVKODINK = "32" THEN .
            ELSE DO: 
               ejsum = FALSE.
               IF vhu = TRUE THEN DO:
                  IF summtrljam.ENR1 = "saknas" OR  summtrljam.ENR2 = "saknas" OR summtrljam.ENR3 = "saknas" THEN DO:
                     ejsum = TRUE.
                  END.
               END.
               IF ejsum = FALSE THEN DO:                           
                  ASSIGN
                  tpris1 = tpris1 + summtrljam.TOTPRIS
                  tpris2 = tpris2 + summtrljam.TOTPRIS2
                  tpris3 = tpris3 + summtrljam.TOTPRIS3.                  
               END.
            END.                
         END.    
         ASSIGN 
         jamtemp.TOTPRIS = tpris1
         jamtemp.TOTPRIS2 = tpris2
         jamtemp.TOTPRIS3 = tpris3.      
         IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA"  THEN DO:              
            FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev1 NO-LOCK NO-ERROR.
            IF AVAILABLE jmflev_temp THEN DO:  
               jamtemp.LEV1 = jmflev_temp.FAKTOR.
            END.
            FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev2 NO-LOCK NO-ERROR.
            IF AVAILABLE jmflev_temp THEN DO:  
               jamtemp.LEV2 = jmflev_temp.FAKTOR.
            END.
            FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev3 NO-LOCK NO-ERROR.
            IF AVAILABLE jmflev_temp THEN DO:  
               jamtemp.LEV3 = jmflev_temp.FAKTOR.
            END.    
            ASSIGN 
            jamtemp.BENAMNING = "Summa netto"
            jamtemp.TOTPRIS = tpris1 * jamtemp.LEV1.
            jamtemp.TOTPRIS2 = tpris2 * jamtemp.LEV2.
            jamtemp.TOTPRIS3 = tpris3 * jamtemp.LEV3.
         END.
      END.   
   END.
   IF optsatt = 2 THEN DO:
      ASSIGN
      rrakn = 0
      ordvar = ""
      tpris1 = 0
      tpris2 = 0
      tpris3 = 0.      
      FOR EACH summtrljam WHERE summtrljam.ANTAL > 0 USE-INDEX ORD NO-LOCK:
         /*one vill inte ha med Draka i prisjämförelser Mats Johansson 20171020 Lena*/
         IF Guru.Konstanter:globforetag = "GRAN" AND summtrljam.LEVKODINK = "32" THEN .
         ELSE DO:
            CREATE jamtemp.
            ASSIGN
            rrakn = rrakn + 1
            jamtemp.RADRAKN = rrakn
            jamtemp.KTYPKOD = summtrljam.KTYPKOD
            jamtemp.FRI1 = STRING(summtrljam.FRI1)
            jamtemp.FRI2 = summtrljam.FRI2
            jamtemp.NUM = STRING(summtrljam.NUM)
            jamtemp.ORD = STRING(summtrljam.ORD)
            jamtemp.UPPLAG = STRING(summtrljam.UPPLAG)
            jamtemp.ENR = summtrljam.ENR
            jamtemp.ENR1 = summtrljam.ENR1
            jamtemp.ENR2 = summtrljam.ENR2
            jamtemp.ENR3 = summtrljam.ENR3            
            jamtemp.LEV1 = summtrljam.PRIS
            jamtemp.LEV2 = summtrljam.PRIS2
            jamtemp.LEV3 = summtrljam.PRIS3
            jamtemp.TOTPRIS = summtrljam.TOTPRIS
            jamtemp.TOTPRIS2 = summtrljam.TOTPRIS2
            jamtemp.TOTPRIS3 = summtrljam.TOTPRIS3
            jamtemp.TP1TP2 = summtrljam.TP1TP2
            jamtemp.TP1TP3 = summtrljam.TP1TP3
            jamtemp.NETTO1 = summtrljam.NETTO1
            jamtemp.NETTO2 = summtrljam.NETTO2
            jamtemp.NETTO3 = summtrljam.NETTO3
            jamtemp.ENHET1 = summtrljam.ENHET1
            jamtemp.ENHET2 = summtrljam.ENHET2
            jamtemp.ENHET3 = summtrljam.ENHET3
            jamtemp.ENHET = summtrljam.ENHET
            jamtemp.BENAMNING = summtrljam.BENAMNING
            jamtemp.ANTAL = STRING(summtrljam.ANTAL).
            IF summtrljam.UPPLAG = ? THEN jamtemp.UPPLAG = "".
            IF ordvar NE jamtemp.ORD  THEN DO:
               FIND FIRST jambuff WHERE jambuff.RADRAKN = 99999999 NO-ERROR.
               IF AVAILABLE jambuff THEN DO:               
                  ASSIGN
                  rrakn = rrakn + 1
                  jamtemp.RADRAKN = rrakn               
                  jambuff.RADRAKN = rrakn - 1               
                  jambuff.TOTPRIS = tpris1
                  jambuff.TOTPRIS2 = tpris2
                  jambuff.TOTPRIS3 = tpris3.
               END.            
               CREATE jambuff.
               IF jamtemp.KTYPKOD NE "" THEN ASSIGN jambuff.BENAMNING = "Summa " + jamtemp.KTYPKOD + " Id: " + jamtemp.FRI2.
               ELSE ASSIGN jambuff.BENAMNING = "Kablar upplag: " + jamtemp.UPPLAG.
               ASSIGN
               jambuff.RADRAKN = 99999999
               tpris1 = 0
               tpris2 = 0
               tpris3 = 0.
            END.          
            ejsum = FALSE.
            IF vhu = TRUE THEN DO:
               IF summtrljam.ENR1 = "saknas" OR  summtrljam.ENR2 = "saknas" OR summtrljam.ENR3 = "saknas" THEN DO:
                  ejsum = TRUE.
               END.
            END.
            IF ejsum = FALSE THEN DO:            
               ASSIGN
               tpris1 = tpris1 + summtrljam.TOTPRIS
               tpris2 = tpris2 + summtrljam.TOTPRIS2
               tpris3 = tpris3 + summtrljam.TOTPRIS3.                           
            END.
            ejsum = FALSE.
            ordvar = jamtemp.ORD.
         END.
      END.         
      FIND FIRST jambuff WHERE jambuff.RADRAKN = 99999999 NO-ERROR.
      IF AVAILABLE jambuff THEN DO:               
         ASSIGN
         rrakn = rrakn + 1         
         jambuff.RADRAKN = rrakn 
         jambuff.TOTPRIS = tpris1
         jambuff.TOTPRIS2 = tpris2
         jambuff.TOTPRIS3 = tpris3.
      END.
      CREATE jamtemp.      
      ASSIGN
      rrakn = rrakn + 1         
      jamtemp.RADRAKN = rrakn. 
   END.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ENABLE BRW_JAMF WITH FRAME FRAME-VINST.
   BRW_JAMF:HIDDEN = FALSE.     
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev1 NO-LOCK NO-ERROR.
   IF AVAILABLE jmflev_temp THEN DO:  
      FILL-IN-LEV1 = jmflev_temp.LEVNAMN.
   END.
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev2 NO-LOCK NO-ERROR.
   IF AVAILABLE jmflev_temp THEN DO:  
      FILL-IN-LEV2 = jmflev_temp.LEVNAMN.
   END.
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev3 NO-LOCK NO-ERROR.
   IF AVAILABLE jmflev_temp THEN DO:  
      FILL-IN-LEV3 = jmflev_temp.LEVNAMN.
   END.   
   jamtemp.ENR1:LABEL IN BROWSE BRW_JAMF = FILL-IN-LEV1 + "/!" + Guru.Konstanter:genk + " 1".
   jamtemp.ENR2:LABEL IN BROWSE BRW_JAMF = FILL-IN-LEV2 + "/!" + Guru.Konstanter:genk + " 2".
   jamtemp.ENR3:LABEL IN BROWSE BRW_JAMF = FILL-IN-LEV3 + "/!" + Guru.Konstanter:genk + " 3".
      
   RUN enable_UI.      
   RUN openbdynspec_UI IN brwproc[1].   
   FILL-IN-LEV1:HIDDEN = TRUE.
   FILL-IN-LEV2:HIDDEN = TRUE.
   FILL-IN-LEV3:HIDDEN = TRUE.
   IF optsatt = 1 OR optsatt = 3 THEN DO:           
      jamtemp.FRI2:VISIBLE IN BROWSE BRW_JAMF = FALSE.           
      jamtemp.FRI3:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.UPPLAG:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
   END.
   ELSE DO:       
      jamtemp.FRI2:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      jamtemp.FRI3:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      jamtemp.UPPLAG:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
   END.      
   IF lev1 NE ? THEN DO:      
      jamtemp.ENR1:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      jamtemp.LEV1:VISIBLE IN BROWSE BRW_JAMF = TRUE.       
      jamtemp.TOTPRIS:VISIBLE IN BROWSE BRW_JAMF = TRUE.            
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev1 NO-LOCK NO-ERROR.       
   END.
   ELSE DO:
      jamtemp.ENR2:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.LEV2:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.TOTPRIS2:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
   END.
   IF lev2 NE ? THEN DO:      
      jamtemp.ENR2:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      jamtemp.LEV2:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      jamtemp.TOTPRIS2:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev2 NO-LOCK NO-ERROR.       
   END.
   ELSE DO:
      jamtemp.ENR2:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.LEV2:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.TOTPRIS2:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
   END.
   IF lev3 NE ? THEN DO:      
      jamtemp.ENR3:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      jamtemp.LEV3:VISIBLE IN BROWSE BRW_JAMF = TRUE.    
      jamtemp.TOTPRIS3:VISIBLE IN BROWSE BRW_JAMF = TRUE.      
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = lev3 NO-LOCK NO-ERROR.      
   END.
   ELSE DO:
      jamtemp.ENR3:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.LEV3:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
      jamtemp.TOTPRIS3:VISIBLE IN BROWSE BRW_JAMF = FALSE.      
   END.
   {FRMSIZE.I}          
   ENABLE BTN_EXCEL WITH FRAME {&FRAME-NAME}. 
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).   
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).    
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_JAMF:HANDLE IN FRAME {&FRAME-NAME}).
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN setcolindex_UI IN brwproc[1] (INPUT "RADRAKN"). 
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN BYTLEV3APP.P PERSISTENT SET bytlev3apph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BYTLEV3APP.P PERSISTENT SET bytlev3apph.
   END.  
   RUN JAMEXC.P PERSISTENT SET jamexh.                                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-LEV1 FILL-IN-LEV2 FILL-IN-LEV3 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE BRW_JAMF BTN_SKRIV BTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-2 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.
   IF optsatt = 1 OR optsatt = 2 THEN DO:   
      IF jamtemp.BENAMNING BEGINS "Summa" OR jamtemp.BENAMNING BEGINS "Kablar" THEN DO: 
         jamtemp.BENAMNING:FGCOLOR IN BROWSE BRW_JAMF = 12.
         IF DECIMAL(jamtemp.TOTPRIS3) = 0 THEN DO:
            IF DECIMAL(jamtemp.TOTPRIS) < DECIMAL(jamtemp.TOTPRIS2)  THEN DO:
               jamtemp.TOTPRIS:FGCOLOR IN BROWSE BRW_JAMF = 12.
            END.
            ELSE IF DECIMAL(jamtemp.TOTPRIS2) < DECIMAL(jamtemp.TOTPRIS) THEN DO:
               jamtemp.TOTPRIS2:FGCOLOR IN BROWSE BRW_JAMF = 12.
            END.            
         END.
         ELSE DO:         
            IF DECIMAL(jamtemp.TOTPRIS) < DECIMAL(jamtemp.TOTPRIS2) AND DECIMAL(jamtemp.TOTPRIS) < DECIMAL(jamtemp.TOTPRIS3) THEN DO:
               jamtemp.TOTPRIS:FGCOLOR IN BROWSE BRW_JAMF = 12.
            END.
            ELSE IF DECIMAL(jamtemp.TOTPRIS2) < DECIMAL(jamtemp.TOTPRIS) AND DECIMAL(jamtemp.TOTPRIS2) < DECIMAL(jamtemp.TOTPRIS3) THEN DO:
               jamtemp.TOTPRIS2:FGCOLOR IN BROWSE BRW_JAMF = 12.
            END.
            ELSE IF DECIMAL(jamtemp.TOTPRIS3) < DECIMAL(jamtemp.TOTPRIS) AND DECIMAL(jamtemp.TOTPRIS3) < DECIMAL(jamtemp.TOTPRIS2) THEN DO:
               jamtemp.TOTPRIS3:FGCOLOR IN BROWSE BRW_JAMF = 12.
            END.      
         END.
      END.      
   END.
   IF optsatt = 3 THEN DO:               
      IF DECIMAL(jamtemp.TOTPRIS3) = 0 THEN DO:
         IF DECIMAL(jamtemp.TOTPRIS) < DECIMAL(jamtemp.TOTPRIS2) THEN DO:
            jamtemp.LEV1:FGCOLOR IN BROWSE BRW_JAMF = 12.
            jamtemp.TOTPRIS:FGCOLOR IN BROWSE BRW_JAMF = 12.
         END.
         ELSE IF DECIMAL(jamtemp.TOTPRIS2) < DECIMAL(jamtemp.TOTPRIS)  THEN DO:
            jamtemp.LEV2:FGCOLOR IN BROWSE BRW_JAMF = 12.
            jamtemp.TOTPRIS2:FGCOLOR IN BROWSE BRW_JAMF = 12.
         END.         
      END.
      ELSE DO:      
         IF DECIMAL(jamtemp.TOTPRIS) < DECIMAL(jamtemp.TOTPRIS2) AND DECIMAL(jamtemp.TOTPRIS) < DECIMAL(jamtemp.TOTPRIS3) THEN DO:
            jamtemp.LEV1:FGCOLOR IN BROWSE BRW_JAMF = 12.
            jamtemp.TOTPRIS:FGCOLOR IN BROWSE BRW_JAMF = 12.
         END.
         ELSE IF DECIMAL(jamtemp.TOTPRIS2) < DECIMAL(jamtemp.TOTPRIS) AND DECIMAL(jamtemp.TOTPRIS2) < DECIMAL(jamtemp.TOTPRIS3) THEN DO:
            jamtemp.LEV2:FGCOLOR IN BROWSE BRW_JAMF = 12.
            jamtemp.TOTPRIS2:FGCOLOR IN BROWSE BRW_JAMF = 12.
         END.
         ELSE IF DECIMAL(jamtemp.TOTPRIS3) < DECIMAL(jamtemp.TOTPRIS) AND DECIMAL(jamtemp.TOTPRIS3) < DECIMAL(jamtemp.TOTPRIS2) THEN DO:
            jamtemp.LEV3:FGCOLOR IN BROWSE BRW_JAMF = 12.
            jamtemp.TOTPRIS3:FGCOLOR IN BROWSE BRW_JAMF = 12.
         END.            
      END.
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skaptidut_UI WINDOW-2 
PROCEDURE skaptidut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
DEFINE INPUT  PARAMETER excellvar AS LOGICAL NO-UNDO.
   {AMERICANEUROPEAN.I}   
   IF excellvar = TRUE THEN excllbred = 20. 
   ELSE excllbred = 0.     
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   IF optsatt = 1 OR optsatt = 3 THEN DO:
      CREATE tidut.
      ASSIGN      
      SUBSTRING(tidut.UT,1) = "Leverantörer"
      SUBSTRING(tidut.UT,25 + excllbred) = FILL-IN-LEV1.
      IF lev2 NE "" THEN SUBSTRING(tidut.UT,64 + excllbred) = FILL-IN-LEV2.
      IF lev3 NE "" THEN SUBSTRING(tidut.UT,114 + excllbred) = FILL-IN-LEV3.
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Benämning"
      SUBSTRING(tidut.UT,17 + excllbred) = "Antal"
      SUBSTRING(tidut.UT,25 + excllbred) = "Enr1"
      SUBSTRING(tidut.UT,36 + excllbred) = "Enh1"
      SUBSTRING(tidut.UT,42 + excllbred) = "Pris1"
      SUBSTRING(tidut.UT,53 + excllbred) = "Totpris1".      
      IF lev2 NE "" THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,64 + excllbred) = "Enr2"
         SUBSTRING(tidut.UT,75 + excllbred) = "Enh2"
         SUBSTRING(tidut.UT,81 + excllbred) = "Pris2"
         SUBSTRING(tidut.UT,92 + excllbred) = "Totpris2"
         SUBSTRING(tidut.UT,103 + excllbred) = "Tp1-Tp2".
         IF lev3 NE "" THEN DO:      
            ASSIGN
            SUBSTRING(tidut.UT,114 + excllbred) = "Enr3"
            SUBSTRING(tidut.UT,125 + excllbred) = "Enh3"
            SUBSTRING(tidut.UT,131 + excllbred) = "Pris3"
            SUBSTRING(tidut.UT,142 + excllbred) = "Totpris3"
            SUBSTRING(tidut.UT,153 + excllbred) = "Tp1-Tp3".      
         END.
      
      END.
      CREATE tidut.
      str=                                                                    
         "===============.===.=======.==========.========.==========.==========.========.==========.==========.========.==========".           
      SUBSTRING(tidut.UT,1) = str.                     
      FOR EACH jamtemp USE-INDEX radrakn NO-LOCK:
         CREATE tidut.
         
         ASSIGN
         SUBSTRING(tidut.UT,1) = SUBSTRING(jamtemp.BENAMNING,1,15 + excllbred)                  
         SUBSTRING(tidut.UT,17 + excllbred) = jamtemp.ANTAL
         SUBSTRING(tidut.UT,25 + excllbred) = jamtemp.ENR1
         SUBSTRING(tidut.UT,36 + excllbred) = jamtemp.ENHET1
         SUBSTRING(tidut.UT,42 + excllbred) = STRING(jamtemp.LEV1)
         SUBSTRING(tidut.UT,53 + excllbred) = STRING(jamtemp.TOTPRIS).
         IF jamtemp.NETTO1 = TRUE THEN SUBSTRING(tidut.UT,200) = "N".
         ELSE IF jamtemp.NETTO1 = ? THEN SUBSTRING(tidut.UT,200) = "M".
         IF lev2 NE "" THEN DO:      
            ASSIGN
            SUBSTRING(tidut.UT,64 + excllbred) = jamtemp.ENR2
            SUBSTRING(tidut.UT,75 + excllbred) = jamtemp.ENHET2
            SUBSTRING(tidut.UT,81 + excllbred) = STRING(jamtemp.LEV2)
            SUBSTRING(tidut.UT,92 + excllbred) = STRING(jamtemp.TOTPRIS2)
            SUBSTRING(tidut.UT,103 + excllbred) = STRING(jamtemp.TOTPRIS - jamtemp.TOTPRIS2).
            IF jamtemp.NETTO2 = TRUE THEN SUBSTRING(tidut.UT,201) = "N".
            ELSE IF jamtemp.NETTO2 = ? THEN SUBSTRING(tidut.UT,201) = "M".
            IF lev3 NE "" THEN DO:      
               ASSIGN
               SUBSTRING(tidut.UT,114 + excllbred) = jamtemp.ENR3
               SUBSTRING(tidut.UT,125 + excllbred) = jamtemp.ENHET3
               SUBSTRING(tidut.UT,131 + excllbred) = STRING(jamtemp.LEV3)
               SUBSTRING(tidut.UT,142 + excllbred) = STRING(jamtemp.TOTPRIS3)
               SUBSTRING(tidut.UT,153 + excllbred) = STRING(jamtemp.TOTPRIS - jamtemp.TOTPRIS3).
               IF jamtemp.NETTO3 = TRUE THEN SUBSTRING(tidut.UT,202) = "N".
               ELSE IF jamtemp.NETTO3 = ? THEN SUBSTRING(tidut.UT,202) = "M".
            END.            
         END.
      END.
   END.
   ELSE IF optsatt = 2 THEN DO:
      CREATE tidut.      
      ASSIGN      
      SUBSTRING(tidut.UT,1) = "Leverantörer"
      SUBSTRING(tidut.UT,25 + excllbred) = FILL-IN-LEV1.
      IF lev2 NE "" THEN SUBSTRING(tidut.UT,64 + excllbred) = FILL-IN-LEV2.
      IF lev3 NE "" THEN SUBSTRING(tidut.UT,114 + excllbred) = FILL-IN-LEV3.
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Benämning"      
      SUBSTRING(tidut.UT,17 + excllbred) = "Antal"
      SUBSTRING(tidut.UT,25 + excllbred) = "Enr1"
      SUBSTRING(tidut.UT,36 + excllbred) = "Enh1"
      SUBSTRING(tidut.UT,42 + excllbred) = "Pris1"
      SUBSTRING(tidut.UT,53 + excllbred) = "Totpris1".
      IF lev2 NE "" THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,64 + excllbred) = "Enr2"
         SUBSTRING(tidut.UT,75 + excllbred) = "Enh1"
         SUBSTRING(tidut.UT,81 + excllbred) = "Pris2"
         SUBSTRING(tidut.UT,92 + excllbred) = "Totpris2"
         SUBSTRING(tidut.UT,103 + excllbred) = "Tp1-Tp2".
         IF lev3 NE "" THEN DO:      
            ASSIGN
            SUBSTRING(tidut.UT,114 + excllbred) = "Enr3"
            SUBSTRING(tidut.UT,125 + excllbred) = "Enh3"
            SUBSTRING(tidut.UT,131 + excllbred) = "Pris3"
            SUBSTRING(tidut.UT,142 + excllbred) = "Totpris3"
            SUBSTRING(tidut.UT,153 + excllbred) = "Tp1-Tp3".
         END.
      END.
      IF lev3 = ""  THEN DO:         
         ASSIGN
         SUBSTRING(tidut.UT,114 + excllbred) = "Upplag"
         SUBSTRING(tidut.UT,120 + excllbred) = "Id"
         SUBSTRING(tidut.UT,127 + excllbred) = "Fri id".
      END.
      ELSE DO:      
         ASSIGN
         SUBSTRING(tidut.UT,164 + excllbred) = "Upplag"
         SUBSTRING(tidut.UT,170 + excllbred) = "Id"
         SUBSTRING(tidut.UT,177 + excllbred) = "Fri id".
      END.
      CREATE tidut.
      CREATE tidut.
      str=                                                                    
         "===============.===.=======.==========.========.==========.==========.========.==========.==========.========.==========.=====.======.==========".           
      SUBSTRING(tidut.UT,1) = str.                     
      FOR EACH jamtemp USE-INDEX radrakn NO-LOCK:
         CREATE tidut.
         IF jamtemp.BENAMNING BEGINS "Summa" OR jamtemp.BENAMNING BEGINS "Kablar" THEN DO:
            SUBSTRING(tidut.UT,1) = jamtemp.BENAMNING.
         END.
         ELSE ASSIGN SUBSTRING(tidut.UT,1) = SUBSTRING(jamtemp.BENAMNING,1,15 + excllbred).
         ASSIGN
         /*SUBSTRING(tidut.UT,17) = jamtemp.ENHET*/
         SUBSTRING(tidut.UT,17 + excllbred) = jamtemp.ANTAL
         SUBSTRING(tidut.UT,25 + excllbred) = jamtemp.ENR1
         SUBSTRING(tidut.UT,36 + excllbred) = jamtemp.ENHET1
         SUBSTRING(tidut.UT,42 + excllbred) = STRING(jamtemp.LEV1)
         SUBSTRING(tidut.UT,53 + excllbred) = STRING(jamtemp.TOTPRIS).
         IF jamtemp.NETTO1 = TRUE THEN SUBSTRING(tidut.UT,200 + excllbred) = "N".
         ELSE IF jamtemp.NETTO1 = ? THEN SUBSTRING(tidut.UT,200 + excllbred) = "M".
         IF lev2 NE "" THEN DO:      
            ASSIGN
            SUBSTRING(tidut.UT,64 + excllbred) = jamtemp.ENR2
            SUBSTRING(tidut.UT,75 + excllbred) = jamtemp.ENHET2
            SUBSTRING(tidut.UT,81 + excllbred) = STRING(jamtemp.LEV2)
            SUBSTRING(tidut.UT,92 + excllbred) = STRING(jamtemp.TOTPRIS2).
            SUBSTRING(tidut.UT,103 + excllbred) = STRING(jamtemp.TOTPRIS - jamtemp.TOTPRIS2).
            IF jamtemp.NETTO2 = TRUE THEN SUBSTRING(tidut.UT,201 + excllbred) = "N".
            ELSE IF jamtemp.NETTO2 = ? THEN SUBSTRING(tidut.UT,201 + excllbred) = "M".
            IF lev3 NE "" THEN DO:      
               ASSIGN               
               SUBSTRING(tidut.UT,114 + excllbred) = jamtemp.ENR3
               SUBSTRING(tidut.UT,125 + excllbred) = jamtemp.ENHET3
               SUBSTRING(tidut.UT,131 + excllbred) = STRING(jamtemp.LEV3)
               SUBSTRING(tidut.UT,142 + excllbred) = STRING(jamtemp.TOTPRIS3)
               SUBSTRING(tidut.UT,153 + excllbred) = STRING(jamtemp.TOTPRIS - jamtemp.TOTPRIS3).
               IF jamtemp.NETTO3 = TRUE THEN SUBSTRING(tidut.UT,202 + excllbred) = "N".
               ELSE IF jamtemp.NETTO3 = ? THEN SUBSTRING(tidut.UT,202 + excllbred) = "M".               
            END.
         END.
         IF lev3 = "" THEN DO:      
            ASSIGN
            SUBSTRING(tidut.UT,114 + excllbred) = jamtemp.UPPLAG
            SUBSTRING(tidut.UT,120 + excllbred) = jamtemp.FRI2
            SUBSTRING(tidut.UT,127 + excllbred) = jamtemp.FRI3.
         END.
         ELSE DO:         
            ASSIGN
            SUBSTRING(tidut.UT,164 + excllbred) = jamtemp.UPPLAG
            SUBSTRING(tidut.UT,170 + excllbred) = jamtemp.FRI2
            SUBSTRING(tidut.UT,177 + excllbred) = jamtemp.FRI3.
         END.
         IF tidut.UT BEGINS "Summa" THEN CREATE tidut.
         IF tidut.UT BEGINS "Kablar" THEN CREATE tidut.

      END.
   END.   
   IF vejt = TRUE THEN DO:
      CREATE tidut.
      ASSIGN SUBSTRING(tidut.UT,132,1) = "$".
      FIND FIRST saknastmp WHERE saknastmp.LEV1 = lev1 NO-LOCK NO-ERROR.
      IF AVAILABLE saknastmp THEN DO:
      
         CREATE tidut.
         CREATE tidut.
         ASSIGN      
         SUBSTRING(tidut.UT,1) = "Följande enr saknas hos leverantör: " + saknastmp.LEVNAMN1.
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Från enr"
         SUBSTRING(tidut.UT,12) = "Från leverantör"
         SUBSTRING(tidut.UT,33) = "Till enr"
         SUBSTRING(tidut.UT,44) = "Till leverantör".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "==========.====================.==========.====================.".         
         FOR EACH saknastmp WHERE saknastmp.LEV1 = lev1 NO-LOCK:      
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = saknastmp.ENR
            SUBSTRING(tidut.UT,12) = saknastmp.BLEVNAMN            
            SUBSTRING(tidut.UT,44) = saknastmp.LEVNAMN1.
         END.        
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,132,1) = "$".
      END.      
      FIND FIRST saknastmp WHERE saknastmp.LEV1 = lev2 NO-LOCK NO-ERROR.
      IF AVAILABLE saknastmp THEN DO:      
         CREATE tidut.
         CREATE tidut.
         ASSIGN      
         SUBSTRING(tidut.UT,1) = "Följande enr saknas hos leverantör: " + saknastmp.LEVNAMN1.
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Från enr"
         SUBSTRING(tidut.UT,12) = "Från leverantör"
         SUBSTRING(tidut.UT,33) = "Till enr"
         SUBSTRING(tidut.UT,44) = "Till leverantör".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "==========.====================.==========.====================.".         
         FOR EACH saknastmp WHERE saknastmp.LEV1 = lev2 NO-LOCK:      
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = saknastmp.ENR
            SUBSTRING(tidut.UT,12) = saknastmp.BLEVNAMN            
            SUBSTRING(tidut.UT,44) = saknastmp.LEVNAMN1.
         END.        
         CREATE tidut.
         ASSIGN SUBSTRING(tidut.UT,132,1) = "$".
      END.      
      FIND FIRST saknastmp WHERE saknastmp.LEV1 = lev3 NO-LOCK NO-ERROR.
      IF AVAILABLE saknastmp THEN DO:      
         CREATE tidut.
         CREATE tidut.
         ASSIGN      
         SUBSTRING(tidut.UT,1) = "Följande enr saknas hos leverantör: " + saknastmp.LEVNAMN1.
         CREATE tidut.
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "Från enr"
         SUBSTRING(tidut.UT,12) = "Från leverantör"
         SUBSTRING(tidut.UT,33) = "Till enr"
         SUBSTRING(tidut.UT,44) = "Till leverantör".
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = "==========.====================.==========.====================.".         
         FOR EACH saknastmp WHERE saknastmp.LEV1 = lev3 NO-LOCK:      
            CREATE tidut.
            ASSIGN
            SUBSTRING(tidut.UT,1) = saknastmp.ENR
            SUBSTRING(tidut.UT,12) = saknastmp.BLEVNAMN            
            SUBSTRING(tidut.UT,44) = saknastmp.LEVNAMN1.
         END.              
      END.      
   END.
   {EUROPEANAMERICAN.I}
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGL.P. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

