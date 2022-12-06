&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 -  1:43 pm

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
{PRIARBANLTEMP.I}
{GLOBVAR2DEL1.I}
{ARBART.I}


DEFINE NEW SHARED VARIABLE arbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE vemanvandare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
{TIDUTTTNEW.I} 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE bgfarg AS INTEGER NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER skrivskydd AS LOGICAL NO-UNDO.


DEFINE NEW SHARED TEMP-TABLE tempfarg
   FIELD kod AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_ARBART

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES arbarttemp2

/* Definitions for BROWSE BRW_ARBART                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ARBART arbarttemp2.ARBARTKOD ~
arbarttemp2.ARBBENAMNING arbarttemp2.AKTIV arbarttemp2.VISAFARGKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ARBART 
&Scoped-define QUERY-STRING-BRW_ARBART FOR EACH arbarttemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ARBART OPEN QUERY BRW_ARBART FOR EACH arbarttemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ARBART arbarttemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ARBART arbarttemp2


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_ARBART}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_ARBART FBTN_VPER FBTN_SKR FILL-IN-2 ~
FILL-IN-7 FILL-IN-3 FILL-IN-8 FILL-IN-4 FILL-IN-9 FILL-IN-5 FILL-IN-10 ~
FILL-IN-6 FILL-IN-11 BTN_NY BTN_AND BTN_BORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-2 FILL-IN-7 FILL-IN-3 FILL-IN-8 ~
FILL-IN-4 FILL-IN-9 FILL-IN-5 FILL-IN-10 FILL-IN-6 FILL-IN-11 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ARBART FOR 
      arbarttemp2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ARBART WINDOW-1 _STRUCTURED
  QUERY BRW_ARBART NO-LOCK DISPLAY
      arbarttemp2.ARBARTKOD COLUMN-LABEL "Arbetsart" FORMAT ">>9":U
      arbarttemp2.ARBBENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
            WIDTH 18.5
      arbarttemp2.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
            WIDTH 6.5
      arbarttemp2.VISAFARGKOD COLUMN-LABEL "Färg" FORMAT "X(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 44 BY 13.54
         TITLE "Arbetsart" ROW-HEIGHT-CHARS .46.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_ARBART AT ROW 1.5 COL 1.5
     FBTN_VPER AT ROW 2 COL 46.88
     FBTN_SKR AT ROW 3.08 COL 46.88
     FILL-IN-2 AT ROW 5.25 COL 46 NO-LABEL
     FILL-IN-7 AT ROW 5.25 COL 52 NO-LABEL
     FILL-IN-3 AT ROW 6.25 COL 46 NO-LABEL
     FILL-IN-8 AT ROW 6.25 COL 52 NO-LABEL
     FILL-IN-4 AT ROW 7.25 COL 46 NO-LABEL
     FILL-IN-9 AT ROW 7.25 COL 52 NO-LABEL
     FILL-IN-5 AT ROW 8.25 COL 46 NO-LABEL
     FILL-IN-10 AT ROW 8.25 COL 52 NO-LABEL
     FILL-IN-6 AT ROW 9.25 COL 46 NO-LABEL
     FILL-IN-11 AT ROW 9.25 COL 52 NO-LABEL
     BTN_NY AT ROW 15.5 COL 2.5
     BTN_AND AT ROW 15.5 COL 16.5
     BTN_BORT AT ROW 15.5 COL 30.5
     BTN_AVB AT ROW 15.5 COL 46.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.63 BY 15.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: arbarttemp2 T "?" NO-UNDO temp-db arbarttemp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Arbetsart"
         HEIGHT             = 16.08
         WIDTH              = 61.13
         MAX-HEIGHT         = 18.67
         MAX-WIDTH          = 83.25
         VIRTUAL-HEIGHT     = 18.67
         VIRTUAL-WIDTH      = 83.25
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_ARBART 1 FRAME-A */
/* SETTINGS FOR FILL-IN FILL-IN-10 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-11:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-8 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-9 IN FRAME FRAME-A
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ARBART
/* Query rebuild information for BROWSE BRW_ARBART
     _TblList          = "Temp-Tables.arbarttemp2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbarttemp2.ARBARTKOD
"ARBARTKOD" "Arbetsart" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.arbarttemp2.ARBBENAMNING
"ARBBENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "18.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.arbarttemp2.AKTIV
"AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.arbarttemp2.VISAFARGKOD
"VISAFARGKOD" "Färg" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_ARBART */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_ARBART
&Scoped-define SELF-NAME BRW_ARBART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ARBART WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_ARBART IN FRAME FRAME-A /* Arbetsart */
DO:
   IF skrivskydd = FALSE THEN DO:
      run andra.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
   RUN andra.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   RUN bort.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO: 
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN ut_UI. 
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER WINDOW-1
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-A /* Visa */
DO:
   skrivut = FALSE.  
   RUN ut_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-10 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-10 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-11 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-11 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-2 WINDOW-1
ON ENTRY OF FILL-IN-2 IN FRAME FRAME-A
DO:
  MESSAGE "hej" VIEW-AS ALERT-BOX.
   RUN andrafarg_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-2 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-3 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-3 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-4 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-4 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-5 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-5 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-6 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-6 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-7 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-7 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-8 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-8 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-9 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-9 IN FRAME FRAME-A
DO:
   RUN andrafarg_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */

ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.




/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(arbapph) THEN DELETE PROCEDURE arbapph.
   skrivskydd = FALSE.
   {BORTBRWPROC.I}
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
   RUN enable_UI.   
   {FRMSIZE.I} 
   RUN arbhmt2_UI IN arbapph (OUTPUT TABLE arbarttemp2).             
   arbarttemp2.ARBARTKOD:LABEL IN BROWSE BRW_ARBART = Guru.Konstanter:gartk. 
   
   BRW_ARBART:TITLE = Guru.Konstanter:gartk.
   WINDOW-1:TITLE = Guru.Konstanter:gartk.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").   
   RUN farg_UI.
   
   IF skrivskydd = TRUE THEN DO: 
      RUN gomma_UI.
      RUN avmark_UI IN brwproc[1].
   END.
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF skrivskydd = TRUE THEN {&WINDOW-NAME}:ALWAYS-ON-TOP = TRUE.
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
      (INPUT BRW_ARBART:HANDLE IN FRAME {&FRAME-NAME}).         
   
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE). /*FÄRG*/
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN ARBAPP.P PERSISTENT SET arbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ARBAPP.P PERSISTENT SET arbapph.
   END.   
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" THEN.
   ELSE arbarttemp2.AKTIV:VISIBLE IN BROWSE BRW_ARBART = FALSE.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE arbarttemp2 THEN DO:
      RETURN NO-APPLY.
   END.
   vemanvandare = arbarttemp2.ARBARTKOD.
   RUN ARBANY2.W (INPUT-OUTPUT vemanvandare).
   IF musz = FALSE THEN DO: 
      DELETE arbarttemp2.
      RUN hamtaen2_UI IN arbapph (INPUT vemanvandare,OUTPUT TABLE arbarttemp2 APPEND).                   
      FIND FIRST arbarttemp2 WHERE arbarttemp2.ARBARTKOD = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE arbarttemp2 THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(arbarttemp2)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].     
      END.    
   END. 
   
   musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andrafarg_UI WINDOW-1 
PROCEDURE andrafarg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF FILL-IN-2:SELECTED IN FRAME FRAME-A = TRUE THEN bgfarg = INTEGER(FILL-IN-2:BGCOLOR).
   ELSE IF FILL-IN-3:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-3:BGCOLOR).
   ELSE IF FILL-IN-4:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-4:BGCOLOR).
   ELSE IF FILL-IN-5:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-5:BGCOLOR).
   ELSE IF FILL-IN-6:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-6:BGCOLOR).
   ELSE IF FILL-IN-7:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-7:BGCOLOR).
   ELSE IF FILL-IN-8:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-8:BGCOLOR).
   ELSE IF FILL-IN-9:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-9:BGCOLOR).
   ELSE IF FILL-IN-10:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-10:BGCOLOR).
   ELSE IF FILL-IN-11:SELECTED = TRUE THEN bgfarg = INTEGER(FILL-IN-11:BGCOLOR).

   /*MESSAGE "bgfarg = " bgfarg VIEW-AS ALERT-BOX.*/

 
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE arbarttemp2 THEN DO:
      RETURN NO-APPLY.
   END.   
   
   arbarttemp2.FARGKOD = bgfarg.
   
   /*RUN arbspara2_UI IN arbapph (INPUT vemanvandare, INPUT TABLE arbarttemp2,OUTPUT TABLE felmeddtemp).*/
   RUN arbsparafarg_UI IN arbapph (INPUT arbarttemp2.ARBARTKOD, INPUT arbarttemp2.FARGKOD, INPUT arbarttemp2.AKTIV). /*int, int, log, */
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(arbarttemp2)).              
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[1].     
   RUN avmark_UI IN brwproc[1].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort WINDOW-1 
PROCEDURE bort :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE arbarttemp2 THEN DO:
      RETURN NO-APPLY.
   END.
   
   MESSAGE "Vill du verkligen ta bort denna " LC(Guru.Konstanter:gartk)"?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE arbarttemp2.ARBBENAMNING
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      {muswait.i}  
      RUN arbbort_UI IN arbapph (INPUT arbarttemp2.ARBARTKOD,OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.                                     
      DELETE arbarttemp2.
      RUN selnextprevrow_UI IN brwproc[1].
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].                         
      {musarrow.i}
   END.
      
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
  DISPLAY FILL-IN-2 FILL-IN-7 FILL-IN-3 FILL-IN-8 FILL-IN-4 FILL-IN-9 FILL-IN-5 
          FILL-IN-10 FILL-IN-6 FILL-IN-11 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_ARBART FBTN_VPER FBTN_SKR FILL-IN-2 FILL-IN-7 FILL-IN-3 FILL-IN-8 
         FILL-IN-4 FILL-IN-9 FILL-IN-5 FILL-IN-10 FILL-IN-6 FILL-IN-11 BTN_NY 
         BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE farg_UI WINDOW-1 
PROCEDURE farg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   i = 1.
   DO WHILE i <= 10:
      CREATE tempfarg.
         ASSIGN
         tempfarg.KOD = i.
      
      i = i + 1.
   END.

   FILL-IN-2:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-2:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-3:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-3:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-4:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-4:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-5:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-5:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-6:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-6:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-7:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-7:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-8:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-8:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-9:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-9:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-10:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-10:SELECTABLE IN FRAME FRAME-A = TRUE.
   FILL-IN-11:READ-ONLY IN FRAME FRAME-A = TRUE.
   FILL-IN-11:SELECTABLE IN FRAME FRAME-A = TRUE.





   ASSIGN
   FILL-IN-2:BGCOLOR IN FRAME FRAME-A = 1
   FILL-IN-3:BGCOLOR IN FRAME FRAME-A = 2
   FILL-IN-4:BGCOLOR = 3
   FILL-IN-5:BGCOLOR = 4
   FILL-IN-6:BGCOLOR = 9
   FILL-IN-7:BGCOLOR = 10
   FILL-IN-8:BGCOLOR = 11
   FILL-IN-9:BGCOLOR = 12
   FILL-IN-10:BGCOLOR = 13
   FILL-IN-11:BGCOLOR = 14.
   

   

   /*RUN cmbfarg_UI.*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gomma_UI WINDOW-1 
PROCEDURE gomma_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Dubbeklick i BRW har felkontroll i eventet för browsern
-------------------------------------------------------------*/
   BTN_NY:VISIBLE IN FRAME FRAME-A = FALSE.
   BTN_AND:VISIBLE IN FRAME FRAME-A = FALSE.
   BTN_BORT:VISIBLE IN FRAME FRAME-A = FALSE.
   
   FILL-IN-2:VISIBLE = FALSE.
   FILL-IN-3:VISIBLE = FALSE.
   FILL-IN-4:VISIBLE = FALSE.
   FILL-IN-5:VISIBLE = FALSE.
   FILL-IN-6:VISIBLE = FALSE.
   FILL-IN-7:VISIBLE = FALSE.
   FILL-IN-8:VISIBLE = FALSE.
   FILL-IN-9:VISIBLE = FALSE.
   FILL-IN-10:VISIBLE = FALSE.
   FILL-IN-11:VISIBLE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}.
   
   vemanvandare = ?.
   RUN ARBANY2.W (INPUT-OUTPUT vemanvandare).
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN hamtaen2_UI IN arbapph (INPUT vemanvandare,OUTPUT TABLE arbarttemp2 APPEND).                   
      FIND FIRST arbarttemp2 WHERE arbarttemp2.ARBARTKOD = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE arbarttemp2 THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(arbarttemp2)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].     
      END.      
   END.
   musz = FALSE.     
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
   IF brwh:NAME = "BRW_ARBART" THEN RUN rowfarg_UI.
END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowfarg_UI WINDOW-1 
PROCEDURE rowfarg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   
   IF AVAILABLE arbarttemp2 THEN DO:
      IF arbarttemp2.FARGKOD > 0 THEN DO:
         arbarttemp2.VISAFARGKOD:FGCOLOR IN BROWSE BRW_ARBART = arbarttemp2.FARGKOD. 
         arbarttemp2.VISAFARGKOD:BGCOLOR IN BROWSE BRW_ARBART = arbarttemp2.FARGKOD.             
      END.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/                
   RUN visaalla2_UI IN arbapph (INPUT TABLE arbarttemp2,OUTPUT TABLE tidut).                              
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Arbetsart").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

