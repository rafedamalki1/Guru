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
{LEVTEMP.I}
{GLOBVAR2DEL1.I}
{EXTRADATA.I}
DEFINE NEW SHARED VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE levapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE vemanvandare AS CHARACTER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{TIDUTTTNEW.I}
DEFINE BUFFER levtempbuff FOR levtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_LEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES levtemp

/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV levtemp.ORDNING levtemp.LEVKOD ~
levtemp.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV levtemp.LEVKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_LEV levtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_LEV levtemp
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH levtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH levtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LEV levtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV levtemp


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_LEV}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_LEV FBTN_VPER FBTN_SKR BTN_MORDUPP ~
BTN_MORDNER BTN_NY BTN_AND BTN_BORT BTN_AVB 

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

DEFINE BUTTON BTN_MORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_MORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LEV FOR 
      levtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV WINDOW-1 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      levtemp.ORDNING FORMAT "->,>>>,>>9":U
      levtemp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U
      levtemp.LEVNAMN FORMAT "x(256)":U WIDTH 39
  ENABLE
      levtemp.LEVKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 49 BY 16.75
         TITLE "Leverantör".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_LEV AT ROW 1.5 COL 1.5
     FBTN_VPER AT ROW 3.75 COL 51.75
     FBTN_SKR AT ROW 4.83 COL 51.75
     BTN_MORDUPP AT ROW 10 COL 55.5 WIDGET-ID 4
     BTN_MORDNER AT ROW 12.92 COL 55.5 WIDGET-ID 2
     BTN_NY AT ROW 19 COL 5.5
     BTN_AND AT ROW 19 COL 19.5
     BTN_BORT AT ROW 19 COL 33.5
     BTN_AVB AT ROW 19 COL 51.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 66.13 BY 19.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: levtemp T "?" NO-UNDO temp-db levtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Leverantör"
         HEIGHT             = 19.42
         WIDTH              = 66.38
         MAX-HEIGHT         = 21.42
         MAX-WIDTH          = 84.63
         VIRTUAL-HEIGHT     = 21.42
         VIRTUAL-WIDTH      = 84.63
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
/* BROWSE-TAB BRW_LEV 1 FRAME-A */
ASSIGN 
       BTN_MORDNER:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.levtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.levtemp.ORDNING
     _FldNameList[2]   > Temp-Tables.levtemp.LEVKOD
"levtemp.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.levtemp.LEVNAMN
"levtemp.LEVNAMN" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "39" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_LEV
&Scoped-define SELF-NAME BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_LEV IN FRAME FRAME-A /* Leverantör */
DO:
  RUN andra.
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


&Scoped-define SELF-NAME BTN_MORDNER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MORDNER WINDOW-1
ON CHOOSE OF BTN_MORDNER IN FRAME FRAME-A /* Ner */
DO:
   run ner_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MORDUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MORDUPP WINDOW-1
ON CHOOSE OF BTN_MORDUPP IN FRAME FRAME-A
DO:
   RUN upp_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
  RUN ny. 
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
   IF VALID-HANDLE(levapph) THEN DELETE PROCEDURE levapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
  
   RUN levhmt_UI IN levapph (OUTPUT TABLE levtemp). 
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").            
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   levtemp.ordning:VISIBLE IN BROWSE BRW_LEV = FALSE.
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
   ASSIGN
   levtemp.LEVKOD:READ-ONLY IN BROWSE BRW_LEV = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN LEVAPP.P PERSISTENT SET levapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      IF Guru.Konstanter:varforetypval[27] = 1 THEN RUN LEVAPP2.P PERSISTENT SET levapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN LEVAPP.P PERSISTENT SET levapph.
      IF Guru.Konstanter:varforetypval[27] = 1 THEN RUN LEVAPP2.P PERSISTENT SET levapph.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE levtemp THEN DO:
      RETURN NO-APPLY.
   END.
   
   vemanvandare = levtemp.LEVKOD.
   RUN LEVNY.W (INPUT-OUTPUT vemanvandare).
   IF musz = FALSE THEN DO: 
      DELETE levtemp.
      RUN hamtaen_UI IN levapph (INPUT vemanvandare,OUTPUT TABLE levtemp APPEND).                   
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE levtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(levtemp)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].     
      END.    
   END.   
   musz = FALSE.
   
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
   IF NOT AVAILABLE levtemp THEN DO:
      RETURN NO-APPLY.
   END.
   IF levtemp.LEVKOD = "99" THEN DO:
      MESSAGE "Leverantör 99 special kan ej tas bort av systemtekniska skäl."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   MESSAGE "Vill du ta bort denna post ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE levtemp.LEVNAMN
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      {muswait.i}
      RUN levbort_UI IN levapph (INPUT levtemp.LEVKOD,OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.
      IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "SOLE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         skrivut = FALSE.
         MESSAGE "En lista kommer nu att skapas. Den visar artiklar upplagda i depå för den nu borttagna leverantören."
         VIEW-AS ALERT-BOX.                                 
         {AVBGOM.I}
         EMPTY TEMP-TABLE extralevtemp NO-ERROR.          
         CREATE extralevtemp.
         BUFFER-COPY levtemp TO extralevtemp.
         RUN visart_UI IN levapph (INPUT TABLE extralevtemp, OUTPUT TABLE tidut).   
         RUN UTRAPP.W (INPUT "Leverantörens artiklar").
         {AVBFRAM.I}
      END.                                 
      DELETE levtemp.
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
  ENABLE BRW_LEV FBTN_VPER FBTN_SKR BTN_MORDUPP BTN_MORDNER BTN_NY BTN_AND 
         BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ner_UI WINDOW-1 
PROCEDURE ner_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lord AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE levtemp THEN DO:
      RETURN NO-APPLY.
   END.
   FIND FIRST levtempbuff WHERE levtempbuff.ORDNING >= levtemp.ORDNING AND levtempbuff.LEVKOD NE levtemp.LEVKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE levtempbuff THEN RETURN.
   RUN uppner_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny WINDOW-1 
PROCEDURE ny :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i} 
   
   vemanvandare = ?.
   RUN LEVNY.W (INPUT-OUTPUT vemanvandare).
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN hamtaen_UI IN levapph (INPUT vemanvandare,OUTPUT TABLE levtemp APPEND).                   
      FIND FIRST levtemp WHERE levtemp.LEVKOD = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE levtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(levtemp)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].     
      END.      
   END.
   musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uppner_UI WINDOW-1 
PROCEDURE uppner_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE lord AS INTEGER NO-UNDO.
   lord = levtemp.ORDNING.
   levtemp.ORDNING = levtempbuff.ORDNING.
   levtempbuff.ORDNING = lord.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"                   
   inextradatatemp.HUVUDCH = levtemp.LEVKOD.                                              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   FIND FIRST extradatatemp WHERE NO-LOCK NO-ERROR.
   extradatatemp.SOKINT[1] = levtemp.ORDNING .  
   RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"                   
   inextradatatemp.HUVUDCH = levtempbuff.LEVKOD.                                              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   FIND FIRST extradatatemp WHERE NO-LOCK NO-ERROR.
   extradatatemp.SOKINT[1] = levtempbuff.ORDNING .  
   RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp).
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(levtemp)).              
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[1].     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upp_UI WINDOW-1 
PROCEDURE upp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
    
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE levtemp THEN DO:
      RETURN NO-APPLY.
   END. 
 
   FIND LAST levtempbuff WHERE levtempbuff.ORDNING <= levtemp.ORDNING AND levtempbuff.LEVKOD NE levtemp.LEVKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE levtempbuff THEN RETURN.
   RUN uppner_UI.
        
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
   RUN visaalla_UI IN levapph (OUTPUT TABLE tidut).                              
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Leverantörer").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

