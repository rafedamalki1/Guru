&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1





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

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE priapph AS HANDLE NO-UNDO.
DEFINE VARIABLE vemanvandare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

{TIDUTTTNEW.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_PRI

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES priotemp

/* Definitions for BROWSE BRW_PRI                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_PRI priotemp.PKOD priotemp.P-BENAMNING ~
priotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRI priotemp.PKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRI priotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRI priotemp
&Scoped-define QUERY-STRING-BRW_PRI FOR EACH priotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRI OPEN QUERY BRW_PRI FOR EACH priotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRI priotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRI priotemp


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_PRI}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_PRI FBTN_VPER FBTN_SKR BTN_NY BTN_AND ~
BTN_BORT BTN_AVB 

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

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PRI FOR 
      priotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRI WINDOW-1 _STRUCTURED
  QUERY BRW_PRI NO-LOCK DISPLAY
      priotemp.PKOD COLUMN-LABEL "Prioritet" FORMAT ">>9":U
      priotemp.P-BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
      priotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      priotemp.PKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 43.5 BY 9.04
         TITLE "Prioritet".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_PRI AT ROW 1.5 COL 1.5
     FBTN_VPER AT ROW 3 COL 45.75
     FBTN_SKR AT ROW 4.08 COL 45.75
     BTN_NY AT ROW 11 COL 1.88
     BTN_AND AT ROW 11 COL 14.63
     BTN_BORT AT ROW 11 COL 27.25
     BTN_AVB AT ROW 11 COL 45.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.25 BY 11.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Prioritet"
         HEIGHT             = 11.46
         WIDTH              = 61
         MAX-HEIGHT         = 21.54
         MAX-WIDTH          = 82.38
         VIRTUAL-HEIGHT     = 21.54
         VIRTUAL-WIDTH      = 82.38
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
/* BROWSE-TAB BRW_PRI 1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRI
/* Query rebuild information for BROWSE BRW_PRI
     _TblList          = "Temp-Tables.priotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.priotemp.PKOD
"PKOD" "Prioritet" ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.priotemp.P-BENAMNING
"P-BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.priotemp.AKTIV
"AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_PRI */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_PRI
&Scoped-define SELF-NAME BRW_PRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRI WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_PRI IN FRAME FRAME-A /* Prioritet */
DO:
   run andra.
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
   IF VALID-HANDLE(priapph) THEN DELETE PROCEDURE priapph.
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
   WINDOW-1:TITLE = Guru.Konstanter:gpriok.
   BRW_PRI:TITLE = Guru.Konstanter:gpriok.
   priotemp.PKOD:LABEL IN BROWSE BRW_PRI = Guru.Konstanter:gpriok.
   RUN prihmt2_UI IN priapph (OUTPUT TABLE priotemp).             
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
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
   priotemp.PKOD:READ-ONLY IN BROWSE BRW_PRI = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PRI:HANDLE IN FRAME {&FRAME-NAME}).         
   IF Guru.Konstanter:appcon THEN DO:
      RUN PRIAPP.P PERSISTENT SET priapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PRIAPP.P PERSISTENT SET priapph.
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
   IF NOT AVAILABLE priotemp THEN DO:
      RETURN NO-APPLY.
   END.
   
   vemanvandare = priotemp.PKOD.
   RUN PRINY.W (INPUT-OUTPUT vemanvandare).
   IF musz = FALSE THEN DO: 
      DELETE priotemp.
      RUN hamtaen2_UI IN priapph (INPUT vemanvandare,OUTPUT TABLE priotemp APPEND).                   
      FIND FIRST priotemp WHERE priotemp.PKOD = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE priotemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(priotemp)).              
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
   IF NOT AVAILABLE priotemp THEN DO:
      RETURN NO-APPLY.
   END. 
   MESSAGE "Vill du ta bort denna post ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE priotemp.P-BENAMNING
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      {muswait.i}
      RUN pribort_UI IN priapph (INPUT priotemp.PKOD,OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.                                     
      DELETE priotemp.
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
  ENABLE BRW_PRI FBTN_VPER FBTN_SKR BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
   RUN PRINY.W (INPUT-OUTPUT vemanvandare).
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN hamtaen2_UI IN priapph (INPUT vemanvandare,OUTPUT TABLE priotemp APPEND).                   
      FIND FIRST priotemp WHERE priotemp.PKOD = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE priotemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(priotemp)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].     
      END.      
   END.
   musz = FALSE.       
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
   RUN visaalla_UI IN priapph (OUTPUT TABLE tidut).                              
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Prioritet").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

