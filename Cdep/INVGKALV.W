&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/09/96 - 10:13 am

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DEPATEMP.I}

DEFINE NEW SHARED VARIABLE sortering AS INTEGER NO-UNDO.  
DEFINE NEW SHARED VARIABLE startdat AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE teor AS LOGICAL NO-UNDO.       

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE invgapph AS HANDLE NO-UNDO.
DEFINE VARIABLE datvar AS DATE NO-UNDO.


DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE dat_tab NO-UNDO
   FIELD IBDATUM AS DATE
   INDEX DATUM IBDATUM  DESCENDING.

/*DEFINE QUERY balans FOR MTRLDEP.*/


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define BROWSE-NAME BRW_INV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dat_tab

/* Definitions for BROWSE BRW_INV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_INV dat_tab.IBDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_INV 
&Scoped-define QUERY-STRING-BRW_INV FOR EACH dat_tab NO-LOCK
&Scoped-define OPEN-QUERY-BRW_INV OPEN QUERY BRW_INV FOR EACH dat_tab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_INV dat_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_INV dat_tab


/* Definitions for FRAME FRAME-C                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_INV BTN_VISA BTN_skriv RAD_SORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_SORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_skriv 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE RAD_SORT AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Fack-id", 1,
"Enr", 2,
"Benämning", 3
     SIZE 35.25 BY 1.08 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_INV FOR 
      dat_tab SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_INV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_INV WINDOW-3 _STRUCTURED
  QUERY BRW_INV NO-LOCK DISPLAY
      dat_tab.IBDATUM COLUMN-LABEL "Föreg. inventering" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 21 BY 5.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     BRW_INV AT ROW 1.17 COL 1.5
     BTN_VISA AT ROW 1.17 COL 39
     BTN_skriv AT ROW 2.25 COL 39
     RAD_SORT AT ROW 8.42 COL 1.88 NO-LABEL
     BTN_AVB AT ROW 8.42 COL 39
     "Sortera på:" VIEW-AS TEXT
          SIZE 11.5 BY 1.13 AT ROW 7.17 COL 1.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.13 BY 8.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db dat_tab
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Inventering"
         HEIGHT             = 8.71
         WIDTH              = 53.5
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 82
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-C
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_INV TEXT-1 FRAME-C */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_INV
/* Query rebuild information for BROWSE BRW_INV
     _TblList          = "Temp-Tables.dat_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.dat_tab.IBDATUM
"dat_tab.IBDATUM" "Föreg. inventering" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_INV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_INV
&Scoped-define SELF-NAME BRW_INV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_INV WINDOW-3
ON VALUE-CHANGED OF BRW_INV IN FRAME FRAME-C
DO:   
   startdat = dat_tab.IBDATUM.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_skriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-3
ON CHOOSE OF BTN_skriv IN FRAME FRAME-C /* Skriv ut */
DO: 
   ASSIGN   
   sortering = INPUT RAD_SORT. 
   RUN SKRIVVAL.W (INPUT FALSE). 
   {muswait.i}      
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:                 
      skrivut = TRUE.      
      {AVBGOM.I}     
      RUN INVGKAL3V.P (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, INPUT startdat, INPUT sortering ).      
      {AVBFRAM.I}         
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-3
ON MOUSE-MENU-CLICK OF BTN_skriv IN FRAME FRAME-C /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-3
ON CHOOSE OF BTN_VISA IN FRAME FRAME-C /* Visa */
DO:  
   {muswait.i}
   ASSIGN   
   sortering = INPUT RAD_SORT.
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}   
   RUN INVGKAL3V.P (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, INPUT startdat, INPUT sortering ).   
   {AVBFRAM.I}       
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SORT WINDOW-3
ON VALUE-CHANGED OF RAD_SORT IN FRAME FRAME-C
DO:
   RAD_SORT = INPUT RAD_SORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(invgapph) THEN DELETE PROCEDURE invgapph.
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
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa.
   ASSIGN WINDOW-3:TITLE = "Inventerat lager - " + depatemp.BENAMNING. 
   ASSIGN
   RAD_SORT = 2.
   RUN dattabhmt_UI IN invgapph (INPUT vald_depa, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE dat_tab).                
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   
   FIND FIRST dat_tab NO-LOCK NO-ERROR.
   IF AVAILABLE dat_tab THEN DO:
      ASSIGN      
      startdat = dat_tab.IBDATUM.
      OPEN QUERY BRW_INV FOR EACH dat_tab NO-LOCK .      
   END.
   ELSE DO:       
      MESSAGE "Det finns inga inventerade värden."
      VIEW-AS ALERT-BOX.
   END.    
   status-ok = RAD_SORT:DELETE("Fack-id").
   status-ok = RAD_SORT:DELETE("Enr").
   status-ok = RAD_SORT:DELETE("Benämning").
   RAD_SORT:ADD-LAST("Fack-id", 1).
   RAD_SORT:ADD-LAST(Guru.Konstanter:genk, 2).
   RAD_SORT:ADD-LAST("Benämning", 3).       
   RUN enable_UI.   
   BTN_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   {FRMSIZE.I}     
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_INV:HANDLE IN FRAME {&FRAME-NAME}).         
   IF Guru.Konstanter:appcon THEN DO:
      RUN INVGAPP.P PERSISTENT SET invgapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN INVGAPP.P PERSISTENT SET invgapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  DISPLAY RAD_SORT 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE BRW_INV BTN_VISA BTN_skriv RAD_SORT BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

