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

  Created: 09/24/96 -  3:39 pm

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
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DEPATEMP.I}
{SPECMTRLTEMP.I}
DEFINE NEW SHARED VARIABLE ny AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE valnummer AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE foljesedel AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.

DEFINE VARIABLE valvuapph AS HANDLE NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE best_nr_koll AS INTEGER NO-UNDO.
DEFINE VARIABLE valnr AS INTEGER NO-UNDO.



&Scoped-define NEW NEW  
&Scoped-define SHARED SHARED 
{bestnrtab.I}
/*DEFINE NEW SHARED TEMP-TABLE best_nr_tab NO-UNDO
    FIELD Bestnr AS INTEGER 
    FIELD bestdatum AS DATE LABEL "Best-datum"                  
    FIELD bestallare AS CHARACTER LABEL "Beställare"                  
    INDEX BESTNR BESTNR DESCENDING.*/ 
DEFINE NEW SHARED TEMP-TABLE off_mtrl NO-UNDO     
    FIELD TOTALT AS DECIMAL.
DEFINE NEW SHARED TEMP-TABLE gam_mtrl NO-UNDO
   FIELD DATUM AS DATE
   FIELD DATUM2 AS DATE
   FIELD KOM AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BESTNR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES best_nr_tab

/* Definitions for BROWSE BRW_BESTNR                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_BESTNR best_nr_tab.Bestnr ~
best_nr_tab.Bestdatum best_nr_tab.Bestallare 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BESTNR 
&Scoped-define QUERY-STRING-BRW_BESTNR FOR EACH best_nr_tab NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BESTNR OPEN QUERY BRW_BESTNR FOR EACH best_nr_tab NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BESTNR best_nr_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BESTNR best_nr_tab


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BESTNR BTN_VISA BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BESTNR FOR 
      best_nr_tab SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BESTNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BESTNR WINDOW-1 _STRUCTURED
  QUERY BRW_BESTNR NO-LOCK DISPLAY
      best_nr_tab.Bestnr FORMAT ">>>>>9":U
      best_nr_tab.Bestdatum FORMAT "99/99/99":U
      best_nr_tab.Bestallare FORMAT "x(12)":U WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 36 BY 12.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_BESTNR AT ROW 1.25 COL 1.5
     BTN_VISA AT ROW 8 COL 38.5
     BTN_AVB AT ROW 14.5 COL 38.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 53.5 BY 14.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db best_nr_tab
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beställning"
         HEIGHT             = 15
         WIDTH              = 53.63
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 84.5
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 84.5
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_BESTNR 1 FRAME-A */
ASSIGN 
       BRW_BESTNR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BESTNR
/* Query rebuild information for BROWSE BRW_BESTNR
     _TblList          = "Temp-Tables.best_nr_tab"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.best_nr_tab.Bestnr
     _FldNameList[2]   = Temp-Tables.best_nr_tab.Bestdatum
     _FldNameList[3]   > Temp-Tables.best_nr_tab.Bestallare
"best_nr_tab.Bestallare" ? ? "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BESTNR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa */
DO:
   IF AVAILABLE best_nr_tab THEN DO:   
      ASSIGN
      ny = FALSE
      valnummer = best_nr_tab.BESTNR
      valnr = best_nr_tab.BESTNR.
      EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
      FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa USE-INDEX DEP-NR NO-LOCK NO-ERROR.       
      RUN mtrlbbhmt_UI IN valvuapph (INPUT valaonr,INPUT valdelnr,INPUT valnummer, OUTPUT TABLE spec_mtrl,OUTPUT TABLE off_mtrl ,OUTPUT TABLE gam_mtrl).                  
      {AVBGOM.I}
      RUN FOLJTRP2V.W.   
      {AVBFRAM.I}
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BESTNR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(valvuapph) THEN DELETE PROCEDURE valvuapph.  
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
   foljesedel = FALSE.
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa NO-LOCK NO-ERROR.
   &Scoped-define FORMATNAMN valaonr
   {AOFORMAT2.I}
   ASSIGN WINDOW-1:TITLE = "Visa uttag för depå - " + depatemp.BENAMNING + " mot " +
   Guru.Konstanter:gaok + ":" + formataonr + " Delnr:" + STRING(valdelnr,Guru.Konstanter:varforetypchar[1]).    
   IF alla = TRUE THEN DO:   
      RUN hmtbest_UI IN valvuapph (INPUT valaonr,INPUT valdelnr,INPUT ?,OUTPUT best_nr_koll, OUTPUT TABLE best_nr_tab).                   
   END.
   ELSE DO:
      RUN hmtbest_UI IN valvuapph (INPUT valaonr,INPUT valdelnr,INPUT depatemp.DEP-NR,OUTPUT best_nr_koll, OUTPUT TABLE best_nr_tab).                   
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].
    
  RUN enable_UI.   
   {FRMSIZE.I}    
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
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BESTNR:HANDLE IN FRAME {&FRAME-NAME}).         
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALVUAPP.P PERSISTENT SET valvuapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALVUAPP.P PERSISTENT SET valvuapph.
   END.   
END PROCEDURE.

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
  ENABLE BRW_BESTNR BTN_VISA BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

