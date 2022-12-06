&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/12/96 -  9:29 am

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

&Scoped-define SHARED SHARED
{HUVLEVTEMP.I}
{LEVTEMP.I}
{MTRLTEMP.I}
DEFINE NEW SHARED VARIABLE lev1 AS CHARACTER NO-UNDO.    
DEFINE NEW SHARED VARIABLE lev2 AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE excmtrlapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_LEVFR BTN_BEST BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEVFR 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEVFR AS CHARACTER FORMAT "X(30)":U 
     LABEL "Leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_LEVFR AT ROW 2.13 COL 11.88 COLON-ALIGNED
     BTN_BEST AT ROW 4 COL 3
     BTN_AVB AT ROW 4 COL 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 31.88 BY 4.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Visa art. med pris 0 för"
         HEIGHT             = 4.33
         WIDTH              = 32
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
ASSIGN 
       CMB_LEVFR:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avbryt */
DO:       
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BEST WINDOW-2
ON CHOOSE OF BTN_BEST IN FRAME FRAME-B /* Ok */
DO:     
   
   MESSAGE "Vill du se listan i Excel klicka 'Ja' annars 'Nej'."
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Excel?" UPDATE svar AS LOGICAL.  
   {muswait.i}
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev1 NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
      vald_lev = levtemp.LEVKOD.
      RUN mtrlhmt_UI IN excmtrlapph (INPUT vald_lev,OUTPUT TABLE mtrltemp).
   END.
          
   IF svar THEN DO:     
      RUN EXCMTRLU.P.
   END.
   ELSE DO:
      {AVBGOM.I}
      RUN PRISNOLLU2.W.
      {AVBFRAM.I}
   END.      
   {musarrow.i}   
   APPLY "GO" TO FRAME {&FRAME-NAME}.              
   APPLY "CLOSE":U TO THIS-PROCEDURE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVFR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVFR WINDOW-2
ON VALUE-CHANGED OF CMB_LEVFR IN FRAME FRAME-B /* Leverantör */
DO:                                 
   lev1 = INPUT CMB_LEVFR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(excmtrlapph) THEN DELETE PROCEDURE excmtrlapph.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXCMTRLAPP.P PERSISTENT SET excmtrlapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXCMTRLAPP.P PERSISTENT SET excmtrlapph.
   END.  
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR. 
   /*FIND FIRST huvlevtemp NO-LOCK NO-ERROR.*/
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.
   ASSIGN         
   status-ok = CMB_LEVFR:DELETE("0").   
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEVFR:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}     
   CMB_LEVFR:SCREEN-VALUE = levtemp.LEVNAMN.    
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN.
   lev1 = levtemp.LEVNAMN. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      ASSIGN      
      status-ok = CMB_LEVFR:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.       
   END.        
   ASSIGN     
   CMB_LEVFR = INPUT CMB_LEVFR
   lev1 = INPUT CMB_LEVFR.
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
  DISPLAY CMB_LEVFR 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_LEVFR BTN_BEST BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

