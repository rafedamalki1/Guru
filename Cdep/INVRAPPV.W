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

  Created: 10/08/96 -  1:50 pm

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
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BTN_VISA BTN_skriv RAD_SORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL RAD_SORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

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
"Ben?mning", 3
     SIZE 35.25 BY 1.08 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Inventerat", 1,
"Teoretiskt", 2
     SIZE 24.63 BY 1.08 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     RAD_VAL AT ROW 2 COL 13.88 NO-LABEL
     BTN_VISA AT ROW 2 COL 50.5
     BTN_skriv AT ROW 3.08 COL 50.5
     RAD_SORT AT ROW 4.88 COL 13.88 NO-LABEL
     BTN_AVB AT ROW 4.88 COL 50.5
     "Sortera p?:" VIEW-AS TEXT
          SIZE 11.5 BY 1.13 AT ROW 4.88 COL 2
     "Visa:" VIEW-AS TEXT
          SIZE 5.25 BY 1.13 AT ROW 2 COL 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.5 BY 6.46.


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
         TITLE              = "Inventering"
         HEIGHT             = 6.5
         WIDTH              = 65
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_skriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-2
ON CHOOSE OF BTN_skriv IN FRAME FRAME-B /* Skriv ut */
DO: 
   ASSIGN   
   sortering = INPUT RAD_SORT. 
   RUN SKRIVVAL.W (INPUT FALSE).
   {muswait.i}       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:                 
      skrivut = TRUE.
      IF RAD_VAL = 1 THEN DO:   
         {AVBGOM.I}
         RUN INVENUTFV.W.
         {AVBFRAM.I} 
      END.
      IF RAD_VAL = 2 THEN DO:
         {AVBGOM.I}
         RUN INVE2UTFV.W.
         {AVBFRAM.I}
      END.        
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_skriv IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-2
ON CHOOSE OF BTN_VISA IN FRAME FRAME-B /* Visa */
DO: 
   {muswait.i}
   ASSIGN   
   sortering = INPUT RAD_SORT.
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   IF RAD_VAL = 1 THEN DO:   
      {AVBGOM.I}
      RUN INVENUTFV.W.
      {AVBFRAM.I} 
   END.
   IF RAD_VAL = 2 THEN DO:
      {AVBGOM.I}      
      RUN INVE2UTFV.W.
      {AVBFRAM.I}
   END.    
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SORT WINDOW-2
ON VALUE-CHANGED OF RAD_SORT IN FRAME FRAME-B
DO:
   RAD_SORT = INPUT RAD_SORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-2
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-B
DO:
   RAD_VAL = INPUT RAD_VAL.
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
   RUN disable_UI.

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
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa NO-ERROR.   
   status-ok = RAD_SORT:DELETE("Fack-id").
   status-ok = RAD_SORT:DELETE("Enr").
   status-ok = RAD_SORT:DELETE("Ben?mning").
   RAD_SORT:ADD-LAST("Fack-id", 1).
   RAD_SORT:ADD-LAST(Guru.Konstanter:genk, 2).
   RAD_SORT:ADD-LAST("Ben?mning", 3).   
   ASSIGN WINDOW-2:TITLE = "Inventerat / teoretiskt lager f?r dep? - " + depatemp.BENAMNING.
   ASSIGN
   RAD_VAL = 1    
   RAD_SORT = 2.
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
  DISPLAY RAD_VAL RAD_SORT 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE RAD_VAL BTN_VISA BTN_skriv RAD_SORT BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

