&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

  Created: 10/11/96 -  2:05 pm

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
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE avdatum AS DATE NO-UNDO.  
DEFINE NEW SHARED VARIABLE sortering AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE VARIABLE ar AS INTEGER FORMAT "9999" NO-UNDO.

&Scoped-define NEW NEW

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_VISA BTN_skriv RAD_PERIOD FILL-IN-AR ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PERIOD FILL-IN-AR FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT 

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

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     BTN_VISA AT ROW 2 COL 55
     BTN_skriv AT ROW 3.1 COL 55
     RAD_PERIOD AT ROW 3.13 COL 1.5 NO-LABEL
     FILL-IN-AR AT ROW 3.33 COL 26.63 COLON-ALIGNED NO-LABEL
     FILL-IN-STARTDAT AT ROW 4.54 COL 26.63 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 4.54 COL 42.75 COLON-ALIGNED
     BTN_AVB AT ROW 6.25 COL 55
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 69.13 BY 6.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 6.5
         WIDTH              = 69.25
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
                                                                        */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME FRAME-C
   SHARED                                                               */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STOPPDAT IN FRAME FRAME-C
   SHARED                                                               */
ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME FRAME-C
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_skriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_skriv WINDOW-3
ON CHOOSE OF BTN_skriv IN FRAME FRAME-C /* Skriv ut */
DO: 
   sortering = INPUT RAD_PERIOD.  
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i}                 
      skrivut = TRUE.
      IF RAD_PERIOD = 1 THEN DO:
         ASSIGN
         bdatum = DATE(01,01,FILL-IN-AR)
         ar = INPUT FILL-IN-AR + 1
         avdatum = DATE(01,01,ar)
         avdatum = avdatum - 1.         
      END.   
      ELSE DO:
         ASSIGN
         bdatum = FILL-IN-STARTDAT
         avdatum = FILL-IN-STOPPDAT.
      END.  
      {AVBGOM.I}
      RUN VOLUTFV.W.
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
   sortering = INPUT RAD_PERIOD. 
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      bdatum = DATE(01,01,FILL-IN-AR)
      ar = INPUT FILL-IN-AR + 1
      avdatum = DATE(01,01,ar)
      avdatum = avdatum - 1. 
   END.           
   ELSE DO:
      ASSIGN
      bdatum = FILL-IN-STARTDAT
      avdatum = FILL-IN-STOPPDAT.
   END.  
   {AVBGOM.I}
   RUN VOLUTFV.W.
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AR WINDOW-3
ON LEAVE OF FILL-IN-AR IN FRAME FRAME-C
DO:
   FILL-IN-AR = INPUT FILL-IN-AR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT WINDOW-3
ON LEAVE OF FILL-IN-STARTDAT IN FRAME FRAME-C /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT WINDOW-3
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME FRAME-C /* Från */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.                                        
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STOPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT WINDOW-3
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME FRAME-C /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT WINDOW-3
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME FRAME-C /* Till */
DO:    
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STOPPDAT.
   RUN AlmanBtn.w. 
   FILL-IN-STOPPDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD WINDOW-3
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME FRAME-C
DO:
   RAD_PERIOD = INPUT RAD_PERIOD. 
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      FILL-IN-AR:HIDDEN = FALSE
      FILL-IN-STOPPDAT:HIDDEN = TRUE
      FILL-IN-STARTDAT:HIDDEN = TRUE.
   END.
   IF RAD_PERIOD = 2 THEN DO:
      ASSIGN
      FILL-IN-AR:HIDDEN = TRUE
      FILL-IN-STOPPDAT:HIDDEN = FALSE
      FILL-IN-STARTDAT:HIDDEN = FALSE.
   END.    
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
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa.
   ASSIGN WINDOW-3:TITLE = "Volymvärde för depå - " + depatemp.BENAMNING.    
   ASSIGN
   FILL-IN-STOPPDAT = TODAY
   FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY))   
   FILL-IN-AR = YEAR(TODAY)
   RAD_PERIOD = 1.
  RUN enable_UI.   
   {FRMSIZE.I}                   
  ASSIGN
  FILL-IN-STOPPDAT:HIDDEN = TRUE
  FILL-IN-STARTDAT:HIDDEN = TRUE.
  {musarrow.i}
   {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY RAD_PERIOD FILL-IN-AR FILL-IN-STARTDAT FILL-IN-STOPPDAT 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE BTN_VISA BTN_skriv RAD_PERIOD FILL-IN-AR FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

