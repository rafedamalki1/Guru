&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WWSTART 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/06 - 10:23 am

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


   /*
orgpropath = PROPATH.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-16 SEL_UPP BTN_KOR ~
BTN_MEDD BTN_BYT BTN_BYTW BTN_UPPDAT BTN_AVB ED_WWW 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP ED_WWW FILL-IN-GURU FILL-IN-FUNK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WWSTART AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt användare" 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_BYTW 
     LABEL "Byt fönsterstorlek" 
     SIZE 19 BY 1.5 TOOLTIP "Anpassa Guru till din skärmupplösning.".

DEFINE BUTTON BTN_KOR 
     LABEL "Kör funktion" 
     SIZE 19 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON BTN_MEDD 
     LABEL "Skapa meddelande till Guruanvändare" 
     SIZE 19 BY 1.5.

DEFINE BUTTON BTN_UPPDAT 
     LABEL "Uppdatera program" 
     SIZE 19 BY 1.5 TOOLTIP "Uppdatera din Guru applikation med den senaste versionen.".

DEFINE VARIABLE ED_WWW AS CHARACTER INITIAL "Nyheter på www.elpool.se." 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 38 BY .75 NO-UNDO.

DEFINE VARIABLE FILL-IN-FUNK AS CHARACTER FORMAT "X(256)":U INITIAL "Funktioner:" 
      VIEW-AS TEXT 
     SIZE 14 BY 1
     FGCOLOR 1 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-GURU AS CHARACTER FORMAT "X(256)":U INITIAL "Välkommen till" 
      VIEW-AS TEXT 
     SIZE 50.5 BY 1
     FONT 17 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 43 BY 18.75.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 63 BY .5
     BGCOLOR 1 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 20 BY 18.75
     BGCOLOR 8 .

DEFINE VARIABLE SEL_UPP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 38 BY 16.5
     BGCOLOR 7 FGCOLOR 15 FONT 17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     SEL_UPP AT ROW 3.25 COL 3.5 NO-LABEL
     BTN_KOR AT ROW 4.75 COL 44.5
     BTN_MEDD AT ROW 7.5 COL 44.5
     BTN_BYT AT ROW 10.25 COL 44.5
     BTN_BYTW AT ROW 13 COL 44.5
     BTN_UPPDAT AT ROW 15.75 COL 44.5
     BTN_AVB AT ROW 18.38 COL 44.5
     ED_WWW AT ROW 20.13 COL 3.5 NO-LABEL
     FILL-IN-GURU AT ROW 1 COL 3 NO-LABEL
     FILL-IN-FUNK AT ROW 2.88 COL 43 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 2.75 COL 1
     RECT-2 AT ROW 2.75 COL 44
     RECT-16 AT ROW 2.25 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.25 BY 20.63
         BGCOLOR 8 
         DEFAULT-BUTTON BTN_KOR.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WWSTART ASSIGN
         HIDDEN             = YES
         TITLE              = "GURU"
         COLUMN             = 9.25
         ROW                = 6.29
         HEIGHT             = 20.71
         WIDTH              = 63.38
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = 8
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 16
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WWSTART
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
   FRAME-NAME                                                           */
ASSIGN 
       BTN_UPPDAT:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       ED_WWW:READ-ONLY IN FRAME FRAME-B        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-FUNK IN FRAME FRAME-B
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-FUNK:READ-ONLY IN FRAME FRAME-B        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-GURU IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
THEN WWSTART:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-B
/* Query rebuild information for FRAME FRAME-B
     _Query            is NOT OPENED
*/  /* FRAME FRAME-B */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WWSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WWSTART WWSTART
ON WINDOW-CLOSE OF WWSTART /* GURU */
DO:   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&Scoped-define SELF-NAME BTN_BYT
&Scoped-define SELF-NAME BTN_BYTW
&Scoped-define SELF-NAME BTN_KOR
&Scoped-define SELF-NAME BTN_MEDD
&Scoped-define SELF-NAME BTN_UPPDAT
&Scoped-define SELF-NAME ED_WWW
&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WWSTART
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-B
DO:
   SEL_UPP = INPUT SEL_UPP.     
   RUN vart_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WWSTART 


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
   RUN enable_UI.
     
   {&WINDOW-NAME}:HIDDEN = FALSE.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WWSTART  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WWSTART)
  THEN DELETE WIDGET WWSTART.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WWSTART  _DEFAULT-ENABLE
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
  DISPLAY SEL_UPP ED_WWW FILL-IN-GURU FILL-IN-FUNK 
      WITH FRAME FRAME-B IN WINDOW WWSTART.
  ENABLE RECT-1 RECT-2 RECT-16 SEL_UPP BTN_KOR BTN_MEDD BTN_BYT BTN_BYTW 
         BTN_UPPDAT BTN_AVB ED_WWW 
      WITH FRAME FRAME-B IN WINDOW WWSTART.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

