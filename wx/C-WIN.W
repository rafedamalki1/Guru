&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-BARABREDD

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_KONSTGRP MBTN_KONST MBTN_KONSTMTRL ~
MBTN_KONSTKALK MBTN_MADM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON MBTN_KONST 
     LABEL "Konstruktioner" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_KONSTGRP 
     LABEL "Konstruktionsgrupp-Konstruktioner" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_KONSTKALK 
     LABEL "Konstruktioner-Kalkyl" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_KONSTMTRL 
     LABEL "Konstruktioner-Materiel" 
     SIZE 7.5 BY 2.33.

DEFINE BUTTON MBTN_MADM 
     LABEL "Materieladm." 
     SIZE 7.5 BY 2.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-BARABREDD
     MBTN_KONSTGRP AT ROW 1 COL 2.5
     MBTN_KONST AT ROW 1 COL 16.5
     MBTN_KONSTMTRL AT ROW 1 COL 28.5
     MBTN_KONSTKALK AT ROW 1 COL 42.5
     MBTN_MADM AT ROW 1 COL 54.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-BARABREDD
                                                                        */
ASSIGN 
       MBTN_KONST:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       MBTN_KONSTGRP:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       MBTN_KONSTKALK:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       MBTN_KONSTMTRL:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

ASSIGN 
       MBTN_MADM:HIDDEN IN FRAME FRAME-BARABREDD           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KONST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KONST C-Win
ON CHOOSE OF MBTN_KONST IN FRAME FRAME-BARABREDD /* Konstruktioner */
DO:
   RUN btnvit_UI (INPUT MBTN_KONST:HANDLE).
   RUN rowleave_UI.
   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KONSTGRP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KONSTGRP C-Win
ON CHOOSE OF MBTN_KONSTGRP IN FRAME FRAME-BARABREDD /* Konstruktionsgrupp-Konstruktioner */
DO:
   RUN btnvit_UI (INPUT MBTN_KONSTGRP:HANDLE).
   RUN rowleave_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KONSTKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KONSTKALK C-Win
ON CHOOSE OF MBTN_KONSTKALK IN FRAME FRAME-BARABREDD /* Konstruktioner-Kalkyl */
DO:
   
   /*
   RUN btnvit_UI (INPUT MBTN_KONSTKALK:HANDLE).
   */
   RUN btnvit_UI (INPUT MBTN_KONSTGRP:HANDLE).
   RUN rowleave_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_KONSTMTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_KONSTMTRL C-Win
ON CHOOSE OF MBTN_KONSTMTRL IN FRAME FRAME-BARABREDD /* Konstruktioner-Materiel */
DO:
   RUN btnvit_UI (INPUT MBTN_KONSTMTRL:HANDLE).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_MADM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_MADM C-Win
ON CHOOSE OF MBTN_MADM IN FRAME FRAME-BARABREDD /* Materieladm. */
DO:        
   RUN btnvit_UI (INPUT MBTN_MADM:HANDLE).
   RUN rowleave_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    MBTN_KONSTGRP:LOAD-IMAGE("BILDER\xbtn_konstgrupp.gif") IN FRAME FRAME-BARABREDD NO-ERROR.
   /*
   MMBTN_KONSTGRP:LOAD-IMAGE("BILDER\xbtn_kopp.gif") IN FRAME FRAME-BARABREDD NO-ERROR.
   */
   MBTN_KONST:LOAD-IMAGE("BILDER\xbtn_konstruktion.gif") IN FRAME FRAME-BARABREDD NO-ERROR.
   MBTN_KONSTMTRL:LOAD-IMAGE("BILDER\xbtn_konstmtrl.gif") IN FRAME FRAME-BARABREDD NO-ERROR.
   MBTN_KONSTKALK:LOAD-IMAGE("BILDER\xbtn_konstkalk.gif") IN FRAME FRAME-BARABREDD NO-ERROR.
   MBTN_MADM:LOAD-IMAGE("BILDER\xbtn_mtrladm.gif") IN FRAME FRAME-BARABREDD NO-ERROR.
  
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE MBTN_KONSTGRP MBTN_KONST MBTN_KONSTMTRL MBTN_KONSTKALK MBTN_MADM 
      WITH FRAME FRAME-BARABREDD IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BARABREDD}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

