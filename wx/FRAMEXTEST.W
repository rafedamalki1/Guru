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
{ALLDEF.I}
DEFINE VARIABLE hframeproc AS HANDLE NO-UNDO.
DEFINE VARIABLE hfillinproc AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE logcheck AS LOGICAL NO-UNDO.
DEFINE VARIABLE htab AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-4 ~
BTN-OK BTN-AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 9.5 BY 1.25.

DEFINE BUTTON BTN-OK 
     LABEL "OK" 
     SIZE 9.5 BY 1.25.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 2" 
     VIEW-AS FILL-IN 
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 3" 
     VIEW-AS FILL-IN 
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 4" 
     VIEW-AS FILL-IN 
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 15 BY 4.

DEFINE BUTTON BUTTON-2 
     LABEL "Button 2" 
     SIZE 9 BY 1.25.

DEFINE BUTTON BUTTON-3 
     LABEL "Button 3" 
     SIZE 18 BY 2.25.

DEFINE BUTTON BUTTON-4 
     LABEL "Btn 4" 
     SIZE 7 BY 1.25.

DEFINE VARIABLE FILL-IN-10 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 2.5 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 11" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 2.5 NO-UNDO.

DEFINE VARIABLE FILL-IN-12 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35 BY 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 2.5 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 5" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fill 6" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.5 BY 1.25 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-1 AT ROW 1.75 COL 20.5
     FILL-IN-2 AT ROW 3.5 COL 20.5
     FILL-IN-3 AT ROW 5.25 COL 20.5
     FILL-IN-4 AT ROW 7 COL 20.5
     BTN-OK AT ROW 12.5 COL 26.5
     BTN-AVB AT ROW 12.5 COL 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 48.38 BY 18.08
         TITLE "DEFAULT-FRAME"
         CANCEL-BUTTON BTN-AVB.

DEFINE FRAME FRAME-F
     FILL-IN-7 AT ROW 1.25 COL 8.5 NO-LABEL
     FILL-IN-8 AT ROW 2.75 COL 5.5 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 21.5 ROW 14
         SIZE 25.63 BY 3.96
         TITLE "Frame F".

DEFINE FRAME FRAME-B
     FILL-IN-5 AT ROW 1.5 COL 3.5
     FILL-IN-6 AT ROW 2.75 COL 3.5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 4 ROW 8.25
         SIZE 25.38 BY 3.96
         TITLE "Frame B".

DEFINE FRAME FRAME-A
     FILL-IN-9 AT ROW 1.25 COL 1.5 COLON-ALIGNED NO-LABEL
     FILL-IN-10 AT ROW 1.25 COL 6.5 COLON-ALIGNED NO-LABEL
     FILL-IN-11 AT ROW 1.25 COL 33 COLON-ALIGNED
     FILL-IN-12 AT ROW 4 COL 1 COLON-ALIGNED NO-LABEL
     BUTTON-1 AT ROW 6.5 COL 3
     BUTTON-2 AT ROW 6.5 COL 20
     BUTTON-3 AT ROW 8 COL 20
     BUTTON-4 AT ROW 6.25 COL 31
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 52.75 ROW 3.33
         SIZE 39.5 BY 13.75
         TITLE "Frame A".


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
         HEIGHT             = 18.96
         WIDTH              = 96.38
         MAX-HEIGHT         = 18.96
         MAX-WIDTH          = 96.38
         VIRTUAL-HEIGHT     = 18.96
         VIRTUAL-WIDTH      = 96.38
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-B:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-F:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-B:MOVE-AFTER-TAB-ITEM (BTN-AVB:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-B:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-F:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-A
   Custom                                                               */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME FRAME-B
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-6 IN FRAME FRAME-B
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-F
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME FRAME-F
   ALIGN-L                                                              */
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


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A C-Win
ON LEFT-MOUSE-CLICK OF FRAME FRAME-A /* Frame A */
DO:
  MESSAGE "Jiha" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-OK C-Win
ON CHOOSE OF BTN-OK IN FRAME DEFAULT-FRAME /* OK */
DO:
/*    RUN fillincontrol_UI IN hframeproc. */
   
   RUN checkfillin_UI IN hfillinproc (OUTPUT logcheck).
   IF logcheck = FALSE THEN APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON ENTRY OF FILL-IN-1 IN FRAME DEFAULT-FRAME /* FILL-IN-1 */
DO:
/*    DEFINE VARIABLE tempch AS CHARACTER NO-UNDO.            */
/*    tempch = ENTRY(1,FILL-IN-1:PRIVATE-DATA, ";") NO-ERROR. */
/*    IF tempch NE "" THEN MESSAGE tempch.                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* &Scoped-define SELF-NAME FILL-IN-1                           */
/* &ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win    */
/* ON ENTRY OF FILL-IN-2 IN FRAME DEFAULT-FRAME /* FILL-IN-1 */ */
/* DO:                                                          */
/*    MESSAGE "Heopp...".                                       */
/* END.                                                         */
/*                                                              */
/* /* _UIB-CODE-BLOCK-END */                                    */
/* &ANALYZE-RESUME                                              */


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON MOUSE-SELECT-CLICK OF FILL-IN-2 IN FRAME DEFAULT-FRAME /* FILL-IN-1 */
DO:
   DEFINE VARIABLE tempfilinfo AS CHARACTER NO-UNDO.
   tempfilinfo = "Filnamn: " + 
      SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)) + CHR(10).             
   tempfilinfo = tempfilinfo + "Storlek: " + STRING((FILE-INFO:FILE-SIZE) / 1000) + " kB" + CHR(10).
   tempfilinfo = tempfilinfo + "Senast ändrad: " + STRING(FILE-INFO:FILE-MOD-DATE) + " " +
      STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS").
   MESSAGE tempfilinfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON LEAVE OF FILL-IN-1 IN FRAME DEFAULT-FRAME /* FILL-IN-1 */
DO:
   FILL-IN-1 = INPUT FILL-IN-1.  
   IF FILL-IN-1:PRIVATE-DATA = ? OR FILL-IN-1:PRIVATE-DATA = "" THEN DO:
      FILL-IN-1:PRIVATE-DATA = FILL-IN-1:SCREEN-VALUE.
   END.    
   ELSE DO: 
      FILL-IN-1:PRIVATE-DATA = FILL-IN-1:PRIVATE-DATA + ";" + FILL-IN-1:SCREEN-VALUE.
   END.
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
   RUN enable_UI.
   RUN FILLINXCONTROL.P PERSISTENT SET hfillinproc.
  
/*    RUN addfillinchar_UI IN hfillinproc                                               */
/*       (INPUT FILL-IN-1:HANDLE IN FRAME DEFAULT-FRAME, INPUT "Du måste skriva rätt!", */
/*        INPUT "Mikael").                                                              */
/*                                                                                      */
/*    RUN addfillincheck_UI IN hfillinproc                                              */
/*       (INPUT FILL-IN-5:HANDLE IN FRAME FRAME-B, INPUT "Du måste skriva något!").     */
/*                                                                                      */
/*    FRAME FRAME-B:VISIBLE = FALSE.                                                    */

/*    ON TAB ANYWHERE                                          */
/*    DO:                                                      */
/*       IF SELF:NAME = "FILL-IN-4" THEN DO:                   */
/*          DEFINE VARIABLE fa AS HANDLE NO-UNDO.              */
/*          DEFINE VARIABLE fd AS HANDLE NO-UNDO.              */
/*          fd = FRAME DEFAULT-FRAME:WINDOW.                   */
/* /*          MESSAGE fd:KEEP-FRAME-Z-ORDER fd:SENSITIVE. */  */
/* /*                                                      */  */
/* /*          fa = fd:FIRST-CHILD.                        */  */
/* /*          DO WHILE VALID-HANDLE(fa) = TRUE:           */  */
/* /*             IF fa:NAME = "FRAME-A" THEN DO:          */  */
/* /*                                                      */  */
/* /*             END.                                     */  */
/* /*             MESSAGE fa:NAME fa:TYPE fa:SENSITIVE.    */  */
/* /*             fa = fa:NEXT-SIBLING.                    */  */
/* /*          END.                                        */  */
/*                                                             */
/*          BTN-OK:TAB-STOP IN FRAME DEFAULT-FRAME = FALSE.    */
/*          APPLY "LEFT-MOUSE-CLICK" TO FRAME FRAME-A.         */
/*          APPLY "ENTRY" TO FILL-IN-9 IN FRAME FRAME-A.       */
/*       END.                                                  */
/*       ELSE IF SELF:NAME = "FILL-IN-9" THEN DO:              */
/*          APPLY "ENTRY" TO FILL-IN-1 IN FRAME DEFAULT-FRAME. */
/*       END.                                                  */
/*    END.                                                     */

/*    RUN FRAMEXCONTROL.P PERSISTENT SET hframeproc (INPUT FRAME DEFAULT-FRAME:HANDLE). */
   ON MOUSE-MENU-DBLCLICK OF CURRENT-WINDOW PERSISTENT RUN visafil_UI IN THIS-PROCEDURE.

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
  DISPLAY FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-4 BTN-OK BTN-AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FILL-IN-9 FILL-IN-10 FILL-IN-11 FILL-IN-12 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  ENABLE FILL-IN-9 FILL-IN-10 FILL-IN-11 FILL-IN-12 BUTTON-1 BUTTON-2 BUTTON-3 
         BUTTON-4 
      WITH FRAME FRAME-A IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY FILL-IN-5 FILL-IN-6 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  ENABLE FILL-IN-5 FILL-IN-6 
      WITH FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  DISPLAY FILL-IN-7 FILL-IN-8 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  ENABLE FILL-IN-7 FILL-IN-8 
      WITH FRAME FRAME-F IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-F}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visafil_UI C-Win  
PROCEDURE visafil_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempfilinfo AS CHARACTER NO-UNDO.
   FILE-INFO:FILE-NAME = "DYNBRW.P".
   tempfilinfo = "Filnamn: " + 
      SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)) + CHR(10).             
   tempfilinfo = tempfilinfo + "Storlek: " + STRING((FILE-INFO:FILE-SIZE) / 1000) + " kB" + CHR(10).
   tempfilinfo = tempfilinfo + "Senast ändrad: " + STRING(FILE-INFO:FILE-MOD-DATE) + " " +
      STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS").
   MESSAGE tempfilinfo.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
