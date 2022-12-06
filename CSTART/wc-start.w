&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: wc-start.w

  Description: ANGE FÖRETAGSSTART

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
{VALDBTEMP.I} 
DEFINE VARIABLE runvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE startvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER FORMAT "x(78)" NO-UNDO.
DEFINE VARIABLE finnsfil AS LOGICAL NO-UNDO.
DEFINE VARIABLE cWorkDirectory AS CHARACTER NO-UNDO.
DEFINE VARIABLE intdir AS INTEGER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-START Btn_OK Btn_Avb 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-START 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Avb AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 12 BY 1.5
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 12 BY 1.5
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-START AS CHARACTER FORMAT "X(256)":U 
     LABEL "Företag" 
     VIEW-AS FILL-IN 
     SIZE 32.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-START AT ROW 5.25 COL 13 COLON-ALIGNED
     Btn_OK AT ROW 10 COL 13
     Btn_Avb AT ROW 10 COL 33
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 60.75 BY 12.79.


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
         TITLE              = "Ange företagsid!"
         HEIGHT             = 12.79
         WIDTH              = 60.75
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ange företagsid! */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ange företagsid! */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Avb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Avb C-Win
ON CHOOSE OF Btn_Avb IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   FILL-IN-START = INPUT FRAME {&FRAME-NAME} FILL-IN-START.
   {&WINDOW-NAME}:HIDDEN = TRUE.
   {&WINDOW-NAME}:MOVE-TO-BOTTOM ().
   RUN ok_UI.
   RETURN.
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
   BTN_OK:LOAD-IMAGE ("BILDER\btn_ok.gif") NO-ERROR.
   BTN_AVB:LOAD-IMAGE ("BILDER\btn_avb.gif") NO-ERROR.                  
   RUN enable_UI.
   
   RUN filkoll_UI.
   IF finnsfil = FALSE THEN DO: 
      {&WINDOW-NAME}:HIDDEN = FALSE.
      {&WINDOW-NAME}:MOVE-TO-TOP ().     
   END.   
   ELSE DO:
      RUN ok_UI.      
   END.
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
  DISPLAY FILL-IN-START 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FILL-IN-START Btn_OK Btn_Avb 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE filkoll_UI C-Win 
PROCEDURE filkoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   finnsfil = FALSE.
   FILE-INFO:FILE-NAME = "wc-start.r".
   IF FILE-INFO:FULL-PATHNAME = ? THEN  FILE-INFO:FILE-NAME = "wc-start.w".
   intdir = R-INDEX(FILE-INFO:FULL-PATHNAME,"\").   
   cWorkDirectory = SUBSTRING(FILE-INFO:FULL-PATHNAME, 1, intdir - 1).
   intdir = R-INDEX(cWorkDirectory,"\").   
   cWorkDirectory = SUBSTRING(cWorkDirectory, 1, intdir ).  
   INPUT FROM OS-DIR(cWorkDirectory) NO-ECHO.
   REPEAT:
      SET filnamn ^ ^. 
      IF filnamn MATCHES "foretagstart.*" THEN DO: 
         finnsfil = TRUE.         
         FILL-IN-START = ENTRY(2, filnamn, ".").   
         LEAVE.              
      END.
   END.
   INPUT CLOSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok_UI C-Win 
PROCEDURE ok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   RUN WC-CHECK.P (INPUT FILL-IN-START, OUTPUT runvar, OUTPUT TABLE valdbtemp).  
  
   startvar = runvar.
   RUN sok_UI. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sok_UI C-Win 
PROCEDURE sok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF startvar = "" THEN DO:
      MESSAGE "Du måste ange ett id." VIEW-AS ALERT-BOX.
      quit.
   END.
   IF startvar = ? THEN DO:
      MESSAGE "Du måste ange rätt id." VIEW-AS ALERT-BOX.
      quit.
   END.
   /*
   runvar = SEARCH(startvar).
   */
   IF runvar = ? THEN DO:
      MESSAGE "Kontakta Elpool i Umeå AB 090-184540." VIEW-AS ALERT-BOX.
      RETURN.
   END.  
   IF finnsfil = FALSE THEN DO:
      OUTPUT TO VALUE(cWorkDirectory + "foretagstart." + FILL-IN-START) NO-ECHO.
      OUTPUT CLOSE.
   END.
   RUN VALDBGURU.w (INPUT runvar,INPUT TABLE valdbtemp).
   /*
   RUN VALUE(startvar).
  */
   
   QUIT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

