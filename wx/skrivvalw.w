&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEFINE INPUT PARAMETER liggande AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE valut AS INTEGER NO-UNDO.
DEFINE VARIABLE utskriv AS LOGICAL NO-UNDO.
DEFINE VARIABLE utvar AS LOGICAL NO-UNDO.
DEFINE SHARED TEMP-TABLE tidut
   FIELD UT AS CHARACTER FORMAT "X(132)".


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 23.25 BY 1.33 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 1.33
         WIDTH              = 23.25
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
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
  {win_M_START.I} 
   RUN enable_UI.     
   IF vartpro = "berlist44" THEN vartpro = vartpro.
   ELSE SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF vartpro NE "MED" THEN DO: 
      FIND FIRST tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         FRAME {&FRAME-NAME}:HIDDEN = FALSE.          
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE felmeddtemp THEN DO:            
            /*Kör ursprunglig kod - Ingen felmeddtemp*/
            RUN skrivligg_UI.
            IF utskriv = TRUE THEN DO:
               musz = FALSE. 
               RUN skrivval_UI.
            END.
            ELSE IF utskriv = FALSE THEN musz = TRUE.     
         END.
         ELSE DO:            
            /*Visa och skrivut från Berlista.w och Välj skrivare*/
            IF felmeddtemp.VAL = 4 OR felmeddtemp.VAL = 1 THEN DO:
               RUN skrivligg_UI.
               IF utskriv = TRUE THEN DO:
                  musz = FALSE. 
                  RUN skrivval_UI.
               END.
               ELSE IF utskriv = FALSE THEN musz = TRUE.                                     
            END.
         END.
      END.
      ELSE DO:
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE felmeddtemp THEN DO:
            /*Kör ursprunglig kod - Ingen felmeddtemp*/
            RUN NAVUT.W (OUTPUT valut).                   
            IF valut = 1 THEN DO:                                     
               RUN skrivligg_UI.
               IF utskriv = TRUE THEN DO:
                  musz = FALSE. 
                  RUN skrivval_UI.
               END.
               ELSE musz = TRUE.              
            END.
            ELSE IF valut = 2 THEN DO: /*som epost */
               utvar = TRUE.         
               RUN EMEDDw.W (INPUT utvar).
               musz = TRUE.
            END.
            ELSE IF valut = 3 THEN DO:
               utvar = TRUE.         
               CLIPBOARD:MULTIPLE = TRUE.
               CLIPBOARD:ITEMS-PER-ROW = 1.
               FOR EACH tidut NO-LOCK:
                  CLIPBOARD:VALUE = tidut.UT.   
               END.    
               CLIPBOARD:MULTIPLE = FALSE.
               musz = TRUE.
            END.
            ELSE IF valut = 4 THEN DO: /* epost program */
               utvar = TRUE.
               RUN EMEDDSPROG.P.
               musz = TRUE.
            END.
         END.
         ELSE DO:            
            /*Skriv ut från btn_skriv i berlist4 = val, eller val av skrivare*/
            IF felmeddtemp.VAL = 3 OR felmeddtemp.VAL = 4 THEN DO:
               RUN NAVUT.W (OUTPUT valut).
               IF valut = 1 THEN DO:                      
                  musz = FALSE.
                  RUN EKLOGS.P.
               END.
               ELSE IF valut = 2 THEN DO:
                  utvar = TRUE.         
                  RUN EMEDDw.W (INPUT utvar).
                  musz = TRUE.
               END.
               ELSE IF valut = 3 THEN DO:
                  utvar = TRUE.         
                  CLIPBOARD:MULTIPLE = TRUE.
                  CLIPBOARD:ITEMS-PER-ROW = 1.
                  FOR EACH tidut NO-LOCK:
                     CLIPBOARD:VALUE = tidut.UT.   
                  END.    
                  CLIPBOARD:MULTIPLE = FALSE.
                  musz = TRUE.
               END.
               ELSE IF valut = 4 THEN DO: /* epost program */
                  utvar = TRUE.
                  RUN EMEDDSPROG.P.
                 musz = TRUE.
               END.
            END.
            IF felmeddtemp.VAL = 1 THEN DO:
               /*Skriv ut från btn_skriv i berlista = inga val*/
               musz = FALSE.
               RUN EKLOGS.P.
            END.            
         END.
      END.      
   END.
   ELSE DO:
      SYSTEM-DIALOG PRINTER-SETUP UPDATE utskriv.
      IF utskriv = TRUE THEN musz = FALSE. 
      ELSE musz = TRUE. 
      RUN skrivval_UI.
   END.  
   {musarrow.i}   
   {win_M_SLUT.I}
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivligg_UI c-win
PROCEDURE skrivligg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                  
      {SKRIVLS.I}
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivval_UI c-win
PROCEDURE skrivval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>                                  
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rb-print AS CHARACTER NO-UNDO.
   rb-print = SESSION:PRINTER-NAME.
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKRIVAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT "",INPUT rb-print, INPUT-OUTPUT globsidl,INPUT-OUTPUT globsids).
   END.
   ELSE DO:
      RUN SKRIVAPP.P 
      (INPUT "",INPUT rb-print, INPUT-OUTPUT globsidl,INPUT-OUTPUT globsids).
   END.          
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
