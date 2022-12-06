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

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_KOPI-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ADM 
     LABEL "Administration" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AOF-2 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATG 
     LABEL "Åtgärder" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATT 
     LABEL "Attestera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BA 
     LABEL "Begär attest" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXP 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HAOF-2 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMP 
     LABEL "Import" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INAKTIV 
     LABEL "Aktiv/Inaktiv":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INK 
     IMAGE-UP FILE "bilder/xbtn_inkopsmal.gif":U
     LABEL "Inköp" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALK-2 
     IMAGE-UP FILE "BILDER\xbtn_bered.gif":U
     LABEL "Bereda":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPI-2 
     LABEL "Kopiera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LAS 
     IMAGE-UP FILE "bilder/xbtn_lastaber.gif":U
     LABEL "Låsta beredningar" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LIST 
     LABEL "Listor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SCHAKTP 
     IMAGE-UP FILE "bilder/xbtn_smalschakt.gif":U
     LABEL "Schaktprotokoll":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP-2 
     LABEL "Beredningshuvud":L 
     SIZE 14 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_KALK-2 AT ROW 7.96 COL 67 WIDGET-ID 22
     BTN_UPP-2 AT ROW 8.5 COL 67 WIDGET-ID 32
     BTN_LIST AT ROW 9.21 COL 67 WIDGET-ID 28
     BTN_SCHAKTP AT ROW 10.25 COL 67 WIDGET-ID 30
     BTN_INK AT ROW 10.71 COL 67 WIDGET-ID 20
     BTN_HAOF-2 AT ROW 11 COL 67 WIDGET-ID 14
     BTN_AOF-2 AT ROW 11 COL 67 WIDGET-ID 4
     BTN_ATG AT ROW 12.21 COL 67 WIDGET-ID 6
     BTN_KOPI-2 AT ROW 13.46 COL 67 WIDGET-ID 24
     BTN_INAKTIV AT ROW 14.71 COL 67 WIDGET-ID 18
     BTN_ADM AT ROW 16 COL 67 WIDGET-ID 2
     BTN_LAS AT ROW 16 COL 67 WIDGET-ID 26
     BTN_EXP AT ROW 16 COL 67 WIDGET-ID 12
     BTN_IMP AT ROW 16 COL 67 WIDGET-ID 16
     BTN_BA AT ROW 16 COL 67 WIDGET-ID 10
     BTN_ATT AT ROW 16 COL 67 WIDGET-ID 8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 135.63 BY 30.83 WIDGET-ID 100.


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
         HEIGHT             = 30.83
         WIDTH              = 135.63
         MAX-HEIGHT         = 30.83
         MAX-WIDTH          = 135.63
         VIRTUAL-HEIGHT     = 30.83
         VIRTUAL-WIDTH      = 135.63
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
/* SETTINGS FOR BUTTON BTN_ADM IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ADM:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_AOF-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AOF-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_ATG IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ATG:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_ATT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ATT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_BA IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_EXP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_HAOF-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_HAOF-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_IMP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_IMP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_INAKTIV IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_INAKTIV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_INK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_INK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_KALK-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KALK-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_LAS IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_LAS:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_LIST IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_LIST:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_SCHAKTP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_SCHAKTP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_UPP-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPP-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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
  ENABLE BTN_KOPI-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

