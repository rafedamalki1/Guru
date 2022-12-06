&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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
SESSION:SUPPRESS-WARNINGS = YES.
/*ALLA*/
DEFINE VARIABLE ttbuffcopyh AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE Guru.SharedVariable:btnupph AS HANDLE NO-UNDO.
{WHANDLTEMP.I}
   

DEFINE NEW SHARED TEMP-TABLE visa NO-UNDO
   FIELD UT AS CHARACTER    
   FIELD TYP AS CHARACTER       
   FIELD ORDNING AS INTEGER
   FIELD UPPFOLJVAL AS INTEGER
   FIELD KUURVAL AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   INDEX ORDNING IS PRIMARY ORDNING KUURVAL
   INDEX UT UT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_UPP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES visa

/* Definitions for BROWSE BRW_UPP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UPP visa.UT visa.TYP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPP 
&Scoped-define QUERY-STRING-BRW_UPP FOR EACH visa NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UPP OPEN QUERY BRW_UPP FOR EACH visa NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UPP visa
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPP visa


/* Definitions for FRAME FRAME-UPP                                      */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-UPP ~
    ~{&OPEN-QUERY-BRW_UPP}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE IMAGE IMAGE-4
     FILENAME "BILDER/xbgmenu.gif":U TRANSPARENT
     SIZE 5.5 BY 2.

DEFINE BUTTON BTN_AVB-9 AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA-9 
     LABEL "Visa" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UPP FOR 
      visa SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPP C-Win _STRUCTURED
  QUERY BRW_UPP NO-LOCK DISPLAY
      visa.UT COLUMN-LABEL "Lista" FORMAT "X(50)":U
      visa.TYP COLUMN-LABEL "Typ" FORMAT "X(4)":U WIDTH 4.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 48.25 BY 23.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     IMAGE-4 AT ROW 1 COL 120.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-UPP
     BRW_UPP AT ROW 3 COL 39.38
     BTN_VISA-9 AT ROW 8 COL 111
     BTN_AVB-9 AT ROW 25.83 COL 111
     "Funktioner:" VIEW-AS TEXT
          SIZE 14.5 BY .92 AT ROW 6.5 COL 111
          FGCOLOR 1 FONT 17
     "Uppföljning:" VIEW-AS TEXT
          SIZE 17.63 BY .92 AT ROW 1 COL 2.25
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.5
         SIZE 125 BY 25.92.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ansvarigtemp T "?" NO-UNDO temp-db ansvarigtemp
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: avdelningtemp T "?" NO-UNDO temp-db avdelningtemp
      TABLE: depatemp T "?" NO-UNDO temp-db depatemp
      TABLE: faktplantemp T "?" NO-UNDO temp-db faktplantemp
      TABLE: godkannartemp T "?" NO-UNDO temp-db godkannartemp
      TABLE: jurperstemp T "?" NO-UNDO temp-db jurperstemp
      TABLE: markpers T "?" NO-UNDO temp-db markpers
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: plannrtemp T "?" NO-UNDO temp-db plannrtemp
      TABLE: spec_mtrl T "?" NO-UNDO temp-db spec_mtrl
      TABLE: urberedningtemp T "?" NO-UNDO temp-db urberedningtemp
      TABLE: urstorntemp T "?" NO-UNDO temp-db urstorntemp
      TABLE: urvardtemp T "?" NO-UNDO temp-db urvardtemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: utvaldfasttemp T "?" NO-UNDO temp-db utvaldfasttemp
      TABLE: valberedningtemp T "?" NO-UNDO temp-db valberedningtemp
      TABLE: valdaaotemp T "?" NO-UNDO temp-db valdaaotemp
      TABLE: valdfasttemp T "?" NO-UNDO temp-db valdfasttemp
      TABLE: valperstemp T "?" NO-UNDO temp-db valperstemp
      TABLE: valplantemp T "?" NO-UNDO temp-db valplantemp
      TABLE: valvardtemp T "?" NO-UNDO temp-db valvardtemp
      TABLE: vavdelningtemp T "?" NO-UNDO temp-db vavdelningtemp
      TABLE: vfaktplantemp T "?" NO-UNDO temp-db vfaktplantemp
      TABLE: visa T "?" NO-UNDO temp-db visa
      TABLE: vomrtemp T "?" NO-UNDO temp-db vomrtemp
      TABLE: vstorntemp T "?" NO-UNDO temp-db vstorntemp
      TABLE: xgurutemp T "?" NO-UNDO temp-db xgurutemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-UPP:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FRAME FRAME-UPP
                                                                        */
/* BROWSE-TAB BRW_UPP TEXT-1 FRAME-UPP */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPP
/* Query rebuild information for BROWSE BRW_UPP
     _TblList          = "Temp-Tables.visa"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.visa.UT
"visa.UT" "Lista" "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.visa.TYP
"visa.TYP" "Typ" "X(4)" "character" ? ? ? ? ? ? no ? no no "4.25" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-UPP
/* Query rebuild information for FRAME FRAME-UPP
     _Query            is NOT OPENED
*/  /* FRAME FRAME-UPP */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME BRW_UPP

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:   
   
     
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
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
   RUN btnupp0_UI.   
   RUN frame_UI (INPUT "upp").
   
   
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:HIDDEN = FALSE. 
   {&WINDOW-NAME}:MOVE-TO-TOP (). 
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI C-Win 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnupp0_UI C-Win 
PROCEDURE btnupp0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   RUN btnuppstart_UI.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnuppstart_UI C-Win 
PROCEDURE btnuppstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   HIDE FRAME FRAME-START.

   IF NOT VALID-HANDLE(Guru.SharedVariable:btnupph) THEN DO:
      RUN btnupp_UI.
   END.  
   ELSE RUN franstart_UI IN Guru.SharedVariable:btnupph. 
   RUN frame_UI (INPUT "UPP").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnupp_UI C-Win 
PROCEDURE btnupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 0.   
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr, C-WIN:HANDLE).
   ordningnr = ordningnr + 1. 
   RUN whandle_UI (INPUT ordningnr, BRW_UPP:HANDLE IN FRAME FRAME-UPP).  
   ordningnr = ordningnr + 1.    
   RUN whandle_UI (INPUT ordningnr, BTN_VISA-9:HANDLE IN FRAME FRAME-UPP).  
   
   RUN xUPPMENY.P PERSISTENT SET Guru.SharedVariable:btnupph (INPUT THIS-PROCEDURE,INPUT TABLE whandltemp).                                         
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE IMAGE-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BRW_UPP BTN_VISA-9 BTN_AVB-9 
      WITH FRAME FRAME-UPP IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-UPP}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frame_UI C-Win 
PROCEDURE frame_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS CHARACTER NO-UNDO.
   ASSIGN
   FRAME FRAME-UPP:HIDDEN = TRUE.   
   IF vilkenframe = "UPP" THEN FRAME FRAME-UPP:HIDDEN = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI C-Win 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


