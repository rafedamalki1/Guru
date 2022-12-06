&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File       : tt-viewer.w

  Description: shows a list of all dynamic temp-tables in the current
               Progress session.
               Usefull for debugging dynamic applications. 

  Author     : Peter van Dam (Netsetup)

  Created    : May, 2001

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

DEFINE TEMP-TABLE dynTt
  FIELD iTable AS INT COLUMN-LABEL "Nr." FORMAT ">>9"
  FIELD TableHandle AS HANDLE
  FIELD TableName   AS CHARACTER FORMAT 'x(30)' COLUMN-LABEL "Table name"
  FIELD iRecords    AS INTEGER COLUMN-LABEL "Records"
  INDEX TableHandle IS UNIQUE TableHandle
  INDEX iTtable IS PRIMARY UNIQUE iTable
  INDEX TableName   TableName.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dynTt

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 dynTt.iTable dynTt.tableName dynTt.iRecords   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH dynTt.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 dynTt
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 dynTt


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 butt-destroy butt-refresh 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON butt-destroy 
     LABEL "&Destroy All" 
     SIZE 15 BY 1.14.

DEFINE BUTTON butt-refresh 
     LABEL "&Refresh" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      dynTt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _FREEFORM
  QUERY BROWSE-1 DISPLAY
      dynTt.iTable
      dynTt.tableName
      dynTt.iRecords
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 48 BY 16.19 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-1 AT ROW 1.48 COL 3
     butt-destroy AT ROW 18.14 COL 3
     butt-refresh AT ROW 18.14 COL 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "Dynamic Temp Table Viewer"
         HEIGHT             = 18.62
         WIDTH              = 51.6
         MAX-HEIGHT         = 34.33
         MAX-WIDTH          = 204.8
         VIRTUAL-HEIGHT     = 34.33
         VIRTUAL-WIDTH      = 204.8
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
   Size-to-Fit                                                          */
/* BROWSE-TAB BROWSE-1 1 DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:SCROLLABLE       = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH dynTt.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Temp Table Viewer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Temp Table Viewer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win
ON DEFAULT-ACTION OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
  DEFINE VARIABLE hQueryBrowse AS HANDLE     NO-UNDO.

  IF AVAILABLE dynTT THEN
  DO:
    RUN wQueryBrowse PERSISTENT SET hQueryBrowse (dynTt.TableName).
    RUN setTtHandle IN hQueryBrowse(dynTt.TableHandle).
    RUN realize IN hQueryBrowse.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butt-destroy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butt-destroy C-Win
ON CHOOSE OF butt-destroy IN FRAME DEFAULT-FRAME /* Destroy All */
DO:
  RUN destroyTts IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME butt-refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL butt-refresh C-Win
ON CHOOSE OF butt-refresh IN FRAME DEFAULT-FRAME /* Refresh */
DO:
  RUN initTt.
  RUN ENABLE_ui.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

IF SEARCH("netsetup.ico") NE ? THEN
  {&WINDOW-NAME}:LOAD-ICON("netsetup.ico").

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

ASSIGN {&browse-name}:DOWN = {&browse-name}:DOWN + 1.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN initTt.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyTts C-Win 
PROCEDURE destroyTts :
/*------------------------------------------------------------------------------
  Purpose   : Destroy all dynamic temp-tables currently in memory
              (if this button is pressed I guess those tt's should
              not have been there - use this button with caution)    
------------------------------------------------------------------------------*/

  DEFINE VARIABLE iTable AS INTEGER NO-UNDO.
  DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
  DEFINE VARIABLE iRecords AS INT NO-UNDO.
  DEFINE VARIABLE hTt AS HANDLE NO-UNDO.

  RUN initTt. /* First refresh dynTt */

  MESSAGE "This action can be fatal for your current Progress session" SKIP(1)
          "Are you sure you want to do this?"
          VIEW-AS ALERT-BOX WARNING
                  BUTTONS YES-NO TITLE "Destroy all dynamic temp-tables"
                  UPDATE lReply AS LOGICAL.

  IF lReply NE TRUE THEN
    RETURN NO-APPLY.

  SESSION:SET-WAIT-STATE("general").

  FOR EACH dynTt:
    DELETE OBJECT dynTt.TableHandle.
    DELETE dynTt.
  END.

  RUN enable_UI. /* show the result */

  SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win 
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
------------------------------------------------------------------------------*/
  ENABLE BROWSE-1 butt-destroy butt-refresh 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.

  ASSIGN butt-destroy:SENSITIVE = (CAN-FIND(FIRST dynTt)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTt C-Win 
PROCEDURE initTt :
/*------------------------------------------------------------------------------
  Purpose: Fill or refresh the contents of dynTt.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTable AS INTEGER NO-UNDO.
  DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
  DEFINE VARIABLE hBuffer AS HANDLE NO-UNDO.
  DEFINE VARIABLE iRecords AS INT NO-UNDO.
  DEFINE VARIABLE hTt AS HANDLE NO-UNDO.

  SESSION:SET-WAIT-STATE("general").

  hTt = SESSION:FIRST-BUFFER.
  DO WHILE VALID-HANDLE(hTt):

    IF VALID-HANDLE(hTt:TABLE-HANDLE) THEN
    DO:

      ASSIGN iRecords = 0.

      FIND dynTt WHERE dynTt.TableHandle = hTt:TABLE-HANDLE NO-ERROR.
      IF NOT AVAILABLE dynTt THEN
      DO:
        FIND LAST dynTt NO-ERROR.
        IF AVAILABLE dynTt THEN
          ASSIGN iTable = dynTt.iTable + 1.
        ELSE
          ASSIGN iTable = 1.

        CREATE dynTt.
        ASSIGN dynTt.iTable = iTable
               dynTt.tableHandle = hTt:TABLE-HANDLE
               dynTt.tableName   = IF tableHandle:NAME <> '':U 
                                   THEN tableHandle:NAME
                                   ELSE hTt:NAME.
      END.

      IF dynTt.tableHandle:HAS-RECORDS = TRUE THEN DO:

        CREATE QUERY hQuery.
        
        /* DO NOT use the DEFAULT-BUFFER-HANDLE since it is probably
           points to the current record in the process that owns the 
           dynamic temp-table! (I know the mess that can cause from
           my own experience!) */

        CREATE BUFFER hBuffer FOR TABLE dynTt.tableHandle:DEFAULT-BUFFER-HANDLE.

        hQuery:SET-BUFFERS(hBuffer).
        hQuery:QUERY-PREPARE(SUBSTITUTE("for each &1", hBuffer:NAME)).
        hQuery:QUERY-OPEN().
        hQuery:GET-FIRST().

        DO WHILE hBuffer:AVAILABLE:
           
          ASSIGN iRecords = iRecords + 1.

          hQuery:GET-NEXT().

        END.

        ASSIGN dynTt.iRecords = iRecords.

        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        DELETE OBJECT hBuffer.

      END.
      ELSE 
        ASSIGN dynTt.iRecords = 0.
      
    END.
    hTt = hTt:NEXT-SIBLING.
  END.

  /* Clean up temp-tables that have disappeared by now */

  FOR EACH dynTt:
    IF NOT VALID-HANDLE(dynTt.TableHandle) THEN
      DELETE dynTt.
  END.

  SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
