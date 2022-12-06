&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports2000       PROGRESS
*/
&Scoped-define WINDOW-NAME CodeWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _TEMP-TABLE 
/* ***********Included Temp-Table & Buffer definitions **************** */

DEFINE TEMP-TABLE ttState LIKE State.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS CodeWin 
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
DEFINE VARIABLE hCodeSupport AS HANDLE NO-UNDO.
DEFINE VARIABLE hCodeSet AS HANDLE NO-UNDO.
DEFINE VARIABLE hStateQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hRepBrowse AS HANDLE NO-UNDO.
DEFINE VARIABLE hRepQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hCustomQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE hCustomBrowse AS HANDLE NO-UNDO.
DEFINE VARIABLE hCustomSet AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME CodeFrame
&Scoped-define BROWSE-NAME StateBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer ttState

/* Definitions for BROWSE StateBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-StateBrowse ttState.Region ttState.State ~
ttState.StateName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-StateBrowse 
&Scoped-define QUERY-STRING-StateBrowse FOR EACH ttState NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-StateBrowse OPEN QUERY StateBrowse FOR EACH ttState NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-StateBrowse ttState
&Scoped-define FIRST-TABLE-IN-QUERY-StateBrowse ttState


/* Definitions for FRAME CodeFrame                                      */
&Scoped-define FIELDS-IN-QUERY-CodeFrame Customer.Name Customer.City ~
Customer.CustNum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-CodeFrame Customer.Name ~
Customer.City Customer.CustNum 
&Scoped-define ENABLED-TABLES-IN-QUERY-CodeFrame Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-CodeFrame Customer
&Scoped-define OPEN-BROWSERS-IN-QUERY-CodeFrame ~
    ~{&OPEN-QUERY-StateBrowse}
&Scoped-define QUERY-STRING-CodeFrame FOR EACH Customer SHARE-LOCK
&Scoped-define OPEN-QUERY-CodeFrame OPEN QUERY CodeFrame FOR EACH Customer SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-CodeFrame Customer
&Scoped-define FIRST-TABLE-IN-QUERY-CodeFrame Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.Name Customer.City Customer.CustNum 
&Scoped-define ENABLED-TABLES Customer
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS cRegion cState StateBrowse 
&Scoped-Define DISPLAYED-FIELDS Customer.Name Customer.City ~
Customer.CustNum 
&Scoped-define DISPLAYED-TABLES Customer
&Scoped-define FIRST-DISPLAYED-TABLE Customer
&Scoped-Define DISPLAYED-OBJECTS cRegion cState 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR CodeWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cRegion AS CHARACTER FORMAT "X(256)":U 
     LABEL "Region" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "<select>","East","West","Central","South" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE cState AS CHARACTER FORMAT "X(256)":U 
     LABEL "State" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY StateBrowse FOR 
      ttState SCROLLING.

DEFINE QUERY CodeFrame FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE StateBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS StateBrowse CodeWin _STRUCTURED
  QUERY StateBrowse NO-LOCK DISPLAY
      ttState.Region FORMAT "x(8)":U
      ttState.State FORMAT "x(20)":U WIDTH 9.4
      ttState.StateName FORMAT "x(20)":U WIDTH 17.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 4.52 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME CodeFrame
     Customer.Name AT ROW 1.95 COL 44 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Customer.City AT ROW 1.95 COL 82 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     Customer.CustNum AT ROW 2 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     cRegion AT ROW 3.14 COL 12 COLON-ALIGNED
     cState AT ROW 3.14 COL 44 COLON-ALIGNED
     StateBrowse AT ROW 6.48 COL 90
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 130.2 BY 10.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttState T "?" ? sports2000 State
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW CodeWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Test Window for Code Tables"
         HEIGHT             = 10
         WIDTH              = 130.2
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 148.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 148.6
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
/* SETTINGS FOR WINDOW CodeWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME CodeFrame
                                                                        */
/* BROWSE-TAB StateBrowse cState CodeFrame */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CodeWin)
THEN CodeWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME CodeFrame
/* Query rebuild information for FRAME CodeFrame
     _TblList          = "sports2000.Customer"
     _Query            is OPENED
*/  /* FRAME CodeFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE StateBrowse
/* Query rebuild information for BROWSE StateBrowse
     _TblList          = "Temp-Tables.ttState"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ttState.Region
     _FldNameList[2]   > Temp-Tables.ttState.State
"State" ? ? "character" ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.ttState.StateName
"StateName" ? ? "character" ? ? ? ? ? ? no ? no no "17.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE StateBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME CodeWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CodeWin CodeWin
ON END-ERROR OF CodeWin /* Test Window for Code Tables */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CodeWin CodeWin
ON WINDOW-CLOSE OF CodeWin /* Test Window for Code Tables */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cRegion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cRegion CodeWin
ON VALUE-CHANGED OF cRegion IN FRAME CodeFrame /* Region */
DO:
  DEFINE VARIABLE hStateBuf AS HANDLE NO-UNDO.
    IF cRegion:SCREEN-VALUE NE "<select>" THEN
    DO:
        RUN fetchCustomTable IN hCodeSupport (INPUT "ttState",
            INPUT "State,StateName",
            INPUT "Region = '" + cRegion:SCREEN-VALUE + "'",
            OUTPUT DATASET-HANDLE hCustomSet).
    END.
    cState:LIST-ITEMS = "". /* Empty the old list if any. */
    CREATE QUERY hCustomQuery.
    hStateBuf = hCustomSet:GET-BUFFER-HANDLE("ttState").
    hCustomQuery:ADD-BUFFER(hStateBuf).
    hCustomQuery:QUERY-PREPARE("FOR EACH ttState").
    hCustomQuery:QUERY-OPEN().
    hCustomQuery:GET-FIRST().
    DO WHILE NOT hCustomQuery:QUERY-OFF-END:
        cState:ADD-LAST(hStateBuf:BUFFER-FIELD("StateName"):BUFFER-VALUE).
        hCustomQuery:GET-NEXT().
    END.
    cState:SCREEN-VALUE = cState:ENTRY(1).
    DELETE OBJECT hCustomQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME StateBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK CodeWin 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
    RUN shutdownCodeWindow.
    RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  RUN startupCodeWindow.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI CodeWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(CodeWin)
  THEN DELETE WIDGET CodeWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI CodeWin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-CodeFrame}
  GET FIRST CodeFrame.
  DISPLAY cRegion cState 
      WITH FRAME CodeFrame IN WINDOW CodeWin.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Name Customer.City Customer.CustNum 
      WITH FRAME CodeFrame IN WINDOW CodeWin.
  ENABLE Customer.Name Customer.City Customer.CustNum cRegion cState 
         StateBrowse 
      WITH FRAME CodeFrame IN WINDOW CodeWin.
  {&OPEN-BROWSERS-IN-QUERY-CodeFrame}
  VIEW CodeWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shutdownCodeWindow CodeWin 
PROCEDURE shutdownCodeWindow :
/*---------------------------------------------------------------------
Procedure: shutdownCodeWindow
Purpose: Cleanup supporting procedure and any other objects when deleting
the window.
---------------------------------------------------------------------*/
APPLY "CLOSE" TO hCodeSupport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE startupCodeWindow CodeWin 
PROCEDURE startupCodeWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN codeSupport.p PERSISTENT SET hCodeSupport.
    RUN fetchCodeTables IN hCodeSupport (INPUT "ttSalesRep,ttState",
    OUTPUT DATASET-HANDLE hCodeSet).
    CREATE QUERY hStateQuery.
    hStateQuery:ADD-BUFFER(hCodeSet:GET-BUFFER-HANDLE("ttState")).
    StateBrowse:QUERY IN FRAME CodeFrame = hStateQuery.
    hStateQuery:QUERY-PREPARE("FOR EACH ttState").
    hStateQuery:QUERY-OPEN().
    CREATE QUERY hRepQuery.
    hRepQuery:ADD-BUFFER(hCodeSet:GET-BUFFER-HANDLE("ttSalesRep")).
    CREATE BROWSE hRepBrowse ASSIGN
        QUERY = hRepQuery
        ROW-MARKERS = NO
        FRAME = FRAME CodeFrame:HANDLE
        HIDDEN = NO
        NO-VALIDATE = YES
        WIDTH = 74
        HEIGHT = 5
        ROW = 6
        SEPARATORS = YES
        SENSITIVE = YES.
    hRepBrowse:ADD-COLUMNS-FROM("ttSalesRep").
    hRepQuery:QUERY-PREPARE("FOR EACH ttSalesRep").
    hRepQuery:QUERY-OPEN().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

