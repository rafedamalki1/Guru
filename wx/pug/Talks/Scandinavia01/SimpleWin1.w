&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 
  Description: 
  Author: 
  Created: 

----------------------------------------------------------------------*/

CREATE WIDGET-POOL.
define variable hBrowse as handle.
define variable hTableProgram as handle.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME CustFrame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fName fAddress fCity fState fPostalCode ~
fCountry BtnOK BtnGet BtnDone RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fName fAddress fCity fState fPostalCode ~
fCountry 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnGet 
     LABEL "&Get" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BtnOK 
     LABEL "&Save" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fAddress AS CHARACTER FORMAT "X(25)":U 
     LABEL "Address" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fCity AS CHARACTER FORMAT "X(25)":U 
     LABEL "City" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fCountry AS CHARACTER FORMAT "X(25)":U 
     LABEL "Country" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fName AS CHARACTER FORMAT "X(25)":U 
     LABEL "Name" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fPostalCode AS CHARACTER FORMAT "X(25)":U 
     LABEL "Zip" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE fState AS CHARACTER FORMAT "X(25)":U 
     LABEL "State" 
     VIEW-AS FILL-IN 
     SIZE 35 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 72 BY 11.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME CustFrame
     fName AT ROW 9.57 COL 22 COLON-ALIGNED
     fAddress AT ROW 10.76 COL 22 COLON-ALIGNED
     fCity AT ROW 11.95 COL 22 COLON-ALIGNED
     fState AT ROW 13.14 COL 22 COLON-ALIGNED
     fPostalCode AT ROW 14.33 COL 22 COLON-ALIGNED
     fCountry AT ROW 15.48 COL 22 COLON-ALIGNED
     BtnOK AT ROW 17.91 COL 10
     BtnGet AT ROW 17.91 COL 34
     BtnDone AT ROW 17.91 COL 58
     RECT-1 AT ROW 8.86 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.4 BY 19.81
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Customer TempTable"
         HEIGHT             = 19.81
         WIDTH              = 78.4
         MAX-HEIGHT         = 19.81
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 19.81
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
/* SETTINGS FOR FRAME CustFrame
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME CustFrame
/* Query rebuild information for FRAME CustFrame
     _Query            is NOT OPENED
*/  /* FRAME CustFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer TempTable */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer TempTable */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME CustFrame /* Done */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnGet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnGet C-Win
ON CHOOSE OF BtnGet IN FRAME CustFrame /* Get */
DO:
  define variable curfield  as handle.
  define variable FieldList as char.
  define variable ValueList as char.
  define variable FOrmatList as char.
  define variable LabelList as char.
  define variable DataTypes as char.
  define variable hTempTable as handle.
  define variable hTTBuffer  as handle.
  define variable hQuery    as handle.
  define variable Cntr      as int.
  assign
  curfield = frame custframe:handle
  curfield = curfield:first-child
  curfield = curfield:first-tab-item.
  
  do while valid-handle(curfield):
    if CurField:type = "fill-in"
      then 
      assign
      cntr = cntr + 1
      FieldList = if Cntr <> 1 then FieldList 
                + chr(1) + CurField:name else CurField:name
      ValueList = if Cntr <> 1 then ValueList   
                + chr(1) + CurField:screen-value else CurField:screen-value
      DataTypes = if Cntr <> 1 then DataTypes  
                + chr(1) + CurField:data-type else curField:data-type
      FormatList = if Cntr <> 1 then FormatList 
                + chr(1) + CurField:format else CurField:format
      LabelList = if Cntr <> 1 then LabelList 
                + chr(1) + CurField:label else CurField:label.
    CurField = CurField:next-sibling.
  end.
  
  run TempTable.p persistent set hTableProgram.
  run BuildTable  in hTableProgram (FieldLIst,DataTypes,FormatList,LabelList).

  run FindRecord  in hTableProgram (FIeldList,ValueList,
                                    output table-handle hTempTable).

  create query hQuery.
  hTTBuffer = hTempTable:default-buffer-handle.
  hQuery:set-buffers(hTTBUffer).
  
  hQuery:query-prepare("for each " + hTempTable:name).
  

  /* Now assign the query to the browse */
  hBrowse:query = hQuery.

  /* Add all the columns from the buffer except the count field */
  hBrowse:add-columns-from(hTTBuffer).

  hQuery:query-open().
  hBrowse:visible = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME CustFrame /* Save */
DO:
  define variable curfield  as handle.
  define variable FieldList as char.
  define variable ValueList as char.
  define variable DataTypes as char.
  assign
  curfield = frame custframe:handle
  curfield = curfield:first-child
  curfield = curfield:first-tab-item.

  do while valid-handle(curfield):
    if CurField:type = "fill-in"
      then 
      assign
      FieldList = if FieldList <> "" then FieldList 
                + chr(1) + CurField:name else CurField:name
      ValueList = if ValueList <> "" then ValueList   
                + chr(1) + CurField:screen-value else CurField:screen-value
      DataTypes = if DataTypes <> "" then DataTypes  
                + chr(1) + CurField:data-type else curField:data-type.
    CurField = CurField:next-sibling.
  end.
  
  run TempTable.p persistent set hTableProgram.
  run BuildTable     in hTableProgram (FieldLIst,DataTypes).
  run PopulateTable  in hTableProgram (ValueList).
  run CreateRecord   in hTableProgram.
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
CREATE BROWSE hBrowse
  ASSIGN 
    WIDTH      = 70
    HEIGHT     = 6
    EXPANDABLE = NO
    COLUMN     = 4
    ROW        = 1
    FRAME      = FRAME {&frame-name}:HANDLE
    READ-ONLY  = NO
    SENSITIVE  = YES
    .
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
  DISPLAY fName fAddress fCity fState fPostalCode fCountry 
      WITH FRAME CustFrame IN WINDOW C-Win.
  ENABLE fName fAddress fCity fState fPostalCode fCountry BtnOK BtnGet BtnDone 
         RECT-1 
      WITH FRAME CustFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-CustFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

