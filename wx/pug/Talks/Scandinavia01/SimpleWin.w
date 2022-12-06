&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 
  Description: 
  Author: 
  Created: 
  Comments: This routine is by no means a definitive maintenance program.
  It is simply a demonstration of how to use the dynamic temp table features.
  If you create variables on the screen which represent fields from any
  DB table, you can retrieve and save data from the database with 
  no hard-coded definitions anywhere, except for the name of the table
  in the preprocessor name below.
  Here are the rules for this application:
  Variable should represent fields from one table only.
  They should be only character fields in the DB.
  Each variable should be named after the field name, with a single 
  letter in front of the name, preferably "f", though this should not
  matter.
  Thus, fname, faddress, fcity, fstate, fpostalcode.
  When you run the program, you can search the DB for data by filling 
  in any of the fields with values. Choose retrieve and you should
  get data back from the DB. As you click on different rows in the 
  dynamic browser, you will see the data in the fields on the screen.
  If you type in new values, and press save, the data will be saved
  back to the DB.
----------------------------------------------------------------------*/

CREATE WIDGET-POOL.
&global-define TableName      customer

define variable hBrowse       as handle.
define variable hTempTable    as handle.
define variable hTTBuffer     as handle.
define variable hQuery        as handle.
define variable FieldList     as char.  
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
     LABEL "&Retrieve" 
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
         SIZE 79 BY 19.81
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
         WIDTH              = 79
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
ON CHOOSE OF BtnGet IN FRAME CustFrame /* Retrieve */
DO:
  define variable curfield  as handle.
  
  define variable ValueList as char.
  define variable FormatList as char.
  define variable LabelList as char.
  define variable DataTypes as char.
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
  
  if not valid-handle(hTableProgram) then do:
    run TempTable.p persistent set hTableProgram no-error.    
    dynamic-function("SetTableName" in hTableProgram, "{&TableName}").
  end.

  if valid-handle(hTableProgram) then do:
    run BuildTable  in hTableProgram (FieldList,DataTypes,FormatList,LabelList).
    run GetData     in hTableProgram (FieldList,ValueList,
                                    output table-handle hTempTable).
  end.
  /* create and open a query on the temp table retrieved from the DB */
  create query hQuery.
  hTTBuffer = hTempTable:default-buffer-handle.
  hQuery:set-buffers(hTTBuffer).
  
  hQuery:query-prepare("for each " + hTempTable:name).
  

  /* Now assign the query to the browse */
  hBrowse:query = hQuery.

  /* Add all the columns from the buffer except the DB Rowid field */
  hBrowse:add-columns-from(hTTBuffer,"DBRowid").

  hQuery:query-open().
  hBrowse:visible = yes.
  apply "home" to hBrowse.
  apply "entry" to hBrowse.
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
  define variable Formats   as char.
  define variable Labels    as char.
  define variable DBRowid   as rowid.
  define variable curTTFld  as handle.
  assign
  curfield = frame custframe:handle
  curfield = curfield:first-child
  curfield = curfield:first-tab-item.

  do while valid-handle(curfield):
    if CurField:type = "fill-in"
      then 
      assign
      /* pick the current field in the temp table record */
      CurTTFld = hTTBuffer:buffer-field(curfield:name)
      /* assign it the value of the screen variable */
      CurTTFld:buffer-value = curfield:screen-value
      /* build lists as indicated */
      FieldList = if FieldList <> "" then FieldList 
                + chr(1) + CurField:name else CurField:name
      ValueList = if ValueList <> "" then ValueList   
                + chr(1) + CurField:screen-value else CurField:screen-value
      DataTypes = if DataTypes <> "" then DataTypes  
                + chr(1) + CurField:data-type else curField:data-type
      Formats   = if Formats <> "" then Formats  
                + chr(1) + CurField:format else curField:format
      Labels    = if Labels <> "" then Labels  
                + chr(1) + CurField:label else curField:label
                              .
    CurField = CurField:next-sibling.
  end.
  
  if not valid-handle(hTableProgram)
    then do:
    run TempTable.p persistent set hTableProgram.
    run BuildTable     in hTableProgram (FieldList,DataTypes,Formats,Labels).
    run PopulateTable  in hTableProgram (ValueList).
  end.

  assign
  curTTFld = hTTBuffer:buffer-field("DBRowid") /* grab the DB rowid */
  DBRowid    = CurTTFld:buffer-value.

  run CreateRecord   in hTableProgram (ValueList,DBRowid).
  hBrowse:refresh().
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
/* create the dynamic browse...at present it is not visible */
create browse hBrowse
  assign 
    width      = 70
    height     = 6
    expandable = NO
    column     = 4
    row        = 1
    frame      = frame {&frame-name}:handle
    read-only  = NO
    sensitive  = YES
    visible    = no
    triggers:
       on value-changed persistent run ShowData in this-procedure.
   end triggers.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ShowData C-Win 
PROCEDURE ShowData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  define variable CurWidget  as handle.
  define variable cntr       as int.
  define variable CurField   as char.
  define variable CurTTFIeld as handle.
  
  if valid-handle(hTTBuffer) then do:
    assign
      CurWidget = frame custframe:handle
      CurWidget = CurWidget:first-child
      CurWidget = CurWidget:first-tab-item.
      /* go through variables in the screen, get the TT field and
         display to the field */
      do while valid-handle(CurWidget):
        if CurWidget:type = "fill-in"
          then 
          assign
          cntr       = cntr + 1
          CurTTField = hTTBuffer:buffer-field(cntr)        
          CurWidget:screen-value = CurTTField:buffer-value 
          .
          CurWidget = CurWidget:next-sibling.
      end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

