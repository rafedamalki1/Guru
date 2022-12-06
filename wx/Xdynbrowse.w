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
define variable   table-name  as handle.
  define variable   col-handle  as handle.
  define variable   fld-handle  as handle.
      
  define variable   query-value as char.
  
  define variable   cntr        as int.
define variable query-phrase as char.
define variable where-phrase as char.
define variable sort-phrase  as char.
define variable fieldlist    as char.

define variable   dyn-query   as handle.
define variable   dyn-browse  as handle.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


/*{EGENBNS.I}*/
DEFINE VARIABLE brwh AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE NO-UNDO.
DEFINE VARIABLE brwproc2 AS HANDLE NO-UNDO.
DEFINE VARIABLE varsum AS INTEGER NO-UNDO.
DEFINE VARIABLE brwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE exbrwfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE exfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE newfields AS CHARACTER NO-UNDO.
DEFINE VARIABLE indexvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ttname AS CHARACTER NO-UNDO.
DEFINE VARIABLE tabname AS CHARACTER NO-UNDO.
DEFINE VARIABLE okvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.

globforetag = FORETAG.FORETAG.
{FORESTYR.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 S-Tables S-fields S-fields-sort ~
S-fields-view Btn-rpt E-query BtnClear Btninfo BtnIndex BTN_FLYTTA ~
FILL-IN-startc FILL-IN-TILL Btn-Done 
&Scoped-Define DISPLAYED-OBJECTS S-Tables S-fields S-fields-sort ~
S-fields-view E-query FILL-IN-startc FILL-IN-TILL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Done DEFAULT 
     LABEL "&Done" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn-rpt DEFAULT 
     LABEL "&Report" 
     SIZE 11 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BtnClear 
     LABEL "&Clear" 
     SIZE 11 BY 1.13.

DEFINE BUTTON BtnIndex 
     LABEL "Index" 
     SIZE 11 BY 1.13.

DEFINE BUTTON Btninfo 
     LABEL "Where" 
     SIZE 11 BY 1.13.

DEFINE BUTTON BTN_FLYTTA 
     LABEL "Flytta" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE E-query AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 93 BY 4.17
     FONT 2 NO-UNDO.

DEFINE VARIABLE FILL-IN-startc AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Flytta kolumn fr�n" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILL AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "till" 
     VIEW-AS FILL-IN 
     SIZE 5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 7.88.

DEFINE VARIABLE S-fields AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.67 NO-UNDO.

DEFINE VARIABLE S-fields-sort AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.67 NO-UNDO.

DEFINE VARIABLE S-fields-view AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     SIZE 24 BY 6.67 NO-UNDO.

DEFINE VARIABLE S-Tables AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 28 BY 6.92 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     S-Tables AT ROW 1.96 COL 4 NO-LABEL
     S-fields AT ROW 1.96 COL 35 NO-LABEL
     S-fields-sort AT ROW 1.96 COL 62 NO-LABEL
     S-fields-view AT ROW 1.96 COL 89 NO-LABEL
     Btn-rpt AT ROW 9.33 COL 102
     E-query AT ROW 9.58 COL 4 NO-LABEL
     BtnClear AT ROW 10.75 COL 102
     Btninfo AT ROW 12.21 COL 102
     BtnIndex AT ROW 13.63 COL 102
     BTN_FLYTTA AT ROW 14.5 COL 59
     FILL-IN-startc AT ROW 14.63 COL 20.5 COLON-ALIGNED
     FILL-IN-TILL AT ROW 14.63 COL 38.5 COLON-ALIGNED
     Btn-Done AT ROW 15.5 COL 102
     "View" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 94
     "Sort" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 67
     "Filter" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 39
     "Tables" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 1 COL 8
     RECT-3 AT ROW 9.08 COL 99
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119 BY 27.88.


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
         TITLE              = "Smart Query Generator"
         HEIGHT             = 27.88
         WIDTH              = 119
         MAX-HEIGHT         = 31.63
         MAX-WIDTH          = 143.38
         VIRTUAL-HEIGHT     = 31.63
         VIRTUAL-WIDTH      = 143.38
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
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Smart Query Generator */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Smart Query Generator */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Done C-Win
ON CHOOSE OF Btn-Done IN FRAME DEFAULT-FRAME /* Done */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-rpt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-rpt C-Win
ON CHOOSE OF Btn-rpt IN FRAME DEFAULT-FRAME /* Report */
DO:

  
   
  /* get the name from the tables selection list */
  create buffer table-name for table s-tables:screen-value.
  create query dyn-query.

  /* set the query and create the browse */
  dyn-query:set-buffers(table-name).
  create browse dyn-browse
  assign 
  width      = 110
  height     = 10
  expandable = yes
  column     = 4
  row        = 18.25
  frame      = frame {&frame-name}:handle
  read-only  = false
  sensitive  = true.
 
  /* attach the query to the browser */
  dyn-browse:query = dyn-query.
  /* query value is the string which will be used to open the query */
  query-value = query-phrase.
  
  DEFINE VARIABLE wherevar AS CHARACTER NO-UNDO.
  wherevar = e-query:screen-value.
  IF INDEX(wherevar,"Where phrase") = 0 THEN where-phrase = "".
  ELSE DO:                                         
     wherevar = REPLACE(wherevar,CHR(10),"").                                                     
     wherevar = SUBSTRING(wherevar,INDEX(wherevar,"Where phrase") + 14).
     wherevar = REPLACE(wherevar,"Where phrase ="," AND "). 
     where-phrase = wherevar.
  END.
  
  
  /*lopa edit*/
  if where-phrase <> "" then query-value = query-value + " where " + where-phrase.
  if sort-phrase  <> "" then query-value = query-value + " by "    + sort-phrase.
  
  /* prepare and open the query */ 
  MESSAGE query-value "�r query�n r�tt?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE valborl AS LOGICAL.
   CASE valborl:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.      
   END CASE.
  dyn-query:query-prepare(query-value). 
  dyn-query:query-open().
 
 
  

  do cntr = 1 to num-entries(s-fields-view:screen-value):
    col-handle = dyn-browse:add-like-column
    (s-tables:screen-value + "." + entry(cntr,s-fields-view:screen-value)).
    col-handle:read-only = no.
  end.
  
   
   assign
   dyn-browse:sensitive  = yes
   dyn-browse:visible    = yes
   .
   apply "home" to dyn-browse.
   RUN DYNTTSKARP.P PERSISTENT SET brwproc (INPUT dyn-browse,INPUT FRAME {&FRAME-NAME}:HANDLE). 
   RUN DYNBRW.P PERSISTENT SET brwproc2 (INPUT dyn-browse).
   RUN settrigger_UI IN brwproc (INPUT 1,INPUT "rowleave_UI").
   IF wherevar = "" THEN wherevar = wherevar.
   ELSE DO:
      wherevar = " WHERE " + wherevar. 
      RUN setcolsortvar_UI IN brwproc2 (INPUT wherevar).
   END.
   

   

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnClear C-Win
ON CHOOSE OF BtnClear IN FRAME DEFAULT-FRAME /* Clear */
DO:
  e-query:screen-value in frame {&frame-name} = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnIndex
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnIndex C-Win
ON CHOOSE OF BtnIndex IN FRAME DEFAULT-FRAME /* Index */
DO:
  e-query:screen-value = e-query:screen-value + 
  chr(10) + chr(10) + dyn-query:index-information(1).
  if num-entries(dyn-query:index-information(1)) > 1
  and entry(1,dyn-query:index-information(1)) = "WHOLE-INDEX"
  then
   e-query:screen-value = e-query:screen-value + " SCANNED THE WHOLE TABLE FOR "
   + string(dyn-query:num-results) + " RECORDS!".
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btninfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btninfo C-Win
ON CHOOSE OF Btninfo IN FRAME DEFAULT-FRAME /* Where */
DO:
  e-query:screen-value = e-query:screen-value + 
  chr(10) + chr(10) + dyn-query:prepare-string.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FLYTTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FLYTTA C-Win
ON CHOOSE OF BTN_FLYTTA IN FRAME DEFAULT-FRAME /* Flytta */
DO:
   FILL-IN-startc = INPUT FILL-IN-startc.
   FILL-IN-TILL = INPUT FILL-IN-TILL.
   RUN movecol_UI IN brwproc (INPUT FILL-IN-startc,INPUT FILL-IN-TILL).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME S-fields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-fields C-Win
ON MOUSE-SELECT-DBLCLICK OF S-fields IN FRAME DEFAULT-FRAME
DO:
  run wherebldr.w (self:screen-value).
  
  /* return value provides a comma-delimited list: the first entry
  is the comparator sign (= >= or <=)
  the second is the actual value. But the where builder doesn't know what field,
  so we have to get the field and determine the data type */
  where-phrase = s-fields:screen-value + " " + entry(1,return-value). /* e.g. cust-num >= */
  find first _field where _field._FILE-RECID = RECID(_file) AND _field-name = s-fields:screen-value no-lock no-error.
  if _data-type begins "ch" then where-phrase = where-phrase + " '" + entry(2,return-value) + "'".
  else where-phrase = where-phrase + " " + entry(2,return-value).
  /* e.g. cust-num >= 4 */
  e-query:screen-value = 
  e-query:screen-value + chr(13) + 
  "Where phrase = " + where-phrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME S-fields-sort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-fields-sort C-Win
ON MOUSE-SELECT-DBLCLICK OF S-fields-sort IN FRAME DEFAULT-FRAME
DO:
  if s-fields-sort:screen-value <> "" then
  sort-phrase = "by " + s-fields-sort:screen-value.
  
  .
  sort-phrase = s-fields-sort:screen-value.

  e-query:screen-value = 
  e-query:screen-value + chr(13) + 
  "Sort phrase = by " + sort-phrase.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME S-Tables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-Tables C-Win
ON MOUSE-SELECT-DBLCLICK OF S-Tables IN FRAME DEFAULT-FRAME
DO:
  query-phrase = "for each " + s-tables:screen-value + " no-lock".
  e-query:screen-value = "Query phrase =  '" + query-phrase + "'".
  . 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL S-Tables C-Win
ON VALUE-CHANGED OF S-Tables IN FRAME DEFAULT-FRAME
DO:
  DEBUGGER:SET-BREAK().
  assign
  s-fields:list-items = ""
  s-fields-sort:list-items = ""
  s-fields-view:list-items = "".
  find _file no-lock where _file-name = s-tables:screen-value.
  for each _field no-lock of _file:

    s-fields:add-last(_field-name).
    s-fields-sort:add-last(_field-name).
    s-fields-view:add-last(_field-name).
  end.
  display s-fields s-fields-sort s-fields-view 
  with frame {&FRAME-NAME}.
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
  run load-tables.
  
  RUN enable_UI.
  s-tables:screen-value in frame {&FRAME-NAME} = entry(1,s-tables:list-items).
  apply "value-changed" to s-tables.
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
  DISPLAY S-Tables S-fields S-fields-sort S-fields-view E-query FILL-IN-startc 
          FILL-IN-TILL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-3 S-Tables S-fields S-fields-sort S-fields-view Btn-rpt E-query 
         BtnClear Btninfo BtnIndex BTN_FLYTTA FILL-IN-startc FILL-IN-TILL 
         Btn-Done 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-tables C-Win 
PROCEDURE load-tables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  for each _file no-lock:
  if _file-name begins "_" or _file-name begins "SYS" then next.
     s-tables:add-last(_file-name) in frame {&FRAME-NAME}.
   end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI C-Win 
PROCEDURE rowleave_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE colfieldh AS HANDLE NO-UNDO.
   DEFINE VARIABLE tabfalth AS HANDLE NO-UNDO.
   tempcounter = 1.
   dyn-browse:SELECT-FOCUSED-ROW() NO-ERROR.
   DO TRANSACTION:
      dyn-query:GET-CURRENT(EXCLUSIVE-LOCK).        
      DO WHILE tempcounter LE dyn-browse:NUM-COLUMNS:
         colfieldh = dyn-browse:GET-BROWSE-COLUMN(tempcounter).      
         tabfalth = table-name:BUFFER-FIELD(colfieldh:NAME).
         tabfalth:BUFFER-VALUE = colfieldh:SCREEN-VALUE.         
         tempcounter = tempcounter + 1.
         
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
