&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME dsOrderWin

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _TEMP-TABLE 
/* ***********Included Temp-Table & Buffer definitions **************** */

{dsOrderTT.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dsOrderWin 
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

{dsOrder.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME dsFrame
&Scoped-define BROWSE-NAME ItemBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttItem ttOLine

/* Definitions for BROWSE ItemBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-ItemBrowse ttItem.Itemnum ttItem.ItemName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-ItemBrowse 
&Scoped-define QUERY-STRING-ItemBrowse FOR EACH ttItem NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-ItemBrowse OPEN QUERY ItemBrowse FOR EACH ttItem NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-ItemBrowse ttItem
&Scoped-define FIRST-TABLE-IN-QUERY-ItemBrowse ttItem


/* Definitions for BROWSE OlineBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-OlineBrowse ttOLine.Ordernum ttOLine.Linenum ~
ttOLine.Itemnum ttOLine.Price ttOLine.Qty ttOLine.ExtendedPrice 
&Scoped-define ENABLED-FIELDS-IN-QUERY-OlineBrowse 
&Scoped-define QUERY-STRING-OlineBrowse FOR EACH ttOLine NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-OlineBrowse OPEN QUERY OlineBrowse FOR EACH ttOLine NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-OlineBrowse ttOLine
&Scoped-define FIRST-TABLE-IN-QUERY-OlineBrowse ttOLine


/* Definitions for FRAME dsFrame                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-dsFrame ~
    ~{&OPEN-QUERY-ItemBrowse}~
    ~{&OPEN-QUERY-OlineBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS iOrderNum iCustNum cCustName cRepName ~
dOrderTotal OlineBrowse ItemBrowse 
&Scoped-Define DISPLAYED-OBJECTS iOrderNum iCustNum cCustName cRepName ~
dOrderTotal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR dsOrderWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE cCustName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cust Name" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE cRepName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Rep Name" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE dOrderTotal AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Order Total" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE iCustNum AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE iOrderNum AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Order Num" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ItemBrowse FOR 
      ttItem SCROLLING.

DEFINE QUERY OlineBrowse FOR 
      ttOLine SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ItemBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ItemBrowse dsOrderWin _STRUCTURED
  QUERY ItemBrowse NO-LOCK DISPLAY
      ttItem.Itemnum FORMAT "zzzzzzzzz9":U
      ttItem.ItemName FORMAT "x(25)":U WIDTH 18.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 34 BY 6.43 FIT-LAST-COLUMN.

DEFINE BROWSE OlineBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS OlineBrowse dsOrderWin _STRUCTURED
  QUERY OlineBrowse NO-LOCK DISPLAY
      ttOLine.Ordernum FORMAT "zzzzzzzzz9":U
      ttOLine.Linenum FORMAT ">>9":U
      ttOLine.Itemnum FORMAT "zzzzzzzzz9":U
      ttOLine.Price FORMAT "->,>>>,>>9.99":U
      ttOLine.Qty FORMAT "->>>>9":U
      ttOLine.ExtendedPrice FORMAT "->>>,>>9.99":U WIDTH 28.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 6.67 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dsFrame
     iOrderNum AT ROW 1.48 COL 11 COLON-ALIGNED
     iCustNum AT ROW 2.91 COL 11 COLON-ALIGNED
     cCustName AT ROW 2.91 COL 41 COLON-ALIGNED
     cRepName AT ROW 2.91 COL 73 COLON-ALIGNED
     dOrderTotal AT ROW 2.91 COL 106 COLON-ALIGNED
     OlineBrowse AT ROW 4.81 COL 4
     ItemBrowse AT ROW 4.81 COL 91
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 128 BY 12.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttItem T "?" ? TEMP-DB ttItem
      TABLE: ttOLine T "?" ? TEMP-DB ttOLine
      TABLE: ttOrder T "?" ? TEMP-DB ttOrder
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW dsOrderWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Test Window for Order DataSet"
         HEIGHT             = 12.48
         WIDTH              = 128
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 128
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
/* SETTINGS FOR WINDOW dsOrderWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME dsFrame
                                                                        */
/* BROWSE-TAB OlineBrowse dOrderTotal dsFrame */
/* BROWSE-TAB ItemBrowse OlineBrowse dsFrame */
ASSIGN 
       cCustName:READ-ONLY IN FRAME dsFrame        = TRUE.

ASSIGN 
       cRepName:READ-ONLY IN FRAME dsFrame        = TRUE.

ASSIGN 
       dOrderTotal:READ-ONLY IN FRAME dsFrame        = TRUE.

ASSIGN 
       iCustNum:READ-ONLY IN FRAME dsFrame        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(dsOrderWin)
THEN dsOrderWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ItemBrowse
/* Query rebuild information for BROWSE ItemBrowse
     _TblList          = "Temp-Tables.ttItem"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ttItem.Itemnum
     _FldNameList[2]   > Temp-Tables.ttItem.ItemName
"ttItem.ItemName" ? ? "character" ? ? ? ? ? ? no ? no no "18.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE ItemBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE OlineBrowse
/* Query rebuild information for BROWSE OlineBrowse
     _TblList          = "Temp-Tables.ttOLine"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ttOLine.Ordernum
     _FldNameList[2]   = Temp-Tables.ttOLine.Linenum
     _FldNameList[3]   = Temp-Tables.ttOLine.Itemnum
     _FldNameList[4]   = Temp-Tables.ttOLine.Price
     _FldNameList[5]   = Temp-Tables.ttOLine.Qty
     _FldNameList[6]   > Temp-Tables.ttOLine.ExtendedPrice
"ttOLine.ExtendedPrice" ? ? "decimal" ? ? ? ? ? ? no ? no no "28.4" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE OlineBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME dsOrderWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dsOrderWin dsOrderWin
ON END-ERROR OF dsOrderWin /* Test Window for Order DataSet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dsOrderWin dsOrderWin
ON WINDOW-CLOSE OF dsOrderWin /* Test Window for Order DataSet */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iOrderNum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iOrderNum dsOrderWin
ON LEAVE OF iOrderNum IN FRAME dsFrame /* Order Num */
DO:
  ASSIGN iOrderNum.
  IF iOrderNum NE 0 THEN
  DO:
    DATASET dsOrder:GET-RELATION(1):QUERY:QUERY-CLOSE().
    DATASET dsOrder:GET-RELATION(2):QUERY:QUERY-CLOSE().
    DATASET dsOrder:EMPTY-DATASET.
    RUN OrderMain.p (INPUT iOrderNum, OUTPUT DATASET dsOrder
       BY-REFERENCE).
    FIND FIRST ttOrder.
    DO WITH FRAME dsFrame:
      ASSIGN iCustNum:SCREEN-VALUE = STRING(ttOrder.CustNum)
             cCustName:SCREEN-VALUE = ttOrder.CustName
             cRepName:SCREEN-VALUE = ttOrder.RepName
             dOrderTotal:SCREEN-VALUE = STRING(ttOrder.OrderTotal).
      /* {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}} */
      DATASET dsOrder:GET-BUFFER-HANDLE(1):SYNCHRONIZE().
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ItemBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dsOrderWin 


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
  /* Replace the default AppBuilder-generated static queries with
    the ones that are part of the Data-Relations. */
    OlineBrowse:QUERY =
    DATASET dsOrder:GET-RELATION("OrderLine"):QUERY.
    ItemBrowse:QUERY =
    DATASET dsOrder:GET-RELATION("LineItem"):QUERY.
    ItemBrowse:SET-REPOSITIONED-ROW(4, "CONDITIONAL").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dsOrderWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(dsOrderWin)
  THEN DELETE WIDGET dsOrderWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI dsOrderWin  _DEFAULT-ENABLE
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
  DISPLAY iOrderNum iCustNum cCustName cRepName dOrderTotal 
      WITH FRAME dsFrame IN WINDOW dsOrderWin.
  ENABLE iOrderNum iCustNum cCustName cRepName dOrderTotal OlineBrowse 
         ItemBrowse 
      WITH FRAME dsFrame IN WINDOW dsOrderWin.
  {&OPEN-BROWSERS-IN-QUERY-dsFrame}
  VIEW dsOrderWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

