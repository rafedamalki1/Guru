&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
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

DEFINE VAR ctlGrid AS COM-HANDLE.
DEFINE VAR ctlGraph AS COM-HANDLE.
DEFINE VAR numRowsFilled AS INT INIT 0.
DEFINE VAR monthData AS INT EXTENT 12.

DEFINE QUERY qry FOR customer SCROLLING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS GraphType SortBy 
&Scoped-Define DISPLAYED-OBJECTS GraphType SortBy 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CFGraph AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFGraph AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CFGrid AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCFGrid AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE GraphType AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "2D Bar", 3,
"3D Bar", 4,
"Line", 6
     SIZE 16 BY 2.62 NO-UNDO.

DEFINE VARIABLE SortBy AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "By Name", "Name",
"By City", "City"
     SIZE 16 BY 1.91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     GraphType AT ROW 1.24 COL 67 NO-LABEL
     SortBy AT ROW 1.48 COL 29 NO-LABEL
     "Graph Type:" VIEW-AS TEXT
          SIZE 13 BY .95 AT ROW 1.24 COL 53
     "Sort by:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.48 COL 19
     "Customer Information" VIEW-AS TEXT
          SIZE 25 BY 1.1 AT ROW 3.86 COL 38
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.4 BY 29.71.


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
         TITLE              = "Customer and Sales Report"
         HEIGHT             = 29.71
         WIDTH              = 98.4
         MAX-HEIGHT         = 29.71
         MAX-WIDTH          = 100.8
         VIRTUAL-HEIGHT     = 29.71
         VIRTUAL-WIDTH      = 100.8
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


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CFGrid ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 5.05
       COLUMN       = 3
       HEIGHT       = 7.86
       WIDTH        = 94
       HIDDEN       = no
       SENSITIVE    = yes.

CREATE CONTROL-FRAME CFGraph ASSIGN
       FRAME        = FRAME DEFAULT-FRAME:HANDLE
       ROW          = 13.38
       COLUMN       = 2
       HEIGHT       = 17.14
       WIDTH        = 97
       HIDDEN       = no
       SENSITIVE    = yes.
      CFGrid:NAME = "CFGrid":U .
/* CFGrid OCXINFO:CREATE-CONTROL from: {CB5103A4-C04E-11CF-91F7-C2863C385E30} type: vsFlexArray */
      CFGraph:NAME = "CFGraph":U .
/* CFGraph OCXINFO:CREATE-CONTROL from: {0842D100-1E19-101B-9AAF-1A1626551E7C} type: Graph */
      CFGrid:MOVE-AFTER(SortBy:HANDLE IN FRAME DEFAULT-FRAME).
      CFGraph:MOVE-AFTER(CFGrid).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Customer and Sales Report */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Customer and Sales Report */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CFGrid
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFGrid C-Win
PROCEDURE CFGrid.vsFlexArray.RowColChange .
/*------------------------------------------------------------------------------
  Purpose: This event fires when the user selects a new row or column in the
           grid.  We are interested in the row change.  We repopulate
           the graph to show total sales for the customer or city associated
           with the new current row.    
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR Id AS RECID.
    DEFINE VAR coll AS COM-HANDLE.
    DEFINE VAR ix AS INT.
    DEFINE BUFFER bcust FOR customer.
  
    Id = ctlGrid:RowData(ctlGrid:Row).
    IF Id <> 0 THEN DO:
        REPOSITION qry TO RECID Id. /* This repositions to before record */
        GET NEXT qry.               /* This gets the record */
        
        /* These will increment automatically, since AutoInc is TRUE */
        ctlGraph:ThisPoint = 1.
        ctlGraph:ThisSet = 1.

        DO ix = 1 TO 12:
            monthData[ix] = 0.
        END.
        
        IF SortBy = "name" THEN DO:
            RUN FillMonthData (BUFFER customer).
            ctlGraph:GraphTitle = "Total Sales/Month for customer: " +
                                  customer.name.
        END.
        ELSE DO:
            FOR EACH bcust WHERE bcust.City = customer.City:
                RUN FillMonthData(BUFFER bcust).
            END.
            ctlGraph:GraphTitle = "Total Sales/Month for city: " +
                                  customer.city.      
        END.
        
        DO ix = 1 TO 12:
            ctlGraph:GraphData = monthData[ix].
        END.
        ctlGraph:DrawMode = 2.  /* Redraw */
    END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFGrid C-Win
PROCEDURE CFGrid.vsFlexArray.Scroll .
/*------------------------------------------------------------------------------
  Purpose: This event fires when the user scrolls the grid.  If we haven't
           yet filled the row that is coming into view, we fill it now.    
   xParameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR bottomId AS RECID.
    /* bottomRow: 0 based, but will work as if 1 based because of title row */
    DEFINE VAR bottomRow AS INT.  
    DEFINE VAR Id AS INT.

    bottomRow = ctlGrid:BottomRow.
    IF numRowsFilled < bottomRow THEN DO: 
        Id = ctlGrid:RowData(bottomRow - 1).
        REPOSITION qry TO RECID Id. /* This positions to before the row */
        GET NEXT qry.  /* This positions on the row */
        GET NEXT qry.  
        IF AVAILABLE(customer) THEN DO:
            ctlGrid:Rows = bottomRow + 1. /* plus 1 because of title row */
            RUN FillRow(bottomRow, bottomRow * 5).
        END.            
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CFGrid C-Win
PROCEDURE CFGrid.vsFlexArray.Validate .
/*------------------------------------------------------------------------------
  Purpose: This event fires when the user has changed the data in a cell
           and then clicks outside of that cell.  Check with the user to be
           sure he wants these changes and if so, store the new value in
           the database.    
  Parameters:  Required for OCX.
    Row
    Col
    Cancel
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT        PARAMETER p-Row    AS INTEGER NO-UNDO.
DEFINE INPUT        PARAMETER p-Col    AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER p-Cancel AS LOGICAL NO-UNDO.

    DEFINE VAR ok AS LOGICAL.
    DEFINE VAR oldVal AS CHAR.
    DEFINE VAR newVal AS CHAR.
    DEFINE VAR cellIx AS INT.
    DEFINE VAR Id AS RECID.

    /* Remember the original value and reset column with new value - at
       least temporarily.  This shouldn't be necessary but otherwise the control 
       puts back the original value while this alert box is up (though it does
       save the edit value - you just don't see it - which is really confusing).  
       Because of this we have to implement cancel functionality ourselves - 
       i.e. once we reset TextArray value, the control loses the original value 
       and won't be able to reset it if user says he doesn't want to save.
    */   
    cellIx = p-Row * 5 + p-Col.
    oldVal = ctlGrid:TextArray(cellIx).
    newVal = ctlGrid:EditText.
    ctlGrid:TextArray(cellIx) = newVal.
    
    Message "Change cell value to:" newVal 
        VIEW-AS ALERT-BOX BUTTONS YES-NO SET ok.
        
    if NOT ok THEN
        ctlGrid:TextArray(cellIx) = oldVal.
    ELSE DO:
        /* Update the database */
        Id = ctlGrid:RowData(p-Row).
        REPOSITION qry TO RECID Id. /* This positions to before the row */
        GET NEXT qry.  /* This positions on the row */
        
        RUN UpdateField(p-Col, newVal, OUTPUT ok).
        IF NOT ok THEN  /* database error occurred */
            ctlGrid:TextArray(cellIx) = oldVal.
    END.    
        
    p-Cancel = YES.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME GraphType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL GraphType C-Win
ON VALUE-CHANGED OF GraphType IN FRAME DEFAULT-FRAME
DO:
  /* Update the graph to use the graph style
     the user just selected 
  */
  GraphType = INTEGER(SELF:SCREEN-VALUE).
  ctlGraph:GraphType = GraphType.
  ctlGraph:DrawMode = 2.  /* Redraw */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SortBy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SortBy C-Win
ON VALUE-CHANGED OF SortBy IN FRAME DEFAULT-FRAME
DO:
  /* Just reset the variable SortBy and re-fill the grid.
     This will cause it to flip between sorting by name vs. City 
  */
  SortBy = SELF:SCREEN-VALUE.
  RUN FillGrid.
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
  
  /* This event doesn't happen otherwise (I think it should!), and we 
     don't get the default graph type applied.
  */   
  APPLY "Value-Changed" TO GraphType IN FRAME DEFAULT-FRAME.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "flex.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chCFGraph = CFGraph:COM-HANDLE
    UIB_S = chCFGraph:LoadControls( OCXFile, "CFGraph":U)
    chCFGrid = CFGrid:COM-HANDLE
    UIB_S = chCFGrid:LoadControls( OCXFile, "CFGrid":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, flex.wrx, could not be found." skip
             "The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY GraphType SortBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE GraphType SortBy 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillGrid C-Win 
PROCEDURE FillGrid :
/*------------------------------------------------------------------------------
  Purpose:  Fill the Flex Array grid with customer data.   
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR ix AS INT.
    DEFINE VAR rowIx AS INT.
    DEFINE VAR cellIx AS INT.
    DEFINE VAR startRows AS INT.
    
    ctlGrid:Redraw = FALSE.  /* To avoid flickering */
    
    /* Clear any rows that exist already.  Keep removing the first item,
       n times.  If we say RemoveItem(ix), as we remove rows, the # of rows
       gets smaller and after the 1/2 way point, ix will be > than the
       # of rows!
    */   
    IF numRowsFilled > 0 THEN
        DO ix = 0 TO numRowsFilled - 1:
            ctlGrid:RemoveItem(0).
        END.
    
    startRows = 10.  /* We'll start with 10 and add more as needed */
    ctlGrid:Rows = startRows + 1.    /* Add 1 for title row */

    numRowsFilled = 0.
    RUN SetUpColumns.  /* Set column headers and column widths */
    RUN OpenQuery.

    cellIx = 5.  /* Start at cell after column headers */  
    DO rowIx = 1 TO startRows:
        GET NEXT qry.
        RUN FillRow(rowIx, cellIx).
        cellIx = cellIx + 5.  
    END.   
    
    ctlGrid:Select(1,0).  /* Set the first row as the current row */
    ctlGrid:Redraw = TRUE.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillMonthData C-Win 
PROCEDURE FillMonthData :
/*------------------------------------------------------------------------------
  Purpose: Set data in the monthData array, one for each month.
           This will be used later to set points in the Graph control.
  Parameters:  The customer record that we want to get sales information for.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bcust FOR customer.

    DEFINE VAR currmth AS INT.
    
    FOR EACH order of bcust, each orderline of order BREAK BY MONTH(order.orderdate):
        ACCUMULATE orderline.extendedprice (TOTAL BY MONTH(order.orderdate)).
        IF LAST-OF(MONTH(order.orderdate)) THEN DO:
            currmth = MONTH(order.orderdate).
            monthData[currmth] = monthData[currmth] + 
                                 ACCUM TOTAL BY MONTH(order.orderdate) orderline.extendedprice.
        END.    
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillRow C-Win 
PROCEDURE FillRow :
/*------------------------------------------------------------------------------
  Purpose: Fill one row of the Flex Array.    
  Parameters: rowIx - index of the current row to fill
              cellIx - index of the cell to start filling.
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER rowIx AS INT.
    DEFINE INPUT PARAMETER cellIx AS INT.

    ctlGrid:RowData(rowIx) = RECID(customer).
    
    IF SortBy = "name" THEN DO:
        ctlGrid:TextArray(cellIx) = customer.name.
        ctlGrid:TextArray(cellIx + 1) = customer.city.
        ctlGrid:TextArray(cellIx + 2) = customer.state.
        ctlGrid:TextArray(cellIx + 3) = customer.phone.
        ctlGrid:TextArray(cellIx + 4) = customer.balance.
    END.    
    ELSE DO:
        ctlGrid:TextArray(cellIx) = customer.city.
        ctlGrid:TextArray(cellIx + 1) = customer.state.
        ctlGrid:TextArray(cellIx + 2) = customer.name.
        ctlGrid:TextArray(cellIx + 3) = customer.phone.
        ctlGrid:TextArray(cellIx + 4) = customer.balance.
    END.
    numRowsFilled = numRowsFilled + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls C-Win 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR colIx AS INT.

    /* Initialize grid and graph attributes.  Some of these could be
       done with the property sheet - but leaving it here is more
       instructive to the code reader.
    */      
    ctlGrid = chCFGrid:vsFlexArray.
    
    ctlGrid:VirtualData = TRUE.
    ctlGrid:AllowUserResizing = TRUE.
    ctlGrid:colAlignment(0) = 7. /* Align CenterLeft */
    ctlGrid:Row = 0.  /* Current row */

    /* Change fonts on title row */
    DO colIx = 0 TO ctlGrid:Cols - 1:
        ctlGrid:Col = colIx.
        ctlGrid:CellFontSize = 12 .
        ctlGrid:CellAlignment = 4. /* Center */
    END.
  
    ctlGraph = chCFGraph:Graph.
    ctlGraph:FontSize = 120.  /* % of system font size */
         
    /* This sets up the y axis */
    ctlGraph:YAxisStyle = 2.
    ctlGraph:YAxisTicks = 20.
    ctlGraph:YAxisMin = 0.
    ctlGraph:YAxisMax = 50000.

    /* This sets up the x axis */
    ctlGraph:NumPoints = 12.
    ctlGraph:LabelEvery = 1.
    CtlGraph:LabelText = "Jan".
    CtlGraph:LabelText = "Feb".
    CtlGraph:LabelText = "Mar".    
    CtlGraph:LabelText = "Apr".    
    CtlGraph:LabelText = "May".    
    CtlGraph:LabelText = "Jun".    
    CtlGraph:LabelText = "Jul".    
    CtlGraph:LabelText = "Aug".    
    CtlGraph:LabelText = "Sep".    
    CtlGraph:LabelText = "Oct".    
    CtlGraph:LabelText = "Nov".    
    CtlGraph:LabelText = "Dec".    
        
    /* Initialize radio set variables */  
    SortBy = "Name".
    GraphType = 4.  /* 3DBar */
    
    RUN FillGrid.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF SortBy = "name" THEN  
        OPEN QUERY qry FOR EACH CUSTOMER BY Name.
    ELSE
        OPEN QUERY qry FOR EACH CUSTOMER BY city BY state.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetUpColumns C-Win 
PROCEDURE SetUpColumns :
/*------------------------------------------------------------------------------
  Purpose: Set up column labels, widths, etc..
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF SortBy = "Name" THEN DO:
        ctlGrid:ColWidth(0) = 2000.  /* Name */
        ctlGrid:ColWidth(1) = 1200.  /* City */
        ctlGrid:ColWidth(2) = 1300.  /* State */

        /* The first row of the grid contains the column headers */
        ctlGrid:TextArray(0) = "Name".
        ctlGrid:TextArray(1) = "City".
        ctlGrid:TextArray(2) = "State".
    END.    
    ELSE DO:
        ctlGrid:ColWidth(0) = 1200.  /* City */
        ctlGrid:ColWidth(1) = 1300.  /* State */
        ctlGrid:ColWidth(2) = 2000.  /* Name */

        /* Merge City and State column */    
        ctlGrid:MergeCol(0) = TRUE.
        ctlGrid:MergeCol(1) = TRUE.
        
        /* The first row of the grid contains the column headers */
        ctlGrid:TextArray(0) = "City".
        ctlGrid:TextArray(1) = "State".
        ctlGrid:TextArray(2) = "Name".
    END.
    
    /* Set the column header and width for the last 2 columns. */
    ctlGrid:ColWidth(3) = 1200. 
    ctlGrid:ColWidth(4) = 1000. 
    ctlGrid:TextArray(3) = "Phone".
    ctlGrid:TextArray(4) = "Balance".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateField C-Win 
PROCEDURE UpdateField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER p-Col AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER p-newVal AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER updated AS LOGICAL.

DO ON ERROR UNDO, LEAVE:
    IF p-Col < 3 THEN DO:
        IF SortBy = "name" THEN DO:
            IF p-Col = 0 THEN
                customer.name = p-newVal.
            ELSE IF p-Col = 1 THEN
                customer.city = p-newVal.
            ELSE IF p-Col = 2 THEN
                customer.state = p-newVal.
        END.        
        ELSE DO:
            IF p-Col = 0 THEN
                customer.city = p-newVal.
            ELSE IF p-Col = 1 THEN
                customer.state = p-newVal.
            ELSE IF p-Col = 2 THEN
                customer.name = p-newVal.       
        END.
    END.
    ELSE DO:
        IF p-Col = 3 THEN
            customer.phone = p-newVal.
        ELSE /* column 4 */   
            customer.balance = DECIMAL(p-newVal).
    END.
    
    updated = TRUE.
    return.
END.  
  
updated = FALSE.    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


