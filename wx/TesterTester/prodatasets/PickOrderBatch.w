&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEFINE VARIABLE hOrderProc AS HANDLE NO-UNDO.

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
&Scoped-define INTERNAL-TABLES ttItem ttOline ttOrder

/* Definitions for BROWSE ItemBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-ItemBrowse ttItem.Itemnum ttItem.ItemName 
&Scoped-define ENABLED-FIELDS-IN-QUERY-ItemBrowse 
&Scoped-define QUERY-STRING-ItemBrowse FOR EACH ttItem NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-ItemBrowse OPEN QUERY ItemBrowse FOR EACH ttItem NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-ItemBrowse ttItem
&Scoped-define FIRST-TABLE-IN-QUERY-ItemBrowse ttItem


/* Definitions for BROWSE OlineBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-OlineBrowse ttOline.Ordernum ttOline.Linenum ~
ttOline.Itemnum ttOline.Price ttOline.Qty ttOline.ExtendedPrice ~
ttOline.Discount ttOline.OrderLineStatus 
&Scoped-define ENABLED-FIELDS-IN-QUERY-OlineBrowse ttOline.Price ~
ttOline.Qty ttOline.Discount 
&Scoped-define ENABLED-TABLES-IN-QUERY-OlineBrowse ttOline
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-OlineBrowse ttOline
&Scoped-define QUERY-STRING-OlineBrowse FOR EACH ttOline NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-OlineBrowse OPEN QUERY OlineBrowse FOR EACH ttOline NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-OlineBrowse ttOline
&Scoped-define FIRST-TABLE-IN-QUERY-OlineBrowse ttOline


/* Definitions for BROWSE OrderBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-OrderBrowse ttOrder.Ordernum ttOrder.CustNum ~
ttOrder.SalesRep ttOrder.OrderDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-OrderBrowse 
&Scoped-define QUERY-STRING-OrderBrowse FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-OrderBrowse OPEN QUERY OrderBrowse FOR EACH ttOrder NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-OrderBrowse ttOrder
&Scoped-define FIRST-TABLE-IN-QUERY-OrderBrowse ttOrder


/* Definitions for FRAME dsFrame                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-dsFrame ~
    ~{&OPEN-QUERY-ItemBrowse}~
    ~{&OPEN-QUERY-OlineBrowse}~
    ~{&OPEN-QUERY-OrderBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS OrderBrowse iCustNum daOrderDate cSalesRep ~
OlineBrowse ItemBrowse cStatus 
&Scoped-Define DISPLAYED-OBJECTS iCustNum daOrderDate cSalesRep cStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR dsOrderWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnSave 
     LABEL "Save Changes" 
     SIZE 19 BY 1.14.

DEFINE VARIABLE cStatus AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 72 BY 1.67 NO-UNDO.

DEFINE VARIABLE cSalesRep AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE daOrderDate AS DATE FORMAT "99/99/99":U 
     LABEL "Order Date" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE iCustNum AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Cust Num" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ItemBrowse FOR 
      ttItem SCROLLING.

DEFINE QUERY OlineBrowse FOR 
      ttOline SCROLLING.

DEFINE QUERY OrderBrowse FOR 
      ttOrder SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ItemBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ItemBrowse dsOrderWin _STRUCTURED
  QUERY ItemBrowse NO-LOCK DISPLAY
      ttItem.Itemnum FORMAT "zzzzzzzzz9":U
      ttItem.ItemName FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 35 BY 6.43 FIT-LAST-COLUMN.

DEFINE BROWSE OlineBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS OlineBrowse dsOrderWin _STRUCTURED
  QUERY OlineBrowse NO-LOCK DISPLAY
      ttOline.Ordernum FORMAT "zzzzzzzzz9":U
      ttOline.Linenum FORMAT ">>9":U
      ttOline.Itemnum FORMAT "zzzzzzzzz9":U
      ttOline.Price FORMAT "->,>>>,>>9.99":U
      ttOline.Qty FORMAT "->>>>9":U
      ttOline.ExtendedPrice FORMAT "->>>,>>9.99":U
      ttOline.Discount FORMAT ">>9%":U
      ttOline.OrderLineStatus FORMAT "x(20)":U
  ENABLE
      ttOline.Price
      ttOline.Qty
      ttOline.Discount
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 101 BY 6.43 FIT-LAST-COLUMN.

DEFINE BROWSE OrderBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS OrderBrowse dsOrderWin _STRUCTURED
  QUERY OrderBrowse NO-LOCK DISPLAY
      ttOrder.Ordernum FORMAT "zzzzzzzzz9":U
      ttOrder.CustNum FORMAT ">>>>9":U
      ttOrder.SalesRep FORMAT "x(4)":U
      ttOrder.OrderDate FORMAT "99/99/99":U WIDTH 53
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 89 BY 4.52 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME dsFrame
     OrderBrowse AT ROW 1.48 COL 9
     iCustNum AT ROW 1.48 COL 110 COLON-ALIGNED
     daOrderDate AT ROW 2.67 COL 110 COLON-ALIGNED
     cSalesRep AT ROW 3.86 COL 110 COLON-ALIGNED
     OlineBrowse AT ROW 6.24 COL 8
     ItemBrowse AT ROW 6.24 COL 110
     BtnSave AT ROW 12.91 COL 8
     cStatus AT ROW 12.91 COL 34 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 145.8 BY 13.76.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ttItem T "?" ? temp-db ttItem
      TABLE: ttOLine T "?" ? temp-db ttOLine
      TABLE: ttOrder T "?" ? temp-db ttOrder
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW dsOrderWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Test Window for Order DataSet"
         HEIGHT             = 13.76
         WIDTH              = 145.8
         MAX-HEIGHT         = 13.76
         MAX-WIDTH          = 145.8
         VIRTUAL-HEIGHT     = 13.76
         VIRTUAL-WIDTH      = 145.8
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
/* BROWSE-TAB OrderBrowse 1 dsFrame */
/* BROWSE-TAB OlineBrowse cSalesRep dsFrame */
/* BROWSE-TAB ItemBrowse OlineBrowse dsFrame */
/* SETTINGS FOR BUTTON BtnSave IN FRAME dsFrame
   NO-ENABLE                                                            */
ASSIGN 
       cStatus:READ-ONLY IN FRAME dsFrame        = TRUE.

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
     _FldNameList[2]   = Temp-Tables.ttItem.ItemName
     _Query            is OPENED
*/  /* BROWSE ItemBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE OlineBrowse
/* Query rebuild information for BROWSE OlineBrowse
     _TblList          = "Temp-Tables.ttOline"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ttOline.Ordernum
     _FldNameList[2]   = Temp-Tables.ttOline.Linenum
     _FldNameList[3]   = Temp-Tables.ttOline.Itemnum
     _FldNameList[4]   > Temp-Tables.ttOline.Price
"ttOline.Price" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.ttOline.Qty
"ttOline.Qty" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = Temp-Tables.ttOline.ExtendedPrice
     _FldNameList[7]   > Temp-Tables.ttOline.Discount
"ttOline.Discount" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   = Temp-Tables.ttOline.OrderLineStatus
     _Query            is OPENED
*/  /* BROWSE OlineBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE OrderBrowse
/* Query rebuild information for BROWSE OrderBrowse
     _TblList          = "Temp-Tables.ttOrder"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ttOrder.Ordernum
     _FldNameList[2]   = Temp-Tables.ttOrder.CustNum
     _FldNameList[3]   = Temp-Tables.ttOrder.SalesRep
     _FldNameList[4]   > Temp-Tables.ttOrder.OrderDate
"ttOrder.OrderDate" ? ? "date" ? ? ? ? ? ? no ? no no "53" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE OrderBrowse */
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


&Scoped-define SELF-NAME BtnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnSave dsOrderWin
ON CHOOSE OF BtnSave IN FRAME dsFrame /* Save Changes */
DO:
  DEFINE VARIABLE hDSChanges AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hDSOrder   AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hQuery     AS HANDLE     NO-UNDO.
  DEFINE VARIABLE hBuffer    AS HANDLE     NO-UNDO.


  hDSOrder = DATASET dsOrder:HANDLE.
  CREATE DATASET hDSChanges.
  hDSChanges:CREATE-LIKE(hDSOrder).
  hDSChanges:GET-CHANGES(hDSOrder).
  TEMP-TABLE ttOline:TRACKING-CHANGES = FALSE.
  
  RUN updateOrder.p (INPUT-OUTPUT DATASET-HANDLE hDSChanges BY-REFERENCE).
   
  /* Check the ERROR status that may have been returned. */
   cStatus = "".
   IF hDSChanges:ERROR THEN
   DO:
       /* There was an error somewhere in the updates. Find it. */
       CREATE QUERY hQuery.
       hBuffer = hDSChanges:GET-BUFFER-HANDLE(2).
       hQuery:ADD-BUFFER(hBuffer).
       hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
       hQuery:QUERY-OPEN().
       hQuery:GET-FIRST().
       DO WHILE NOT hQuery:QUERY-OFF-END:
           IF hBuffer:ERROR THEN
               cStatus = cStatus + hBuffer:ERROR-STRING + CHR(10).
           hQuery:GET-NEXT().
       END.
       hQuery:QUERY-CLOSE().
       DELETE OBJECT hQuery.
   END.
   DISPLAY cStatus WITH FRAME dsFrame.
   /* END of Error status checking. */

  hDSChanges:MERGE-CHANGES(hDSOrder). 
   
   /* This forces the relation queries to re-open and refresh the browse. Doing
   {&OPEN-QUERY-OlineBrowse} does not work because the browse query is the
   relation query, not the base ttOline query. */
   BUFFER ttOrder:SYNCHRONIZE().
   
   DELETE OBJECT hDSChanges.
   /* Re-enable the filter fields to select another set of Orders.
Also, set TRACKING-CHANGES back to TRUE to capture
any further changes made to this Order. */
    ASSIGN iCustNum:SENSITIVE IN FRAME dsFrame = TRUE
           daOrderDate:SENSITIVE IN FRAME dsFrame = TRUE
           cSalesRep:SENSITIVE IN FRAME dsFrame = TRUE
           SELF:SENSITIVE = FALSE
           TEMP-TABLE ttOline:TRACKING-CHANGES = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cSalesRep
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cSalesRep dsOrderWin
ON LEAVE OF cSalesRep IN FRAME dsFrame /* Sales Rep */
DO:
  DEFINE VARIABLE cSelection AS CHARACTER NO-UNDO.
    ASSIGN iCustNum daOrderDate cSalesRep.
    IF iCustNum NE 0 THEN
        cSelection = "CustNum = " + STRING(iCustNum).
    IF daOrderDate NE ? THEN
        cSelection = cSelection + (IF cSelection NE "" THEN " AND " ELSE "") +
          "OrderDate = " + QUOTER(daOrderDate).
    IF cSalesRep NE "" THEN
        cSelection = cSelection + (IF cSelection NE "" THEN " AND " ELSE "") +
          "SalesRep = " + QUOTER(cSalesRep).
    IF cSelection NE "" THEN /* There were selection criteria */
        RUN fetchOrders IN hOrderProc (INPUT cSelection,
            OUTPUT DATASET dsOrder).
    ELSE /* No selection so retrieve (the first) batch of rows. */
        RUN fetchOrderBatch IN hOrderProc
            (INPUT 0, /* Start at the first Order */
            INPUT "OrderNum,CustNum,SalesRep,OrderDate",
            OUTPUT DATASET dsOrder).
    /* RUN fetchOrders IN hOrderProc (INPUT cSelection, OUTPUT DATASET
        dsOrder). */
    BtnSave:SENSITIVE = FALSE.
    /* Set up an OFF-END event handler for the Order buffer to do batching. */
    QUERY OrderBrowse:SET-CALLBACK-PROCEDURE("OFF-END","OffEndOrder",
        THIS-PROCEDURE).
    /* Also a FIND-FAILED event handler for the Item table. */
    BUFFER ttItem:SET-CALLBACK-PROCEDURE("FIND-FAILED","FindFailedItem",
        THIS-PROCEDURE).
    OPEN QUERY OrderBrowse FOR EACH ttOrder.
    OPEN QUERY OrderBrowse FOR EACH ttOrder.
    DATASET dsOrder:GET-BUFFER-HANDLE(1):SYNCHRONIZE().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME OlineBrowse
&Scoped-define SELF-NAME OlineBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL OlineBrowse dsOrderWin
ON ROW-LEAVE OF OlineBrowse IN FRAME dsFrame
DO:
  DEFINE VARIABLE hCol AS HANDLE     NO-UNDO.
  
  IF OlineBrowse:MODIFIED THEN
    ASSIGN INPUT BROWSE OlineBrowse
        {&ENABLED-FIELDS-IN-QUERY-OlineBrowse}
        /* Disable the Order Number until changes are saved. */
        iCustNum:SENSITIVE IN FRAME dsFrame = FALSE
        daOrderDate:SENSITIVE IN FRAME dsFrame = FALSE
        cSalesRep:SENSITIVE IN FRAME dsFrame = FALSE
        BtnSave:SENSITIVE IN FRAME dsFrame = TRUE.
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL OlineBrowse dsOrderWin
ON VALUE-CHANGED OF OlineBrowse IN FRAME dsFrame
DO:
  OlineBrowse:MODIFIED = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME OrderBrowse
&Scoped-define SELF-NAME OrderBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL OrderBrowse dsOrderWin
ON MOUSE-SELECT-DBLCLICK OF OrderBrowse IN FRAME dsFrame
DO:
    DEFINE VARIABLE iOrderNum AS INTEGER NO-UNDO.
    iOrderNum = ttOrder.OrderNum.
    CLOSE QUERY OrderBrowse.
    DELETE ttOrder.
    FOR EACH ttOline WHERE ttOline.OrderNum = iOrderNum:
        DELETE ttOline.
    END.
    RUN fetchOrderDetail IN hOrderProc
            (INPUT iOrderNum,
            /* INPUT NOT CAN-FIND (FIRST ttItem), */
            OUTPUT DATASET dsOrder APPEND).
    OPEN QUERY OrderBrowse FOR EACH ttOrder.
    FIND ttOrder WHERE ttOrder.OrderNum = iOrderNum.
    REPOSITION OrderBrowse TO ROWID ROWID(ttOrder).
    DATASET dsOrder:GET-BUFFER-HANDLE(1):SYNCHRONIZE().
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
DO:
    DELETE PROCEDURE hOrderProc.
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
  RUN orderSupportBatch.p PERSISTENT SET hOrderProc.
  /* In order to see the effect of the REPOSITION relation mode,
     replace the default AppBuilder-generated static queries with
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
  DISPLAY iCustNum daOrderDate cSalesRep cStatus 
      WITH FRAME dsFrame IN WINDOW dsOrderWin.
  ENABLE OrderBrowse iCustNum daOrderDate cSalesRep OlineBrowse ItemBrowse 
         cStatus 
      WITH FRAME dsFrame IN WINDOW dsOrderWin.
  {&OPEN-BROWSERS-IN-QUERY-dsFrame}
  VIEW dsOrderWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FindFailedItem dsOrderWin 
PROCEDURE FindFailedItem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER DATASET FOR dsOrder.
DEFINE VARIABLE cItemName AS CHARACTER NO-UNDO.
    RUN fetchItem IN hOrderProc (ttOline.ItemNum, OUTPUT cItemName).
    CREATE ttItem.
    ASSIGN ttItem.ItemNum = ttOline.ItemNum
           ttItem.ItemName = cItemName.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OffEndOrder dsOrderWin 
PROCEDURE OffEndOrder :
/*----------------------------------------------------------------------
Purpose: Procedure OffEndOrder handles the OFF-END event on the Order query.
    It asks the support procedure for another batch of rows unless the
    LAST-BATCH has already been returned.
Parameters: INPUT DATASET dsOrder
-------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER DATASET FOR dsOrder.
    DEFINE VARIABLE iOrderNum AS INTEGER NO-UNDO.
    /* If the LAST-BATCH flag doesn't indicate that all rows have been returned,
       then pass the current last OrderNum to tell where to start, and
       get another batch. */
    IF NOT BUFFER ttOrder:LAST-BATCH THEN
    DO:
        FIND LAST ttOrder.
        iOrderNum = ttOrder.OrderNum.
        RUN fetchOrderBatch IN hOrderProc
            (INPUT iOrderNum,
            INPUT "", /* Field list only needs to be set once. */
            OUTPUT DATASET dsOrder APPEND).
        BtnSave:SENSITIVE IN FRAME dsFrame = FALSE.
        RETURN NO-APPLY.
    END. /* END DO IF NOT LAST-BATCH */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OlineModified dsOrderWin 
PROCEDURE OlineModified :
/*------------------------------------------------------------------------------
  Purpose:     Event procedure for ROW-MODIFY of the ttOline temp-table.
  Parameters:  The Dataset dsorder
  Notes:       The modified temp-table row and its before-table companion
               should both be available as the current record in their buffers.
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER DATASET FOR dsOrder.

    ttOline.OrderLineStatus =
        IF ttOline.Price > ttOlineBefore.Price THEN "Price Up" ELSE "Price Down".
        ttOline.OrderLineStatus:SCREEN-VALUE IN BROWSE OlineBrowse = ttOline.OrderLineStatus.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

