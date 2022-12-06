/* OrderSource.p -- Data-Sources and FILL events for Order ProDataSet */
{dsOrderTT.i}
{dsOrder.i}

DEFINE QUERY qOrder FOR Order, Customer, SalesRep.
DEFINE DATA-SOURCE srcOrder FOR QUERY qOrder
  Order KEYS (OrderNum), Customer KEYS (CustNum), SalesRep KEYS (SalesRep).
DEFINE DATA-SOURCE srcOline FOR OrderLine.
DEFINE DATA-SOURCE srcItem FOR ITEM KEYS (ItemNum).

PROCEDURE fetchOrder:
  DEFINE INPUT PARAMETER piOrderNum AS INTEGER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.

  QUERY qOrder:QUERY-PREPARE("FOR EACH Order WHERE Order.OrderNum = " +
    STRING(piOrderNum) + ", FIRST Customer OF Order, FIRST SalesRep OF Order").
  DATASET dsOrder:EMPTY-DATASET().
  IF VALID-HANDLE(DATASET dsOrder:GET-BUFFER-HANDLE(1):DATA-SOURCE) THEN
    DATASET dsOrder:FILL().
  ELSE DO:
    DATASET dsOrder:GET-BUFFER-HANDLE(1):TABLE-HANDLE:ERROR-STRING =
      "Data-Sources not attached".
    DATASET dsOrder:ERROR = TRUE.
  END.
  RETURN.
END PROCEDURE. /* fetchOrder */

PROCEDURE postOlineFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.
  
  DEFINE VARIABLE dTotal AS DECIMAL NO-UNDO.

  /* Here as well "ttOline" uses the local definition for compilation but 
     points to the ttOline table in the input parameter at run time. */
  FOR EACH ttOline WHERE ttOline.OrderNum = ttOrder.OrderNum:
    dTotal = dTotal + ttOline.ExtendedPrice.
  END.
  ttOrder.OrderTotal = dTotal.
END PROCEDURE. /* postOlineFill */

PROCEDURE postItemRowFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.

  DEFINE VARIABLE iType      AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cItemTypes AS CHARACTER NO-UNDO
   INITIAL "BASEBALL,CROQUET,FISHING,FOOTBALL,GOLF,SKI,SWIM,TENNIS".
  DEFINE VARIABLE iTypeNum   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cType      AS CHARACTER NO-UNDO.

  DO iType = 1 TO NUM-ENTRIES(cItemTypes):
    cType = ENTRY(iType, cItemTypes).
    IF INDEX(ttItem.ItemName, cType) NE 0 THEN
      ttItem.ItemName = REPLACE(ttItem.ItemName, cType, cType).
  END.
END PROCEDURE. /* postItemRowFill */

FUNCTION attachDataSet RETURNS LOGICAL (INPUT phDataSet AS HANDLE):
  phDataSet:GET-BUFFER-HANDLE("ttOline"):SET-CALLBACK-PROCEDURE
    ("AFTER-FILL", "postOlineFill", THIS-PROCEDURE).
  phDataSet:GET-BUFFER-HANDLE("ttItem"):SET-CALLBACK-PROCEDURE
    ("AFTER-ROW-FILL", "postItemRowFill", THIS-PROCEDURE).
  phDataSet:GET-BUFFER-HANDLE("ttOrder"):ATTACH-DATA-SOURCE (DATA-SOURCE srcOrder:HANDLE, "Customer.Name,CustName").
  phDataSet:GET-BUFFER-HANDLE("ttOline"):ATTACH-DATA-SOURCE (DATA-SOURCE srcOline:HANDLE).
  phDataSet:GET-BUFFER-HANDLE("ttItem"):ATTACH-DATA-SOURCE (DATA-SOURCE srcItem:HANDLE).
END FUNCTION. /* attachDataSet */

FUNCTION detachDataSet RETURNS logic (INPUT phDataSet AS HANDLE):
  DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
  
  DO iBuff = 1 TO DATASET dsOrder:NUM-BUFFERS:
    phDataSet:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
  END.
END FUNCTION. /* detachDataSet */
