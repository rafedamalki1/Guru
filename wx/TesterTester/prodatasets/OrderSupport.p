/* OrderSupport.p -- FILL events for OrderDset.p */
{dsOrderTT.i}
{dsOrder.i}

DEFINE QUERY qOrder FOR Order, Customer, SalesRep.

DEFINE VARIABLE iBuff      AS INTEGER    NO-UNDO.
DEFINE VARIABLE hBuff      AS HANDLE     NO-UNDO.
DEFINE VARIABLE hDataSet   AS HANDLE     NO-UNDO.
DEFINE VARIABLE cSelection AS CHARACTER  NO-UNDO.

DEFINE DATA-SOURCE srcOrder FOR QUERY qOrder 
  Order KEYS (OrderNum), Customer KEYS (CustNum), SalesRep KEYS (SalesRep).
DEFINE DATA-SOURCE srcOline FOR OrderLine.
DEFINE DATA-SOURCE srcItem FOR ITEM  KEYS (ItemNum).
    
hDataSet = DATASET dsOrder:HANDLE.
hDataSet:SET-CALLBACK-PROCEDURE
  ("BEFORE-FILL", "preDataSetFill", THIS-PROCEDURE).
hDataSet:SET-CALLBACK-PROCEDURE
  ("AFTER-FILL", "postDataSetFill", THIS-PROCEDURE).
hDataSet:GET-BUFFER-HANDLE("ttOrder"):SET-CALLBACK-PROCEDURE
  ("BEFORE-FILL", "preOrderFill", THIS-PROCEDURE).
hDataSet:GET-BUFFER-HANDLE("ttOline"):SET-CALLBACK-PROCEDURE
  ("AFTER-FILL", "postOlineFill", THIS-PROCEDURE).
hDataSet:GET-BUFFER-HANDLE("ttItem"):SET-CALLBACK-PROCEDURE
  ("AFTER-ROW-FILL", "postItemRowFill", THIS-PROCEDURE).

PROCEDURE preDataSetFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.

  QUERY qOrder:QUERY-PREPARE("FOR EACH Order WHERE " +
    cSelection + ", FIRST Customer OF Order, FIRST SalesRep OF Order").
END PROCEDURE. /* preDataSetFill */

PROCEDURE postDataSetFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.
   
  DO iBuff = 1 TO DATASET dsOrder:NUM-BUFFERS:
    DATASET dsOrder:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
  END.
END PROCEDURE. /* postDataSetFill */

PROCEDURE preOrderFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.

  BUFFER ttOrder:ATTACH-DATA-SOURCE
    (DATA-SOURCE srcOrder:HANDLE, "Customer.Name,CustName").
  BUFFER ttOline:ATTACH-DATA-SOURCE(DATA-SOURCE srcOline:HANDLE).
  BUFFER ttItem:ATTACH-DATA-SOURCE(DATA-SOURCE srcItem:HANDLE).
END PROCEDURE. /* preOrderFill */

PROCEDURE postOlineFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.

  DEFINE VARIABLE dTotal AS DECIMAL    NO-UNDO.
     
  FOR EACH ttOline WHERE ttOline.OrderNum = ttOrder.OrderNum:
    dTotal = dTotal + ttOline.ExtendedPrice.
  END.
  ttOrder.OrderTotal = dTotal.
END PROCEDURE. /* postRecordFill */

PROCEDURE postItemRowFill:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.

  DEFINE VARIABLE iType      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cItemTypes AS CHARACTER  NO-UNDO
    INITIAL "BASEBALL,CROQUET,FISHING,FOOTBALL,GOLF,SKI,SWIM,TENNIS".
  DEFINE VARIABLE iTypeNum   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cType      AS CHARACTER  NO-UNDO.

  DO iType = 1 TO NUM-ENTRIES(cItemTypes):
    cType = ENTRY(iType, cItemTypes).
    IF INDEX(ttItem.ItemName, cType) NE 0 THEN
      ttItem.ItemName = REPLACE(ttItem.ItemName, cType, cType).
  END.
END PROCEDURE.

PROCEDURE fetchOrders:
  DEFINE INPUT PARAMETER pcSelection AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER DATASET FOR dsOrder BY-VALUE.

  cSelection = pcSelection.
  hDataSet:EMPTY-DATASET.
  hDataSet:GET-BUFFER-HANDLE(2):FILL-MODE = "NO-FILL". /* ttOline */
  hDataSet:GET-BUFFER-HANDLE(3):FILL-MODE = "NO-FILL". /* ttItem */
  hDataSet:FILL().
END PROCEDURE.

PROCEDURE fetchOrderDetail:
    DEFINE INPUT PARAMETER piOrderNum AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER lFillItems AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER DATASET FOR dsOrder BY-VALUE.
    hDataSet:EMPTY-DATASET.
    cSelection = "OrderNum = " + STRING(piOrderNum).
    hDataSet:GET-BUFFER-HANDLE(2):FILL-MODE = "APPEND". /* ttOline */
    hDataSet:GET-BUFFER-HANDLE(3):FILL-MODE = /* ttItem */
    IF lFillItems THEN "APPEND" ELSE "NO-FILL".
    hDataSet:FILL().
END PROCEDURE.
