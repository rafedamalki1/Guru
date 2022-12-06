/* DynamicMain.p -- gets DynamicDataSet.p to create, fill, and return a 
   dynamic DataSet for these temp-tables. */
DEFINE VARIABLE hDataSet AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttCust     NO-UNDO LIKE Customer.
DEFINE TEMP-TABLE ttOrder    NO-UNDO LIKE Order.
DEFINE TEMP-TABLE ttSalesRep NO-UNDO LIKE SalesRep.

RUN DynamicDataSet.p 
  (INPUT STRING(BUFFER ttCust:HANDLE) + "," +
     STRING(BUFFER ttOrder:HANDLE) + "," + STRING(BUFFER ttSalesRep:HANDLE),
   INPUT "CustNum,CustNum",
   INPUT "Customer,Order,SalesRep",
   INPUT "CustNum,OrderNum,SalesRep",
   INPUT "1",
   OUTPUT DATASET-HANDLE hDataSet).

FOR EACH ttCust:
  DISPLAY ttCust.CustNum ttCust.Name.
END.
FOR EACH ttOrder:
  DISPLAY ttOrder.CustNum ttOrder.OrderNum.
END.
FOR EACH ttSalesRep:
  DISPLAY ttSalesRep.SalesRep ttSalesRep.RepName.
END.

DELETE OBJECT hDataSet.
