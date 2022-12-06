/* dsCodeTT.i -- cacheing object for various code tables */
DEFINE TEMP-TABLE ttSalesRep
  FIELD RepCode AS CHARACTER FORMAT "x(4)"
  FIELD RepName AS CHARACTER FORMAT "x(20)"
  FIELD Region AS CHARACTER FORMAT "x(12)"
  FIELD AnnualQuota AS DECIMAL
  FIELD TotalBalance AS DECIMAL
  INDEX RepCode IS UNIQUE RepCode.

DEFINE TEMP-TABLE ttDept LIKE Department.

DEFINE TEMP-TABLE ttState LIKE State.
