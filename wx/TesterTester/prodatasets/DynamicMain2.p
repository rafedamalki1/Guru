/* DynamicMain2.p -- gets DynamicDataSet2.p to create, fill, and return a 
   dynamic DataSet for these temp-tables. */

DEFINE VARIABLE hDataSet  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hBuffer   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hQuery    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hRelation AS HANDLE  NO-UNDO.
DEFINE VARIABLE iBuffer   AS INTEGER NO-UNDO.

RUN DynamicDataSet2.p 
  (INPUT "Customer,Order,SalesRep",
   INPUT "CustNum,OrderNum,SalesRep",
   INPUT "CustNum,CustNum",
   INPUT "= 1",
   OUTPUT DATASET-HANDLE hDataSet).

CREATE QUERY hQuery.
/* This block shows how to access the DataSet's buffers and the data in their 
   temp-table records. */
/* 
DO iBuffer = 1 TO hDataSet:NUM-BUFFERS:
  hBuffer = hDataSet:GET-BUFFER-HANDLE(iBuffer).
  hBuffer:FIND-FIRST().
  MESSAGE "Buffer " hBuffer:NAME SKIP
    hBuffer:BUFFER-FIELD(1):NAME
    hBuffer:BUFFER-FIELD(1):BUFFER-VALUE SKIP
    hBuffer:BUFFER-FIELD(2):NAME
    hBuffer:BUFFER-FIELD(2):BUFFER-VALUE VIEW-AS ALERT-BOX.
END.
*/
/* This block shows the attribute and method that access the list of DataSet 
   buffers that are not children in a relation.*/
/* 
DO iBuffer = 1 TO hDataSet:NUM-TOP-BUFFERS:
  hBuffer = hDataSet:GET-TOP-BUFFER(iBuffer).
  MESSAGE "Buffer " iBuffer hBuffer:NAME VIEW-AS ALERT-BOX.
END.
*/
/* This block shows some of the Data-Relation methods and attributes. */
hRelation = hDataSet:GET-RELATION(1).
/* 
MESSAGE "Buffer" hRelation:CHILD-BUFFER:NAME "is the child of"
  hRelation:PARENT-BUFFER:NAME VIEW-AS ALERT-BOX.
MESSAGE "WHERE-STRING: " hRelation:WHERE-STRING SKIP
  "RELATION-FIELDS: " hRelation:RELATION-FIELDS VIEW-AS ALERT-BOX.
*/
hRelation:PARENT-BUFFER:FIND-FIRST(). 
hQuery = hRelation:QUERY.
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
hBuffer = hRelation:CHILD-BUFFER.
/* 
DO WHILE NOT hQuery:QUERY-OFF-END:
  MESSAGE hBuffer:BUFFER-FIELD(1):NAME
    hBuffer:BUFFER-FIELD(1):BUFFER-VALUE SKIP
    hBuffer:BUFFER-FIELD(2):NAME
    hBuffer:BUFFER-FIELD(2):BUFFER-VALUE VIEW-AS ALERT-BOX.
    hQuery:GET-NEXT().
END.
*/

DELETE OBJECT hDataSet.
