
/*------------------------------------------------------------------------
    File        : FillDSCustomer.p
    Purpose     : 

    Syntax      :

    Description : Client side procedure to fill the Dynamic customer prodataset, 
                  make a change and save the changes on the Appserver.

    Author(s)   : 
    Created     : Thu Dec 31 09:53:39 GMT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE hserver    AS HANDLE  NO-UNDO.
DEFINE VARIABLE h_BEproc   AS HANDLE  NO-UNDO.
DEFINE VARIABLE hDataSet   AS HANDLE  NO-UNDO.
DEFINE VARIABLE ibuffer    AS INTEGER NO-UNDO.
DEFINE VARIABLE hBuffer    AS HANDLE  NO-UNDO.
DEFINE VARIABLE hCustBuff  AS HANDLE  NO-UNDO.
DEFINE VARIABLE hDSChanges AS HANDLE  NO-UNDO.
DEFINE VARIABLE httCust    AS HANDLE  NO-UNDO.


CREATE SERVER hServer.
hserver:CONNECT("-AppService asbroker1 -H localhost -S 5162 ").

/* Start the Business Entity Procedure */
RUN BECustomer.p ON hserver PERSISTENT SET h_BEproc.

/* Populate the dynamic prodataset */
RUN fetchCustomer IN h_BEproc
    (INPUT "ttCust," + "ttOrder," + "ttSalesRep",
    INPUT "CustNum,CustNum",
    INPUT "Customer,Order,SalesRep",
    INPUT "CustNum,OrderNum,SalesRep",
    INPUT "1",
    INPUT-OUTPUT DATASET-HANDLE hDataSet BY-VALUE).

hDataSet:WRITE-XML("file","fulldataset.xml",TRUE,?,?,FALSE,FALSE).

/* Now the prodataset is populated make a client side change to its data */
/* Get the prodataset custoner buffer handle */ 
DO iBuffer = 1 TO hDataSet:NUM-BUFFERS:
    hBuffer = hDataSet:GET-BUFFER-HANDLE(iBuffer).
    IF hBuffer:NAME = "ttCust" THEN
        hCustBuff = hBuffer.
END.

/* Make a change to the customer Buffer */
httCust = hCustBuff:TABLE-HANDLE.
httCust:TRACKING-CHANGES = TRUE.
hCustBuff:FIND-FIRST().
hCustBuff:BUFFER-FIELD(2):BUFFER-VALUE = hCustBuff:BUFFER-FIELD(2):BUFFER-VALUE + "X".
MESSAGE "CustBuffer " hCustBuff:NAME SKIP
    hCustBuff:BUFFER-FIELD(1):NAME
    hCustBuff:BUFFER-FIELD(1):BUFFER-VALUE SKIP
    hCustBuff:BUFFER-FIELD(2):NAME
    hCustBuff:BUFFER-FIELD(2):BUFFER-VALUE
    VIEW-AS ALERT-BOX.

/* Create Changes dataset */
CREATE DATASET hDSChanges.
hDSChanges:CREATE-LIKE (hDataSet).
/* Populate the changes dataset with the changes */
hDSChanges:GET-CHANGES (hDataSet).
httCust:TRACKING-CHANGES = FALSE.
hDSChanges:WRITE-XML("file","datasetChanges.xml",TRUE,?,?,FALSE,FALSE).

/* Save the change to the database - Get error 12310 here without the NO-ERROR - not sure why? */
RUN SaveChanges IN h_BEproc (INPUT-OUTPUT DATASET-HANDLE hDSChanges) NO-ERROR.

/* Delete the Data Access Object on the server */
RUN deleteDAproc IN h_BEproc.
/* Delete Business Entity */
DELETE PROCEDURE h_BEProc.
hserver:DISCONNECT().
DELETE OBJECT hserver.
