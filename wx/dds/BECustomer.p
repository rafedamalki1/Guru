
/*------------------------------------------------------------------------
    File        : BECustomer.p
    Purpose     : 

    Syntax      :

    Description : Server side Business Entity procedure for Customer prodataset

    Author(s)   : 
    Created     : Thu Dec 31 09:55:53 GMT 2015
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE VARIABLE h_DataSet    AS HANDLE  NO-UNDO.
DEFINE VARIABLE h_DACustomer AS HANDLE  NO-UNDO.
DEFINE VARIABLE lError       AS LOGICAL NO-UNDO.

/* Start the Data Access Procedure */
RUN DACustomer.p PERSISTENT SET h_DACustomer.

/* **********************  Internal Procedures  *********************** */

PROCEDURE DeleteDAproc:
    IF VALID-HANDLE(h_DACustomer) THEN
        DELETE PROCEDURE h_DACustomer.
END PROCEDURE.

PROCEDURE fetchCustomer:
    DEFINE INPUT PARAMETER pcBuffers    AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER pcFields     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcSources    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcSourceKeys AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcKeyValue   AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet.
        
    CREATE DATASET phDataSet.
    
    /* Attach data source to the Business entities (passed BY-REFERENCE) dataset */
    RUN attachSource IN h_DACustomer (
        INPUT "ttCust," + "ttOrder," + "ttSalesRep",
        INPUT "CustNum,CustNum",
        INPUT "Customer,Order,SalesRep",
        INPUT "CustNum,OrderNum,SalesRep",
        INPUT "1",
        INPUT-OUTPUT DATASET-HANDLE phDataSet BY-REFERENCE).
   
    /* Fill the Business Entities dataset (passed BY-REFERENCE) */
    RUN fetchCustomer IN h_DACustomer (INPUT-OUTPUT DATASET-HANDLE phDataSet BY-REFERENCE).

    /* Dettach data source from the Business entities (passed BY-REFERENCE) dataset */   
    RUN dettachSource IN h_DACustomer (INPUT DATASET-HANDLE phdataset BY-REFERENCE).   
END PROCEDURE.

PROCEDURE SaveChanges:
    DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phChangeDataSet.

    /* Attach data source to the Business entities (passed BY-REFERENCE) dataset */
    RUN attachSource IN h_DACustomer (
        INPUT "ttCust," + "ttOrder," + "ttSalesRep",
        INPUT "CustNum,CustNum",
        INPUT "Customer,Order,SalesRep",
        INPUT "CustNum,OrderNum,SalesRep",
        INPUT "1",
        INPUT-OUTPUT DATASET-HANDLE phChangeDataSet BY-REFERENCE).
       
    /* Commit the changes */   
    RUN commitChanges IN h_DACustomer (INPUT-OUTPUT DATASET-HANDLE phChangeDataset BY-REFERENCE).
  
    /* Dettach data source from the Business entities (passed BY-REFERENCE) dataset */
    RUN dettachSource IN h_DACustomer (INPUT dataset-handle phChangedataset BY-REFERENCE).
END PROCEDURE.


