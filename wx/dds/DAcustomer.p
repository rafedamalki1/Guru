
/*------------------------------------------------------------------------
    File        : DAcustomer.p
    Purpose     : 

    Syntax      :

    Description : Server side Data Access Object for the customer prodataset 

    Author(s)   : 
    Created     : Thu Dec 31 09:52:06 GMT 2015
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttCust NO-UNDO LIKE Customer.
DEFINE TEMP-TABLE ttOrder NO-UNDO LIKE Order.
DEFINE TEMP-TABLE ttSalesRep NO-UNDO LIKE SalesRep.

/* **********************  Internal Procedures  *********************** */

PROCEDURE attachSource:
    DEFINE INPUT PARAMETER pcBuffers    AS CHARACTER NO-UNDO.   
    DEFINE INPUT PARAMETER pcFields     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcSources    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcSourceKeys AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pcKeyValue   AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE phDataSet.

    DEFINE VARIABLE iEntry      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hDataSource AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuffer     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery      AS HANDLE    NO-UNDO.

    DEFINE VARIABLE h_buff      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cQuery      AS CHARACTER NO-UNDO.
     
    DO iEntry = 1 TO NUM-ENTRIES(pcBuffers):
        CREATE BUFFER h_buff FOR TABLE ENTRY(iEntry, pcBuffers).
    
        phDataSet:ADD-BUFFER(h_buff).
    END.

    /* If no relation then create one between ttCust and ttOrder */
    /* If there is a relation (as there is when committing changes), do nothing */ 
    IF phDataSet:num-relations = 0 THEN 
        phDataSet:ADD-RELATION(phDataSet:GET-BUFFER-HANDLE(1), phDataSet:GET-BUFFER-HANDLE(2), pcFields).
                       
    DO iEntry = 1 TO NUM-ENTRIES(pcSources):
        CREATE DATA-SOURCE hDataSource.
        CREATE BUFFER hBuffer FOR TABLE ENTRY(iEntry, pcSources).
        hDataSource:ADD-SOURCE-BUFFER(hBuffer, ENTRY(iEntry,pcSourceKeys)).
        phDataSet:GET-BUFFER-HANDLE(iEntry):ATTACH-DATA-SOURCE(hDataSource).
 
        IF iEntry = 1 THEN 
        DO:
            CREATE QUERY hQuery.
            hQuery:ADD-BUFFER(hBuffer).
            hQuery:QUERY-PREPARE("FOR EACH " + ENTRY(1, pcSources) +
                " WHERE " + ENTRY(1, pcSourceKeys) + " = " + pcKeyValue).
            hDataSource:QUERY = hQuery.

        END. /* DO IF iEntry = 1 */
    END. /* DO iEntry = 1 TO NUM-ENTRIES */
END PROCEDURE.

PROCEDURE CommitChanges:
    DEFINE INPUT-OUTPUT PARAMETER DATASET-handle phDataSet.
    
    /* Generic external procedure to commit the changes to the database */ 
    RUN commitChanges.p (INPUT-OUTPUT DATASET-HANDLE phDataset BY-REFERENCE).
END PROCEDURE.

PROCEDURE fetchCustomer:
    DEFINE INPUT-OUTPUT PARAMETER DATASET-handle phDataSet.
    /* populate the prodatset with data */
    phDataSet:FILL().
END PROCEDURE.

PROCEDURE dettachSource:
    DEFINE INPUT PARAMETER DataSET-handle phDataSet.
        
    DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
  
    DO iBuff = 1 TO phDataSet:NUM-BUFFERS:
        phDataSet:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
    END.
END PROCEDURE.