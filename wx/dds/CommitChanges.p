/*------------------------------------------------------------------------
    File        : CommitChanges.p 
    Purpose     :  

    Syntax      :

    Description : Generic server side procedure to write client side prodataset changes to the database
                  Called from the Data Access object DACustomer.p

    Author(s)   : 
    Created     : Thu Dec 31 09:52:06 GMT 2015
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER DATASET-HANDLE hDataSet.

DEFINE VARIABLE hTopBuff    AS HANDLE  NO-UNDO.
DEFINE VARIABLE iBuff       AS INTEGER NO-UNDO.
DEFINE VARIABLE hSourceProc AS HANDLE  NO-UNDO.

hSourceProc = SOURCE-PROCEDURE.

DO iBuff = 1 TO hDataSet:NUM-TOP-BUFFERS:
    hTopBuff = hDataSet:GET-TOP-BUFFER(iBuff).
    /* Skip the reposition children. */
    IF hTopBuff:PARENT-RELATION NE ? THEN NEXT. 
    RUN traverseBuffers (hTopBuff).
END. /* END DO iBuff */

PROCEDURE traverseBuffers:
    DEFINE INPUT PARAMETER phBuffer AS HANDLE NO-UNDO.

    DEFINE VARIABLE iChildRel AS INTEGER NO-UNDO.

    RUN saveBuffer(phBuffer).
    DO iChildRel = 1 TO phBuffer:NUM-CHILD-RELATIONS:
        RUN traverseBuffers (phBuffer:GET-CHILD-RELATION(iChildRel):CHILD-BUFFER).
    END. /* END DO iChildRel */
END PROCEDURE. /* traverseBuffers */

PROCEDURE saveBuffer:
    DEFINE INPUT PARAMETER phBuffer AS HANDLE NO-UNDO.

    DEFINE VARIABLE hBeforeBuff AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBeforeQry  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cLogicProc  AS CHARACTER NO-UNDO.

    hBeforeBuff = phBuffer:BEFORE-BUFFER.
    IF VALID-HANDLE(hBeforeBuff) THEN 
    DO:
/*        Create a query on the before buffer BIttCust, then read through the change records*/
/*        and commit the changes to the database                                            */
        CREATE QUERY hBeforeQry.
        hBeforeQry:ADD-BUFFER(hBeforeBuff).
        hBeforeQry:QUERY-PREPARE("FOR EACH " + hBeforeBuff:NAME).
        hBeforeQry:QUERY-OPEN().
        hBeforeQry:GET-FIRST().

        DO WHILE NOT hBeforeQry:QUERY-OFF-END:
/*/*            Logic procedure for changes validation (ttCustDelete, ttCustCreate, ttCustModify)*/*/
/*            cLogicProc = phBuffer:TABLE-HANDLE:NAME +                                            */
/*                IF hBeforeBuff:ROW-STATE = ROW-DELETED THEN "Delete"                             */
/*                ELSE IF hBeforeBuff:ROW-STATE = ROW-CREATED THEN "Create"                        */
/*                ELSE "Modify".                                                                   */
               
      
            /*      RUN VALUE (cLogicProc) IN hSourceProc                   */
            /*        (INPUT DATASET-HANDLE hDataSet BY-REFERENCE) NO-ERROR.*/

            MESSAGE "CommitChanges : " hBeforeBuff:NAME SKIP
                hBeforeBuff:BUFFER-FIELD(1):NAME
                hBeforeBuff:BUFFER-FIELD(1):BUFFER-VALUE SKIP
                hBeforeBuff:BUFFER-FIELD(2):NAME
                hBeforeBuff:BUFFER-FIELD(2):BUFFER-VALUE
                VIEW-AS ALERT-BOX.

            /* The actual SAVE to the database */
            IF NOT hBeforeBuff:ERROR THEN
                hBeforeBuff:SAVE-ROW-CHANGES().

            /* If there was an error signal that this row did not make it into the database. */
            IF hBeforeBuff:ERROR THEN
                ASSIGN 
                    hDataSet:ERROR       = TRUE
                    hBeforeBuff:REJECTED = TRUE.
            hBeforeQry:GET-NEXT().
        END. /* DO WHILE NOT QUERY-OFF-END */
        DELETE OBJECT hBeforeQry.
    END. /* DO IF VALID-HANDLE */
END PROCEDURE. /* saveChanges */
