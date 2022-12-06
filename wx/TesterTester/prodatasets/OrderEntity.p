/* OrderEntity.p -- Entity procedure for ProDataSet dsOrder */
{dsOrderTT.I}
{dsOrder.i}

DEFINE VARIABLE hDataSet    AS HANDLE NO-UNDO.
DEFINE VARIABLE hSourceProc AS HANDLE NO-UNDO.

hDataSet = DATASET dsOrder:HANDLE.

ON CLOSE OF THIS-PROCEDURE DO:
  DELETE PROCEDURE hSourceProc.
  DELETE PROCEDURE THIS-PROCEDURE.
END.

RUN OrderSource.p PERSISTENT SET hSourceProc.

PROCEDURE fetchOrder:
  DEFINE INPUT PARAMETER piOrderNum AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER DATASET FOR dsOrder BY-VALUE.

  /* This turns around and runs an equivalent procedure in the Data-Source 
     procedure, passing in the static dataSet. */
  DYNAMIC-FUNCTION("attachDataSet" IN hSourceProc, hDataSet).
  RUN fetchOrder IN hSourceProc 
    (INPUT piOrderNum, INPUT-OUTPUT DATASET dsOrder BY-REFERENCE).
  DYNAMIC-FUNCTION("detachDataSet" IN hSourceProc, INPUT hDataSet).
END PROCEDURE. /* fetchOrder */

PROCEDURE saveChanges:
   DEFINE INPUT PARAMETER DATASET FOR dsOrder.
   /*anders
  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrder.
*/
  DEFINE VARIABLE hDataSet AS HANDLE NO-UNDO.

  hDataSet = DATASET dsOrder:HANDLE.
  DYNAMIC-FUNCTION("attachDataSet" IN hSourceProc, INPUT hDataSet).
  RUN commitChanges.p (INPUT-OUTPUT DATASET dsOrder BY-REFERENCE).
  DYNAMIC-FUNCTION("detachDataSet" IN hSourceProc, INPUT hDataSet).
END PROCEDURE. /* saveChanges */

PROCEDURE ttOlineModify:
  DEFINE INPUT PARAMETER DATASET FOR dsOrder.

  /* If the customer doubled the quantity ordered, then increase the 
     discount by 20%. */
  IF ttOline.Qty >= (ttOlineBefore.Qty * 2) AND
    ttOline.Discount = ttOlineBefore.Discount THEN
    ttOline.Discount = ttOlineBefore.Discount * 1.2.
  ELSE IF ttOline.Qty <= (ttOlineBefore.Qty * .5) THEN
    ASSIGN 
      BUFFER ttOline:ERROR = TRUE
      BUFFER ttOline:ERROR-STRING = "Line " + STRING(ttOline.LineNum) +
        ": You can't drop the Qty that much!".
  RETURN.
END PROCEDURE. /* ttOlineModify */
