DEFINE INPUT PARAMETER  wid-hand  AS WIDGET-HANDLE.
DEFINE PARAMETER BUFFER curr-buff FOR Customer.

FIND curr-buff WHERE curr-buff.Cust-num = 
                     INT(wid-hand:SCREEN-VALUE) NO-ERROR.

IF NOT AVAILABLE(curr-buff)
THEN DO:
   MESSAGE "Record not found.".
   RETURN ERROR.
END. 
   
RETURN.
