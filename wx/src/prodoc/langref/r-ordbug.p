DEFINE SHARED BUFFER CustBuf FOR customer.

FOR EACH order OF CustBuf:
    DISPLAY CustBuf.Name CustBuf.Cust-num CustBuf.City CustBuf.State 
            CustBuf.Postal-code order.Order-num.
    FOR EACH order-line OF order, item OF order-line:
        DISPLAY item.Item-name item.Item-num order-line.Qty.
    END. /* FOR EACH order-line */
END. /* FOR EACH order */
