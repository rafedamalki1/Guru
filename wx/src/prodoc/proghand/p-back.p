/* p-back.p */

DEFINE RECTANGLE back-rect  SIZE 50 BY 4 NO-FILL EDGE-PIXELS 1.

DEFINE FRAME x 
    customer.cust-num  SKIP(2)
    customer.name AT 5 SKIP
    customer.address AT 5 SKIP
    customer.address2 AT 5 SKIP
    customer.city AT 5 customer.state customer.postal-code SKIP(2)
    customer.phone customer.balance customer.sales-rep
    BACKGROUND back-rect AT COLUMN 2 ROW 8
    WITH USE-TEXT.

FOR EACH customer:
    DISPLAY cust-num name address address2 city state postal-code
            phone balance sales-rep WITH FRAME x.
END.

