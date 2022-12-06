/* p-frrow.p */

cust-loop:
FOR EACH customer:
        DISPLAY customer WITH FRAME cust-frame 
                2 COLUMNS TITLE "CUSTOMER INFORMATION".
        FOR EACH order OF customer
                    ON ENDKEY UNDO cust-loop, LEAVE cust-loop:
          DISPLAY order-num order-date ship-date promise-date
                  carrier instructions po
                  WITH 2 COLUMNS 1 DOWN OVERLAY TITLE "CUSTOMER'S ORDERS"
                       ROW FRAME-ROW(cust-frame) + 8
                       COLUMN FRAME-COL(cust-frame) + 1.
        END.
END.

