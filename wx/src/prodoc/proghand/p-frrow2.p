/* p-frrow2.p */

DEFINE BUTTON ok-button LABEL "OK" AUTO-GO.
DEFINE BUTTON cancel-button LABEL "CANCEL" AUTO-ENDKEY.

cust-loop:
FOR EACH customer:
        DISPLAY customer WITH FRAME cust-frame ROW 3
                2 COLUMNS TITLE "CUSTOMER INFORMATION".
        FOR EACH order OF customer
                    ON ENDKEY UNDO cust-loop, LEAVE cust-loop:
          DISPLAY order-num order-date ship-date promise-date
                  carrier instructions po SKIP
                  ok-button AT 25 cancel-button AT 50
                  WITH 2 COLUMNS 1 DOWN OVERLAY TITLE "CUSTOMER'S ORDERS"
                       ROW FRAME-ROW(cust-frame) + 8
                       COLUMN FRAME-COL(cust-frame) + 1
                       VIEW-AS DIALOG-BOX.  
          SET ok-button cancel-button.                       
        END.
END.

