/* r-fore3.p */

FOR EACH customer, LAST order OF customer:
     DISPLAY customer.cust-num customer.name order.order-num
             order.order-date order.instructions.
     PAUSE 1 NO-MESSAGE.
     instructions = "Last order".
     DISPLAY instruction.
END.
