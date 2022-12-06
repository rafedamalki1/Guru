/* p-itlst.p */

FOR EACH customer WHERE credit-limit > 50000 BY postal-code:
  DISPLAY name address city state postal-code credit-limit WITH SIDE-LABELS.
    FOR EACH order OF customer:
      DISPLAY order WITH SIDE-LABELS.
      FOR EACH order-line OF order, item OF order-line:
        DISPLAY line-num item.item-num item-name qty order-line.price
         WITH SIDE-LABELS.
      END.
    END.
END.
