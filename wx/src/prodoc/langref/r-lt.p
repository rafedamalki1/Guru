/* r-lt.p */

FOR EACH item WHERE on-hand < allocated:
    DISPLAY item.item-num item-name on-hand allocated.
END.
