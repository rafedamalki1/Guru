/* r-le.p */

FOR EACH item WHERE on-hand <= 0:
    DISPLAY item.item-num item-name on-hand.
END.
