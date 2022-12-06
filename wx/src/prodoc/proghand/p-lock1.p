/* p-lock1.p */

DEFINE VARIABLE qty-shipped AS INTEGER LABEL "Number Shipped".

REPEAT:
    PROMPT-FOR item.item-num.
    FIND item USING item-num EXCLUSIVE-LOCK.
    DISPLAY item-name on-hand.
    SET qty-shipped.
    on-hand = on-hand - qty-shipped.
    DISPLAY on-hand.
END.
