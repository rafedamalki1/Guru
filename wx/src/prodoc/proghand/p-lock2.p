/* p-lock2.p */

DEFINE VARIABLE qty-recvd AS INTEGER LABEL "Number Received".

REPEAT:
    PROMPT-FOR item.item-num.
    FIND item USING item-num.
    DISPLAY item-name on-hand.
    SET qty-recvd.
    on-hand = on-hand + qty-recvd.
    DISPLAY on-hand.
END.
