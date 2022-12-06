/* r-avail.p */

REPEAT:
    PROMPT-FOR item.item-num.
    FIND item USING item-num NO-ERROR.
    IF AVAILABLE item
    THEN DISPLAY item-name price.
    ELSE MESSAGE "Not found".
END.
