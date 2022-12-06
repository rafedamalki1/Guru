/* r-subt.p */

DEFINE VARIABLE free-stock LIKE on-hand LABEL "Free Stock".

FOR EACH item:
    free-stock = on-hand - allocated.
    DISPLAY item-num item-name on-hand allocated free-stock.
END.
