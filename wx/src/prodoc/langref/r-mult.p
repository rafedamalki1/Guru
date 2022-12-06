/* r-mult.p */

DEFINE VARIABLE inv-value AS DECIMAL LABEL "VALUE".

FOR EACH item:
    inv-value = on-hand * price.
    IF inv-value < 0
    THEN inv-value = 0.
    DISPLAY item.item-num item-name on-hand price inv-value.
END.
