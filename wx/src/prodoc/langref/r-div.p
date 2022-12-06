/* r-div.p */

DISPLAY "INVENTORY COMMITMENTS AS A PERCENT OF UNITS ON HAND".

FOR EACH item:
    DISPLAY item.item-num item-name alloc on-hand (alloc / on-hand) * 100
	FORMAT ">>9" LABEL "PCT".
END.
