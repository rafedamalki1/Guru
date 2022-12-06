/* p-txn.p */

DEFINE VARIABLE newqty LIKE qty LABEL "New Quantity".

REPEAT WITH 2 COLUMNS:
    PROMPT-FOR order-line.order-num line-num.
    FIND order-line USING order-num AND line-num.
    FIND item OF order-line.
    DISPLAY order-line.qty item.item-num item.item-name item.allocated.
    SET newqty.
    item.allocated = item.allocated - order-line.qty + newqty. 
    order-line.qty = newqty.
    DISPLAY item.allocated @ new-alloc
            LIKE item.allocated LABEL "New Alloc" SKIP(1).
    PAUSE.
END.
