/* p-frmval.p */

DEFINE BUTTON bContinue LABEL "Continue".
DEFINE BUTTON bQuit LABEL "Quit".
DEFINE FRAME ItemFrame
    item.item-num
    item.item-name
        VALIDATE(item-name BEGINS "B", 
                 "Must enter item-name starting with B")
    item.on-hand
        VALIDATE(on-hand > 0, "Must enter items on-hand > 0.")
    item.allocated
        VALIDATE(allocated <= on-hand, 
                 "Allocated cannot be greater than on-hand items.")
    bContinue
    bQuit
WITH SIDE-LABELS.

ON RETURN OF item.item-num IN FRAME ItemFrame DO:
    FIND FIRST item 
        WHERE item-num = INTEGER(item-num:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE(item) THEN DO:
        DISABLE item-num WITH FRAME ItemFrame.
        DISPLAY item-name on-hand allocated WITH FRAME ItemFrame.
        ENABLE ALL EXCEPT item-num WITH FRAME ItemFrame.
    END.
    ELSE DO:
        MESSAGE "Item not on file.  Enter another.".
        RETURN NO-APPLY.
    END.
END.
ON CHOOSE OF bContinue IN FRAME ItemFrame DO:
    IF NOT FRAME ItemFrame:VALIDATE() THEN
        RETURN NO-APPLY.
    ASSIGN item.item-name item.on-hand item.allocated.
    DISABLE ALL EXCEPT bQuit WITH FRAME ItemFrame.
    ENABLE item.item-num WITH FRAME ItemFrame.
END.
    
FIND FIRST item NO-ERROR.
DISPLAY 
    item.item-num item.item-name item.on-hand item.allocated 
    bContinue bQuit
WITH FRAME ItemFrame.
ENABLE item.item-num bQuit WITH FRAME ItemFrame.
WAIT-FOR CHOOSE OF bQuit IN FRAME ItemFrame.
