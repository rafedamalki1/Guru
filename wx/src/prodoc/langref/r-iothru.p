/* r-iothru.p */

FOR EACH item WHERE item-num < 10:
    DISPLAY item-num price LABEL "Price before recalculation".
END.

INPUT-OUTPUT THROUGH r-iothru UNBUFFERED.

FOR EACH item WHERE item-num < 10:
    EXPORT price.
    SET price.
END.

INPUT-OUTPUT CLOSE.

FOR EACH item WHERE item-num < 10 WITH COLUMN 40:
    DISPLAY item-num price LABEL "Price after recalculation".
END.
