/* r-scrnline.p */

DEFINE VARIABLE nbrdown AS INTEGER.

IF SCREEN-LINES > 21
THEN nbrdown = 7.
ELSE nbrdown = 6.

FOR EACH customer WITH nbrdown DOWN:
    DISPLAY cust-num name address city state country.
END.
