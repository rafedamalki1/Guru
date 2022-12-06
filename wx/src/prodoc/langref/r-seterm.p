/* r-seterm.p */

FOR EACH customer:
    DISPLAY customer.
END.

TERMINAL = "wy60w".
OUTPUT TO TERMINAL PAGED.
FOR EACH customer:
    DISPLAY customer WITH WIDTH 132.
END.

OUTPUT CLOSE.
TERMINAL = "wy60".
DISPLAY "Back to 80 columns." WITH CENTERED.
