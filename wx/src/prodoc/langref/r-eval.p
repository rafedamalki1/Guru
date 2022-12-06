/* r-eval.p */

DEFINE VARIABLE i AS INTEGER FORMAT ">9".

FORM HEADER "This is the header - i is"
     i WITH FRAME a ROW i COLUMN i i DOWN.

DO i = 1 TO 8 WITH FRAME a.
    DISPLAY i.
    PAUSE.
END.
