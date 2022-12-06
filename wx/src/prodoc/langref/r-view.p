/* r-view1.p */

FORM "Please choose one of:" SKIP(1)
     "1 - order entry"       SKIP
     "2 - invoices   "       SKIP
     "3 - exit       "       WITH CENTERED FRAME menu.

REPEAT:
    VIEW FRAME menu.
    READKEY.
    IF LASTKEY = KEYCODE("1") THEN RUN ordentry.
    ELSE
    IF LASTKEY = KEYCODE("2") THEN RUN invoice.
    ELSE
    IF LASTKEY = KEYCODE("3") THEN LEAVE.
    ELSE MESSAGE "Sorry, that is not in the list.".
END.
