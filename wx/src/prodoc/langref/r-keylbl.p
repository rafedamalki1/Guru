/* r-keylbl.p */

DISPLAY "Press the " + KBLABEL("GO") + " key to leave procedure"
   FORMAT "x(50)".
REPEAT:
    READKEY.
    HIDE MESSAGE.
    IF LASTKEY = KEYCODE(KBLABEL("GO")) THEN RETURN.
    MESSAGE "Sorry, you pressed the"  KEYLABEL(LASTKEY) "key.".
END.
