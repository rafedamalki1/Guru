/* p-keys.p */

REPEAT:
    DISPLAY "Press any key".
    READKEY.
    DISPLAY LASTKEY LABEL "Key Code"
	    KEYLABEL(LASTKEY) LABEL "Key Label"
	    KEYFUNCTION(LASTKEY) LABEL "Key Function" FORMAT "x(12)".
    IF KEYFUNCTION(LASTKEY) = "end-error" THEN LEAVE.
END.
