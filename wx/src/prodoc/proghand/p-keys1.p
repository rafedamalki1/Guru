/* p-keys1.p */

REPEAT:
    DISPLAY "Press any key".
    READKEY.
    DISPLAY LASTKEY LABEL "Key Code"
	    KEYLABEL(LASTKEY) LABEL "Key Label" FORMAT "x(20)"
	    KEYFUNCTION(LASTKEY) LABEL "Key Function" FORMAT "x(20)".
    IF KEYFUNCTION(LASTKEY) = "end-error" THEN LEAVE.
END.
