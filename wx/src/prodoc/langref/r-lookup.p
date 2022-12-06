/* r-lookup.p */

DEFINE VARIABLE stlist AS CHARACTER
    INITIAL "ME,MA,VT,RI,CT,NH".
DEFINE VARIABLE state AS CHARACTER FORMAT "x(2)".

REPEAT:
    SET state LABEL
	"Enter a New England state, 2 characters".
    IF LOOKUP( state, stlist) =  0
    THEN MESSAGE "This is not a New England state".
END.
