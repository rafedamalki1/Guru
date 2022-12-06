/* r-minmum.p */

DEFINE VARIABLE want LIKE on-hand
    LABEL "How many do you want?".
DEFINE VARIABLE ans AS LOGICAL.

REPEAT:
    PROMPT-FOR item.item-num want.
    FIND item USING item-num.
    ans = no.
    IF MINIMUM(INPUT want,on-hand) = INPUT want
    THEN DO:
	MESSAGE "We have enough" item-name "in stock.".
	MESSAGE "Any other items to check?" UPDATE ans.
	IF NOT ans THEN LEAVE.
    END.
    ELSE DO:
	MESSAGE "We only have" on-hand item-name "in stock.".
	MESSAGE "Any other items to check?"
	    UPDATE ans.
	IF NOT ans THEN LEAVE.
    END.
END.
