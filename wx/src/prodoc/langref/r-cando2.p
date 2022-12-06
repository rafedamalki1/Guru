/* r-cando2.p */

IF NOT CAN-DO("manager, !acctg8, acctg*")
THEN DO:
	MESSAGE "You are not authorized to run this procedure.".
	RETURN.
END.
