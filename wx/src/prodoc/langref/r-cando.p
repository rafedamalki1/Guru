/* r-cando.p */

DO FOR permission:
    FIND permission "custedit".
    IF NOT CAN-DO(permission.can-run) THEN DO:
	MESSAGE "You are not authorized to run this procedure".
	RETURN.
    END.
END.
