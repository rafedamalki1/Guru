/* r-status.p */

STATUS DEFAULT "All Around Sports Order Processing System".
STATUS INPUT "Enter data, or use the " + KBLABEL("END-ERROR") +
	     " key to exit".
FOR EACH customer:
    DISPLAY name.
    FOR EACH order OF customer:
	UPDATE order-num promise-date order-date ship-date.
    END.
    UPDATE credit-limit.
END.
