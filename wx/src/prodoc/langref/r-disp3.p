/* r-disp3.p */

FOR EACH customer:
    DISPLAY name SKIP address SKIP address2 SKIP
	city + ", " + state FORMAT "x(16)"
	postal-code WHEN postal-code NE "" SKIP(2)
	WITH NO-BOX NO-LABELS USE-TEXT.
END.
