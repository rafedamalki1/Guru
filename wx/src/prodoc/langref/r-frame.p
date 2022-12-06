/* r-frame.p */

FOR EACH customer:
    FORM
	HEADER "No-box, No-Underline, No-labels, 5 DOWN"
	SKIP   "Centered" SKIP(2)
	WITH NO-BOX NO-UNDERLINE NO-LABELS
	CENTERED 5 DOWN.
    DISPLAY cust-num name phone.
END.
