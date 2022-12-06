/* p-csmnu3.p */

DEFINE VARIABLE Selection AS INTEGER FORMAT "9".
RUN _prostar.p.

REPEAT:
    FORM
	SKIP(2) "     M A I N  M E N U  "
	SKIP(1) "  1) Add a new customer"
	SKIP(1) "  2) Change customer information"
	SKIP(1) "  3) Display orders    "
	SKIP(1) "  4) Create mailing labels"
	SKIP(1) "  5) Delete a customer "
	SKIP(1) "  6) EXIT              "
	    WITH CENTERED TITLE "Maintenance and Reporting".
    UPDATE SKIP(2) SPACE(1) selection AUTO-RETURN WITH SIDE-LABELS.
    HIDE.
	 IF selection EQ 1 THEN RUN p-adcust.p.
    ELSE IF selection EQ 2 THEN RUN p-chcust.p.
    ELSE IF selection EQ 3 THEN RUN p-itlist.p.
    ELSE IF selection EQ 4 THEN RUN p-rept6.p.
    ELSE IF selection EQ 5 THEN RUN p-delcus.p.
    ELSE IF selection EQ 6 THEN QUIT.
    ELSE MESSAGE "Incorrect selection - please try again".
END.
