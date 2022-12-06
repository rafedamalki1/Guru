/* r-collbl.p */

DEFINE VARIABLE credit-percent AS INTEGER
    COLUMN-LABEL "Enter   !percentage!increase ".

FOR EACH customer:
    DISPLAY name credit-limit.
    SET credit-percent.
    credit-limit = (credit-limit * (credit-percent / 100))
		 + credit-limit.
    DISPLAY credit-limit @ new-credit LIKE credit-limit
	LABEL "New max cred".
END.
