/* r-frmat.p */

DEFINE VARIABLE password AS CHARACTER.

UPDATE password FORMAT "x(6)" BLANK
    VALIDATE(password = "secret", "Sorry, wrong password")
    HELP "Maybe the password is 'secret' !"
    WITH FRAME passw CENTERED SIDE-LABELS.
HIDE FRAME passw.

REPEAT:
    PROMPT-FOR customer.cust-num COLON 20.
    FIND customer USING cust-num.
    UPDATE
	name     LABEL "Customer Name" COLON 20
		 VALIDATE(name ne "", "Please enter a name")
	address  HELP  "Please enter two lines of address"
		 COLON 20 LABEL "Address"
	address2 NO-LABEL COLON 22
	city     COLON 20
	state    COLON 20
	postal-code    COLON 20  SKIP(3)
	phone    AT 5 FORMAT "(999) 999-9999"
	contact  TO 60
	WITH CENTERED SIDE-LABELS.
END.
