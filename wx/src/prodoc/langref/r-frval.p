/* r-frval.p */

FOR EACH customer:
    DISPLAY cust-num name address city state postal-code
	    WITH 1 COLUMN DOWN COLUMN 20.
    SET address city state postal-code.
END.

DISPLAY
    "You were updating field:" FRAME-FIELD
      FORMAT "x(20)" SKIP
    "Of file:" FRAME-FILE SKIP
    "And it had the value:" FRAME-VALUE
      FORMAT "x(20)" SKIP(2)
    "When you pressed the END-ERROR key"
    WITH FRAME helper NO-LABELS COLUMN 20
	  ROW 14 NO-BOX.
