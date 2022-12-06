/* r-string.p */

DISPLAY SKIP(2) "The time is now" STRING(TIME,"HH:MM AM") SKIP(2)
					   WITH NO-BOX NO-LABELS CENTERED.

FOR EACH customer:
  DISPLAY name + "  --" + STRING(cust-num, ">>>9") FORMAT "x(30)" AT 1
          address AT 33
	  city + ", " + st + " " + postal-code FORMAT "x(22)"
	  AT 33 SKIP(1) WITH NO-BOX NO-LABELS CENTERED.
END.
