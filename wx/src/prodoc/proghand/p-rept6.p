/* p-rept6.p */

OUTPUT TO mail.lst.

FOR EACH customer WHERE curr-bal >= 1400 BY zip:
  PUT contact SKIP
      name SKIP
      address SKIP.
  IF address2 NE "" THEN PUT address2 SKIP.
  PUT city + ", " + st + "  " + STRING(zip," 99999") FORMAT "X(23)" SKIP(1).
  IF address2 EQ "" THEN PUT SKIP(1).
END.
