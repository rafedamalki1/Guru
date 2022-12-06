/* r-widget.p */

DEFINE BUTTON b_next LABEL "Next".
DEFINE BUTTON b_prev LABEL "Previous".
DEFINE BUTTON b_quit LABEL "Quit".

DEFINE FRAME butt-frame
  b_next b_prev
  WITH CENTERED ROW SCREEN-LINES - 1.

DEFINE FRAME info
  customer.cust-num customer.name
  b_quit AT ROW-OF customer.cust-num + 2 COLUMN-OF customer.cust-num + 18
  WITH CENTERED TITLE "Customers"  ROW 2 1 COL.
  
ON CHOOSE OF b_next, b_prev
DO:
  IF SELF:LABEL = "Next" THEN
    FIND NEXT customer NO-LOCK.
  ELSE FIND PREV customer NO-LOCK.
  DISPLAY customer.cust-num customer.name WITH FRAME info.
END.
  
ENABLE b_next b_prev WITH FRAME butt-frame.
ENABLE b_quit WITH FRAME info.

WAIT-FOR END-ERROR OF FRAME butt-frame OR
  CHOOSE OF b_quit IN FRAME info FOCUS b_next IN FRAME butt-frame.
