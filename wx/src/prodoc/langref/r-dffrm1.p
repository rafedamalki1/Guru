/* r-dffrm1.p */
DEFINE BUTTON b_dtl LABEL "Detail".
DEFINE BUTTON b_next LABEL "Next".
DEFINE BUTTON b_quit LABEL "Quit" AUTO-ENDKEY. 

DEFINE FRAME cust-info 
  customer.cust-num
  customer.name FORMAT "X(20)"
  customer.phone
  WITH CENTERED ROW 3.

DEFINE FRAME cust-dtl
  customer except customer.cust-num customer.name customer.phone
  WITH CENTERED SIDE-LABELS ROW 7.

DEFINE FRAME butt-frame
  b_dtl b_next b_quit
  WITH ROW 1.
  
ON CHOOSE OF b_dtl
  DISPLAY customer except customer.cust-num customer.name customer.phone
    WITH FRAME cust-dtl.
    
ON CHOOSE OF b_next
DO:
  HIDE FRAME cust-dtl.
  FIND NEXT customer NO-LOCK NO-ERROR.
  IF NOT AVAILABLE customer
  THEN FIND LAST customer NO-LOCK.

  DISPLAY customer.cust-num
          customer.name
          customer.phone
    WITH FRAME cust-info.
END. 
ENABLE ALL WITH FRAME butt-frame.

APPLY "CHOOSE" TO b_next IN FRAME butt-frame.

WAIT-FOR CHOOSE OF b_quit.
