/* r-clsqry.p */

DEFINE NEW SHARED BUFFER x-cust FOR customer.
DEFINE NEW SHARED QUERY  q-cust FOR x-cust.

DEFINE BUTTON b_quit LABEL "Quit"
  TRIGGERS:
    ON CHOOSE QUIT.
  END.
  
DEFINE BUTTON b_ascend  LABEL "Ascending".
DEFINE BUTTON b_descend LABEL "Descending".
DEFINE BUTTON b_num     LABEL "Cust-Num".

FORM b_ascend b_descend b_num b_quit
  WITH FRAME butt-frame ROW 1.

ON CHOOSE OF b_ascend
DO:
  CLOSE QUERY q-cust.
  OPEN QUERY q-cust FOR EACH x-cust NO-LOCK
    BY x-cust.name.
  DISABLE ALL WITH FRAME butt-frame.
  RUN r-query.p.
END.

ON CHOOSE OF b_descend
DO:
  CLOSE QUERY q-cust.
  OPEN QUERY q-cust FOR EACH x-cust NO-LOCK
    BY x-cust.name DESCENDING.
  DISABLE ALL WITH FRAME butt-frame.
  RUN r-query.p.
END.

ON CHOOSE OF b_num
DO:
  CLOSE QUERY q-cust.
  OPEN QUERY q-cust FOR EACH x-cust NO-LOCK
    BY x-cust.cust-num.
  DISABLE ALL WITH FRAME butt-frame.
  RUN r-query.p.
END.

DO WHILE TRUE:
  ENABLE ALL WITH FRAME butt-frame.
  WAIT-FOR CHOOSE OF b_ascend, b_descend, b_num, b_quit.
END.
    
    
