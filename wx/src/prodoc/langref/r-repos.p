/* r-repos.p */

DEFINE QUERY  q-order FOR customer, order SCROLLING.

DEFINE BUTTON b_quit LABEL "Quit".
  
DEFINE BUTTON b_frwd     LABEL "FORWARD".
DEFINE BUTTON b_back     LABEL "BACKWARD".

DEFINE VAR num AS INTEGER INIT 1 NO-UNDO.

FORM b_frwd b_back b_quit
  WITH FRAME butt-frame ROW 1.

ON CHOOSE OF b_back, b_frwd
DO:
  PROMPT-FOR num  LABEL "Records To Skip" 
    WITH FRAME pos-info CENTERED ROW 5 overlay.
  HIDE FRAME pos-info NO-PAUSE.
  IF SELF:LABEL = "BACKWARD" THEN
    REPOSITION q-order BACKWARDS INPUT num + 1.
  ELSE REPOSITION q-order FORWARDS INPUT num - 1.
  RUN getone.
END.

OPEN QUERY q-order FOR EACH customer,
  EACH order OF customer NO-LOCK.

RUN getone.

ENABLE b_back b_frwd b_quit WITH FRAME butt-frame.

WAIT-FOR CHOOSE OF b_quit OR WINDOW-CLOSE OF CURRENT-WINDOW. 

PROCEDURE getone:
  GET NEXT q-order.
  IF NOT AVAILABLE(customer) THEN
  DO:
    REPOSITION q-order BACKWARDS 1.
    GET NEXT q-order.
  END.
  DISPLAY customer.cust-num customer.name skip
          order.order-num order.order-date
    WITH FRAME order-info CENTERED ROW 5 SIDE-LABELS OVERLAY.
END PROCEDURE.
