/* r-enable.p */

DEFINE VARIABLE ok AS LOGICAL NO-UNDO.

DEFINE BUTTON b_quit LABEL "Quit" AUTO-ENDKEY. 
DEFINE BUTTON b_save LABEL "Save".

DEFINE BUTTON b_undo LABEL "Undo".

DEFINE FRAME butt-frame
  b_save b_undo b_quit
  WITH CENTERED ROW SCREEN-LINES - 2.
  
FORM
  customer
WITH FRAME cust-info SIDE-LABELS CENTERED
TITLE "Update Customer Credit Limit".

ON CHOOSE OF b_save, b_undo IN FRAME butt-frame
DO:
  DISABLE b_save b_undo WITH FRAME butt-frame.
  DISABLE customer.credit-limit WITH FRAME cust-info.
  ENABLE customer.cust-num WITH FRAME cust-info.
  IF SELF:LABEL = "save" THEN
    ASSIGN FRAME cust-info customer.credit-limit.
  CLEAR FRAME cust-info NO-PAUSE.
  APPLY "ENTRY" TO customer.cust-num IN FRAME cust-info.
END.
  
ON GO OF customer.cust-num IN FRAME cust-info
DO:
  FIND customer USING customer.cust-num EXCLUSIVE NO-ERROR.
  IF AVAILABLE(customer) THEN
  DO:
    DISABLE customer.cust-num WITH FRAME cust-info. 
    ENABLE customer.credit-limit WITH FRAME cust-info.
    ENABLE ALL WITH FRAME butt-frame.
    DISPLAY customer WITH FRAME cust-info.
  END.
  ELSE
  DO:
     MESSAGE "No Customer Record exist for customer number"
             INPUT customer.cust-num ", Please re-enter."
       VIEW-AS ALERT-BOX WARNING BUTTONS OK-CANCEL UPDATE OK.
    IF NOT ok THEN
      APPLY "CHOOSE" TO b_quit IN FRAME butt-frame. 
  END.
END.  

ENABLE customer.cust-num WITH FRAME cust-info.
ENABLE b_quit WITH FRAME butt-frame.
WAIT-FOR CHOOSE OF b_quit IN FRAME butt-frame FOCUS customer.cust-num
  IN FRAME cust-info. 
