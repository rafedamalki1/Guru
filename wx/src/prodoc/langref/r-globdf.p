&GLOBAL-DEFINE fld-list customer.name customer.address customer.city~
                         customer.state customer.postal-code

DEFINE BUTTON b_quit LABEL "Quit" AUTO-ENDKEY. 
DEFINE BUTTON b_save LABEL "Save".
DEFINE BUTTON b_undo LABEL "Undo".
 
DEFINE FRAME cust-dtl
  {&fld-list}
  b_save AT ROW-OF customer.postal-code + 2 COLUMN-OF customer.postal-code
  b_undo AT ROW-OF b_save + 1 COLUMN-OF b_save
  WITH CENTERED TITLE "Customer Address Detail" 1 COL.

FORM
  customer.cust-num
  b_quit AT ROW-OF customer.cust-num + 2 COLUMN-OF customer.cust-num
  WITH FRAME cust-info CENTERED SIDE-LABELS
  TITLE "Customer Address Query/Update".
 
ON CHOOSE OF b_save, b_undo IN FRAME cust-dtl
DO:
  IF SELF:LABEL = "Save" THEN
    ASSIGN {&fld-list}.
  HIDE FRAME cust-dtl NO-PAUSE.
  DISABLE ALL WITH FRAME cust-dtl.
  APPLY "ENTRY" TO customer.cust-num IN FRAME cust-info.
END.   

ON GO OF customer.cust-num IN FRAME cust-info
DO:
  FIND customer USING customer.cust-num EXCLUSIVE-LOCK.
  DISPLAY {&fld-list} WITH FRAME cust-dtl.
  ENABLE ALL WITH FRAME cust-dtl.
  APPLY "ENTRY" TO customer.name IN FRAME cust-dtl.
  RETURN NO-APPLY.
END.

ENABLE b_quit customer.cust-num WITH FRAME cust-info.

WAIT-FOR CHOOSE OF b_quit IN FRAME cust-info FOCUS customer.cust-num.
  
