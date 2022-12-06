/* r-deffrm.p */

DEFINE VARIABLE bal-avail LIKE customer.balance
            COLUMN-LABEL "Available!Credit" NO-UNDO.

DEFINE FRAME cust-bal 
  customer.cust-num
  customer.name FORMAT "X(20)"
  customer.credit-limit LABEL "Limit"
  customer.balance
  bal-avail
  WITH CENTERED ROW 3 TITLE "Available Customer Credit" USE-TEXT.
 
FOR EACH customer NO-LOCK WITH FRAME cust-bal:
  DISPLAY customer.cust-num
          customer.name
          customer.credit-limit
          customer.balance
          customer.credit-limit - customer.balance @ bal-avail.
 END.
