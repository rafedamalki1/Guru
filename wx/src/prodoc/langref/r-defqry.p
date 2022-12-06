/* r-defqry.p */

DEFINE QUERY q-salesrep FOR salesrep
    FIELDS (salesrep.sales-rep salesrep.rep-name salesrep.region
            salesrep.month-quota).
DEFINE QUERY q-cust     FOR customer
    FIELDS (customer.cust-num customer.name customer.phone).

DEFINE BROWSE cust-brws QUERY q-cust
  DISPLAY customer.cust-num customer.name customer.phone
    WITH 5 DOWN TITLE "Customer Information".
    
DEFINE BUTTON b_next LABEL "Next".
DEFINE BUTTON b_quit LABEL "Quit" AUTO-ENDKEY.

FORM
   salesrep.sales-rep salesrep.rep-name salesrep.region
   salesrep.month-quota
   WITH FRAME rep-info SIDE-LABELS TITLE "Sales Rep. Info".
   
FORM b_next space(5) b_quit 
  WITH FRAME butt-frame COLUMN 60.

ON CHOOSE OF b_next
   DO:
      GET NEXT q-salesrep.
      IF NOT AVAILABLE(salesrep) THEN GET FIRST q-salesrep.
      RUN disp-rep.
   END.

OPEN QUERY q-salesrep FOR EACH salesrep NO-LOCK.
 
GET FIRST q-salesrep.
RUN disp-rep.

ENABLE cust-brws WITH FRAME cust-info.
ENABLE ALL WITH FRAME butt-frame.
  
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
 
  
PROCEDURE disp-rep.                    
      DISPLAY salesrep.sales-rep
          salesrep.rep-name
          salesrep.region
          salesrep.month-quota
      WITH FRAME rep-info CENTERED SIDE-LABELS TITLE "Sales Rep. Info".
      OPEN QUERY q-cust FOR EACH customer OF salesrep NO-LOCK.
END PROCEDURE.
