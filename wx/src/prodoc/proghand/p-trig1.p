/* p-trig1.p */

DEFINE BUTTON quit-button  LABEL "QUIT"
    TRIGGERS:
       ON CHOOSE
          QUIT.
    END TRIGGERS.
    
DEFINE BUTTON next-button LABEL "NEXT".

FORM
   next-button quit-button
   WITH FRAME butt-frame.
   
FORM
   Customer.Cust-num Customer.Name
   WITH FRAME cframe 10 DOWN USE-TEXT.

OPEN QUERY all-cust FOR EACH Customer.

ON CHOOSE OF next-button
   DO:
      GET NEXT all-cust.
      IF NOT AVAILABLE Customer
      THEN RETURN NO-APPLY.
      
      DOWN WITH FRAME cframe.
      DISPLAY Customer.Cust-num Customer.Name WITH FRAME cframe.  
   END.

ENABLE ALL WITH FRAME butt-frame.
 
GET FIRST all-cust.

DISPLAY Customer.Cust-num Customer.Name WITH FRAME cframe.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
