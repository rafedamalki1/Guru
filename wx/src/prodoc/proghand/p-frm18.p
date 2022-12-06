/* p-frm18.p */

FORM
  customer
  WITH FRAME cust-frame TITLE "Customer".
  
FORM
  salesrep WITH FRAME rep-frame TITLE "Salesrep".
  
ON LEAVE, GO OF customer.cust-num
   DO:
      FIND customer USING customer.cust-num.
      FIND salesrep OF customer.
      DISPLAY customer WITH FRAME cust-frame.
      DISPLAY salesrep WITH FRAME rep-frame.
      
      ENABLE ALL WITH FRAME rep-frame.
      DISABLE customer.cust-num WITH FRAME cust-frame.
   END.

ON GO OF FRAME cust-frame, FRAME rep-frame
   DO:
      IF FOCUS <> customer.cust-num:HANDLE IN FRAME cust-frame
      THEN DO:      
         ASSIGN customer.
         ASSIGN salesrep.
         RUN reset-frames.
      END.
   END.

ON END-ERROR OF FRAME cust-frame, FRAME rep-frame
   DO:
      IF FOCUS <> customer.cust-num:HANDLE IN FRAME cust-frame
      THEN DO: 
         RUN reset-frames.
         RETURN NO-APPLY.
      END.
   END.
  
ENABLE ALL WITH FRAME cust-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.

PROCEDURE reset-frames:
  CLEAR FRAME cust-frame.
  CLEAR FRAME rep-frame.
  DISABLE ALL WITH FRAME rep-frame. 
  ENABLE ALL WITH FRAME cust-frame.
  APPLY "ENTRY" TO customer.cust-num IN FRAME cust-frame.
END PROCEDURE.
