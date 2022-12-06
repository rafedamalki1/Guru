/* p-win1.p */

DEFINE VARIABLE new-win AS WIDGET-HANDLE.

FORM
   customer
   WITH FRAME cust-frame.
   
FORM
   salesrep
   WITH FRAME rep-frame.

CREATE WINDOW new-win
      ASSIGN TITLE = "Sales Representative".

ASSIGN DEFAULT-WINDOW:TITLE = "Customer".

VIEW new-win.
MESSAGE "This is the default window.".
MESSAGE "This is the newly created window." IN WINDOW new-win.  

PAUSE 0 BEFORE-HIDE. 
FOR EACH customer, salesrep OF customer ON ERROR UNDO, LEAVE
       ON ENDKEY UNDO, LEAVE ON STOP UNDO, LEAVE:
   DISPLAY customer WITH FRAME cust-frame.
   ENABLE ALL WITH FRAME cust-frame. 
   
   CURRENT-WINDOW = new-win.
   DISPLAY salesrep WITH FRAME rep-frame.
   ENABLE ALL WITH FRAME rep-frame.
   CURRENT-WINDOW = DEFAULT-WINDOW. 
   
   WAIT-FOR GO OF FRAME cust-frame, FRAME rep-frame. 
   
   ASSIGN customer.
   ASSIGN salesrep. 
END.

DELETE WIDGET new-win.
