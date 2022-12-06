/* p-slider.p */

DEFINE VARIABLE num-rows AS INTEGER LABEL "Number of Rows"
                    VIEW-AS SLIDER MIN-VALUE 1 MAX-VALUE 20
                    TIC-MARKS BOTTOM FREQUENCY 1.

DEFINE BUTTON show-custs LABEL "Show Customers".

FORM
  num-rows HELP "Choose the number of customers per screen." 
  show-custs
  WITH FRAME main-frame THREE-D.
   
DEFINE FRAME cust-frame
  customer.cust-num customer.name
  WITH DOWN THREE-D.

ON CHOOSE OF show-custs
  DO:
     CLEAR FRAME cust-frame ALL.
     FRAME cust-frame:DOWN =
        INTEGER(num-rows:SCREEN-VALUE IN FRAME main-frame).
     FOR EACH customer: 
        DISPLAY cust-num name WITH FRAME cust-frame.
        DOWN WITH FRAME cust-frame.
     END.
      
     HIDE FRAME cust-frame.
  END.

num-rows:MAX-VALUE IN FRAME main-frame = SCREEN-LINES - 2.
      
ENABLE ALL WITH FRAME main-frame.

WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.


