FORM customer WITH FRAME x.

ON GO OF customer.name
   DO:
       FIND FIRST customer WHERE customer.name BEGINS SELF:SCREEN-VALUE.
       DISPLAY customer WITH FRAME x.
       ENABLE ALL WITH FRAME x.
   END.

ENABLE customer.name WITH FRAME x.


WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
