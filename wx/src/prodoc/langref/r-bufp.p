DEFINE BUTTON find-butt LABEL "Find Customer". 

ENABLE find-butt Customer.Cust-num WITH FRAME cust-frame.

ON CHOOSE OF find-butt
  DO: 
     RUN r-fincus.p(INPUT Customer.Cust-num:HANDLE IN FRAME cust-frame,
                    BUFFER Customer). 
     DISPLAY Customer WITH FRAME cust-frame.
  END.

ON ENTRY OF Customer.Cust-num
  HIDE MESSAGE.
    
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
