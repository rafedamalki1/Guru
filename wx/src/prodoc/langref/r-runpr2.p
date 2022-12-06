/* r-runpr2.p */

DEFINE VARIABLE io-param AS INTEGER.
FOR EACH item:
   DISPLAY Item-name On-hand WITH 1 DOWN.
   io-param = Item.On-hand.
   RUN r-param2.p (INPUT-OUTPUT io-param).
   Item.On-hand = io-param.
   DISPLAY io-param LABEL "New Quantity On-hand".
END.
