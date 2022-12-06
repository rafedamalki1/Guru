/* r-defb.p */
 
DEFINE BUFFER other-cust FOR Customer. 

FORM
   Customer
   WITH FRAME cre-cust.
   
ON LEAVE OF Customer.Postal-Code
   DO:
      FIND FIRST other-cust WHERE other-cust.Postal-Code =
                                  Customer.Postal-Code:SCREEN-VALUE AND
                                  other-cust.Cust-num <>
                                  Customer.Cust-num NO-ERROR.
      IF AVAILABLE(other-cust)
      THEN DISPLAY other-cust.City @ Customer.City
                   other-cust.State @ Customer.State 
                   other-cust.Country @ Customer.Country
                   WITH FRAME cre-cust. 
      ENABLE Customer.City Customer.State Customer.Country
         WITH FRAME cre-cust.
   END.
   
CREATE Customer.
UPDATE Customer EXCEPT Customer.City Customer.State Customer.Country
   WITH FRAME cre-cust.
