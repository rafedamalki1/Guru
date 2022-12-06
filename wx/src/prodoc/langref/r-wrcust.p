/**********************************************************************************************/
/* WRCUST.T - To test, use procedure: TTEST3.P                                                              */
/*                                                                                                                                          */
/**********************************************************************************************/
 
TRIGGER PROCEDURE FOR Write OF Customer OLD BUFFER oldCustomer.

/* Variable Definitions */

DEFINE VARIABLE i AS INTEGER INITIAL 0.
DEFINE VARIABLE Outstanding AS INTEGER INITIAL 0.

/* Check to see if the user changed the Customer Number */

IF Customer.Cust-Num <> oldCustomer.Cust-Num AND oldCustomer.Cust-Num <> 0
THEN DO:

   /* If the user changed the Customer Number, find all related orders and change  */
   /* their customer numbers.                                                                                      */
   
   FOR EACH order OF oldCustomer:
      Order.Cust-Num = Customer.Cust-Num.
      i = i + 1.
   END.
   IF i > 0 THEN
     MESSAGE i "orders changed to reflect the new customer number!".
END.

/* Ensure that the Credit Limit value is always Greater than the sum of this
   Customer's Outstanding Balance */

FOR EACH Order OF Customer:
   FOR EACH Order-Line OF Order:
      Outstanding = Outstanding + ( Qty * Order-Line.Price ).
   END.
END.
FOR EACH Invoice OF Customer:
   Outstanding = Outstanding + ( Amount - ( Total-Paid + Adjustment )).
END.

IF Customer.Credit-Limit < Outstanding THEN DO:
   MESSAGE "This Customer has an outstanding balance of: " Outstanding ". The Credit Limit MUST exceed this amount!".
   RETURN ERROR.
END. 






