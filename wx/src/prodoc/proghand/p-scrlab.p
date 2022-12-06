/* p-scrlab.p */

FORM
    Customer.name Customer.address Customer.address2
    Customer.city Customer.st
    WITH FRAME choose-frame 33 DOWN SIZE 40 BY 5.

PAUSE 0 BEFORE-HIDE.    
FOR EACH customer BREAK BY Customer.name:
    DISPLAY name address address2 city st
            WITH FRAME choose-frame.
    IF NOT LAST(customer.name)
    THEN DOWN WITH FRAME choose-frame.
END.

REPEAT:
    CHOOSE ROW Customer.name WITH FRAME choose-frame.   
    FIND customer WHERE Customer.name = FRAME-VALUE.
    DISPLAY customer WITH SIDE-LABELS.
END.

