/* r-or.p */

FOR EACH customer WHERE postal-code = "" OR phone = "":
    DISPLAY cust-num name (COUNT) city state postal-code phone.
END.
