/* r-and.p */

DEFINE VARIABLE low-credit LIKE credit-limit LABEL "Low Credit Limit".
DEFINE VARIABLE hi-credit LIKE credit-limit LABEL "High Credit Limit".

REPEAT:
  SET low-credit hi-credit WITH FRAME cr-range.
  FOR EACH customer WHERE
    (credit-limit >= low-credit) AND (credit-limit <= hi-credit):
      DISPLAY cust-num name credit-limit.
  END.
END.
