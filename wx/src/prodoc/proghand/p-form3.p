/* p-form3.p */

FORM HEADER "Customer Credit Status Report" WITH CENTERED.
VIEW.

FOR EACH customer WITH CENTERED:
    DISPLAY name credit-limit.
END.

