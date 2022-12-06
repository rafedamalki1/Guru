/* p-frm15.p */

DISPLAY "Customer Credit Status Report"
        WITH TITLE "Report Type" CENTERED.

FOR EACH customer:
        DISPLAY name address credit-limit 
                WITH NO-BOX 10 DOWN CENTERED RETAIN 2.
END.

