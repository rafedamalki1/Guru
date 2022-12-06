/* r-ifelsf.p */

FOR EACH customer
    BY IF balance > 10000 THEN 1
    ELSE (IF balance > 1000 THEN 2 ELSE 3) BY sales-rep:
    DISPLAY sales-rep balance name.
END.
