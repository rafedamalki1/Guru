DEFINE VARIABLE cnum     LIKE customer.cust-num.
DEFINE VARIABLE cname    LIKE customer.name.
DEFINE VARIABLE cmax     LIKE customer.credit-limit.

INPUT FROM custdump2.

FOR EACH customer:
   IMPORT DELIMITER ";" cnum cname cmax.
   DISPLAY cnum cname cmax.
END.

INPUT CLOSE.
