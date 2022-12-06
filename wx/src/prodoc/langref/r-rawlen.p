/* r-rawlen.p */

DEFINE VARIABLE i AS INTEGER.

FIND customer WHERE cust-num = 29.
i = LENGTH(name, "RAW").
DISPLAY Name i LABEL "Byte Length".
