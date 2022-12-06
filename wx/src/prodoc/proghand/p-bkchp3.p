/* p-bkchp3.p */

/* This block loops through its statements as long as
   the customer number is less than 5. */

DEFINE VARIABLE i AS INTEGER.

DO i = 1 TO 5:
    FIND NEXT customer.
    DISPLAY customer WITH 2 COLUMNS.
END.
