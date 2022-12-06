/* p-bkchp5.p */

/* The record-spec (made up of a simple WHERE phrase in
   this example) tells the FOR EACH block to read only
   those records from the customer file that have a
   credit-limit greater than 50,000. */

FOR EACH customer WHERE credit-limit > 50000:
    DISPLAY customer WITH 2 COLUMNS.
END.
