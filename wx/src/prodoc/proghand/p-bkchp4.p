/* p-bkchp4.p */

/* This FOR EACH block reads the records from the
   customer file one at a time, processing the state-
   ments in the block for each of those records. */

FOR EACH customer:
    DISPLAY customer WITH 2 COLUMNS.
END.
