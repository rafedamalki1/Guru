/* p-stscp2.p */

/* This code cannot compile. */

DO:
   DO FOR customer:
      FIND FIRST customer.
      DISPLAY cust-num name.
   END.
   
   FIND NEXT customer.
END.
