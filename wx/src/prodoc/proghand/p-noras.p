/* p-noras.p */

/* This procedure cannot compile because the strong-scoped
   customer buffer is referenced outside the DO FOR block. */

DO FOR customer:
   REPEAT:
       INSERT customer.
   END.
END.

DISPLAY customer WITH 2 COLUMNS.
