DEFINE VARIABLE ctr   AS INTEGER.

FOR EACH salesrep:
   DO ctr = 1 TO 12:
      salesrep.month-quota = 2500.
   END.
END.
