/* r-arry2.p */

DEFINE VARIABLE i AS INTEGER.

FOR EACH salesrep:
   DISPLAY sales-rep region
           month-quota[1] month-quota[2].
   DO i = 1 TO 12:
      SET month-quota[i] WITH 1 COLUMN.
   END.
   DISPLAY month-quota WITH FRAME a COLUMN 40 ROW 3 1 COLUMN.
END.
