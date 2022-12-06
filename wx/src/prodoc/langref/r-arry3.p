/* r-arry3.p */

DEFINE VARIABLE i AS INTEGER.

FOR EACH salesrep:
   DISPLAY sales-rep region
           month-quota[1] month-quota[2] WITH 6 DOWN.
   FORM i month-quota[i].
   DO i = 1 TO 12:
      DISPLAY i NO-LABEL.
      SET month-quota[i].
   END.
   DISPLAY month-quota WITH FRAME a COLUMN 40 ROW 3 1 COLUMN.
END.
