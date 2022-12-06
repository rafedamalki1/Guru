/* r-asc.p */

DEFINE VARIABLE ltrl AS INTEGER EXTENT 27.
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE j AS INTEGER.

FOR EACH customer:
    i = ASC(SUBSTRING(name,1,1)).
    IF i < ASC("A") or i > ASC("Z") THEN i = 27.
    ELSE i = i - ASC("A") + 1.
    ltrl[i] = ltrl[i] + 1.
END.

DO j = 1 TO 27 WITH NO-LABELS USE-TEXT:
    IF j <= 26
    THEN DISPLAY CHR(ASC("A") + j - 1) @ ltr-name AS CHARACTER FORMAT "x(5)".
    ELSE DISPLAY "Other" @ ltr-name.
    DISPLAY ltrl[j].
END.
