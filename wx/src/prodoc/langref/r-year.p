/* r-year.p */

DEFINE VARIABLE outfmt AS CHARACTER.
DEFINE VARIABLE orddate AS CHARACTER
    LABEL "Order Date" FORMAT "x(10)".

FOR EACh order:
    IF YEAR(order-date) >= 2000
    THEN outfmt = "99/99/9999".
    ELSE outfmt = "99/99/99".
    orddate = STRING(order-date,outfmt).
    DISPLAY order.order-num orddate terms.
END.
