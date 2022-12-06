/* r-asgn2.p */

DEFINE VARIABLE neword LIKE order-line.order-num LABEL "New Order".
DEFINE VARIABLE newordli LIKE order-line.line-num LABEL "New Order Line".

REPEAT:
    PROMPT-FOR order-line.order-num line-num.
    FIND order-line USING order-line.order-num AND line-num.
    SET neword newordli.
    FIND order WHERE order.order-num = neword.
    ASSIGN order-line.order-num = neword order-line.line-num = newordli.
END.
