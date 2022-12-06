/* p-check2.p */

PROMPT-FOR order-line.order-num line-num WITH NO-VALIDATE.
FIND order-line USING order-num AND line-num.
DISPLAY order-line.
