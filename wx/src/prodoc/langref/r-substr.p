/* r-substr.p */

DEFINE VARIABLE inv-num AS CHARACTER FORMAT "x(11)" LABEL "Invoice Number".
DEFINE VARIABLE snum AS INTEGER FORMAT "9999" LABEL "  Starting Order Number".
DEFINE VARIABLE enum LIKE snum LABEL "    Ending Order Number".
DEFINE VARIABLE num LIKE snum LABEL "Starting Invoice Number".

UPDATE "      Creating Invoices" SKIP(2) snum SKIP(1) enum SKIP(2) num SKIP(2)
       WITH SIDE-LABELS CENTERED NO-BOX.

FOR EACH order WHERE order.order-num >= snum AND order.order-num <= enum:
    inv-num = SUBSTRING(STRING(TODAY),1,2,"CHARACTER") +
              SUBSTRING(STRING(TODAY),7,2,"CHARACTER") + " - " +
              STRING(num,"9999").     
    DISPLAY order-num inv-num WITH CENTERED.

    /* Do creation and printing of invoice here */

    num = num + 1.
END.
