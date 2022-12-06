/* p-frm16.p */

DEFINE FRAME a
   customer.cust-num customer.name
   WITH DOWN USE-TEXT WIDTH 40 TITLE "Customers".

DEFINE FRAME b
    salesrep
    WITH USE-TEXT TITLE "Sales Rep".
  
FRAME a:HEIGHT-CHARS = SCREEN-LINES - (FRAME b:HEIGHT-CHARS + 1).

FOR EACH customer, salesrep OF customer WITH FRAME a:
   DISPLAY cust-num name.
   FRAME b:TITLE = "Sales Rep for " + customer.name.
   DISPLAY salesrep WITH FRAME b.
END.

