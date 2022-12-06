/* p-frm19.p */

FORM
   customer.cust-num AT X 50 Y 14
   customer.name AT X 200 Y 50
   WITH SIDE-LABELS FRAME xcust.
   
   
FIND FIRST customer.
DISPLAY cust-num name WITH FRAME xcust.
