/* Form definition for updating the customer file.           */
/* Please note that there is no TITLE in the frame phrase.   */

FORM
  customer.cust-num  AT 5
  customer.name      AT 5 
  customer.address   AT 5
  customer.address2  AT 5
  customer.city AT 5 customer.st  AT 25 customer.postal-code AT 37 
  SKIP (1)
  customer.contact AT 5
  customer.phone AT 5
  WITH FRAME cust_fr SIDE-LABELS ROW 9 CENTERED.



