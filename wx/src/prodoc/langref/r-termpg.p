/* r-termpg.p */
DEFINE VARIABLE x AS INTEGER.

OUTPUT TO TERMINAL PAGED.

FOR EACH customer BREAK BY sales-rep:
  FIND salesrep OF customer.

  FORM HEADER TODAY
       "Customer Listing For " to 43
       "Page " to 55 PAGE-NUMBER - x  TO 58 FORMAT "99"
       (salesrep.rep-name) FORMAT "x(30)" AT 25
  WITH FRAME hdr PAGE-TOP CENTERED STREAM-IO.

  VIEW FRAME hdr.
  DISPLAY cust-num COLUMN-LABEL "Customer!Number" name  LABEL "Name"
	  phone COLUMN-LABEL "Phone!Number" WITH CENTERED STREAM-IO.
  IF LAST-OF (cust.sales-rep)
  THEN DO:
     x = PAGE-NUMBER.
     PAGE.
  END.
END.

OUTPUT CLOSE.
