/* r-ovrlay.p */

FOR EACH customer:
   DISPLAY customer WITH 2 COLUMNS TITLE "Customer Information".
   FOR EACH order OF customer:
       DISPLAY order WITH 2 COLUMNS OVERLAY
	       TITLE "Customer's Orders" ROW 7 COLUMN 10.
   END.
END.
