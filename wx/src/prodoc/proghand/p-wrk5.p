/* p-wrk5.p */

DEFINE WORK-TABLE cpage
   FIELD w-cat-page LIKE item.cat-page FORMAT ">9"
   FIELD w-ord-value AS DECIMAL FORMAT "->>>,>>9.99" EXTENT 4. 

DEFINE VARIABLE rep-code AS INTEGER.
DEFINE VARIABLE rep-names AS CHARACTER INITIAL
                    "BBB,DKP,SLS".
       
FOR EACH order, customer OF order, EACH order-line OF order,
         item OF order-line:
         
   FIND FIRST cpage WHERE cpage.w-cat-page = item.cat-page NO-ERROR.
   
   IF NOT AVAILABLE cpage 
   THEN DO: 
      CREATE cpage.
      cpage.w-cat-page = item.cat-page.
   END.
    
   rep-code = LOOKUP(customer.sales-rep, rep-names).
   IF rep-code = 0
   THEN rep-code = 4.
   w-ord-value[rep-code] = w-ord-value[rep-code] +
                           (order-line.price * order-line.qty).
END.

FOR EACH cpage BY cpage.w-cat-page:
  FORM HEADER "Page" "BBB" AT 15 "DKP" AT 29 "SLS" AT 43 "Others" AT 54
      WITH TITLE "Order Value by Catalog Page and Sales Rep" NO-LABELS.

   DISPLAY w-cat-page w-ord-value.
END.
