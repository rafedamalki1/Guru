/* r-wrkfil.p */

DEFINE WORK-TABLE showsales
   FIELD region    LIKE salesrep.region LABEL        "Region"
   FIELD state     LIKE customer.state  LABEL        "St"
   FIELD tot-sales LIKE cust.balance    COLUMN-LABEL "Total!Sales".

FOR EACH customer, salesrep OF customer BREAK BY customer.state:
   ACCUMULATE balance (TOTAL by customer.state).
   IF LAST-OF(customer.state) THEN DO:
     CREATE showsales.
     showsales.state = customer.state.
     showsales.tot-sales = ACCUM TOTAL BY customer.state balance.
     showsales.region = salesrep.region.
   END.
END.

FOR EACH showsales BREAK BY showsales.region BY showsales.state:
   IF FIRST-OF (showsales.region)
      THEN DISPLAY showsales.region.
   DISPLAY showsales.state tot-sales (TOTAL BY showsales.region).
END.
