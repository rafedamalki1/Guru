/* r-acc.p */

FOR EACH customer BREAK BY sales-rep BY country:
   ACCUMULATE balance (TOTAL BY sales-rep BY country).
   DISPLAY sales-rep WHEN FIRST-OF(sales-rep) country name balance.
   IF LAST-OF(country) THEN
      DISPLAY ACCUM TOTAL BY country balance COLUMN-LABEL "Country!Total".
   IF LAST-OF(sales-rep) THEN DO:
      DISPLAY sales-rep ACCUM TOTAL BY sales-rep
	balance COLUMN-LABEL "Sales-Rep!Total".
      DOWN 1.
   END.
END.
