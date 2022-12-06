/* r-lastof.p */

FOR EACH item BREAK BY cat-page:
     ACCUMULATE on-hand * price (TOTAL BY cat-page).
     IF LAST-OF(cat-page)
     THEN DISPLAY cat-page (ACCUM TOTAL BY cat-page
		  on-hand * price) LABEL "Value-oh".
END.
