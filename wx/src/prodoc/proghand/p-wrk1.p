/* p-wrk1.p */

FOR EACH item BREAK BY cat-page:
   ACCUMULATE price * on-hand (SUB-TOTAL BY cat-page).
   IF LAST-OF(cat-page)
   THEN DISPLAY cat-page ACCUM SUB-TOTAL BY cat-page (price * on-hand).
END.
