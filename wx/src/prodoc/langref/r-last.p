/* r-last.p */

FOR EACH item BY on-hand * price DESCENDING:
     DISPLAY item-num on-hand * price (TOTAL) LABEL "Value-oh"
         WITH USE-TEXT.
END.

FOR EACH item BREAK BY on-hand * price DESCENDING:
     FORM item.item-num value-oh AS DECIMAL
	  LABEL "Value-oh" WITH COLUMN 40 USE-TEXT.
     DISPLAY item-num on-hand * price @ value-oh.
     ACCUMULATE on-hand * price (TOTAL).
     IF LAST(on-hand * price) THEN DO:
       UNDERLINE value-oh.
       DISPLAY ACCUM TOTAL on-hand * price @ value-oh.
     END.
END.
