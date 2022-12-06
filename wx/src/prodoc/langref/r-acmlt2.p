/* r-acmlt2.p */

FOR EACH item:
    ACCUMULATE on-hand * price (TOTAL).
END.

FOR EACH item BY on-hand * price DESCENDING:
    DISPLAY item-num on-hand price on-hand * price LABEL "Value"
	    100 * (on-hand * price) / (ACCUM TOTAL on-hand * price)
	    LABEL "Value %".
END.
