/* r-cntof.p */

FOR EACH customer BREAK BY state:
    DISPLAY cust-num name sales-rep state.
    ACCUMULATE state (SUB-COUNT BY state).
    IF LAST-OF(state)
    THEN DISPLAY 100 * (ACCUM SUB-COUNT BY state state) / COUNT-OF(state)
		  FORMAT "99.9999%"
		  COLUMN-LABEL "% of Total!Customers".
END.
