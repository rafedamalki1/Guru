/* r-under1.p */

FOR EACH customer BREAK BY state WITH USE-TEXT:
    DISPLAY state cust-num name.
    IF LAST-OF(state) THEN UNDERLINE state.
END.
