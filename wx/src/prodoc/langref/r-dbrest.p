/*dbrest.p */
DEFINE VARIABLE i AS INTEGER.
REPEAT i= 1 to NUM-DBS:
    DISPLAY LDBNAME(i) LABEL "Database"
            DBRESTRICTIONS(i) FORMAT "x(40)" LABEL "Restrictions".
END.
