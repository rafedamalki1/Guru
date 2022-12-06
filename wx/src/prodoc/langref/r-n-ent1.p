/*r-n-ent1.p */
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE regions AS CHARACTER INITIAL
"Northeast,Southeast,Midwest,Northwwest,Southwest".
REPEAT i=1 to NUM-ENTRIES(regions):
    DISPLAY ENTRY(i,regions) FORMAT "x(12)".
END.
