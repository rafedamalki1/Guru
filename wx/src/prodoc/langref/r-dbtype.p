/*r-dbtype.p */
DEFINE VARIABLE i AS INTEGER.
REPEAT i=1 TO NUM-DBS:
  DISPLAY LDBNAME(i) DBTYPE(i) FORMAT "x(40)".
END.