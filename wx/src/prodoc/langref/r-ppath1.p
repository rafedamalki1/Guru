/* r-ppath1.p */

DEFINE VARIABLE i AS INTEGER.

DISPLAY PROPATH.

REPEAT i = 1 TO NUM-ENTRIES(PROPATH):
  DISPLAY ENTRY(i , PROPATH) FORMAT "x(30)".
END.