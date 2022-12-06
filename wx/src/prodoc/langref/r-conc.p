/* r-conc.p - CONCATENATION */

FOR EACH customer:
  DISPLAY  SKIP(1) name SKIP address SKIP
    city + ", " + state FORMAT "x(16)" country postal-code SKIP(2).
END.
