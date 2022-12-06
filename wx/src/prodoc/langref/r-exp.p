/* r-exp.p */

DEFINE VARIABLE principal AS DECIMAL
    FORMAT "->>>,>>9.99" LABEL "Amt Invested".
DEFINE VARIABLE rate AS INTEGER FORMAT "->9" LABEL "Interest %".
DEFINE VARIABLE num-yrs AS INTEGER FORMAT ">>9" LABEL "Number of Years".
DEFINE VARIABLE final-amt AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>>,>>9.99"
    LABEL "Final Amount".

REPEAT:
    UPDATE principal rate num-yrs.
    final-amt = principal * EXP(1 + rate / 100,num-yrs).
    DISPLAY final-amt.
END.
