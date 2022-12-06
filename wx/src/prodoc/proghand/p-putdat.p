/* p-putdat.p */

OUTPUT TO p-datfl8.d.

FOR EACH customer:
    PUT cust-num AT 1 name AT 10 sales-rep AT 40 SKIP.
END.

OUTPUT CLOSE.
