/* p-sestrg.p */

ON WRITE OF customer OLD BUFFER ocust DO:
    IF customer.sales-rep <> ocust.sales-rep
    THEN DO:
       FIND salesrep OF customer NO-LOCK.
       customer.comments = customer.comments + " Salesrep changed to " +
                           salesrep.rep-name + " on " + STRING(TODAY).
    END.
END.
