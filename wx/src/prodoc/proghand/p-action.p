/* p-action.p */

ON F1 HELP.
ON F2 GO.
ON CTRL-X BELL.

FOR EACH customer:
    DISPLAY cust-num.
    UPDATE name max-credit sales-rep.
END.
