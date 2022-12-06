/* p-persp1.p */

DEFINE VARIABLE hdb-tools AS HANDLE.
DEFINE VARIABLE out-message AS CHARACTER FORMAT "x(60)".
DEFINE VARIABLE vcust-num LIKE customer.cust-num INITIAL 0.
DEFINE VARIABLE vname LIKE customer.name.

DEFINE BUTTON bcancel LABEL "Cancel Database Access".

DEFINE FRAME CustFrame
    vcust-num LABEL "Customer Number"
    vname LABEL "Customer Name"
    bcancel
WITH SIDE-LABELS.

ON RETURN OF vcust-num IN FRAME CustFrame DO:
    ASSIGN vcust-num.
    RUN get-cust-name IN hdb-tools (INPUT vcust-num, OUTPUT vname).
    DISPLAY vname WITH FRAME CustFrame.
END.

ON CHOOSE OF bcancel IN FRAME CustFrame DO:
    DEFINE VARIABLE hcurrent AS HANDLE.
    DEFINE VARIABLE hnext AS HANDLE.
    
    hcurrent = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(hcurrent):
        hnext = hcurrent:NEXT-SIBLING.
        IF hcurrent:PRIVATE-DATA = "DATABASE" THEN DO:
            RUN destroy-context IN hcurrent (OUTPUT out-message).
            MESSAGE out-message.
        END.
        hcurrent = hnext.
    END.
    APPLY "WINDOW-CLOSE" TO CURRENT-WINDOW.
END.

RUN p-persp2.p PERSISTENT SET hdb-tools.

ENABLE ALL WITH FRAME CustFrame.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
