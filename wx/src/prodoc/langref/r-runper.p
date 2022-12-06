/* r-runper.p */

DEFINE VARIABLE phand AS HANDLE.
DEFINE VARIABLE nhand AS HANDLE.
DEFINE VARIABLE whand AS WIDGET-HANDLE.
DEFINE BUTTON bStart LABEL "Start Customer Query".
DEFINE BUTTON bRecall LABEL "Recall All Hidden Queries".
DEFINE BUTTON bExit LABEL "Exit".

DEFINE FRAME ControlFrame SKIP (.5) 
    SPACE (2) bStart bRecall bExit SPACE (2) SKIP (.5).

ON CHOOSE OF bStart IN FRAME ControlFrame RUN r-perprc.p PERSISTENT.

ON CHOOSE OF bRecall IN FRAME ControlFrame DO:
    phand = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(phand):
        IF phand:PRIVATE-DATA = "Customer Browse" THEN
            RUN recall-query IN phand.
        phand = phand:NEXT-SIBLING.
    END.
END.

ON CHOOSE OF bExit IN FRAME ControlFrame DO:
    phand = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(phand):
        nhand = phand:NEXT-SIBLING.
        IF phand:PRIVATE-DATA = "Customer Browse" THEN
            RUN destroy-query IN phand.
        phand = nhand.
    END.
    DELETE WIDGET CURRENT-WINDOW.
    APPLY "RETURN" TO THIS-PROCEDURE.
END.

SESSION:SYSTEM-ALERT-BOXES = TRUE.

CREATE WINDOW whand
    ASSIGN
        TITLE = "Customer Query Control"
        SCROLL-BARS = FALSE
        MESSAGE-AREA = FALSE
        MAX-HEIGHT-CHARS = FRAME ControlFrame:HEIGHT-CHARS
        MAX-WIDTH-CHARS = FRAME ControlFrame:WIDTH-CHARS.
CURRENT-WINDOW = whand.
ENABLE ALL WITH FRAME ControlFrame.

WAIT-FOR RETURN OF THIS-PROCEDURE.

