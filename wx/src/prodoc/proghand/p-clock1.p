/* p-clock1.p */

DEFINE VARIABLE show-time AS LOGICAL.
DEFAULT-WINDOW:HIDDEN = TRUE.
REPEAT:
    MESSAGE "Choose:  OK      to see the current time" SKIP
            "         Cancel  to stop clock"
        VIEW-AS ALERT-BOX BUTTONS OK-CANCEL UPDATE show-time.
    IF show-time THEN RUN get-time.
                 ELSE QUIT.
END.
PROCEDURE get-time:
    DEFINE VARIABLE clock-handle AS WIDGET-HANDLE.
    DEFINE VARIABLE current-time AS CHARACTER FORMAT "x(8)".
    DEFINE FRAME clock SKIP(.5) SPACE(1) current-time SKIP(.5)
        WITH THREE-D NO-LABELS TITLE "Current Time" CENTERED.
    current-time:SENSITIVE = TRUE.
    CREATE WINDOW clock-handle ASSIGN
        HEIGHT = FRAME clock:HEIGHT
        WIDTH  = FRAME clock:WIDTH
        TITLE  = "Clock"
        MESSAGE-AREA = FALSE
        STATUS-AREA = FALSE
        ROW = 1
        COLUMN = 1.
    CURRENT-WINDOW = clock-handle.
    current-time = STRING(TIME, "HH:MM:SS").
    DISPLAY current-time WITH FRAME clock.
    PAUSE 5 NO-MESSAGE.
    clock-handle:HIDDEN = TRUE.
END.
