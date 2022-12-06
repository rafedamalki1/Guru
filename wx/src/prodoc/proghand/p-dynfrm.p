

DEFINE VARIABLE ed AS WIDGET-HANDLE.
DEFINE VARIABLE frame1 AS WIDGET-HANDLE.
DEFINE VARIABLE button1 AS WIDGET-HANDLE.

CREATE FRAME frame1
ASSIGN
        WIDTH-CHARS = 50
        HEIGHT-CHARS = 28
        SENSITIVE = YES.
VIEW frame1.

CREATE BUTTON button1
ASSIGN
        X = 20
        Y = 20
        LABEL = "quit"
        FRAME = frame1
        SENSITIVE = YES
        TRIGGERS:
            ON CHOOSE STOP.
        END TRIGGERS.
VIEW button1.

CREATE EDITOR ed
ASSIGN
        WIDTH-CHARS = 20
        HEIGHT-CHARS = 7
        X = 70
        Y = 0
        FRAME = frame1
        SENSITIVE = YES.
VIEW ed.
WAIT-FOR GO OF frame1.


