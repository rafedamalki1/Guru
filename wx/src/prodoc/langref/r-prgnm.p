/*r-prgnm.p */
/* Note this program should be run as a subroutine */
/* The deeper the nesting, the better the illustration */

DEFINE VAR level AS INT INITIAL 1.
REPEAT WHILE PROGRAM-NAME(level) <> ?.
DISPLAY LEVEL PROGRAM-NAME(level) FORMAT "x(30)".
level = level + 1.
END.
