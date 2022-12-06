/* r-prprc2.p */

DEFINE VARIABLE wvar AS INTEGER.
DEFINE VARIABLE xvar AS INTEGER.
DEFINE VARIABLE yvar AS INTEGER.
DEFINE VARIABLE zvar AS INTEGER.

wvar = {&SEQUENCE}.
xvar = {&SEQUENCE}.

&GLOBAL-DEFINE Last-Value {&SEQUENCE}

yvar = {&Last-Value}.
zvar = {&Last-Value}.

MESSAGE "wvar =" wvar SKIP "xvar =" xvar SKIP
        "yvar =" yvar SKIP "zvar =" zvar VIEW-AS ALERT-BOX.
