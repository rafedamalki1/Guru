/* r-entry.p */

DEFINE VARIABLE datein AS DATE.
DEFINE VARIABLE daynum AS INTEGER.
DEFINE VARIABLE daynam AS CHARACTER INITIAL
    "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday".

SET datein LABEL "Enter a date (mm/dd/yy)".
daynum = WEEKDAY(datein).

DISPLAY ENTRY(daynum,daynam) FORMAT "x(9)" LABEL "is a" WITH SIDE-LABELS.
