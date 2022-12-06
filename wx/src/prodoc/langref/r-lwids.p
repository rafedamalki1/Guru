DEFINE VARIABLE event-name AS CHARACTER FORMAT "x(24)" LABEL "Event".
DEFINE VARIABLE widget-list AS CHARACTER LABEL "Widgets"
                VIEW-AS SELECTION-LIST INNER-CHARS 24 INNER-LINES 6
                 SCROLLBAR-VERTICAL.

FORM
   event-name SKIP
   widget-list
   WITH FRAME main-frame SIDE-LABELS.


REPEAT WITH FRAME main-frame:
   DISABLE widget-list.
   SET event-name.
   widget-list:LIST-ITEMS = LIST-WIDGETS(event-name).
   DISPLAY widget-list.
   ENABLE widget-list.
   PAUSE.
END.
