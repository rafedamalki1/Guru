/* p-stimg.p */

DEFINE VARIABLE ok AS LOGICAL.
DEFINE IMAGE icon
   FILE "up".
DISPLAY icon with frame uparrow-frame.
PAUSE.
ok = icon:LOAD-IMAGE("down").
PAUSE.
DISPLAY icon with frame downarrow-frame.
