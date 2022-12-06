/* p-dirman.p */

DEFINE VARIABLE tmp AS INTEGER.
DEFINE RECTANGLE rect1 size-pixels 39 by 39 edge-pixels 3 no-fill.
DEFINE RECTANGLE rect2 size-pixels 40 by 40 edge-pixels 3 no-fill.
DEFINE RECTANGLE rect3 size-pixels 40 by 39 edge-pixels 3 no-fill.
DEFINE RECTANGLE rect4 size-pixels 39 by 40 edge-pixels 3 no-fill.
DEFINE BUTTON manipulable LABEL "Manipulable".

FORM SKIP (1) SPACE (2)  rect1 SPACE(9) manipulable
                         rect2 SPACE(2)  SKIP (1)
              SPACE(9)   rect3 SPACE(10)
                         rect4 SPACE(7)  SKIP(3)
                         "Cust-Num: " tmp SKIP(1)
WITH FRAME a ROW 3 CENTERED NO-LABELS TITLE "Manipulable Widgets".
FRAME a:BOX-SELECTABLE = YES.

rect1:SENSITIVE        IN FRAME A = YES.   /* All Properties sensitive */
rect1:SELECTABLE       IN FRAME A = YES.
rect1:MOVABLE          IN FRAME A = YES.
rect1:RESIZABLE        IN FRAME A = YES.

manipulable:SENSITIVE  IN FRAME A = YES.   /* All Properties sensitive */
manipulable:SELECTABLE IN FRAME A = YES.
manipulable:MOVABLE    IN FRAME A = YES.
manipulable:RESIZABLE  IN FRAME A = YES.

rect2:SENSITIVE        IN FRAME A = YES.   /* Movable, but not resizable */
rect2:SELECTABLE       IN FRAME A = YES.
rect2:MOVABLE          IN FRAME A = YES.
rect2:RESIZABLE        IN FRAME A = NO.

rect3:SENSITIVE        IN FRAME A = YES.   /* Resizable but not Movable */
rect3:SELECTABLE       IN FRAME A = YES.
rect3:MOVABLE          IN FRAME A = NO.
rect3:RESIZABLE        IN FRAME A = YES.

rect4:SENSITIVE        IN FRAME A = YES.   /* Not resizable or Movable */
rect4:SELECTABLE       IN FRAME A = YES.   /* but selectable           */
rect4:MOVABLE          IN FRAME A = NO.
rect4:RESIZABLE        IN FRAME A = NO.

ENABLE ALL WITH FRAME a.
WAIT-FOR GO OF FRAME a.
