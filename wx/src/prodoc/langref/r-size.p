/* r-size.p */

DEFINE BUTTON b_quit LABEL "Quit"
  TRIGGERS:
    ON CHOOSE QUIT.
  END.
  
DEFINE BUTTON b_size LABEL "Size It".

DEFINE RECTANGLE rec SIZE 5 BY 5.

DEFINE FRAME butt-frame
  b_size b_quit
WITH CENTERED ROW SCREEN-LINES - 2.

DEFINE FRAME sz-frame
  SKIP(1) SPACE(1)
  rec 
  WITH SIZE 80 BY 10 TITLE "The rectangle is 5 by 5".
  
ON CHOOSE OF b_size IN FRAME butt-frame
    ASSIGN rec:WIDTH-CHARS IN FRAME sz-frame =
                 RANDOM(1, FRAME sz-frame:WIDTH-CHARS - 3) 
           rec:HEIGHT-CHARS = RANDOM(1, FRAME sz-frame:HEIGHT-CHARS - 2)
           FRAME sz-frame:TITLE =  "The rectangle is " +
                 STRING(rec:WIDTH-CHARS) + " by " +
                 STRING(rec:HEIGHT-CHARS).

ENABLE rec WITH FRAME sz-frame.

ENABLE b_size b_quit WITH FRAME butt-frame.

WAIT-FOR CHOOSE OF b_quit IN FRAME butt-frame.
