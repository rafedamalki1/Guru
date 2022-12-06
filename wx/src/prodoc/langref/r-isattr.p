DEFINE VARIABLE termtype AS LOGICAL FORMAT "spacetaking/non-spacetaking".
 
termtype = IS-ATTR-SPACE.

DISPLAY "You are currently using a" termtype NO-LABEL "terminal"
      WITH FRAME d1 CENTERED ROW 5.
