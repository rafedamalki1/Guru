DEFINE VARIABLE test AS CHARACTER INITIAL "Now is the time" FORMAT "x(30)".

DISPLAY test VIEW-AS TEXT
   LABEL "Labels cannot be changed"
   WITH FRAME a SIDE-LABELS.
   
PAUSE.

UPDATE test VIEW-AS FILL-IN
   LABEL "But fillins can, please enter a new value"
   WITH FRAME b SIDE-LABELS.
   
UPDATE test VIEW-AS EDITOR
   INNER-CHARS 16 INNER-LINES 2 MAX-CHARS 70
   LABEL  "As can editors, please enter a new value:"
   WITH FRAME c. 
   
DISPLAY test VIEW-AS TEXT FORMAT "x(70)"
   LABEL "The final value is:"
   WITH FRAME d.
