DEFINE VARIABLE mark-start   AS DECIMAL NO-UNDO.
DEFINE VARIABLE mark-finish  AS DECIMAL NO-UNDO.
DEFINE VARIABLE units        AS LOGICAL FORMAT "miles/kilometers" NO-UNDO.

FORM
   mark-start  LABEL "Mile marker for highway on-ramp" SKIP
   mark-finish LABEL "Mile marker next to your exit" SKIP(1)
   units LABEL "Measure in <m>iles or <k>ilometers" SKIP(1)
   WITH FRAME question SIDE-LABELS
   TITLE "This program calculates distance driven.".
   
UPDATE mark-start mark-finish units WITH FRAME question.

DISPLAY
  "You have driven" ABSOLUTE(mark-start - mark-finish) units
  WITH NO-LABELS frame answer.
