DEFINE VARIABLE mark AS INTEGER.
DEFINE VARIABLE line-width AS INTEGER.
DEFINE VARIABLE paragraph AS CHARACTER.

paragraph = "The course centers around an existing small "
          + "application that you modify to improve perfo"
          + "rmance. Our highly-qualified instructors dem"
          + "onstrate proven analysis and coding techniqu"
          + "es and provide tips for making the most of y"
          + "our PROGRESS code. You are encouraged to bri"
          + "ng your own application problems to class an"
          + "d actively participate in class discussions "
          + "and hands-on lab exercises.".

SET line-width LABEL "Justify with how many character wide?"
   VALIDATE(line-width >= 20 AND line-width <= 70,
            "Must be between 20 and 70 for this example.")
   WITH SIDE-LABELS FRAME ask.

FORM
   paragraph FORMAT "x(72)"
   WITH DOWN NO-LABELS USE-TEXT.

DISPLAY "L" + FILL("-", line-width - 2) + "R" @ paragraph.
DOWN.

DO WHILE LENGTH(paragraph) > line-width:
   mark = R-INDEX(paragraph, " ", line-width).
   DISPLAY SUBSTR(paragraph, 1, mark) @ paragraph.
   DOWN.
   paragraph = SUBSTR(paragraph, mark + 1).
END.

IF paragraph <> ""
THEN DISPLAY paragraph.
