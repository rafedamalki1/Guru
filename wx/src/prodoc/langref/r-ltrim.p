/* r-ltrim.p */

DEFINE BUTTON b_left LABEL "Left Trim".
DEFINE BUTTON b_right LABEL "Right Trim".
DEFINE BUTTON b_trim  LABEL "Trim". 
DEFINE BUTTON b_quit  LABEL "Quit" AUTO-ENDKEY. 
  
DEFINE VARIABLE i AS INTEGER NO-UNDO.
 
DEFINE VARIABLE txt AS CHARACTER FORMAT "X(26)" INIT
  "***** This is a test *****".
  
DEFINE FRAME butt-frame
  txt i LABEL "String Length" SKIP(2)
  b_left b_right b_trim b_quit
WITH CENTERED TITLE "Original Text String".
 
DEFINE FRAME trimed-frame
  txt LABEL "Trimed Text"
  i   LABEL "Length"
WITH CENTERED.

ON CHOOSE OF b_trim, b_right, b_left IN FRAME butt-frame 
DO:
  FRAME trimed-frame:TITLE  = "Data After " + SELF:LABEL.
  DISPLAY TRIM(txt, "* ") WHEN SELF:LABEL = "Trim" @  txt
    LENGTH(TRIM(txt, "* ")) WHEN SELF:LABEL = "Trim" @ i
    LEFT-TRIM(txt,"* ") WHEN SELF:LABEL = "Left Trim"  @ txt
    LENGTH(LEFT-TRIM(txt,"* ")) WHEN SELF:LABEL = "Left Trim" @ i
    RIGHT-TRIM(txt, "* ") WHEN SELF:LABEL = "Right Trim" @ txt
    LENGTH(RIGHT-TRIM(txt, "* ")) WHEN SELF:LABEL = "Right Trim"  @ i
  WITH FRAME trimed-frame.
END.
       
ENABLE b_left b_right b_trim b_quit WITH FRAME butt-frame.

i = LENGTH(txt).
DISPLAY txt i WITH FRAME butt-frame.

WAIT-FOR CHOOSE OF b_quit IN FRAME butt-frame.
