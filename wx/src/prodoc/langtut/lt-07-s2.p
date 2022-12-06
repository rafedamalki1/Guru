 /* Solution to Language Tutorial Problem 7-2 */

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Mask AS INTEGER INITIAL 1 VIEW-AS RADIO-SET
  RADIO-BUTTONS "Basic Round", 1, "Basic Square", 2, "Deluxe Square", 3.
DEFINE VARIABLE Snorkel AS INTEGER INITIAL 1 VIEW-AS RADIO-SET
  RADIO-BUTTONS "Plastic", 1, "Soft Rubber", 2, "Rubber && Valve", 3.
DEFINE VARIABLE Fins AS INTEGER INITIAL 1 VIEW-AS RADIO-SET
  RADIO-BUTTONS "Plastic", 1, "Rubber", 2, "Flex Rubber", 3.
DEFINE VARIABLE Option1 AS Logical LABEL "Include Safety Manual?"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Option2 AS Logical LABEL "Include Fish ID Chart?"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Option3 AS Logical LABEL "Include Tote Bag?"
  VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE P-code AS CHARACTER LABEL "Package Code" INITIAL "111000".
DEFINE BUTTON btn-Exit LABEL "Exit".             

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
  "Select One from Each Category:" AT ROW  2 COLUMN  2
  Mask                             AT ROW  3 COLUMN  2
  Snorkel                          AT ROW  3 COLUMN 27
  Fins                             AT ROW  3 COLUMN 56
  "Options:"                       AT ROW  6 COLUMN  2
  Option1                          AT ROW  7 COLUMN  2
  Option2                          AT ROW  8 COLUMN  2
  Option3                          AT ROW  9 COLUMN  2
  P-code                           AT ROW 11 COLUMN  2
  btn-Exit                         AT ROW 13 COLUMN  2
   WITH SIDE-LABELS TITLE "Snorkel Value Packages" CENTERED ROW 2 THREE-D.
        
/**********  DEFINE TRIGGERS  **********/
ON VALUE-CHANGED OF Mask, Snorkel, Fins, Option1, Option2, Option3
DO:
    ASSIGN Mask
           Snorkel 
           Fins
           Option1
           Option2 
           Option3
           P-code = STRING(Mask) + STRING(Snorkel) + STRING(Fins) +
                    (IF Option1 THEN "1" ELSE "0") +
                    (IF Option2 THEN "1" ELSE "0") +
                    (IF Option3 THEN "1" ELSE "0").
    DISPLAY P-code WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
DISPLAY Mask Snorkel Fins Option1 Option2 Option3 P-Code WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit IN FRAME Frame1.



