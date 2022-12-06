/* lt-05-01.p */

/************* DEFINE WIDGETS ****************/
DEFINE VARIABLE months AS INTEGER EXTENT 12
   LABEL "January", "February", "March", "April", "May",
      "June", "July", "August", "September", "October", 
      "November", "December"
   INITIAL [31, 28, 30, 31, 30, 31, 30, 31, 30, 31, 30, 31].
DEFINE BUTTON BTN-EXIT LABEL "Exit".

/************* DEFINE FRAMES *****************/
DEFINE FRAME Frame1
   months COLON 5 SKIP(1) 
   BTN-EXIT
      WITH SIDE-LABELS NO-BOX CENTERED THREE-D.

/************* DEFINE TRIGGERS ***************/
ON ENTRY OF months 
DO:
   MESSAGE SELF:LABEL "has" SELF:SCREEN-VALUE "days."
      "The cursor is an array element number" SELF:INDEX.
END.

/*************** MAIN LOGIC ******************/
DISPLAY months WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF BTN-EXIT.
