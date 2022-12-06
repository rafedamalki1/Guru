/* Solution to Language Tutorial Problem 5-6. */

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Holidays AS DATE EXTENT 7
 LABEL "New Year's Day", "Presidents Day", "Memorial Day", 
    "4th of July", "Labor Day", "Thanksgiving", "Christmas" 
 INITIAL [1/1/93, 2/14/93, 5/27/93, 7/4/93, 9/6/93, 11/25/93, 12/25/93].
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    Holidays COLON 16 SKIP(1)
    btn-Exit
        WITH SIDE-LABELS NO-BOX CENTERED THREE-D.
        
/**********  MAIN LOGIC  **********/
DISPLAY Holidays WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


