/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Months AS INTEGER EXTENT 12 
    LABEL "January", "February", "March", "April", "May", 
          "June", "July", "August", "September", "October", 
          "November", "December" 
    INITIAL [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31].
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    Months COLON 11 SKIP(1)
    btn-Exit
        WITH SIDE-LABELS NO-BOX CENTERED THREE-D.
        
/**********  DEFINE TRIGGERS  **********/
ON ENTRY OF Months
DO:
    MESSAGE SELF:LABEL "has" SELF:SCREEN-VALUE "days." 
            "The cursor is in array element number" SELF:INDEX.  
END.

/**********  MAIN LOGIC  **********/
DISPLAY Months WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


