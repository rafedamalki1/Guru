/**********  DEFINE WIDGETS  **********/
DEFINE BUTTON btn-Up    LABEL " Up ".
DEFINE BUTTON btn-Down  LABEL "Down".
DEFINE BUTTON btn-Right LABEL "Right".
DEFINE BUTTON btn-Left  LABEL "Left".
DEFINE BUTTON btn-Reset LABEL "Reset".
DEFINE BUTTON btn-Exit  LABEL "Exit".
/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    btn-Reset AT ROW  8 COLUMN 1
    btn-Up    AT ROW 10 COLUMN 8
    btn-Down  AT ROW 12 COLUMN 8
    btn-Left  AT ROW 11 COLUMN 1
    btn-Right AT ROW 11 COLUMN 15
    btn-Exit  AT ROW 13 COLUMN 1
        WITH SIZE-CHARS 40 BY 14 CENTERED THREE-D.
/**********  DEFINE TRIGGERS  **********/    
ON CHOOSE OF btn-Reset IN FRAME Frame1 DO:
    ASSIGN btn-Reset:ROW = 8
           btn-Reset:COL = 1.
END.           
ON CHOOSE OF btn-Up DO:
    IF btn-Reset:ROW <> 1 THEN
        ASSIGN btn-Reset:ROW = btn-Reset:ROW - 1.   
END.  
ON CHOOSE OF btn-Down DO:
    IF btn-Reset:ROW <> 8 THEN
        ASSIGN btn-Reset:ROW = btn-Reset:ROW + 1. 
END.
ON CHOOSE OF btn-Right DO:
    IF btn-Reset:COL <> 30 THEN
        ASSIGN btn-Reset:COL = btn-Reset:COL + 1.
END.
ON CHOOSE OF btn-Left DO:
    IF btn-Reset:COL <> 1 THEN
        ASSIGN btn-Reset:COL = btn-Reset:COL - 1.
END.   
/**********  MAIN LOGIC  **********/
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit. 


  
