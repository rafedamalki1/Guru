/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE X-Coord AS INTEGER INITIAL 1 VIEW-AS SLIDER 
    MIN-VALUE 1 MAX-VALUE 10 HORIZONTAL SIZE-CHARS 10 BY 2.
DEFINE VARIABLE Y-Coord AS INTEGER INITIAL 1 VIEW-AS SLIDER 
    MIN-VALUE 1 MAX-VALUE 10 VERTICAL SIZE-CHARS 8 BY 10.
DEFINE VARIABLE Point AS CHARACTER INITIAL "X"  FORMAT "x" 
    VIEW-AS TEXT.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    X-Coord NO-LABEL AT ROW 1 COLUMN 9 SKIP
    Y-Coord NO-LABEL
    btn-Exit AT ROW 14 COLUMN 1
    Point NO-LABEL AT ROW 12 COLUMN 9
        WITH CENTERED.

/**********  DEFINE TRIGGERS  **********/
ON VALUE-CHANGED OF X-Coord, Y-Coord
DO:
    ASSIGN X-Coord 
           Y-Coord
           Point:COL = (X-Coord:COL) + (X-Coord - 1)
                       /* (Starting Position) + (Offset) */
           Point:ROW = (Y-Coord:ROW + Y-Coord:HEIGHT - 1) - (Y-Coord - 1).
                       /* (Starting Position) - (Offset) */
END.   

/**********  MAIN LOGIC  **********/
DISPLAY X-Coord Y-Coord Point WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
